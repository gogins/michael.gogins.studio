"""General MIDI files from a Csound score.

Score times are seconds, which is Csound's default (tempo 60, one beat per
second). The file is a Standard MIDI File, format 0, at 120 beats per minute
and 480 ticks per quarter note, so those seconds play back at the same length.
Only notes with a MIDI key (p4) and a velocity (p5) are written. A reverb
event such as ``i 99 0 120`` has neither, so it is left out.

An instrument uses General MIDI program N when its number is N, for N from
1 to 128. A line anywhere in the .csd overrides that::

    ; gm 1 49
    ; gm 10 drums

The program is the musician's number, 1 through 128. ``drums`` puts that
instrument on channel 10. Pan is p7, 0 at the left and 1 at the right.
"""
from __future__ import annotations

import re
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple

from . import fsutil, naming, staging
from .paths import Studio, StudioError

#: Ticks per quarter note, and the tempo that makes one second 960 ticks.
PPQ = 480
TEMPO_BPM = 120
TICKS_PER_SECOND = PPQ * TEMPO_BPM // 60

#: Melodic channels, 0-based. Channel 9 (MIDI channel 10) is reserved for drums.
MELODIC_CHANNELS = [0, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15]
DRUM_CHANNEL = 9

_SCORE = re.compile(r"<CsScore>(.*?)</CsScore>", re.IGNORECASE | re.DOTALL)
_GM = re.compile(
    r";\s*gm\s+([0-9]+(?:\.[0-9]+)?)\s+(drums|[0-9]+)\b",
    re.IGNORECASE,
)
_NOTE = re.compile(
    r"^i\s*([0-9]+(?:\.[0-9]+)?)"
    r"\s+([+-]?[0-9]*\.?[0-9]+)"
    r"\s+([+-]?[0-9]*\.?[0-9]+)"
    r"\s+([+-]?[0-9]*\.?[0-9]+)"
    r"\s+([+-]?[0-9]*\.?[0-9]+)"
    r"(?:\s+[+-]?[0-9]*\.?[0-9]+)?"
    r"(?:\s+([+-]?[0-9]*\.?[0-9]+))?"
)
_TEMPO = re.compile(r"^t\b(.*)$", re.MULTILINE)


class MidiError(StudioError):
    """The score cannot be written as General MIDI."""


def write_alongside(directory: Path, name: str) -> Dict[str, Any]:
    """Write ``outputs/<name>.mid`` from the .csd in this directory.

    Called whenever a render has a complete .csd, so the MIDI file is part of
    the same version. A score with no General MIDI notes removes a stale file
    and returns a warning instead of failing the render. The manifest is left
    for accept, which hashes whatever is in the directory.
    """
    directory = Path(directory)
    csd_path = directory / naming.csd_name(name)
    rel = f"{naming.OUTPUTS_DIR}/{name}.mid"
    destination = directory / rel
    if not csd_path.is_file():
        raise MidiError(f"missing {naming.csd_name(name)}")
    try:
        data, report = sequence_from_csd(csd_path.read_text(encoding="utf-8", errors="replace"))
    except MidiError as error:
        if destination.is_file():
            destination.unlink()
        return {"file": None, "notes": 0, "warnings": [str(error)], "manifest_updated": False}
    destination.parent.mkdir(parents=True, exist_ok=True)
    fsutil.atomic_write_bytes(destination, data)
    report["file"] = rel
    report["bytes"] = len(data)
    report["manifest_updated"] = False
    return report


def generate(studio: Studio, target: str) -> Dict[str, Any]:
    """Write ``outputs/<name>.mid`` for a staging directory or the current version.

    A staging directory is hashed when the version is accepted. The current
    version's manifest is updated so ``studio verify`` still matches. An
    archived version is left as it was.
    """
    directory, name, update_manifest = resolve_target(studio, target)
    return write_midi(directory, name, update_manifest=update_manifest)


def resolve_target(studio: Studio, target: str) -> Tuple[Path, str, bool]:
    try:
        directory = staging.resolve(studio, target)
    except staging.StagingError:
        directory = None
    if directory is not None:
        name = staging.load_meta(directory)["composition"]
        return directory, name, False
    naming.validate_composition_name(target)
    directory = studio.composition_dir(target)
    if not directory.is_dir() or not (directory / naming.csd_name(target)).is_file():
        raise MidiError(
            f"no staging directory or composition named {target!r}. "
            "Pass the staging path from 'studio new', or the composition's name "
            "for its current version."
        )
    return directory, target, True


def write_midi(directory: Path, name: str, *, update_manifest: bool) -> Dict[str, Any]:
    csd_path = Path(directory) / naming.csd_name(name)
    if not csd_path.is_file():
        raise MidiError(f"missing {naming.csd_name(name)}")
    data, report = sequence_from_csd(csd_path.read_text(encoding="utf-8", errors="replace"))
    rel = f"{naming.OUTPUTS_DIR}/{name}.mid"
    destination = Path(directory) / rel
    destination.parent.mkdir(parents=True, exist_ok=True)
    fsutil.atomic_write_bytes(destination, data)
    report["file"] = rel
    report["bytes"] = len(data)
    if update_manifest:
        manifest_path = Path(directory) / naming.manifest_name(name)
        if not manifest_path.is_file():
            raise MidiError(f"current version has no {manifest_path.name}")
        manifest = fsutil.read_json(manifest_path)
        manifest["files"].update(fsutil.hash_files(directory, [rel]))
        fsutil.atomic_write_json(manifest_path, manifest)
        report["manifest_updated"] = True
    else:
        report["manifest_updated"] = False
    return report


def sequence_from_csd(text: str) -> Tuple[bytes, Dict[str, Any]]:
    """Parse a .csd and return a General MIDI file plus a short report."""
    notes, assignments, warnings = parse_score(text)
    if not notes:
        raise MidiError(
            "no General MIDI notes. A note needs p4 (key) and p5 (velocity); "
            "mark a program with '; gm INSTR PROGRAM' or '; gm INSTR drums'."
        )
    data = encode_smf(notes, assignments)
    by_instr: Dict[str, Dict[str, Any]] = {}
    for note in notes:
        slot = by_instr.setdefault(note["instrument"], {
            "instrument": note["instrument"],
            "channel": note["channel"] + 1,
            "program": "drums" if note["drums"] else note["program"] + 1,
            "notes": 0,
        })
        slot["notes"] += 1
    duration = max(note["time"] + note["duration"] for note in notes)
    return data, {
        "notes": len(notes),
        "duration_seconds": round(duration, 3),
        "tempo_bpm": TEMPO_BPM,
        "ticks_per_quarter": PPQ,
        "instruments": [by_instr[key] for key in sorted(by_instr, key=_instr_sort)],
        "warnings": warnings,
    }


def parse_score(text: str) -> Tuple[List[Dict[str, Any]], Dict[str, Dict[str, Any]], List[str]]:
    warnings: List[str] = []
    programs, gm_warnings = _gm_directives(text)
    warnings.extend(gm_warnings)
    for match in _TEMPO.finditer(_score_body(text)):
        numbers = re.findall(r"[0-9]*\.?[0-9]+", match.group(1))
        tempos = [float(number) for number in numbers[1::2]]
        if not tempos and numbers:
            tempos = [float(numbers[0])]
        if any(tempo != 60 for tempo in tempos):
            # A t-statement lists time, tempo, time, tempo. Anything other
            # than 60 means a beat is not one second, and this writer still
            # treats p2 as seconds.
            warnings.append(
                "the score sets a tempo other than 60; p2 is still written as seconds"
            )
            break

    raw: List[Dict[str, Any]] = []
    skipped_carry = False
    skipped_range = False
    for line in _score_body(text).splitlines():
        body = line.split(";", 1)[0].strip()
        if not body or body[0] not in "iI":
            continue
        if re.search(r"(^|\s)\.(\s|$)", body):
            if not skipped_carry:
                warnings.append("a note uses Csound carry ('.') and was skipped")
                skipped_carry = True
            continue
        match = _NOTE.match(body)
        if not match:
            continue
        instrument, start, duration, key, velocity, pan = match.groups()
        start_f, duration_f = float(start), float(duration)
        key_f, velocity_f = float(key), float(velocity)
        if start_f < 0 or duration_f <= 0 or velocity_f <= 0:
            continue
        key_i = int(round(key_f))
        velocity_i = int(round(min(127, velocity_f)))
        if not 0 <= key_i <= 127 or not 1 <= velocity_i <= 127:
            if not skipped_range:
                warnings.append("a note is outside MIDI key 0–127 or velocity 1–127 and was skipped")
                skipped_range = True
            continue
        raw.append({
            "instrument": instrument,
            "time": start_f,
            "duration": duration_f,
            "key": key_i,
            "velocity": velocity_i,
            "pan": None if pan is None else _pan(float(pan)),
        })

    order = sorted(set(note["instrument"] for note in raw), key=lambda item: (
        min(note["time"] for note in raw if note["instrument"] == item),
        _instr_sort(item),
    ))
    assignments: Dict[str, Dict[str, Any]] = {}
    melodic = iter(MELODIC_CHANNELS)
    for instrument in order:
        spec = programs.get(instrument)
        if spec is None:
            number = _plain_instrument(instrument)
            if number is None or not 1 <= number <= 128:
                warnings.append(
                    f"instrument {instrument} has no General MIDI program; "
                    "add '; gm {0} PROGRAM'".format(instrument)
                )
                continue
            spec = {"drums": False, "program": number - 1}
        if spec["drums"]:
            channel = DRUM_CHANNEL
        else:
            channel = next(melodic, None)
            if channel is None:
                warnings.append(
                    f"instrument {instrument} was skipped: General MIDI has 15 melodic channels"
                )
                continue
        assignments[instrument] = {"channel": channel, **spec}

    notes = []
    for note in raw:
        assigned = assignments.get(note["instrument"])
        if assigned is None:
            continue
        notes.append({**note, **assigned})
    notes.sort(key=lambda note: (note["time"], note["channel"], note["key"]))
    return notes, assignments, warnings


def encode_smf(notes: List[Dict[str, Any]], assignments: Dict[str, Dict[str, Any]]) -> bytes:
    events: List[Tuple[int, int, bytes]] = [
        (0, 0, b"\xF0\x05\x7E\x7F\x09\x01\xF7"),  # GM System On
        (0, 1, bytes([0xFF, 0x51, 0x03, 0x07, 0xA1, 0x20])),  # 120 BPM
    ]
    seen_program = set()
    for instrument, spec in assignments.items():
        if spec["drums"] or spec["channel"] in seen_program:
            continue
        seen_program.add(spec["channel"])
        events.append((0, 2, bytes([0xC0 | spec["channel"], spec["program"]])))
    for note in notes:
        tick = _ticks(note["time"])
        end = _ticks(note["time"] + note["duration"])
        channel = note["channel"]
        if note["pan"] is not None:
            events.append((tick, 3, bytes([0xB0 | channel, 10, note["pan"]])))
        events.append((tick, 5, bytes([0x90 | channel, note["key"], note["velocity"]])))
        events.append((max(end, tick + 1), 4, bytes([0x80 | channel, note["key"], 0])))
    events.sort()
    return _chunk(b"MThd", b"\x00\x00\x00\x01" + PPQ.to_bytes(2, "big")) + _chunk(
        b"MTrk", _track(events)
    )


def events_from_smf(data: bytes) -> List[Dict[str, Any]]:
    """Decode a file written here. Used to check the writer."""
    if data[:4] != b"MThd" or len(data) < 14:
        raise MidiError("not a Standard MIDI File")
    header_len = int.from_bytes(data[4:8], "big")
    header = data[8:8 + header_len]
    if len(header) < 6:
        raise MidiError("short MIDI header")
    fmt = int.from_bytes(header[0:2], "big")
    tracks = int.from_bytes(header[2:4], "big")
    division = int.from_bytes(header[4:6], "big")
    if fmt != 0 or tracks != 1 or division != PPQ:
        raise MidiError("expected one track at 480 ticks per quarter note")
    cursor = 8 + header_len
    if data[cursor:cursor + 4] != b"MTrk":
        raise MidiError("missing MIDI track")
    length = int.from_bytes(data[cursor + 4:cursor + 8], "big")
    track = data[cursor + 8:cursor + 8 + length]
    events = []
    tick = 0
    status = 0
    index = 0
    while index < len(track):
        delta, index = _vlq(track, index)
        tick += delta
        if index >= len(track):
            break
        byte = track[index]
        if byte == 0xFF:
            kind = track[index + 1]
            size, index = _vlq(track, index + 2)
            payload = track[index:index + size]
            index += size
            if kind == 0x2F:
                break
            if kind == 0x51 and len(payload) == 3:
                events.append({"tick": tick, "tempo": int.from_bytes(payload, "big")})
            continue
        if byte == 0xF0 or byte == 0xF7:
            size, index = _vlq(track, index + 1)
            payload = track[index:index + size]
            index += size
            events.append({"tick": tick, "sysex": payload})
            continue
        if byte & 0x80:
            status = byte
            index += 1
        needed = 1 if (status & 0xF0) in (0xC0, 0xD0) else 2
        payload = track[index:index + needed]
        index += needed
        kind = status & 0xF0
        channel = status & 0x0F
        if kind in (0x90, 0x80):
            events.append({
                "tick": tick, "kind": "on" if kind == 0x90 and payload[1] else "off",
                "channel": channel, "key": payload[0], "velocity": payload[1],
            })
        elif kind == 0xC0:
            events.append({"tick": tick, "kind": "program", "channel": channel, "program": payload[0]})
        elif kind == 0xB0:
            events.append({
                "tick": tick, "kind": "cc", "channel": channel,
                "controller": payload[0], "value": payload[1],
            })
    return events


def _gm_directives(text: str) -> Tuple[Dict[str, Dict[str, Any]], List[str]]:
    found: Dict[str, Dict[str, Any]] = {}
    warnings: List[str] = []
    for match in _GM.finditer(text):
        instrument, spec = match.group(1), match.group(2)
        if instrument in found:
            warnings.append(f"instrument {instrument} has more than one '; gm' line; the last one is used")
        if spec.lower() == "drums":
            found[instrument] = {"drums": True, "program": 0}
            continue
        program = int(spec)
        if not 1 <= program <= 128:
            raise MidiError(f"'; gm {instrument} {spec}' is not a General MIDI program (1–128) or drums")
        found[instrument] = {"drums": False, "program": program - 1}
    return found, warnings


def _score_body(text: str) -> str:
    match = _SCORE.search(text)
    return match.group(1) if match else text


def _plain_instrument(instrument: str) -> Optional[int]:
    if "." in instrument:
        return None
    return int(instrument)


def _instr_sort(instrument: str) -> Tuple[int, str]:
    try:
        return (int(float(instrument)), instrument)
    except ValueError:
        return (0, instrument)


def _pan(value: float) -> int:
    return max(0, min(127, int(round(value * 127))))


def _ticks(seconds: float) -> int:
    return max(0, int(round(seconds * TICKS_PER_SECOND)))


def _chunk(kind: bytes, payload: bytes) -> bytes:
    return kind + len(payload).to_bytes(4, "big") + payload


def _track(events: List[Tuple[int, int, bytes]]) -> bytes:
    out = bytearray()
    previous = 0
    for tick, _order, payload in events:
        out += _vlq_bytes(tick - previous)
        out += payload
        previous = tick
    out += _vlq_bytes(0)
    out += b"\xFF\x2F\x00"
    return bytes(out)


def _vlq_bytes(value: int) -> bytes:
    if value < 0:
        raise MidiError("negative MIDI time")
    pieces = [value & 0x7F]
    value >>= 7
    while value:
        pieces.append((value & 0x7F) | 0x80)
        value >>= 7
    return bytes(reversed(pieces))


def _vlq(data: bytes, index: int) -> Tuple[int, int]:
    value = 0
    while True:
        if index >= len(data):
            raise MidiError("truncated MIDI track")
        byte = data[index]
        index += 1
        value = (value << 7) | (byte & 0x7F)
        if not byte & 0x80:
            return value, index
