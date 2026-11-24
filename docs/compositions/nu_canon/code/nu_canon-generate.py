"""Write nu_canon.csd: Nu-canon at the sixth, hocket rests halved, ~6 min.

The leader is the snapshot in nu_canon-source.csd (Nu at the time this piece
was made), tiled forward until the finished score reaches about 360 seconds.
The comes is every note of that score except the reverb instrument,
transposed up a major sixth and delayed 23 seconds.

After leader+comes (tiled): bar-wise dissonance modulation. Bars are 4.0 s
windows from t = 0. If the next bar has dissonant pitch-class intervals
{1, 2, 6, 10, 11} between simultaneously sounding leader/comes bell notes
(instrument 1), try tonic shifts [-7, -5, -2, -1, 1, 2, 5, 7] from that next
bar onward (all voices). Choose the shift that minimizes the next-bar
dissonance count; if none improves, do not modulate. Accumulated shifts
stack forward. Drones/pads (instruments 2, 3) follow the tonic shift but
are not the dissonance probe.

Hocket (onset >= HOCKET_START):
  Both canonic voices are already active (comes enters at DELAY=23), so N = 2.
  For each voice v (0 = leader, 1 = comes), take that voice's notes whose onset
  is >= HOCKET_START in time order as a sequence indexed i = 0, 1, 2, ...
  Keep a note when (i % N) == (v % N); drop the others as rests. Then retime
  each voice's kept post-hocket notes so the inter-onset interval between
  consecutive kept notes is half the original interval between those same
  notes (first kept onset stays; later kept notes pull forward). Durations
  of kept notes are unchanged. Pitch, velocity, pan and depth of kept notes
  are unchanged (aside from modulation above). Instrument 99 (reverb) is
  never hocketed. Pre-hocket continuous music and DELAY stay unchanged.

After all pitch transforms, MIDI keys of sounding notes (instruments 1, 2, 3)
are reflected into [24, 96] inclusive: above 96 fold down about 96, below 24
fold up about 24, iterating until in range. Instrument 99 is unchanged.
"""

import os
import re
import sys
from pathlib import Path

# Major sixth above the leader. In D Dorian that is the sixth of the mode.
INTERVAL = 9
DELAY = 23.0
HOCKET_START = 60.0
TARGET_DURATION = 360.0
BAR = 4.0
DISSONANT_INTERVALS = frozenset({1, 2, 6, 10, 11})
MODULATION_CANDIDATES = (-7, -5, -2, -1, 1, 2, 5, 7)
# Extra attenuation in dB. At Nu's own instrument levels this canon peaks
# near -1.2 dBFS with no clipped samples, so none is applied.
HEADROOM_DB = 0.0

LEADER_COMMENT = "; nu -- a short study: bells in D Dorian over a drone."
CANON_COMMENT = (
    "; nu_canon -- Nu, with a canon at the major sixth entering after 23 seconds.\n"
    ";\n"
    "; The leader is the Nu score, tiled to about six minutes, at its original\n"
    "; pitch and time. The comes is the same music transposed up a major sixth\n"
    "; (9 semitones) and delayed 23 seconds, panned to the other side. A centered\n"
    "; note is answered on the right.\n"
    ";\n"
    "; Bar-wise (4 s) dissonance modulation: if the next bar has dissonant\n"
    "; pitch-class intervals between leader/comes bells, shift tonic from that\n"
    "; bar onward when a candidate shift reduces the dissonance count.\n"
    ";\n"
    "; From t = 60 s each canonic voice becomes a hocket. With both voices\n"
    "; sounding, N = 2: voice v keeps notes whose index i in that voice's\n"
    "; post-60 sequence satisfies (i % N) == (v % N), so the voices interlock\n"
    "; offset by one note. Dropped notes are rests; kept post-hocket onsets are\n"
    "; then retimed so consecutive kept IOIs are halved (hocket rests cut in\n"
    "; half, increasing tempo). Reverb (i 99) is untouched.\n"
    ";\n"
    "; After tiling, comes interval, dissonance modulation, and hocket, MIDI\n"
    "; keys of sounding notes (i 1, 2, 3) are reflected into [24, 96]."
)


def fmt(value):
    rounded = round(float(value), 6)
    if abs(rounded - round(rounded)) < 1e-9:
        return str(int(round(rounded)))
    return f"{rounded:.6f}".rstrip("0").rstrip(".")


def reflect_key(key, low=24.0, high=96.0):
    """Reflect a MIDI key into [low, high] about the nearer bound until in range.

    Above high: key := high - (key - high) = 2*high - key.
    Below low:  key := low + (low - key) = 2*low - key.
    """
    reflected = float(key)
    while reflected > high or reflected < low:
        if reflected > high:
            reflected = high - (reflected - high)
        else:
            reflected = low + (low - reflected)
    return reflected


def reflect_midi_keys(notes, low=24.0, high=96.0):
    """Reflect p4 of sounding notes into [low, high]; skip instrument 99."""
    out = []
    for fields in notes:
        note = list(fields)
        if int(round(note[0])) != 99:
            note[3] = reflect_key(note[3], low, high)
        out.append(note)
    return out


def canon_pan(pan):
    answered = 1.0 - pan
    if abs(answered - pan) < 1e-6:
        answered = min(1.0, pan + 0.22)
    return answered


def attenuate(orchestra):
    def repl(match):
        value = float(match.group(1)) - HEADROOM_DB
        return f"ampdbfs({fmt(value)} +"

    updated, count = re.subn(r"ampdbfs\((-?\d+(?:\.\d+)?) \+", repl, orchestra)
    if count != 3:
        sys.exit(f"expected to lower 3 instrument levels, changed {count}")
    return updated


def parse_note(tokens):
    """Return [instrument, time, duration, key, velocity, ...] as floats."""
    return [float(token) for token in tokens[1:]]


def format_note(fields):
    return "i " + " ".join(fmt(field) for field in fields)


def note_end(fields):
    return fields[1] + fields[2]


def hocket_voice(notes, voice_index, n_voices):
    """Keep every N-th note for voice v; drop the rest as rests.

    notes: list of field lists already filtered to onset >= HOCKET_START,
    in score order. N = n_voices (currently sounding canonic voices).
    Kept when (i % N) == (voice_index % N).
    """
    kept = []
    for index, fields in enumerate(notes):
        if (index % n_voices) == (voice_index % n_voices):
            kept.append(fields)
    return kept


def retime_hocket_kept(kept_notes):
    """Halve IOIs between consecutive kept notes; first onset unchanged.

    Durations stay the same; later kept notes pull forward.
    """
    if len(kept_notes) <= 1:
        return [list(fields) for fields in kept_notes]
    ordered = sorted((list(fields) for fields in kept_notes), key=lambda n: n[1])
    retimed = [ordered[0]]
    new_onset = ordered[0][1]
    for previous, current in zip(ordered, ordered[1:]):
        original_ioi = current[1] - previous[1]
        new_onset = new_onset + original_ioi * 0.5
        note = list(current)
        note[1] = new_onset
        retimed.append(note)
    return retimed


def tile_notes(source_notes, tile_span, tile_count):
    """Repeat source phrase blocks, advancing onset by each tile's span."""
    tiled = []
    for tile_index in range(tile_count):
        offset = tile_index * tile_span
        for fields in source_notes:
            note = list(fields)
            note[1] += offset
            tiled.append(note)
    return tiled


def pc_interval(key_a, key_b):
    return abs(int(round(key_a)) - int(round(key_b))) % 12


def note_sounds_in_window(fields, start, end):
    onset = fields[1]
    release = note_end(fields)
    return onset < end and release > start


def notes_overlap(a, b):
    return a[1] < note_end(b) and b[1] < note_end(a)


def shift_at_time(shifts, time):
    """Sum of tonic shifts whose from-time is <= time."""
    total = 0
    for from_time, semitones in shifts:
        if time >= from_time:
            total += semitones
    return total


def apply_shifts(notes, shifts):
    shifted = []
    for fields in notes:
        note = list(fields)
        note[3] += shift_at_time(shifts, note[1])
        shifted.append(note)
    return shifted


def count_bar_dissonances(leader_notes, comes_notes, shifts, bar_start, bar_end):
    """Count dissonant overlapping leader/comes bell (instr 1) pairs in a bar."""
    leader_bells = [
        n
        for n in apply_shifts(leader_notes, shifts)
        if int(round(n[0])) == 1 and note_sounds_in_window(n, bar_start, bar_end)
    ]
    comes_bells = [
        n
        for n in apply_shifts(comes_notes, shifts)
        if int(round(n[0])) == 1 and note_sounds_in_window(n, bar_start, bar_end)
    ]
    count = 0
    for leader in leader_bells:
        for comes in comes_bells:
            if not notes_overlap(leader, comes):
                continue
            # Overlap must meet the bar window.
            overlap_start = max(leader[1], comes[1], bar_start)
            overlap_end = min(note_end(leader), note_end(comes), bar_end)
            if overlap_start >= overlap_end:
                continue
            if pc_interval(leader[3], comes[3]) in DISSONANT_INTERVALS:
                count += 1
    return count


def modulate_to_avoid_dissonances(leader_notes, comes_notes):
    """Bar-wise: if next bar is dissonant, try tonic shifts from that bar on."""
    last_time = max(
        [0.0]
        + [note_end(n) for n in leader_notes]
        + [note_end(n) for n in comes_notes]
    )
    shifts = []
    bar_index = 0
    while (bar_index + 1) * BAR < last_time:
        next_start = (bar_index + 1) * BAR
        next_end = next_start + BAR
        current = count_bar_dissonances(
            leader_notes, comes_notes, shifts, next_start, next_end
        )
        if current > 0:
            best_shift = None
            best_count = current
            for candidate in MODULATION_CANDIDATES:
                trial = shifts + [(next_start, candidate)]
                trial_count = count_bar_dissonances(
                    leader_notes, comes_notes, trial, next_start, next_end
                )
                if trial_count < best_count:
                    best_count = trial_count
                    best_shift = candidate
            if best_shift is not None:
                shifts.append((next_start, best_shift))
        bar_index += 1
    return (
        apply_shifts(leader_notes, shifts),
        apply_shifts(comes_notes, shifts),
        shifts,
    )


def build_voices(tiled_notes):
    """Leader + comes from tiled notes, then modulate, then hocket+retime."""
    leader_notes = [list(fields) for fields in tiled_notes]
    comes_notes = []
    for fields in tiled_notes:
        comes = list(fields)
        comes[1] += DELAY
        comes[3] += INTERVAL
        if len(comes) >= 7:
            comes[6] = canon_pan(comes[6])
        comes_notes.append(comes)

    leader_notes, comes_notes, shifts = modulate_to_avoid_dissonances(
        leader_notes, comes_notes
    )

    n_voices = 2
    leader_pre = [n for n in leader_notes if n[1] < HOCKET_START]
    leader_post = [n for n in leader_notes if n[1] >= HOCKET_START]
    comes_pre = [n for n in comes_notes if n[1] < HOCKET_START]
    comes_post = [n for n in comes_notes if n[1] >= HOCKET_START]

    leader_post_kept = hocket_voice(leader_post, voice_index=0, n_voices=n_voices)
    comes_post_kept = hocket_voice(comes_post, voice_index=1, n_voices=n_voices)
    leader_post_retimed = retime_hocket_kept(leader_post_kept)
    comes_post_retimed = retime_hocket_kept(comes_post_kept)

    leader_kept = leader_pre + leader_post_retimed
    comes_kept = comes_pre + comes_post_retimed
    return (
        leader_kept,
        comes_kept,
        leader_post,
        leader_post_retimed,
        comes_post,
        comes_post_retimed,
        shifts,
    )


def build(source_text):
    if LEADER_COMMENT not in source_text:
        sys.exit("source score is not the expected Nu orchestra")
    score_at = source_text.index("<CsScore>")
    score_end = source_text.index("</CsScore>")
    orchestra = attenuate(source_text[:score_at].replace(LEADER_COMMENT, CANON_COMMENT, 1))
    body = source_text[score_at:score_end]
    tail = source_text[score_end:]

    header_lines = []
    source_notes = []
    end_time = None
    for line in body.splitlines(keepends=True):
        tokens = line.split()
        if tokens[:1] == ["e"] and len(tokens) >= 2:
            end_time = float(tokens[1])
            continue
        if tokens[:1] == ["i"] and len(tokens) >= 4 and float(tokens[1]) == 99:
            # Reverb is rewritten below; skip the source line.
            continue
        if tokens[:1] == ["i"] and len(tokens) >= 5:
            source_notes.append(parse_note(tokens))
            continue
        header_lines.append(line)

    if end_time is None:
        sys.exit("source score has no end time")
    if not source_notes:
        sys.exit("source score has no notes")

    tile_span = end_time
    tile_count = 1
    leader_kept = comes_kept = None
    leader_post = leader_hocket = comes_post = comes_hocket = None
    shifts = []
    new_end = 0.0

    while True:
        tiled = tile_notes(source_notes, tile_span, tile_count)
        (
            leader_kept,
            comes_kept,
            leader_post,
            leader_hocket_notes,
            comes_post,
            comes_hocket_notes,
            shifts,
        ) = build_voices(tiled)
        last_sound = max(
            [note_end(n) for n in leader_kept]
            + [note_end(n) for n in comes_kept]
        )
        new_end = last_sound
        if new_end >= TARGET_DURATION:
            break
        tile_count += 1
        if tile_count > 64:
            sys.exit("unable to reach target duration with tiling")

    # Final MIDI-key clamp: reflect outliers into [24, 96] (instr 1–3 only).
    leader_kept = reflect_midi_keys(leader_kept)
    comes_kept = reflect_midi_keys(comes_kept)

    score_lines = []
    score_lines.extend(header_lines)
    score_lines.append(f"i 99 0 {fmt(new_end)}\n")
    score_lines.append(
        "\n; leader (Nu), continuous until 60 s, then hocket voice 0 "
        "(kept IOIs halved)\n"
    )
    for fields in leader_kept:
        score_lines.append(format_note(fields) + "\n")
    score_lines.append(
        "\n; comes at the major sixth (+23 s), continuous until 60 s, then "
        "hocket voice 1 (kept IOIs halved)\n"
    )
    for fields in comes_kept:
        score_lines.append(format_note(fields) + "\n")
    score_lines.append(f"e {fmt(new_end)}\n")

    return (
        orchestra + "".join(score_lines) + tail,
        len(source_notes),
        tile_count,
        len(leader_post),
        len(leader_hocket_notes),
        len(comes_post),
        len(comes_hocket_notes),
        len(shifts),
        new_end,
    )


def main():
    seed = int(os.environ.get("STUDIO_SEED", "1"))
    name = os.environ.get("STUDIO_COMPOSITION", "nu_canon")
    source = Path(__file__).resolve().parent / f"{name}-source.csd"
    (
        text,
        note_count,
        tile_count,
        leader_post,
        leader_hocket,
        comes_post,
        comes_hocket,
        modulation_count,
        end_time,
    ) = build(source.read_text(encoding="utf-8"))
    destination = Path(f"{name}.csd")
    destination.write_text(text, encoding="utf-8")
    print(
        f"wrote {destination} from {source.name}: seed {seed}, "
        f"{note_count} source notes x {tile_count} tiles, "
        f"major sixth +{INTERVAL}, delay {fmt(DELAY)} s, "
        f"hocket from {fmt(HOCKET_START)} s (N=2; rests halved; "
        f"leader {leader_hocket}/{leader_post} kept, "
        f"comes {comes_hocket}/{comes_post} kept), "
        f"{modulation_count} bar-wise tonic shift(s), "
        f"MIDI keys reflected into [24, 96], "
        f"end {fmt(end_time)} s (~6 min target {fmt(TARGET_DURATION)})"
    )


if __name__ == "__main__":
    main()
