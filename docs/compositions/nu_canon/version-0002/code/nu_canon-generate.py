"""Write nu_canon.csd: Nu-canon at the sixth, with a two-voice hocket after 60 s.

The leader is the snapshot in nu_canon-source.csd (Nu at the time this piece
was made). The comes is every note of that score except the reverb instrument,
transposed up a major sixth and delayed 23 seconds.

Hocket (onset >= HOCKET_START):
  Both canonic voices are already active (comes enters at DELAY=23), so N = 2.
  For each voice v (0 = leader, 1 = comes), take that voice's notes whose onset
  is >= HOCKET_START in time order as a sequence indexed i = 0, 1, 2, ...
  Keep a note when (i % N) == (v % N); drop the others as rests. Pitch,
  velocity, pan, depth and duration of kept notes are unchanged. Instrument 99
  (reverb) is never hocketed. The score end / reverb duration is extended if
  the last kept note rings past the previous end.
"""

import os
import re
import sys
from pathlib import Path

# Major sixth above the leader. In D Dorian that is the sixth of the mode.
INTERVAL = 9
DELAY = 23.0
HOCKET_START = 60.0
# Extra attenuation in dB. At Nu's own instrument levels this canon peaks
# near -1.2 dBFS with no clipped samples, so none is applied.
HEADROOM_DB = 0.0

LEADER_COMMENT = "; nu -- a short study: bells in D Dorian over a drone."
CANON_COMMENT = (
    "; nu_canon -- Nu, with a canon at the major sixth entering after 23 seconds.\n"
    ";\n"
    "; The leader is the Nu score, at its original pitch and time. The comes is\n"
    "; the same music transposed up a major sixth (9 semitones) and delayed 23\n"
    "; seconds, panned to the other side. A centered note is answered on the right.\n"
    ";\n"
    "; From t = 60 s each canonic voice becomes a hocket. With both voices\n"
    "; sounding, N = 2: voice v keeps notes whose index i in that voice's\n"
    "; post-60 sequence satisfies (i % N) == (v % N), so the voices interlock\n"
    "; offset by one note. Dropped notes are rests; reverb (i 99) is untouched."
)


def fmt(value):
    rounded = round(float(value), 6)
    if abs(rounded - round(rounded)) < 1e-9:
        return str(int(round(rounded)))
    return f"{rounded:.6f}".rstrip("0").rstrip(".")


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

    # Build leader and comes with full fields; comes delayed and transposed.
    leader_notes = [list(fields) for fields in source_notes]
    comes_notes = []
    for fields in source_notes:
        comes = list(fields)
        comes[1] += DELAY
        comes[3] += INTERVAL
        if len(comes) >= 7:
            comes[6] = canon_pan(comes[6])
        comes_notes.append(comes)

    # Split each voice at HOCKET_START. Before then: continuous; after: hocket.
    # At t >= HOCKET_START the comes has already entered (DELAY=23), so N = 2.
    n_voices = 2
    leader_pre = [n for n in leader_notes if n[1] < HOCKET_START]
    leader_post = [n for n in leader_notes if n[1] >= HOCKET_START]
    comes_pre = [n for n in comes_notes if n[1] < HOCKET_START]
    comes_post = [n for n in comes_notes if n[1] >= HOCKET_START]

    leader_kept = leader_pre + hocket_voice(leader_post, voice_index=0, n_voices=n_voices)
    comes_kept = comes_pre + hocket_voice(comes_post, voice_index=1, n_voices=n_voices)

    last_sound = max(
        [end_time + DELAY]
        + [note_end(n) for n in leader_kept]
        + [note_end(n) for n in comes_kept]
    )
    new_end = max(end_time + DELAY, last_sound)

    score_lines = []
    score_lines.extend(header_lines)
    score_lines.append(f"i 99 0 {fmt(new_end)}\n")
    score_lines.append("\n; leader (Nu), continuous until 60 s, then hocket voice 0\n")
    for fields in leader_kept:
        score_lines.append(format_note(fields) + "\n")
    score_lines.append(
        "\n; comes at the major sixth (+23 s), continuous until 60 s, then hocket voice 1\n"
    )
    for fields in comes_kept:
        score_lines.append(format_note(fields) + "\n")
    score_lines.append(f"e {fmt(new_end)}\n")

    return (
        orchestra + "".join(score_lines) + tail,
        len(source_notes),
        len(leader_post),
        len(leader_kept) - len(leader_pre),
        len(comes_post),
        len(comes_kept) - len(comes_pre),
        new_end,
    )


def main():
    seed = int(os.environ.get("STUDIO_SEED", "1"))
    name = os.environ.get("STUDIO_COMPOSITION", "nu_canon")
    source = Path(__file__).resolve().parent / f"{name}-source.csd"
    (
        text,
        note_count,
        leader_post,
        leader_hocket,
        comes_post,
        comes_hocket,
        end_time,
    ) = build(source.read_text(encoding="utf-8"))
    destination = Path(f"{name}.csd")
    destination.write_text(text, encoding="utf-8")
    print(
        f"wrote {destination} from {source.name}: seed {seed}, "
        f"{note_count} leader notes, {note_count} canon notes, "
        f"major sixth +{INTERVAL}, delay {fmt(DELAY)} s, "
        f"hocket from {fmt(HOCKET_START)} s (N=2; "
        f"leader {leader_hocket}/{leader_post} kept, "
        f"comes {comes_hocket}/{comes_post} kept), "
        f"end {fmt(end_time)} s"
    )


if __name__ == "__main__":
    main()
