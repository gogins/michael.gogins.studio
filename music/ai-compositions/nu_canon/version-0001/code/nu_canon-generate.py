"""Write nu_canon.csd: Nu, plus the same music a major sixth higher at 23 seconds.

The leader is the snapshot in nu_canon-source.csd (Nu at the time this piece
was made). The comes is every note of that score except the reverb instrument,
transposed up a major sixth and delayed 23 seconds.
"""

import os
import re
import sys
from pathlib import Path

# Major sixth above the leader. In D Dorian that is the sixth of the mode.
INTERVAL = 9
DELAY = 23.0
# Extra attenuation in dB. At Nu's own instrument levels this canon peaks
# near -1.2 dBFS with no clipped samples, so none is applied.
HEADROOM_DB = 0.0

LEADER_COMMENT = "; nu -- a short study: bells in D Dorian over a drone."
CANON_COMMENT = (
    "; nu_canon -- Nu, with a canon at the major sixth entering after 23 seconds.\n"
    ";\n"
    "; The leader is the Nu score, at its original pitch and time. The comes is\n"
    "; the same music transposed up a major sixth (9 semitones) and delayed 23\n"
    "; seconds, panned to the other side. A centered note is answered on the right."
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


def canon_line(tokens):
    fields = [float(token) for token in tokens[1:]]
    # p1 instrument, p2 time, p3 duration, p4 key, p5 velocity, p6 depth, p7 pan
    fields[1] += DELAY
    fields[3] += INTERVAL
    if len(fields) >= 7:
        fields[6] = canon_pan(fields[6])
    return "i " + " ".join(fmt(field) for field in fields)


def build(source_text):
    if LEADER_COMMENT not in source_text:
        sys.exit("source score is not the expected Nu orchestra")
    score_at = source_text.index("<CsScore>")
    score_end = source_text.index("</CsScore>")
    orchestra = attenuate(source_text[:score_at].replace(LEADER_COMMENT, CANON_COMMENT, 1))
    body = source_text[score_at:score_end]
    tail = source_text[score_end:]

    leader = []
    notes = []
    end_time = None
    reverb_at = None
    for line in body.splitlines(keepends=True):
        tokens = line.split()
        if tokens[:1] == ["e"] and len(tokens) >= 2:
            end_time = float(tokens[1])
            leader.append(line)
            continue
        if tokens[:1] == ["i"] and len(tokens) >= 4 and float(tokens[1]) == 99:
            reverb_at = len(leader)
            leader.append(line)
            continue
        if tokens[:1] == ["i"] and len(tokens) >= 5:
            notes.append(tokens)
        leader.append(line)

    if end_time is None or reverb_at is None:
        sys.exit("source score has no end time or reverb instrument")
    if not notes:
        sys.exit("source score has no notes")

    new_end = end_time + DELAY
    leader[reverb_at] = f"i 99 0 {fmt(new_end)}\n"
    # `e` ends the score. It has to follow the canon notes, or Csound never plays them.
    rewritten = []
    for line in leader:
        tokens = line.split()
        if tokens[:1] == ["e"]:
            continue
        rewritten.append(line)

    canon = ["\n; canon at the major sixth, entering 23 seconds after the leader\n"]
    canon.extend(canon_line(tokens) + "\n" for tokens in notes)
    canon.append(f"e {fmt(new_end)}\n")
    return orchestra + "".join(rewritten) + "".join(canon) + tail, len(notes), new_end


def main():
    seed = int(os.environ.get("STUDIO_SEED", "1"))
    name = os.environ.get("STUDIO_COMPOSITION", "nu_canon")
    source = Path(__file__).resolve().parent / f"{name}-source.csd"
    text, note_count, end_time = build(source.read_text(encoding="utf-8"))
    destination = Path(f"{name}.csd")
    destination.write_text(text, encoding="utf-8")
    print(
        f"wrote {destination} from {source.name}: seed {seed}, "
        f"{note_count} leader notes, {note_count} canon notes, "
        f"major sixth +{INTERVAL}, delay {fmt(DELAY)} s, end {fmt(end_time)} s"
    )


if __name__ == "__main__":
    main()
