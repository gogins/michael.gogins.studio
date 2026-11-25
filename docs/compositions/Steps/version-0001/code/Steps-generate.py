#!/usr/bin/env python3
"""Generate Steps.csd.

Five lines walk the C major scale from MIDI 24 to MIDI 96 and reflect.
Each sounding note is followed by four rests of the same length. The note
lengths are 5, 4, 3, 2, and 1 eighth notes of 0.3 seconds, so every length
is an integer multiple of the shortest note. The line with the fewest notes
has the longest notes. Every line plays for six minutes.

A single pass of the primes 13, 17, 19, 23, 29 is too short to reach MIDI 96
one scale step at a time, and those primes cannot all be the note counts if
the durations are integer multiples of the shortest note and every line fills
the same six minutes. The counts that do both are 48, 60, 80, 120, and 240.
"""

import os

NAME = os.environ.get("STUDIO_COMPOSITION", "Steps")
SEED = os.environ.get("STUDIO_SEED", "")

LOW = 24
HIGH = 96
N_LINES = 5
MULTIPLIERS = (5, 4, 3, 2, 1)  # eighths; shortest line first
EIGHTH_TENTHS = 3              # 0.3 seconds
PIECE_TENTHS = 3600            # 6 minutes
VELOCITY = 76
PITCH_CLASS_SET = 1 + 4 + 16 + 32 + 128 + 512 + 2048  # C major

SCALE = [midi for midi in range(LOW, HIGH + 1) if midi % 12 in (0, 2, 4, 5, 7, 9, 11)]
SPAN = len(SCALE) - 1
PERIOD = 2 * SPAN


def pitch(step):
    k = step % PERIOD
    index = k if k <= SPAN else PERIOD - k
    return SCALE[index]


def seconds(tenths):
    whole, rem = divmod(tenths, 10)
    if rem == 0:
        return str(whole)
    return f"{whole}.{rem}"


def notes_for(multiplier):
    cycle = N_LINES * multiplier * EIGHTH_TENTHS
    count = PIECE_TENTHS // cycle
    if count * cycle != PIECE_TENTHS:
        raise SystemExit(f"multiplier {multiplier} does not fill 6 minutes")
    return count, cycle


lines = []
for index, multiplier in enumerate(MULTIPLIERS):
    count, cycle = notes_for(multiplier)
    lines.append({
        "instrument": index + 1,
        "channel": index,
        "multiplier": multiplier,
        "count": count,
        "cycle": cycle,
        "duration": multiplier * EIGHTH_TENTHS,
        "pan": ("0.12", "0.31", "0.5", "0.69", "0.88")[index],
        "phase": ("0", "0.2", "0.4", "0.6", "0.8")[index],
    })

if lines[0]["count"] >= lines[-1]["count"]:
    raise SystemExit("the shortest line must have the fewest notes")
if any(pitch(step) == HIGH for step in range(lines[0]["count"])) is False:
    raise SystemExit("the shortest line never reaches the upper bound")

gm = "\n".join(f"; gm {line['instrument']} 12" for line in lines)
instruments = []
for line in lines:
    instruments.append(
        f"""instr {line['instrument']}
  iHz = cpsmidinn(p4)
  iAmp = ampdbfs(-27 + 8 * (p5 / 127))
  iPan = limit(p7, 0, 1)
  iPhase = frac(p9)
  iPitches = p10
  iHomogeneity = p11
  aRise linseg 0, 0.004, 1
  aFall expon 1, p3, 0.001
  aTone poscil3 0.72, iHz, giSine, iPhase
  aOctave poscil3 0.28, iHz * 2, giSine, iPhase
  aLeft, aRight pan2 (aTone + aOctave) * aRise * aFall * iAmp, iPan
  out aLeft * 0.82, aRight * 0.82
  gaLeft += aLeft * 0.22
  gaRight += aRight * 0.22
endin"""
    )

score = [f"i 99 0 {seconds(PIECE_TENTHS)}"]
for line in lines:
    for step in range(line["count"]):
        start = step * line["cycle"]
        score.append(
            "i {instrument} {start} {duration} {key} {velocity} 0.4 {pan} 0.5 {phase} {pcs} 1".format(
                instrument=line["instrument"],
                start=seconds(start),
                duration=seconds(line["duration"]),
                key=pitch(step),
                velocity=VELOCITY,
                pan=line["pan"],
                phase=line["phase"],
                pcs=PITCH_CLASS_SET,
            )
        )
score.append(f"e {seconds(PIECE_TENTHS)}")

orchestra = r"""sr = 48000
ksmps = 32
nchnls = 2
0dbfs = 1

; Steps. Five lines on the C major scale, reflecting between MIDI 24 and 96.
; Instrument n is MIDI channel n-1. General MIDI program 12 (vibraphone).
; An eighth note is 0.3 seconds. Line 1 sounds for 5 eighths and rests for
; 20; each later line is shorter by one eighth, down to a single eighth.
; One scale step per note. The upper and lower bounds reverse the direction.

gaLeft init 0
gaRight init 0
giSine ftgen 0, 0, 65536, 10, 1

""" + "\n\n".join(instruments) + r"""

instr 99
  aLeft, aRight reverbsc gaLeft, gaRight, 0.72, 8000
  out aLeft * 0.45, aRight * 0.45
  clear gaLeft, gaRight
endin
"""

csd = f"""<CsoundSynthesizer>
<CsOptions>
-d
</CsOptions>
<CsInstruments>
{orchestra}
</CsInstruments>
<CsScore>
{gm}
{chr(10).join(score)}
</CsScore>
</CsoundSynthesizer>
"""

with open(f"{NAME}.csd", "w", encoding="utf-8") as output:
    output.write(csd)

print(
    "Steps lines: "
    + ", ".join(
        f"instr {line['instrument']} ch {line['channel']} "
        f"{line['count']} x {line['multiplier']} eighths"
        for line in lines
    )
)
if SEED:
    print(f"seed {SEED} (the score does not use it)")
