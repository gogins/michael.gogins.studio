#!/usr/bin/env python3
"""Generate Steps.csd.

Five lines, with 13, 17, 19, 23, and 29 notes. One pass of a line does not
last six minutes. Each pass starts on MIDI 24, climbs the C major scale to
MIDI 96, and turns back down. That pass repeats at the line's tempo until
six minutes. The 13-note line has the longest notes and the 29-note line
the shortest. Every length is an integer number of eighth notes, and an
eighth note is half a second. After each note the line rests for four
times that length.
"""

import os

NAME = os.environ.get("STUDIO_COMPOSITION", "Steps")
SEED = os.environ.get("STUDIO_SEED", "")

LOW = 24
HIGH = 96
PRIMES = (13, 17, 19, 23, 29)
MULTIPLIERS = (5, 4, 3, 2, 1)  # eighths; shortest line first
EIGHTH_TENTHS = 5              # 0.5 seconds
PIECE_TENTHS = 3600            # 6 minutes
N_LINES = len(PRIMES)
VELOCITY = 76
PITCH_CLASS_SET = 1 + 4 + 16 + 32 + 128 + 512 + 2048  # C major

SCALE = [midi for midi in range(LOW, HIGH + 1) if midi % 12 in (0, 2, 4, 5, 7, 9, 11)]
SPAN = len(SCALE) - 1


def seconds(tenths):
    whole, rem = divmod(tenths, 10)
    if rem == 0:
        return str(whole)
    return f"{whole}.{rem}"


def reflection_cycle(count):
    """One up-and-down on the scale, `count` notes, both bounds included."""
    peak = count // 2
    indices = []
    for i in range(count):
        if i <= peak:
            indices.append(round(SPAN * i / peak))
        else:
            indices.append(round(SPAN * (count - i) / (count - peak)))
    indices[0] = 0
    indices[peak] = SPAN
    for i in range(1, peak + 1):
        if indices[i] < indices[i - 1]:
            indices[i] = indices[i - 1]
    for i in range(peak + 1, count):
        if indices[i] > indices[i - 1]:
            indices[i] = indices[i - 1]
    if indices[peak + 1] >= SPAN:
        raise SystemExit(f"a cycle of {count} does not turn at the top")
    if SCALE[indices[0]] != LOW or SCALE[indices[peak]] != HIGH:
        raise SystemExit(f"a cycle of {count} misses a bound")
    return [SCALE[index] for index in indices]


lines = []
for index, (prime, multiplier) in enumerate(zip(PRIMES, MULTIPLIERS)):
    duration = multiplier * EIGHTH_TENTHS
    cell = N_LINES * duration
    cycle = reflection_cycle(prime)
    events = []
    repetition = 0
    while True:
        for step, key in enumerate(cycle):
            start = (repetition * prime + step) * cell
            if start + duration > PIECE_TENTHS:
                break
            events.append((start, key))
        else:
            repetition += 1
            continue
        break
    if repetition < 1:
        raise SystemExit(f"the {prime}-note line does not complete one reflection")
    lines.append({
        "instrument": index + 1,
        "channel": index,
        "prime": prime,
        "multiplier": multiplier,
        "duration": duration,
        "repetitions": repetition,
        "events": events,
        "pan": ("0.12", "0.31", "0.5", "0.69", "0.88")[index],
        "phase": ("0", "0.2", "0.4", "0.6", "0.8")[index],
    })

if lines[0]["prime"] >= lines[-1]["prime"]:
    raise SystemExit("the first line must be the shortest")
if lines[0]["duration"] <= lines[-1]["duration"]:
    raise SystemExit("the shortest line must have the longest notes")

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
    for start, key in line["events"]:
        score.append(
            "i {instrument} {start} {duration} {key} {velocity} 0.4 {pan} 0.5 {phase} {pcs} 1".format(
                instrument=line["instrument"],
                start=seconds(start),
                duration=seconds(line["duration"]),
                key=key,
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

; Steps. Five lines reflect between MIDI 24 and MIDI 96 and repeat until
; six minutes. A line has a prime number of notes, 13 then 17, 19, 23, 29.
; One pass does not fill the six minutes. Instrument n is MIDI channel n-1.
; An eighth note is 0.5 seconds. The 13-note line sounds for 5 eighths, and
; each longer line is one eighth shorter, down to a single eighth. After a
; note, the line rests for four times that length.

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
        f"{line['prime']} notes x {line['repetitions']} "
        f"at {line['multiplier']} eighths ({len(line['events'])} attacks)"
        for line in lines
    )
)
if SEED:
    print(f"seed {SEED} (the score does not use it)")
