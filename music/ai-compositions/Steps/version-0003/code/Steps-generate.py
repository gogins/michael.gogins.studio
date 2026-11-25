#!/usr/bin/env python3
"""Generate Steps.csd.

Five voices hocket in 5/8. An eighth note is 1/5 second. Voice 1 sounds on
the first eighth of each bar and then rests for four; each later voice is
one eighth later. On the time-pitch graph one eighth has the same length as
one octave, so a voice at angle θ moves tan(θ) octaves per eighth.

Each voice starts on a tone of a root-position 9th chord and moves toward
MIDI 96, reflecting between MIDI 24 and MIDI 96. At every sounding eighth
the path is quantized to the nearest chord tone. When voice 1 has reached
MIDI 24 three times, the chord changes and every voice starts again from
the new root-position chord, aimed upward. The chords are C major 9,
D minor 9, G 9, A minor 9, B half-diminished 9 (B D F A C#), and E major 9,
then the same cycle until six minutes.
"""

import math
import os

NAME = os.environ.get("STUDIO_COMPOSITION", "Steps")
SEED = os.environ.get("STUDIO_SEED", "")

FLOOR = 24
CEIL = 96
SPAN = CEIL - FLOOR
PERIOD = 2 * SPAN
EIGHTHS = 360 * 5  # 0.2-second eighths in six minutes
ANGLES = (7, 14, 21, 28, 35)
VELOCITY = 86

# Root-position voicings. The opening C major 9th starts at MIDI 48.
# B half-diminished 9th is B, D, F, A, C#.
CHORDS = (
    {"name": "Cmaj9", "voices": (48, 52, 55, 59, 62), "pcs": (0, 2, 4, 7, 11)},
    {"name": "Dm9", "voices": (50, 53, 57, 60, 64), "pcs": (0, 2, 4, 5, 9)},
    {"name": "G9", "voices": (55, 59, 62, 65, 69), "pcs": (2, 5, 7, 9, 11)},
    {"name": "Am9", "voices": (57, 60, 64, 67, 71), "pcs": (0, 4, 7, 9, 11)},
    {"name": "Bhd9", "voices": (59, 62, 65, 69, 73), "pcs": (1, 2, 5, 9, 11)},
    {"name": "Emaj9", "voices": (52, 56, 59, 63, 66), "pcs": (3, 4, 6, 8, 11)},
)


def semitones_per_eighth(angle):
    """One eighth on the graph equals one octave, so tan(θ) octaves per eighth."""
    return 12.0 * math.tan(math.radians(angle))


def eighths_until_third_floor(root):
    """How far voice 1 travels from `root`, upward, before its third arrival at 24."""
    travel = (3 * PERIOD) - (root - FLOOR)
    return travel / semitones_per_eighth(ANGLES[0])


def triangle(start, angle, eighths):
    """Pitch and direction (+1 up, -1 down) after `eighths` of motion."""
    u = (start - FLOOR) + semitones_per_eighth(angle) * eighths
    x = math.fmod(u, PERIOD)
    if x < 0:
        x += PERIOD
    if x <= SPAN:
        return FLOOR + x, 1
    return CEIL - (x - SPAN), -1


def quantize(pitch, pcs, direction):
    """Nearest MIDI note from 24 to 96 whose pitch class is in the chord."""
    chord = [tone for tone in range(FLOOR, CEIL + 1) if (tone % 12) in pcs]
    best = min(abs(tone - pitch) for tone in chord)
    ties = [tone for tone in chord if abs(abs(tone - pitch) - best) < 1e-9]
    if len(ties) == 1:
        return ties[0]
    return max(ties) if direction > 0 else min(ties)


def pitch_class_set(pcs):
    return sum(1 << pc for pc in pcs)


def seconds(eighth_index):
    whole, rem = divmod(eighth_index, 5)
    if rem == 0:
        return str(whole)
    return f"{whole}.{rem * 2}"


def segments():
    """Chord regions. Each full region ends on voice 1's third return to MIDI 24."""
    start = 0.0
    index = 0
    built = []
    while start < EIGHTHS - 1e-6:
        chord = CHORDS[index % len(CHORDS)]
        length = eighths_until_third_floor(chord["voices"][0])
        end = min(float(EIGHTHS), start + length)
        built.append((start, end, chord, end >= EIGHTHS - 1e-6))
        if end >= EIGHTHS - 1e-6:
            break
        start = end
        index += 1
    return built


def chord_at(regions, eighth):
    for start, end, chord, _partial in regions:
        if start - 1e-9 <= eighth < end - 1e-9:
            return start, chord
    start, _end, chord, _partial = regions[-1]
    return start, chord


regions = segments()
names = [region[2]["name"] for region in regions]
if "Emaj9" not in names:
    raise SystemExit("the progression never reaches E major 9th: " + ", ".join(names))

events = []
for eighth in range(EIGHTHS):
    voice = eighth % 5
    start, chord = chord_at(regions, eighth)
    pitch, direction = triangle(chord["voices"][voice], ANGLES[voice], eighth - start)
    key = quantize(pitch, set(chord["pcs"]), direction)
    if not FLOOR <= key <= CEIL or (key % 12) not in chord["pcs"]:
        raise SystemExit(f"note {key} is outside the chord at eighth {eighth}")
    events.append((voice, eighth, key, pitch_class_set(chord["pcs"])))

instruments = []
for number in range(1, 6):
    instruments.append(
        f"""instr {number}
  iHz = cpsmidinn(p4)
  iAmp = ampdbfs(-18 + 8 * (p5 / 127))
  iPan = limit(p7, 0, 1)
  iPhase = frac(p9)
  iPitches = p10
  iHomogeneity = p11
  aRise linseg 0, 0.003, 1
  aFall expon 1, p3, 0.001
  aTone poscil3 0.78, iHz, giSine, iPhase
  aBright poscil3 0.22, iHz * 2, giSine, iPhase
  aLeft, aRight pan2 (aTone + aBright) * aRise * aFall * iAmp, iPan
  out aLeft * 0.9, aRight * 0.9
  gaLeft += aLeft * 0.16
  gaRight += aRight * 0.16
endin"""
    )

pans = ("0.12", "0.31", "0.5", "0.69", "0.88")
phases = ("0", "0.2", "0.4", "0.6", "0.8")
gm = "\n".join(f"; gm {number} 1" for number in range(1, 6))
score = [f"i 99 0 {seconds(EIGHTHS)}"]
for voice, eighth, key, pcs in events:
    score.append(
        "i {instrument} {start} 0.2 {key} {velocity} 0.35 {pan} 0.5 {phase} {pcs} 1".format(
            instrument=voice + 1,
            start=seconds(eighth),
            key=key,
            velocity=VELOCITY,
            pan=pans[voice],
            phase=phases[voice],
            pcs=pcs,
        )
    )
score.append(f"e {seconds(EIGHTHS)}")

orchestra = """sr = 48000
ksmps = 32
nchnls = 2
0dbfs = 1

; Steps. Five voices in 5/8, six minutes, eighth note = 1/5 second.
; Voice n sounds on eighth n-1 of each bar (MIDI channel n-1) and rests for
; the other four. One eighth on the time-pitch graph equals one octave, so
; the angles 7, 14, 21, 28, and 35 degrees are the slopes of the five lines.
; Each line reflects between MIDI 24 and MIDI 96. A sounding eighth is the
; nearest tone of the current 9th chord. After voice 1 has returned to MIDI
; 24 three times, the chord changes and the lines start again from that
; chord, aimed upward: Cmaj9, Dm9, G9, Am9, B half-diminished 9, Emaj9, and
; then the same cycle.

gaLeft init 0
gaRight init 0
giSine ftgen 0, 0, 65536, 10, 1

""" + "\n\n".join(instruments) + """

instr 99
  aLeft, aRight reverbsc gaLeft, gaRight, 0.62, 7000
  out aLeft * 0.35, aRight * 0.35
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

print(f"{len(events)} notes, {len(regions)} chord regions")
for start, end, chord, partial in regions:
    print(
        f"  {chord['name']} {start / 5:.3f}s .. {end / 5:.3f}s"
        + (" (cut off at six minutes)" if partial else "")
    )
if SEED:
    print(f"seed {SEED} (the score does not use it)")
