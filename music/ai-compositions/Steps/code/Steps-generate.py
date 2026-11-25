#!/usr/bin/env python3
"""Generate Steps.csd.

Five voices hocket in 5/8. An eighth note is 1/10 second, and each sounding
note is a half note: four eighths, 2/5 second. Voice 1 sounds on the first
eighth of each bar; each later voice is one eighth later. On the time-pitch
graph one eighth has the same length as one octave, so a voice at angle θ
moves tan(θ) octaves per eighth.

Each voice starts on a tone of a root-position 9th chord and moves toward
MIDI 96, reflecting between MIDI 24 and MIDI 96. At every sounding eighth
the path is quantized to the nearest chord tone. Each chord lasts until
voice 1 has completed two reflecting cycles. The harmony cadences in each
key, then modulates by the dominant of the next key: C major, D minor,
G major, A minor, E major, and back to C major.
"""

import math
import os

NAME = os.environ.get("STUDIO_COMPOSITION", "Steps")
SEED = os.environ.get("STUDIO_SEED", "")

FLOOR = 24
CEIL = 96
SPAN = CEIL - FLOOR
PERIOD = 2 * SPAN
EIGHTHS_PER_SECOND = 10  # double the previous 1/5-second eighth
EIGHTHS = 360 * EIGHTHS_PER_SECOND
HALF = 4  # a half note is four eighths
SAWTOOTH_CYCLES = 2
ANGLES = (7, 14, 21, 28, 35)
VELOCITY = 86

# Root-position voicings. The opening C major 9th starts at MIDI 48.
# Each chord is held for two sawtooth cycles, then the next chord in this
# cadence. A new key is entered by its dominant.
def chord(name, key, voices, pcs):
    return {"name": name, "key": key, "voices": voices, "pcs": pcs}


CMAJ9 = chord("Cmaj9", "C major", (48, 52, 55, 59, 62), (0, 2, 4, 7, 11))
G9 = chord("G9", "C major", (55, 59, 62, 65, 69), (2, 5, 7, 9, 11))
A9 = chord("A9", "D minor", (57, 61, 64, 67, 71), (1, 4, 7, 9, 11))
DM9 = chord("Dm9", "D minor", (50, 53, 57, 60, 64), (0, 2, 4, 5, 9))
D9 = chord("D9", "G major", (50, 54, 57, 60, 64), (0, 2, 4, 6, 9))
GMAJ9 = chord("Gmaj9", "G major", (55, 59, 62, 66, 69), (2, 6, 7, 9, 11))
E9 = chord("E9", "A minor", (52, 56, 59, 62, 66), (2, 4, 6, 8, 11))
AM9 = chord("Am9", "A minor", (57, 60, 64, 67, 71), (0, 4, 7, 9, 11))
B9 = chord("B9", "E major", (59, 63, 66, 69, 73), (1, 3, 6, 9, 11))
EMAJ9 = chord("Emaj9", "E major", (52, 56, 59, 63, 66), (3, 4, 6, 8, 11))

# I–V–I in C; V–i–V–i in D minor; V–I–V–I in G major;
# V–i–V–i in A minor; V–I–V–I in E major; V–I back to C.
PROGRESSION = (
    CMAJ9, G9, CMAJ9,
    A9, DM9, A9, DM9,
    D9, GMAJ9, D9, GMAJ9,
    E9, AM9, E9, AM9,
    B9, EMAJ9, B9, EMAJ9,
    G9, CMAJ9,
)


def semitones_per_eighth(angle):
    """One eighth on the graph equals one octave, so tan(θ) octaves per eighth."""
    return 12.0 * math.tan(math.radians(angle))


def eighths_until_floor(root):
    """Eighths from `root`, going up, until voice 1 has completed two cycles at MIDI 24."""
    travel = (SAWTOOTH_CYCLES * PERIOD) - (root - FLOOR)
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
    whole, rem = divmod(eighth_index, EIGHTHS_PER_SECOND)
    if rem == 0:
        return str(whole)
    return f"{whole}.{rem}"


def segments():
    """Chord regions. Each full region ends on voice 1's second return to MIDI 24."""
    start = 0.0
    index = 0
    built = []
    while start < EIGHTHS - 1e-6:
        chord = PROGRESSION[index % len(PROGRESSION)]
        length = eighths_until_floor(chord["voices"][0])
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
if "E major" not in [region[2]["key"] for region in regions]:
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
  iAmp = ampdbfs(-20 + 8 * (p5 / 127))
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
    duration = min(HALF, EIGHTHS - eighth)
    score.append(
        "i {instrument} {start} {duration} {key} {velocity} 0.35 {pan} 0.5 {phase} {pcs} 1".format(
            instrument=voice + 1,
            start=seconds(eighth),
            duration=seconds(duration),
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

; Steps. Five voices in 5/8, six minutes. An eighth note is 1/10 second.
; Each note is a half note, four eighths. Voice n attacks on eighth n-1 of
; each bar (MIDI channel n-1). One eighth on the time-pitch graph equals one
; octave, so the angles 7, 14, 21, 28, and 35 degrees are the slopes.
; Each line reflects between MIDI 24 and MIDI 96. The pitch at an attack is
; the nearest tone of the current chord. A chord lasts two reflecting cycles
; of voice 1. Each key is I–V–I, or i–V–i, and the next key is reached by
; its dominant: C major, D minor, G major, A minor, E major, then C major.

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
        f"  {chord['key']:12s} {chord['name']:6s} {start / EIGHTHS_PER_SECOND:.3f}s .. {end / EIGHTHS_PER_SECOND:.3f}s"
        + (" (cut off at six minutes)" if partial else "")
    )
if SEED:
    print(f"seed {SEED} (the score does not use it)")
