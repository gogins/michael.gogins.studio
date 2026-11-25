# Steps - prompt 003

<!-- recorded: 2026-11-25T19:12:06Z; source: user -->

Make a new piece titled “Steps.”

This piece will be for 5 Csound instruments 1-5, or 5 MIDI channels 0-6. It should be six minutes long.

The piece will be constructed by bouncing arpeggios between the bass floor of MIDI 24 and treble ceiling of MIDI 96.

Each voice will consist of 8th notes and rests. The first voice starts with an eighth note followed by 4 eighth rests. The second voice starts with a rest, then an eighth note, then 3 eighth rests. You can think of the meter as ⅝ or 10/8.

Although each voice consists of the same durations of rests and notes, they in fact behave differently in that the notes are derived by quantizing a “sawtooth” path reflecting between the bass floor and treble ceiling. The “sawtooth” pattern is defined by a starting point and an angle

The starting point is a root position C major 9th chord, assigned to voices 1, 2, 3, 4, and 5 starting at MIDI 48. From each tone, a line is projected towards the treble ceiling. Whenever that line intersects a pitch class of the chord, a note (or rest) is generated. When the line intersects the treble ceiling, it reflects back down towards the bass floor, still generating eighth notes or rests as it intersects the pitch-classes of the chord.

The angles for voices are 7, 14, 21, 28, and 35 degrees. When voice 1 has been reflected to the bass floor 3 times, the chord progresses to D minor 9th. Following the same rule, to G 9th, A minor 9th, B half-diminished 9th, and (modulating) to E major 9th; the same pattern is followed to the end of the piece. Interest arises from the “sawtooth” arpeggios occurring at different rates, and thus creating various hocketting effects with the notes and rests.
