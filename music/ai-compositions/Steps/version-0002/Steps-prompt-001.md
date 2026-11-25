# Steps - prompt 001

<!-- recorded: 2026-11-24T20:15:15Z; source: user -->

Write a piece that consists of lines of notes reflecting between an upper and lower bound. 
The lower bound is MIDI key 24 and the upper bound is MIDI key 96.
Each line consists of a prime number of notes, starting with 13. There are N lines. Each line plays one eighth note followed by N-1 rests. 
The lines all start at the same from key 24 and ascend by scale steps in the major scale, when they reach the upper bound they reflect back down. 
Each line is on a different MIDI channel/Csound instrument starting with 1 for Csound and 0 for MIDI. 
The only other difference between the lines is that they ascend and descend at a different tempo (i.e. angle from time to pitch).
Make a new piece like this that runs for 6 minutes. The shortest line has the longest notes, the longest line has the shortest notes, but the different durations must be integer multiples of the shortest note.

Name this piece "Steps."
