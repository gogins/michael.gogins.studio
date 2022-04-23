<CsoundSynthesizer>
<CsLicense>
H E A R I N G . C S D
Michael Gogins

Some basic lessons in hearing electroacoustically.
</CsLicense>
<CsOptions>
-m165 -d -RWfo hearing.wav
</CsOptions>
<CsInstruments>
sr = 48000
ksmps = 1
nchnls = 1
0dbfs = 2

; gir            ftgen ifn, itime, isize, igen, iarga [, iargb ] [...]
gi_Clicker_table ftgen 0,   0,     65536, 2,    1
instr Clicker
a_signal oscil 1, 1, gi_Clicker_table
out a_signal
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, active(p1)
endin

gi_Siner1024_table ftgen 0,   0,     1024, 10,    1
instr Siner1024
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, active(p1)
a_signal oscil 1, 440, gi_Siner1024_table
out a_signal
endin

gi_Siner8192_table ftgen 0,   0,     8192, 10,    1
instr Siner8192
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, active(p1)
a_signal oscil 1, 440, gi_Siner8192_table
out a_signal
endin

gi_Siner65536_table ftgen 0,   0,     65536, 10,    1
instr Siner65536
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, active(p1)
a_signal oscil 1, 440, gi_Siner65536_table
out a_signal
endin

gi_Sweeper_table ftgen 0,   0,     65536, 10,    1
instr Sweeper
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, active(p1)
k_frequency line 20, p3, 40000
a_signal poscil3 1, k_frequency, gi_Sweeper_table
out a_signal
endin

gi_Aliaser_table ftgen 0,   0,     65536, 10,    1
instr Aliaser
k_carrier init 1
k_modulator line 0, p3, 20
k_index init 5
a_signal foscili 1, 440, k_carrier, k_modulator, k_index, gi_Aliaser_table
out a_signal
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, active(p1)
endin

instr Convolver
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, active(p1)
endin
</CsInstruments>
<CsScore>
i 1   1 .5 0 0 
i 2 ^+2 1 0 0
i 3 ^+2 1 0 0
i 4 ^+2 1 0 0
i 5 ^+2 20 0 0
i 6 ^+21 20 0 0
</CsScore>
</CsoundSynthesizer>

