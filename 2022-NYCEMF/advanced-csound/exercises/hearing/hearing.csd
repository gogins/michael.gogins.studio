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
gi_Clicker_table ftgen 0,   0,     65537, 2,    1
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

gi_Siner65536_table ftgen 0,   0,     65537, 10,    1
instr Siner65536
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, active(p1)
a_signal oscil 1, 440, gi_Siner65536_table
out a_signal
endin

gi_Sweeper_table ftgen 0,   0,     65537, 10,    1
instr Sweeper
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, active(p1)
k_frequency line 20, p3, 40000
a_signal poscil3 1, k_frequency, gi_Sweeper_table
out a_signal
endin

gi_Aliaser_table ftgen 0,   0,     65537, 10,    1
instr Aliaser
k_carrier init 1
k_modulator line 0, p3, 20
k_index init 5
a_signal foscili 1, 440, k_carrier, k_modulator, k_index, gi_Aliaser_table
out a_signal
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, active(p1)
endin

gi_OneGrain_sine ftgen 0,   0,     65537, 10,    1
gi_OneGrain_cosine ftgen 0,   0,     65537, 19,    1, 1, 270, 1
instr OneGrain
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 80
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
a_signal poscil3 1, i_frequency, gi_OneGrain_sine
a_envelope poscil3 1, (1 / i_duration), gi_OneGrain_cosine
a_signal = a_signal * a_envelope
out a_signal

prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, active(p1)
endin

//////////////////////////////////////////////
// Original by Steven Yi.
// Adapted by Michael Gogins.
//////////////////////////////////////////////
gk_FMWaterBell_level init 16
gi_FMWaterBell_attack init 0.002
gi_FMWaterBell_release init 0.01
gi_FMWaterBell_sustain init 20
gi_FMWaterBell_sustain_level init .1
gk_FMWaterBell_index init .5
gk_FMWaterBell_crossfade init .5
gk_FMWaterBell_vibrato_depth init 0.05
gk_FMWaterBell_vibrato_rate init 6
gk_FMWaterBell_midi_dynamic_range init 127
gi_FMWaterBell_cosine ftgen 0, 0, 65537, 11, 1
instr FMWaterBell
i_instrument = p1
i_time = p2
i_duration = p3
; One of the envelopes in this instrument should be releasing, and use this:
i_sustain = 1000
;xtratim gi_FMWaterBell_attack + gi_FMWaterBell_release
xtratim gi_FMWaterBell_attack + gi_FMWaterBell_release
i_midi_key = p4
i_midi_dynamic_range = i(gk_FMWaterBell_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 81
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization * 1.6
k_gain = ampdb(gk_FMWaterBell_level)
a_signal fmbell	1, i_frequency, gk_FMWaterBell_index, gk_FMWaterBell_crossfade, gk_FMWaterBell_vibrato_depth, gk_FMWaterBell_vibrato_rate, gi_FMWaterBell_cosine, gi_FMWaterBell_cosine, gi_FMWaterBell_cosine, gi_FMWaterBell_cosine, gi_FMWaterBell_cosine ;, gi_FMWaterBell_sustain
;a_envelope linsegr 0, gi_FMWaterBell_attack, 1, i_sustain, gi_FMWaterBell_sustain_level, gi_FMWaterBell_release, 0
a_envelope linsegr 0, gi_FMWaterBell_attack, 1, i_sustain, 1, gi_FMWaterBell_release, 0
; ares transegr ia, idur, itype, ib [, idur2] [, itype] [, ic] ...
; a_envelope transegr 0, gi_FMWaterBell_attack, 12, 1, i_sustain, 12, gi_FMWaterBell_sustain_level, gi_FMWaterBell_release, 12, 0
a_signal = a_signal * i_amplitude * a_envelope * k_gain
;_signal = a_signal * i_amplitude * k_gain
out a_signal
prints "%-24.s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_STKBeeThree_level init 16
instr STKBeeThree
; Authors: Michael Gogins
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
i_exponent = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_overall_amps = 64
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_STKBeeThree_level)
asignal STKBeeThree i_frequency, 1.0, 1, 1.5, 2, 4.8, 4, 2.1
; ares phaser1 asig, kfreq, kord, kfeedback [, iskip]
a_signal phaser1 asignal, 8000, 16, .2, .9
i_attack = .004
i_decay = 2
i_release = 0.1
xtratim i_attack + i_release
; ares transegr ia, idur, itype, ib [, idur2] [, itype] [, ic] ...
a_envelope transeg 0, i_attack, i_exponent,   1, i_decay, i_exponent,  .5, i_release, i_exponent, 0
a_signal = a_signal * i_amplitude * a_envelope * k_gain * .75
out a_signal
prints "%-24.s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

instr STKBeeThreer
; Authors: Michael Gogins
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
i_exponent = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_overall_amps = 64
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_STKBeeThree_level)
asignal STKBeeThree i_frequency, 1.0, 1, 1.5, 2, 4.8, 4, 2.1
; ares phaser1 asig, kfreq, kord, kfeedback [, iskip]
a_signal phaser1 asignal, 8000, 16, .2, .9
i_attack = .004
i_decay = 2
i_release = 0.1
xtratim i_attack + i_release
; ares transegr ia, idur, itype, ib [, idur2] [, itype] [, ic] ...
a_envelope transegr 0, i_attack, i_exponent,   1, i_decay, i_exponent,  .5, i_release, i_exponent, 0
a_signal = a_signal * i_amplitude * a_envelope * k_gain * .75
out a_signal
prints "%-24.s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gi_Convolver_01 ftgen 0, 0, 480, 1, "impulses.wav", 4, 1, 1
instr Convolver_original
a_signal diskin2 "kitchen.wav"
out a_signal 
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, active(p1)
endin

instr Convolver_01
a_kitchen diskin2 "kitchen.wav"
a_signal ftconv a_kitchen, gi_Convolver_01, 1024
out a_signal / 50
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, active(p1)
endin

gi_Convolver_02 ftgen 0, 0, 4800, 1, "impulses.wav", 5, 1, 1
instr Convolver_02
a_kitchen diskin2 "kitchen.wav"
a_signal ftconv a_kitchen, gi_Convolver_02, 1024
out a_signal / 100
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, active(p1)
endin

gi_Convolver_03 ftgen 0, 0, 48000, 1, "impulses.wav", 6, 1, 1
instr Convolver_03
a_kitchen diskin2 "kitchen.wav"
a_signal ftconv a_kitchen, gi_Convolver_03, 1024
out a_signal / 200
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, active(p1)
endin

</CsInstruments>
<CsScore>
i 1   1 .5 0 0 
i 2 ^+2 1 0 0
i 3 ^+2 1 0 0
i 4 ^+2 1 0 0
i 5 ^+2 40 0 0
i 6 ^+41 40 0 0
s 1
i "OneGrain" 1 .01 96 60
s 1
i "STKBeeThree" 1 .75 60 60 -10
i "STKBeeThreer" 2 .75 60 60 -10
s 1
i "Convolver_original" 1 21
s 1
i "Convolver_01" 1 21
s 1
i "Convolver_02" 1 21
s 1
i "Convolver_03" 1 21

</CsScore>
</CsoundSynthesizer>

