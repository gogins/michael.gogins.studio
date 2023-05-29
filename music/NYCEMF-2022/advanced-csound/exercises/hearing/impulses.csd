<CsoundSynthesizer>
<CsLicense>
Michael Gogins

Some basic lessons in hearing electroacoustically.
</CsLicense>
<CsOptions>
-m165 -d -RWfo impulses.wav
</CsOptions>
<CsInstruments>
sr = 48000
ksmps = 1
nchnls = 1
0dbfs = 2

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
;ares transegr ia, idur, itype, ib [, idur2] [, itype] [, ic] ...
a_envelope transegr 0, i_attack, i_exponent,   1, i_decay, i_exponent,  .5, i_release, i_exponent, 0
a_signal = a_signal * i_amplitude * a_envelope * k_gain * .75
out a_signal
prints "%-24.s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

</CsInstruments>
<CsScore>
i "STKBeeThree" 1 .01 72 60 -10
i "STKBeeThree" 2 .1 72 60 -10
i "STKBeeThree" 3 .75 72 60 -10
i "FMWaterBell" 4 .01 72 60 -10
i "FMWaterBell" 5 .1 72 60 -10
i "FMWaterBell" 6 .75 72 60 -10

</CsScore>
</CsoundSynthesizer>

