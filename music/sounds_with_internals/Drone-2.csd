<CsoundSynthesizer>
<CsLicense>
Copyright (C) 2021 by Michael Gogins.
All rights reserved.
</CsLicense>
<CsOptions>
-d --midi-key=4 --midi-velocity=5 -m168 -odac
</CsOptions>
<CsInstruments>

sr = 48000
ksmps = 128
nchnls = 2
0dbfs = 150000

connect "Internals_1",  "outleft",  "ReverbLeft",   "input"
connect "Internals_1",  "outright", "ReverbRight",  "input"
connect "Bower",        "outleft",  "ReverbLeft",   "input"
connect "Bower",        "outright", "ReverbRight",  "input"
connect "Phaser",       "outleft",  "ReverbLeft",   "input"
connect "Phaser",       "outright", "ReverbRight",  "input"
connect "Droner",       "outleft",  "ReverbLeft",   "input"
connect "Droner",       "outright", "ReverbRight",  "input"
connect "Sweeper",      "outleft",  "ReverbLeft",   "input"
connect "Sweeper",      "outright", "ReverbRight",  "input"
connect "Buzzer",       "outleft",  "ReverbLeft",   "input"
connect "Buzzer",       "outright", "ReverbRight",  "input"
connect "Blower",       "outleft",  "ReverbLeft",   "input"
connect "Blower",       "outright", "ReverbRight",  "input"
connect "Shiner",       "outleft",  "ReverbLeft",   "input"
connect "Shiner",       "outright", "ReverbRight",  "input"
connect "ReverbLeft",   "output",   "MasterOutput", "inleft"
connect "ReverbRight",  "output",   "MasterOutput",	"inright"

alwayson "ReverbLeft"
alwayson "ReverbRight"
alwayson "MasterOutput"

opcode ratio2midinn, i, iii
i_fundamental, i_numerator, i_denominator xin
i_frequency = i_fundamental * i_numerator / i_denominator
; print i_frequency
; m = 12*log2(fm/440 Hz) + 69
i_log2 = log(2)
i_midinn = 12 * (log(i_frequency / 440) / i_log2) + 69
; print i_midinn
xout i_midinn
endop

instr 101,102
i_instrument = p1
i_time = p2
i_duration = p3
i_fundamental = p4
i_numerator = p5
i_denominator = p6
i_midi_velocity = p7
i_pan = p8
i_ratio = i_numerator / i_denominator
i_frequency = i_fundamental * i_ratio
i_midi_key ratio2midinn i_fundamental, i_numerator, i_denominator
event_i "i", "Internals_1", 0, i_duration, i_midi_key, i_midi_velocity, 0, i_pan
endin

instr 103,104
i_instrument = p1
i_time = p2
i_duration = p3
i_fundamental = p4
i_numerator = p5
i_denominator = p6
i_midi_velocity = p7
i_pan = p8
i_ratio = i_numerator / i_denominator
i_frequency = i_fundamental * i_ratio
i_midi_key ratio2midinn i_fundamental, i_numerator, i_denominator
event_i "i", "Blower", 0, i_duration, i_midi_key, i_midi_velocity, 0, i_pan
endin

gk_Blower_level chnexport "gk_Blower_level", 3
gk_Blower_grainDensity chnexport "gk_Blower_grainDensity", 3
gk_Blower_grainDuration chnexport "gk_Blower_grainDuration", 3
gk_Blower_grainAmplitudeRange chnexport "gk_Blower_grainAmplitudeRange", 3
gk_Blower_grainFrequencyRange chnexport "gk_Blower_grainFrequencyRange", 3
gk_Blower_midi_dynamic_range init 20
gi_Blower_grtab ftgen 0, 0, 65537, 10, 1, .3, .1, 0, .2, .02, 0, .1, .04
gi_Blower_wintab ftgen 0, 0, 65537, 10, 1, 0, .5, 0, .33, 0, .25, 0, .2, 0, .167

instr Blower
//////////////////////////////////////////////
// Original by Hans Mikelson.
// Adapted by Michael Gogins.
//////////////////////////////////////////////
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_Blower_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 90
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Blower_level)
iHz = i_frequency
ihertz = iHz
ip4 = i_amplitude
ip5 = iHz
ip6 = gi_Blower_grtab
ip7 = gi_Blower_wintab
ip8 = 0.033
ip8 = .002
ip9 = 150
ip9 = 100
ip10 = 1.6
ip10 = 3
idur = p3
iamp = i_amplitude ; p4
ifqc = iHz ; cpspch(p5)
igrtab = ip6
iwintab = ip7
ifrng = ip8
idens = ip9
ifade = ip10
igdur = 0.2
iattack = 0.5
i_sustain = p3
idecay = 1.5
kenvelope transegr 0.0, iattack / 2.0, 1.5, .5, iattack / 2.0, -1.5, 1, i_sustain, 0.0, 1, idecay / 2.0, 1.5, .5, idecay / 2.0, -1.5, 0
; kamp linseg 0, ifade, 1, idur - 2 * ifade, 1, ifade, 0
kamp = kenvelope
; Amp Fqc Dense AmpOff PitchOff GrDur GrTable WinTable MaxGrDur
aoutl grain ip4, ifqc, gk_Blower_grainDensity, gk_Blower_grainAmplitudeRange, gk_Blower_grainFrequencyRange, gk_Blower_grainDuration, igrtab, iwintab, 5
aoutr grain ip4, ifqc, gk_Blower_grainDensity, gk_Blower_grainAmplitudeRange, gk_Blower_grainFrequencyRange, gk_Blower_grainDuration, igrtab, iwintab, 5
a_signal = aoutl + aoutr
i_attack = .002
i_release = 0.01
printks2 "csound: gk_Blower_level:               %9.4f\n", gk_Blower_level
printks2 "csound: gk_Blower_grainDensity:        %9.4f\n", gk_Blower_grainDensity
printks2 "csound: gk_Blower_grainAmplitudeRange: %9.4f\n", gk_Blower_grainAmplitudeRange
printks2 "csound: gk_Blower_grainFrequencyRange: %9.4f\n", gk_Blower_grainFrequencyRange
printks2 "csound: gk_Blower_grainDuration:       %9.4f\n", gk_Blower_grainDuration
p3 = i_attack + i_sustain + i_release
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_signal * i_amplitude * a_declicking * k_gain
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
prints  "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
; printks "Blower         i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", 1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_out_left)), dbamp(rms(a_out_right))
endin

gk_Internals_1_mod_amp chnexport "gk_Internals_1_mod_amp", 3
gk_Internals_1_mod_hz chnexport "gk_Internals_1_mod_hz", 3
gk_Internals_1_level chnexport "gk_Internals_1_level", 3
gS_Internals_1_mod_waveform chnexport "gS_Internals_1_mod_waveform", 3
gS_Internals_1_waveform chnexport "gS_Internals_1_waveform", 3
gk_Internals_1_k1 chnexport "gk_Internals_1_k1", 3
gk_Internals_1_k2 chnexport "gk_Internals_1_k2", 3
gk_Internals_1_k3 chnexport "gk_Internals_1_k3", 3
gk_Internals_1_k4 chnexport "gk_Internals_1_k4", 3
gk_Internals_1_k5 chnexport "gk_Internals_1_k5", 3
gk_Internals_1_k6 chnexport "gk_Internals_1_k6", 3
gk_Internals_1_k7 chnexport "gk_Internals_1_k7", 3
gk_Internals_1_k8 chnexport "gk_Internals_1_k8", 3

gi_Internals_1_sine ftgen 0, 0, 65537, 10, 1, 0, .02
instr Internals_1
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
i_phase = p6
i_pan = p7
k1 = gk_Internals_1_k1
k2 = gk_Internals_1_k2
k3 = gk_Internals_1_k3
k4 = gk_Internals_1_k4
k5 = gk_Internals_1_k5
k6 = gk_Internals_1_k6
k7 = gk_Internals_1_k7
k8 = gk_Internals_1_k8
i_amplitude = ampdb(i_midi_velocity)
i_attack =  p3 * (1 / 4) * (4 / 3)
i_sustain = p3 * (1 / 2) * (4 / 3)
i_release =   p3 * (1 / 4) * (4 / 3)
p3 = i_attack + i_sustain + i_release
ak_envelope transeg 0.0, i_attack / 2.0, 1.5, i_amplitude / 2.0, i_attack / 2.0, -1.5, i_amplitude, i_sustain, 0.0, i_amplitude, i_release / 2.0, 1.5, i_amplitude / 2.0, i_release / 2.0, -1.5, 0
i_frequency = cpsmidinn(i_midi_key)
; print i_frequency
i_waveform init 0
if strcmp(gS_Internals_1_waveform, "Sine") == 0 then
i_waveform = 0
endif
if strcmp(gS_Internals_1_waveform, "Sawtooth") == 0 then
i_waveform = 1
endif
if strcmp(gS_Internals_1_waveform, "Triangle") == 0 then
i_waveform = 2
endif
if strcmp(gS_Internals_1_waveform, "Chebyshev") == 0 then
i_waveform = 3 
endif
if i_waveform == 0 then
a_signal poscil3 1, i_frequency, gi_Internals_1_sine
endif
if i_waveform == 1 then
a_signal vco2 1, i_frequency, 8
endif
if i_waveform == 2 then
a_signal vco2 1, i_frequency, 12
endif
if i_waveform == 3 then
a_signal poscil3 1, i_frequency, gi_Internals_1_sine
a_signal chebyshevpoly a_signal, 0, k1, k2, k3, k4, k5, k6, k7, k8
endif

i_mod_waveform init 12
if strcmp(gS_Internals_1_mod_waveform, "Triangle") == 0 then
i_mod_waveform init 12
endif 
if strcmp(gS_Internals_1_mod_waveform, "Sawtooth") == 0 then
i_mod_waveform init 4
endif
if strcmp(gS_Internals_1_mod_waveform, "Square") == 0 then
i_mod_waveform init 10
endif 
prints   "Csound: gS_Internals_1_waveform:     %s\n", gS_Internals_1_waveform
prints   "Csound: gS_Internals_1_mod_waveform: %s\n", gS_Internals_1_mod_waveform
prints   "Csound: i_mod_waveform:              %9.4f\n", i_mod_waveform
printks2 "Csound: gk_Internals_1_mod_amp:      %9.4f\n", gk_Internals_1_mod_amp
printks2 "Csound: gk_Internals_1_mod_hz:       %9.4f\n", gk_Internals_1_mod_hz
printks2 "Csound: gk_Internals_1_level:        %9.4f\n", gk_Internals_1_level
printks2 "Csound: gk_Internals_1_k1:           %9.4f\n", gk_Internals_1_k1
printks2 "Csound: gk_Internals_1_k2:           %9.4f\n", gk_Internals_1_k2
printks2 "Csound: gk_Internals_1_k3:           %9.4f\n", gk_Internals_1_k3
printks2 "Csound: gk_Internals_1_k4:           %9.4f\n", gk_Internals_1_k4
printks2 "Csound: gk_Internals_1_k5:           %9.4f\n", gk_Internals_1_k5
printks2 "Csound: gk_Internals_1_k6:           %9.4f\n", gk_Internals_1_k6
printks2 "Csound: gk_Internals_1_k7:           %9.4f\n", gk_Internals_1_k7
printks2 "Csound: gk_Internals_1_k8:           %9.4f\n", gk_Internals_1_k8
k_gain = ampdb(gk_Internals_1_level)
a_modulator vco2 gk_Internals_1_mod_amp, gk_Internals_1_mod_hz, 12
a_vdelay vdelay3 a_signal, a_modulator, 4
a_vdelay = a_vdelay * ak_envelope * 10
a_output = k_gain * a_vdelay
a_left, a_right pan2 a_output, i_pan
a_damping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
a_left = a_damping * a_left
a_right = a_damping * a_right
outleta "outleft", a_left
outleta "outright", a_right
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gi_Droner_sine ftgen 0, 0, 65537, 10, 1, 0, .02
instr Droner
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
i_phase = p6
i_pan = p7
k1 init .5
k2 init .05
k3 init .1
k4 init .2
k5 init .1
k6 init .05
k7 init .1
k8 init 0
k9 init 0
k10 init 0
k_waveform init 2
i_amplitude = ampdb(i_midi_velocity)
i_attack =  p3 * (1 / 4) * (4 / 3)
i_sustain = p3 * (1 / 2) * (4 / 3)
i_release =   p3 * (1 / 4) * (4 / 3)
p3 = i_attack + i_sustain + i_release
k_envelope transeg 0.0, i_attack / 2.0, 1.5, i_amplitude / 2.0, i_attack / 2.0, -1.5, i_amplitude, i_sustain, 0.0, i_amplitude, i_release / 2.0, 1.5, i_amplitude / 2.0, i_release / 2.0, -1.5, 0
i_frequency = cpsmidinn(i_midi_key)
; print i_frequency
if k_waveform == 0 then
a_signal poscil3 1, i_frequency, gi_Droner_sine
endif
if k_waveform == 1 then
a_signal vco2 1, i_frequency, 8 ; integrated saw
endif
if k_waveform == 2 then
a_signal vco2 1, i_frequency, 12 ; triangle
endif
a_signal chebyshevpoly a_signal, 0, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10
a_signal = a_signal * k_envelope * 10
a_left, a_right pan2 a_signal, i_pan
a_damping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
a_left = a_damping * a_left
a_right = a_damping * a_right
outleta "outleft", a_left
outleta "outright", a_right
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gi_Bower_sine ftgen 0,0,65537,10,1
instr Bower
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
i_phase = p6
i_pan = p7
i_amplitude = ampdb(i_midi_velocity) * 500
i_attack =  p3 * (1 / 4) * (4 / 3)
i_sustain = p3 * (1 / 2) * (4 / 3)
i_release =   p3 * (1 / 4) * (4 / 3)
p3 = i_attack + i_sustain + i_release
k_envelope transeg 0.0, i_attack / 2.0, 1.5, i_amplitude / 2.0, i_attack / 2.0, -1.5, i_amplitude, i_sustain, 0.0, i_amplitude, i_release / 2.0, 1.5, i_amplitude / 2.0, i_release / 2.0, -1.5, 0
i_frequency = cpsmidinn(i_midi_key)
kamp = k_envelope
kfreq = i_frequency
kpres = 0.25
; krat rspline 0.006,0.988,0.1,0.4
krat rspline 0.006,0.988,1,4
kvibf = 4.5
kvibamp = 0
iminfreq = 20
aSig wgbow kamp,kfreq,kpres,krat,kvibf,kvibamp,gi_Bower_sine,iminfreq
;aSig butlp aSig,2000
;aSig pareq aSig,80,6,0.707
a_left, a_right pan2 aSig / 7, i_pan
a_damping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
a_left = a_damping * a_left
a_right = a_damping * a_right
outleta "outleft", a_left
outleta "outright", a_right
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_Phaser_attack init .125
gk_Phaser_release init .125
gk_Phaser_ratio1 init 1
gk_Phaser_ratio2 init 1/5
gk_Phaser_index1 init 1.01
gk_Phaser_index2 init .103
gk_Phaser_level init 20
gk_Phaser_midi_dynamic_range init 127
gi_Phaser_sine ftgen 0,0,65537,10,1
instr Phaser
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_Phaser_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 10
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Phaser_level)
i_attack =  p3 * (1 / 4) * (4 / 3)
i_sustain = p3 * (1 / 2) * (4 / 3)
i_release = p3 * (1 / 4) * (4 / 3)
p3 = i_attack + i_sustain + i_release
a_envelope transegr 0.0, i_attack / 2.0, 1.5, i_amplitude / 2.0, i_attack / 2.0, -1.5, i_amplitude, i_sustain, 0.0, i_amplitude, i_release / 2.0, 1.5, i_amplitude / 2.0, i_release / 2.0, -1.5, 0
a1,a2 crosspm gk_Phaser_ratio1, gk_Phaser_ratio2, gk_Phaser_index1, gk_Phaser_index2, i_frequency, gi_Phaser_sine, gi_Phaser_sine
a_signal = (a1 + a2) * k_gain * a_envelope
#ifdef USE_SPATIALIZATION
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
;printks "Phaser         i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", 1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_left)), dbamp(rms(a_right))
endin

gk_britel init 0
gk_briteh init 2.9
gk_britels init .2 / 3
gk_britehs init 2.5 / 2
gi_Sweeper_sine ftgen 0, 0, 65537, 10, 1
gi_Sweeper_octfn ftgen 0, 0, 65537, -19, 1, 0.5, 270, 0.5
instr Sweeper
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
i_phase = p6
i_pan = p7
i_amplitude = ampdb(i_midi_velocity)
i_attack =  p3 * (1 / 4) * (4 / 3)
i_sustain = p3 * (1 / 2) * (4 / 3)
i_release = p3 * (1 / 4) * (4 / 3)
p3 = i_attack + i_sustain + i_release
k_envelope transeg 0.0, i_attack / 2.0, 1.5, i_amplitude / 2.0, i_attack / 2.0, -1.5, i_amplitude, i_sustain, 0.0, i_amplitude, i_release / 2.0, 1.5, i_amplitude / 2.0, i_release / 2.0, -1.5, 0
i_frequency = cpsmidinn(i_midi_key)
icps = i_frequency
kamp expseg 0.001,0.02,0.2,p3-0.01,0.001
ktonemoddep jspline 0.01,0.05,0.2
ktonemodrte jspline 6,0.1,0.2
ktone poscil3 ktonemoddep, ktonemodrte, gi_Sweeper_sine
kbrite rspline gk_britel, gk_briteh, gk_britels, gk_britehs
ibasfreq init icps
ioctcnt init 3
iphs init 0
;a1 hsboscil kamp, ktone, kbrite, ibasfreq, gi_Sweeper_sine, gi_Sweeper_octfn, ioctcnt, iphs
a1 hsboscil k_envelope, ktone, kbrite, ibasfreq, gi_Sweeper_sine, gi_Sweeper_octfn, ioctcnt, iphs
amod poscil3 0.25, ibasfreq*(1/3), gi_Sweeper_sine
arm = a1*amod
kmix expseg 0.001, 0.01, rnd(1), rnd(3)+0.3, 0.001
kmix=.25
a1 ntrpol a1, arm, kmix
;a1 pareq a1/10, 400, 15, .707
;a1 tone a1, 500
kpanrte jspline 5, 0.05, 0.1
kpandep jspline 0.9, 0.2, 0.4
kpan poscil3 kpandep, kpanrte, gi_Sweeper_sine
a1,a2 pan2 a1, kpan
a1 delay a1, rnd(0.1)
a2 delay a2, rnd(0.11)
kenv linsegr 1, 1, 0
kenv = k_envelope
a_left = a1*kenv*.02
a_right = a2*kenv*.02
a_damping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
a_left = a_damping * a_left
a_right = a_damping * a_right
outleta "outleft", a_left
outleta "outright", a_right
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gi_Buzzer_sine ftgen 0, 0, 65537, 10, 1
instr Buzzer
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
i_phase = p6
i_pan = p7
i_amplitude = ampdb(i_midi_velocity) * 4
i_attack =  p3 * (1 / 4) * (4 / 3)
i_sustain = p3 * (1 / 2) * (4 / 3)
i_release = p3 * (1 / 4) * (4 / 3)
p3 = i_attack + i_sustain + i_release
k_envelope transeg 0.0, i_attack / 2.0, 1.5, i_amplitude / 2.0, i_attack / 2.0, -1.5, i_amplitude, i_sustain, 0.0, i_amplitude, i_release / 2.0, 1.5, i_amplitude / 2.0, i_release / 2.0, -1.5, 0
i_frequency = cpsmidinn(i_midi_key)
;a_signal gbuzz k_envelope, i_frequency, 3, gk_FirstHarmonic, gk_DistortFactor, gisine
gk_Harmonics = 15
a_signal buzz k_envelope, i_frequency, gk_Harmonics, gi_Buzzer_sine
a_signal = a_signal * 3
;a_signal vco2 k_envelope, i_frequency, 12
;a_signal poscil3 k_envelope, i_frequency, giharmonics
;a_signal distort a_signal, gk_DistortFactor * .4, giwaveshaping
a_left, a_right pan2 a_signal, i_pan
a_damping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
a_left = a_damping * a_left
a_right = a_damping * a_right
outleta "outleft", a_left
outleta "outright", a_right
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

instr Shiner
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
i_phase = p6
i_pan = p7
i_amplitude = ampdb(i_midi_velocity) * 4
i_attack =  p3 * (1 / 4) * (4 / 3)
i_sustain = p3 * (1 / 2) * (4 / 3)
i_release = p3 * (1 / 4) * (4 / 3)
p3 = i_attack + i_sustain + i_release
k_envelope transeg 0.0, i_attack / 2.0, 1.5, i_amplitude / 2.0, i_attack / 2.0, -1.5, i_amplitude, i_sustain, 0.0, i_amplitude, i_release / 2.0, 1.5, i_amplitude / 2.0, i_release / 2.0, -1.5, 0
i_frequency = cpsmidinn(i_midi_key)
;a_signal gbuzz k_envelope, i_frequency, 3, gk_FirstHarmonic, gk_DistortFactor, gisine
gk_Harmonics = 15
;a_signal buzz k_envelope, i_frequency, gk_Harmonics, gisine
;a_signal = a_signal
a_signal vco2 k_envelope * 4, i_frequency, 12
;a_signal poscil3 k_envelope, i_frequency, giharmonics
;a_signal distort a_signal, gk_DistortFactor * .4, giwaveshaping
a_left, a_right pan2 a_signal, i_pan
a_damping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
a_left = a_damping * a_left
a_right = a_damping * a_right
outleta "outleft", a_left
outleta "outright", a_right
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_Reverb_Feedback chnexport "gk_Reverb_Feedback", 3
gk_Reverb_DelayModulation chnexport "gk_Reverb_DelayModulation", 3

instr ReverbLeft
printks2 "Csound: gk_Reverb_Feedback:         %9.4f\n", gk_Reverb_Feedback
printks2 "Csound: gk_Reverb_DelayModulation:  %9.4f\n", gk_Reverb_DelayModulation
; p4 = gain of reverb. Adjust empirically
; for desired reverb time. .6 gives
; a good small "live" room sound, .8
; a small hall, .9 a large hall,
; .99 an enormous stone cavern.

; p5 = amount of random pitch modulation
; for the delay lines. 1 is the "normal"
; amount, but this may be too high for
; held pitches such as piano tones.
; Adjust to taste.

; p6 = cutoff frequency of lowpass filters
; in feedback loops of delay lines,
; in Hz. Lower cutoff frequencies results
; in a sound with more high-frequency
; damping.

; 8 delay line FDN reverb, with feedback matrix based upon
; physical modeling scattering junction of 8 lossless waveguides
; of equal characteristic impedance. Based on Julius O. Smith III,
; "A New Approach to Digital Reverberation using Closed Waveguide
; Networks," Proceedings of the International Computer Music
; Conference 1985, p. 47-53 (also available as a seperate
; publication from CCRMA), as well as some more recent papers by
; Smith and others.
; Coded by Sean Costello, October 1999
itone = 16000
a_signal inleta "input"
afilt1 init 0
afilt2 init 0
afilt3 init 0
afilt4 init 0
afilt5 init 0
afilt6 init 0
afilt7 init 0
afilt8 init 0
idel1 = (2473.000/sr)
idel2 = (2767.000/sr)
idel3 = (3217.000/sr)
idel4 = (3557.000/sr)
idel5 = (3907.000/sr)
idel6 = (4127.000/sr)
idel7 = (2143.000/sr)
idel8 = (1933.000/sr)
; k1-k8 are used to add random pitch modulation to the
; delay lines. Helps eliminate metallic overtones
; in the reverb sound.
// ares randi   xamp, xcps [, iseed] [, isize] [, ioffset]
// ares jspline xamp, kcpsMin, kcpsMax
k1 randi .001,  3.1,   .06
k2 randi .0011, 3.5,   .9
k3 randi .0017, 1.11,  .7
k4 randi .0006, 3.973, .3
k5 randi .001,  2.341, .63
k6 randi .0011, 1.897, .7
k7 randi .0017, 0.891, .9
k8 randi .0006, 3.221, .44
; apj is used to calculate "resultant junction pressure" for
; the scattering junction of 8 lossless waveguides
; of equal characteristic impedance. If you wish to
; add more delay lines, simply add them to the following
; equation, and replace the .25 by 2/N, where N is the
; number of delay lines.
apj = .25 * (afilt1 + afilt2 + afilt3 + afilt4 + afilt5 + afilt6 + afilt7 + afilt8)
adum1 delayr 1
adel1 deltap3 idel1 + k1 * gk_Reverb_DelayModulation
delayw a_signal + apj - afilt1
adum2 delayr 1
adel2 deltap3 idel2 + k2 * gk_Reverb_DelayModulation
delayw a_signal + apj - afilt2
adum3 delayr 1
adel3 deltap3 idel3 + k3 * gk_Reverb_DelayModulation
delayw a_signal + apj - afilt3
adum4 delayr 1
adel4 deltap3 idel4 + k4 * gk_Reverb_DelayModulation
delayw a_signal + apj - afilt4
adum5 delayr 1
adel5 deltap3 idel5 + k5 * gk_Reverb_DelayModulation
delayw a_signal + apj - afilt5
adum6 delayr 1
adel6 deltap3 idel6 + k6 * gk_Reverb_DelayModulation
delayw a_signal + apj - afilt6
adum7 delayr 1
adel7 deltap3 idel7 + k7 * gk_Reverb_DelayModulation
delayw a_signal + apj - afilt7
adum8 delayr 1
adel8 deltap3 idel8 + k8 * gk_Reverb_DelayModulation
delayw a_signal + apj - afilt8
; 1st order lowpass filters in feedback
; loops of delay lines.
afilt1 tone adel1 * gk_Reverb_Feedback, itone
afilt2 tone adel2 * gk_Reverb_Feedback, itone
afilt3 tone adel3 * gk_Reverb_Feedback, itone
afilt4 tone adel4 * gk_Reverb_Feedback, itone
afilt5 tone adel5 * gk_Reverb_Feedback, itone
afilt6 tone adel6 * gk_Reverb_Feedback, itone
afilt7 tone adel7 * gk_Reverb_Feedback, itone
afilt8 tone adel8 * gk_Reverb_Feedback, itone
; The outputs of the delay lines are summed
; and sent to the stereo outputs. This could
; easily be modified for a 4 or 8-channel
; sound system.
aout1 = (afilt1 + afilt3 + afilt5 + afilt7)
aout2 = (afilt2 + afilt4 + afilt6 + afilt8)
aout = aout1 + aout2
; To the master output.
outleta "output", aout
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

instr ReverbRight
; p4 = gain of reverb. Adjust empirically
; for desired reverb time. .6 gives
; a good small "live" room sound, .8
; a small hall, .9 a large hall,
; .99 an enormous stone cavern.

; p5 = amount of random pitch modulation
; for the delay lines. 1 is the "normal"
; amount, but this may be too high for
; held pitches such as piano tones.
; Adjust to taste.

; p6 = cutoff frequency of lowpass filters
; in feedback loops of delay lines,
; in Hz. Lower cutoff frequencies results
; in a sound with more high-frequency
; damping.

; 8 delay line FDN reverb, with feedback matrix based upon
; physical modeling scattering junction of 8 lossless waveguides
; of equal characteristic impedance. Based on Julius O. Smith III,
; "A New Approach to Digital Reverberation using Closed Waveguide
; Networks," Proceedings of the International Computer Music
; Conference 1985, p. 47-53 (also available as a seperate
; publication from CCRMA), as well as some more recent papers by
; Smith and others.
; Coded by Sean Costello, October 1999
itone = 16000
a_signal inleta "input"
afilt1 init 0
afilt2 init 0
afilt3 init 0
afilt4 init 0
afilt5 init 0
afilt6 init 0
afilt7 init 0
afilt8 init 0
idel1 = (2473.000/sr)
idel2 = (2767.000/sr)
idel3 = (3217.000/sr)
idel4 = (3557.000/sr)
idel5 = (3907.000/sr)
idel6 = (4127.000/sr)
idel7 = (2143.000/sr)
idel8 = (1933.000/sr)
; k1-k8 are used to add random pitch modulation to the
; delay lines. Helps eliminate metallic overtones
; in the reverb sound.
// ares randi   xamp, xcps             [, iseed] [, isize] [, ioffset]
// ares jspline xamp, kcpsMin, kcpsMax
k1 randi .001,  3.1,   .06
k2 randi .0011, 3.5,   .9
k3 randi .0017, 1.11,  .7
k4 randi .0006, 3.973, .3
k5 randi .001,  2.341, .63
k6 randi .0011, 1.897, .7
k7 randi .0017, 0.891, .9
k8 randi .0006, 3.221, .44
; apj is used to calculate "resultant junction pressure" for
; the scattering junction of 8 lossless waveguides
; of equal characteristic impedance. If you wish to
; add more delay lines, simply add them to the following
; equation, and replace the .25 by 2/N, where N is the
; number of delay lines.
apj = .25 * (afilt1 + afilt2 + afilt3 + afilt4 + afilt5 + afilt6 + afilt7 + afilt8)
adum1 delayr 1
adel1 deltap3 idel1 + k1 * gk_Reverb_DelayModulation
delayw a_signal + apj - afilt1
adum2 delayr 1
adel2 deltap3 idel2 + k2 * gk_Reverb_DelayModulation
delayw a_signal + apj - afilt2
adum3 delayr 1
adel3 deltap3 idel3 + k3 * gk_Reverb_DelayModulation
delayw a_signal + apj - afilt3
adum4 delayr 1
adel4 deltap3 idel4 + k4 * gk_Reverb_DelayModulation
delayw a_signal + apj - afilt4
adum5 delayr 1
adel5 deltap3 idel5 + k5 * gk_Reverb_DelayModulation
delayw a_signal + apj - afilt5
adum6 delayr 1
adel6 deltap3 idel6 + k6 * gk_Reverb_DelayModulation
delayw a_signal + apj - afilt6
adum7 delayr 1
adel7 deltap3 idel7 + k7 * gk_Reverb_DelayModulation
delayw a_signal + apj - afilt7
adum8 delayr 1
adel8 deltap3 idel8 + k8 * gk_Reverb_DelayModulation
delayw a_signal + apj - afilt8
; 1st order lowpass filters in feedback
; loops of delay lines.
afilt1 tone adel1 * gk_Reverb_Feedback, itone
afilt2 tone adel2 * gk_Reverb_Feedback, itone
afilt3 tone adel3 * gk_Reverb_Feedback, itone
afilt4 tone adel4 * gk_Reverb_Feedback, itone
afilt5 tone adel5 * gk_Reverb_Feedback, itone
afilt6 tone adel6 * gk_Reverb_Feedback, itone
afilt7 tone adel7 * gk_Reverb_Feedback, itone
afilt8 tone adel8 * gk_Reverb_Feedback, itone
; The outputs of the delay lines are summed
; and sent to the stereo outputs. This could
; easily be modified for a 4 or 8-channel
; sound system.
aout1 = (afilt1 + afilt3 + afilt5 + afilt7)
aout2 = (afilt2 + afilt4 + afilt6 + afilt8)
aout = aout1 + aout2
; To the master output.
outleta "output", aout
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_MasterLevel init 0
gk_MasterLevel chnexport "gk_MasterLevel", 3
instr MasterOutput
printks2 "Csound: gk_MasterLevel:  %9.4f\n", gk_MasterLevel
a_left inleta "inleft"
a_right inleta "inright"
k_gain = ampdb(gk_MasterLevel)
a_left *= k_gain
a_right *= k_gain
outs a_left, a_right
;fout "Drone-IV-performance.wav", 16, a_left, a_right
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

</CsInstruments>
<CsScore bin="python3">

import sys

with open(sys.argv[1], 'w') as f:
	f.write('''

; p1 p2 p3 p4 p5 p6 p7 p8
; insno onset duration fundamental numerator denominator velocity pan

; C E B
i 101   0 60 [1 * 60]  1  1 60 [-1 + 1 / 2]
i 101   0 60 [2 * 60]  5  4 60 [-1 + 3 / 2]
i 101   0 60 [3 * 60] 28 15 60 [-1 + 2 / 2]
; C Ab E B
i 104  30 50 [1 * 60]  8  5 60 [-1 + 2 / 2]
; G F# B
i 102  60 60 [1 * 60]  3  2 60 [-1 + 1 / 2]
i 102  60 30 [2 * 60] 45 32 64 [-1 + 2 / 2]
i 102  60 60 [3 * 60] 28 15 64 [-1 + 3 / 2]
; G F B
i 102  90 30 [2 * 60]  4  3 64 [-1 + 2 / 2]
; C E B
i 103 120 60 [1 * 60]  1  1 64 [-1 + 1 / 2]
i 103 120 60 [2 * 60]  5  4 56 [-1 + 2 / 2]
i 103 120 30 [3 * 60] 28 15 56 [-1 + 3 / 2]
i 103 150 30 [4 * 60]  1  1 58 [-1 + 3 / 2]
i 103 120 60 [3 * 60]  1  1 56 [-1 + 3 / 2]
e 10.0
''')
</CsScore>
</CsoundSynthesizer>
<bsbPanel>
 <label>Widgets</label>
 <objectName/>
 <x>0</x>
 <y>0</y>
 <width>921</width>
 <height>875</height>
 <visible>true</visible>
 <uuid/>
 <bgcolor mode="nobackground">
  <r>255</r>
  <g>255</g>
  <b>255</b>
 </bgcolor>
 <bsbObject type="BSBScope" version="2">
  <objectName/>
  <x>564</x>
  <y>563</y>
  <width>350</width>
  <height>150</height>
  <uuid>{4ae8d54a-03de-4109-a575-a0b431d3bb11}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <description>Output</description>
  <value>-255.00000000</value>
  <type>scope</type>
  <zoomx>2.00000000</zoomx>
  <zoomy>1.00000000</zoomy>
  <dispx>1.00000000</dispx>
  <dispy>1.00000000</dispy>
  <mode>0.00000000</mode>
  <triggermode>NoTrigger</triggermode>
 </bsbObject>
 <bsbObject type="BSBConsole" version="2">
  <objectName/>
  <x>14</x>
  <y>734</y>
  <width>907</width>
  <height>141</height>
  <uuid>{3d4f056b-b5b1-499f-84f8-44b7ca241828}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <description>Console</description>
  <font>Courier</font>
  <fontsize>8</fontsize>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
 </bsbObject>
</bsbPanel>
<bsbPresets>
</bsbPresets>
