<CsoundSynthesizer>
<CsLicense>
Copyright (C) 2018 by Michael Gogins.
All rights reserved.
</CsLicense>
<CsOptions>
-odac -m163 -d
</CsOptions>
<CsInstruments>
sr = 48000
ksmps = 128
nchnls = 2
0dbfs = 10000

#define USE_SPATIALIZATION #1#

#include "Spatialize.inc"

gk_BformatDecoder_SpeakerRig init 1
gk_BformatDecoder_MasterLevel init 100

gk_Spatialize_SpeakerRigRadius init 5.0
gk_SpatialReverb_ReverbDecay init 0.98
gk_SpatialReverb_CutoffHz init sr
gk_SpatialReverb_RandomDelayModulation init 0.05
gk_SpatialReverb_Gain init 20
gk_LocalReverbByDistance_Wet init 0.95
; This is a fraction of the speaker rig radius.
gk_LocalReverbByDistance_FrontWall init 7
gk_LocalReverbByDistance_ReverbDecay init 0.96
gk_LocalReverbByDistance_CutoffHz init 20000
gk_LocalReverbByDistance_RandomDelayModulation init 0.05

gi_scene_radius = 60
gi_minimum_rate = 1/15
gi_maximum_rate = 1/60

connect "Blower",        "outbformat", "BformatDecoder", "inbformat"
connect "Blower",        "out", "SpatialReverb", "in"
connect "Bower",         "outbformat", "BformatDecoder", "inbformat"
connect "Bower",         "out", "SpatialReverb", "in"
connect "Buzzer",        "outbformat", "BformatDecoder", "inbformat"
connect "Buzzer",        "out", "SpatialReverb", "in"
connect "FMDroner",      "outbformat", "BformatDecoder", "inbformat"
connect "FMDroner",      "out", "SpatialReverb", "in"
connect "Phaser",        "outbformat", "BformatDecoder", "inbformat"
connect "Phaser",        "out", "SpatialReverb", "in"
connect "Shiner",        "outbformat", "BformatDecoder", "inbformat"
connect "Shiner",        "out", "SpatialReverb", "in"
connect "STKBeeThree",   "outbformat", "BformatDecoder", "inbformat"
connect "STKBeeThree",   "out", "SpatialReverb", "in"
connect "Sweeper",       "outbformat", "BformatDecoder", "inbformat"
connect "Sweeper",       "out", "SpatialReverb", "in"
connect "SpatialReverb", "outbformat", "BformatDecoder", "inbformat"

alwayson "SpatialReverb"
alwayson "BformatDecoder"   
;alwayson "Controls"

gk_STKBeeThree_level init 0
instr STKBeeThree
; Authors: Michael Gogins
if p3 == -1 then
  p3 = 1000000
endif
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
k_space_front_to_back = p6
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
i_attack = .002
i_sustain = p3
i_release = 0.01
xtratim i_attack + i_sustain + i_release
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_signal * i_amplitude * a_declicking * k_gain
kx jspline gi_scene_radius * 2, gi_minimum_rate, gi_maximum_rate
ky jspline gi_scene_radius * 2, gi_minimum_rate, gi_maximum_rate
kz jspline gi_scene_radius * 2, gi_minimum_rate, gi_maximum_rate
k_space_front_to_back = kx - gi_scene_radius
k_space_left_to_right = ky - gi_scene_radius
k_space_bottom_to_top = kz - gi_scene_radius
kelapsed timeinsts
printks "STKBeeThree    i %7.2f t %7.2f [%7.2f] d %7.2f k %7.2f f %7.2f v %7.2f kx %7.2f ky %7.2f kz %7.2f A %9.2f\n", 1.0, p1, p2, kelapsed, p3, p4, i_frequency, p5, kx, ky, kz, rms(a_signal)
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
endin

gk_FMDroner_partial1 init 1
gk_FMDroner_partial2 init 2
gk_FMDroner_partial3 init 1
gk_FMDroner_partial4 init .02
gk_FMDroner_partial5 init .1
gk_FMDroner_partial6 init 0
gk_FMDroner_partial7 init 0
gk_FMDroner_partial8 init 0
gk_FMDroner_partial9 init 0
gk_FMDroner_partial10 init 0
gk_FMDroner_index init 7
gk_FMDroner_decay init 12
gk_FMDroner_carrier init 1
gk_FMDroner_modulator init 1.414
gk_FMDroner_transeg_exponent init -18
gk_FMDroner_level init 4
gk_FMDroner_x init 6
gk_FMDroner_y init 0
gk_FMDroner_z init 0
gi_FMDroner_sine ftgen 0, 0, 65536, 10, 1
instr FMDroner
if p3 == -1 then
  p3 = 1000000
endif
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_overall_amps = 80
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_FMDroner_level)
k1 = gk_FMDroner_partial1
k2 = gk_FMDroner_partial2
k3 = gk_FMDroner_partial3
k4 = gk_FMDroner_partial4
k5 = gk_FMDroner_partial5
k6 = gk_FMDroner_partial6
k7 = gk_FMDroner_partial7
k8 = gk_FMDroner_partial8
k9 = gk_FMDroner_partial9
k10 = gk_FMDroner_partial10
i_attack = .05
i_release = 5
i_sustain = p3 
xtratim i_attack + i_sustain + i_release
aenvelope transeg 0.0, i_attack / 2,   1.5, 1 / 2, 
                       i_attack / 2,  -1.5, 1, 
                       i_sustain,      0,   1, 
                       i_release / 2,  1.5, 1 / 2, 
                       i_release / 2, -1.5, 0
ihertz = cpsmidinn(i_midi_key)
kndx transeg i(gk_FMDroner_index), i(gk_FMDroner_decay), i(gk_FMDroner_transeg_exponent), 0
a_signal foscili .5, ihertz, gk_FMDroner_carrier, gk_FMDroner_modulator, kndx, gi_FMDroner_sine
a_damping linseg 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_signal * a_damping
a_signal chebyshevpoly a_signal, 0, k1, -k2, -k3, k4, k5, k6, k7, k8, k9, k10
a_signal dcblock a_signal
a_signal = a_signal * i_amplitude * k_gain * a_damping
kx jspline gi_scene_radius * 2, gi_minimum_rate, gi_maximum_rate
ky jspline gi_scene_radius * 2, gi_minimum_rate, gi_maximum_rate
kz jspline gi_scene_radius * 2, gi_minimum_rate, gi_maximum_rate
kx = kx - gi_scene_radius
ky = ky - gi_scene_radius
kz = kz - gi_scene_radius
kelapsed timeinsts
printks "FMDroner       i %7.2f t %7.2f [%7.2f] d %7.2f k %7.2f f %7.2f v %7.2f kx %7.2f ky %7.2f kz %7.2f A %9.2f\n", 1.0, p1, p2, kelapsed, p3, p4, ihertz, p5, kx, ky, kz, rms(a_signal)
;outs a_signal, a_signal
#ifdef USE_SPATIALIZATION
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_send init 0
a_bsignal, a_spatial_reverb_send Spatialize a_signal, kx, ky, kz
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
endin

gk_Bower_midi_dynamic_range init 127
gk_Bower_attack init .02
gk_Bower_decay init .01
gk_Bower_release init 10
gk_Bower_level init -90
gk_Bower_pressure init 4.4
gi_Bower_sine ftgen 0,0,65536,10,1
instr Bower
if p3 == -1 then
  p3 = 1000000
endif
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_Bower_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = -26
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Bower_level)
i_attack = i(gk_Bower_attack)
i_release = i(gk_Bower_release)
i_sustain = p3
xtratim i_attack + i_sustain + i_release
iamp = i_amplitude
aenvelope transeg 0.0, i_attack / 2.0, 1.5, iamp / 2.0, i_attack / 2.0, -1.5, iamp, i_sustain, 0.0, iamp, i_release / 2.0, 1.5, iamp / 2.0, i_release / 2.0, -1.5, 0
ihertz = cpsmidinn(i_midi_key)
kamp = aenvelope * k_gain
kfreq = ihertz
kpres = 0.25
krat rspline 0.006,0.988,1,4
kvibf = 4.5
kvibamp = 0
iminfreq = 20
a_signal wgbow kamp,kfreq,gk_Bower_pressure,krat,kvibf,kvibamp,gi_Bower_sine,iminfreq
a_damping linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_signal * a_damping
kx jspline gi_scene_radius * 2, gi_minimum_rate, gi_maximum_rate
ky jspline gi_scene_radius * 2, gi_minimum_rate, gi_maximum_rate
kz jspline gi_scene_radius * 2, gi_minimum_rate, gi_maximum_rate
kx = kx - gi_scene_radius
ky = ky - gi_scene_radius
kz = kz - gi_scene_radius
kelapsed timeinsts
printks "Bower          i %7.2f t %7.2f [%7.2f] d %7.2f k %7.2f f %7.2f v %7.2f kx %7.2f ky %7.2f kz %7.2f A %9.2f\n", 1.0, p1, p2, kelapsed, p3, p4, ihertz, p5, kx, ky, kz, rms(a_signal)
#ifdef USE_SPATIALIZATION
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_send init 0
a_bsignal, a_spatial_reverb_send Spatialize a_signal, kx, ky, kz
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
endin

gk_Phaser_attack init .1
gk_Phaser_decay init 1
gk_Phaser_release init 10
gi_Phaser_sine ftgen 0,0,65536,10,1
gkratio1 init 1
gkratio2 init 1/3
gkindex1 init 1
gkindex2 init 0.0125
instr Phaser
insno = p1
istart = p2
iduration = p3
ikey = p4
ivelocity = p5
iphase = p6
ipan = p7
iamp = ampdb(ivelocity) * 8
i_attack = i(gk_Phaser_attack)
i_release = i(gk_Phaser_release)
i_sustain = p3
xtratim i_attack + i_sustain + i_release
kenvelope transeg 0.0, i_attack / 2.0, 1.5, iamp / 2.0, i_attack / 2.0, -1.5, iamp, i_sustain, 0.0, iamp, i_release / 2.0, 1.5, iamp / 2.0, i_release / 2.0, -1.5, 0
ihertz = cpsmidinn(ikey)
khertz = ihertz
ifunction1 = gi_Phaser_sine
ifunction2 = gi_Phaser_sine
a1,a2 crosspm gkratio1, gkratio2, gkindex1, gkindex2, khertz, ifunction1, ifunction2
a_signal = a1 + a2
a_damping linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_signal * a_damping
absignal[] init 16
kx jspline gi_scene_radius * 2, gi_minimum_rate, gi_maximum_rate
ky jspline gi_scene_radius * 2, gi_minimum_rate, gi_maximum_rate
kz jspline gi_scene_radius * 2, gi_minimum_rate, gi_maximum_rate
kx = kx - gi_scene_radius
ky = ky - gi_scene_radius
kz = kz - gi_scene_radius
absignal, asend Spatialize a_signal, kx, ky, kz
outleta "out", asend
outletv "outbformat", absignal
kelapsed timeinsts
printks "Phaser         i %7.2f t %7.2f [%7.2f] d %7.2f k %7.2f f %7.2f v %7.2f kx %7.2f ky %7.2f kz %7.2f A %9.2f\n", 1.0, p1, p2, kelapsed, p3, p4, ihertz, p5, kx, ky, kz, rms(a_signal)
endin

gk_Sweeper_midi_dynamic_range init 127
gk_Sweeper_attack init 2
gk_Sweeper_release init 10
gk_Sweeper_britel init 0
gk_Sweeper_briteh init 4.9
gk_Sweeper_britels init .2 / 3
gk_Sweeper_britehs init 2.5 / 2
gk_Sweeper_level init 40
gi_Sweeper_sine ftgen 0, 0, 65536, 10, 1
gi_Sweeper_octfn ftgen 0, 0, 65536, -19, 1, 0.5, 270, 0.5
instr Sweeper
//////////////////////////////////////////////
// Original by Iain McCurdy.
// Adapted by Michael Gogins.
//////////////////////////////////////////////
if p3 == -1 then
  p3 = 1000000
endif
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_Sweeper_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 58
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Sweeper_level)
i_attack = i(gk_Sweeper_attack)
i_release = i(gk_Sweeper_release)
i_sustain = p3 
xtratim i_attack + i_sustain + i_release
kenvelope transeg 0.0, i_attack / 2.0, 1.5, i_amplitude / 2.0, i_attack / 2.0, -1.5, i_amplitude, i_sustain, 0.0, i_amplitude, i_release / 2.0, 1.5, i_amplitude / 2.0, i_release / 2.0, -1.5, 0
ihertz = i_frequency
icps = ihertz
kamp expseg 0.001,0.02,0.2,p3-0.01,0.001
ktonemoddep jspline 0.01,0.05,0.2
ktonemodrte jspline 6,0.1,0.2
ktone poscil3 ktonemoddep, ktonemodrte, gi_Sweeper_sine
kbrite rspline gk_Sweeper_britel, gk_Sweeper_briteh, gk_Sweeper_britels, gk_Sweeper_britehs
ibasfreq init icps
ioctcnt init 3
iphs init 0
a1 hsboscil kenvelope, ktone, kbrite, ibasfreq, gi_Sweeper_sine, gi_Sweeper_octfn, ioctcnt, iphs
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
;a1,a2 pan2 a1, kpan
a1,a2 pan2 a1, k_space_left_to_right
a1 delay a1, rnd(0.1)
a2 delay a2, rnd(0.11)
kenv linsegr 1, 1, 0
kenv = kenvelope
aleft = a1*kenv*.02
aright = a2*kenv*.02
kgain = ampdb(gk_Sweeper_level)
aleft = aleft * k_gain
aright = aright * k_gain
a_damping linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = (aleft + aright) * a_damping
kelapsed timeinsts
kx jspline gi_scene_radius * 2, gi_minimum_rate, gi_maximum_rate
ky jspline gi_scene_radius * 2, gi_minimum_rate, gi_maximum_rate
kz jspline gi_scene_radius * 2, gi_minimum_rate, gi_maximum_rate
kx = kx - gi_scene_radius
ky = ky - gi_scene_radius
kz = kz - gi_scene_radius
printks "Sweeper        i %7.2f t %7.2f [%7.2f] d %7.2f k %7.2f f %7.2f v %7.2f kx %7.2f ky %7.2f kz %7.2f A %9.2f\n", 1.0, p1, p2, kelapsed, p3, p4, ihertz, p5, kx, ky, kz, rms(a_signal)
#ifdef USE_SPATIALIZATION
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_send init 0
a_bsignal, a_spatial_reverb_send Spatialize a_signal, kx, ky, kz
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
endin

gk_Buzzer_harmonics init 9
gk_Buzzer_attack init .05
gk_Buzzer_release init 10
gk_Buzzer_level init -86
gi_Buzzer_sine ftgen 0, 0, 65536, 10, 1
instr Buzzer
insno = p1
istart = p2
iduration = p3
ikey = p4
ivelocity = p5
iphase = p6
ipan = p7
iamp = ampdb(ivelocity) * 4
i_attack = i(gk_Buzzer_attack)
i_release = i(gk_Buzzer_release)
i_sustain = p3
xtratim i_attack + i_sustain + i_release
aenvelope transeg 0.0, i_attack / 2.0, 1.5, iamp / 2.0, i_attack / 2.0, -1.5, iamp, i_sustain, 0.0, iamp, i_release / 2.0, 1.5, iamp / 2.0, i_release / 2.0, -1.5, 0
ihertz = cpsmidinn(ikey)
asignal buzz aenvelope, ihertz, gk_Buzzer_harmonics, gi_Buzzer_sine
k_gain = ampdb(gk_Buzzer_level)
a_damping linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
asignal = asignal * k_gain * a_damping
;asignal vco2 kenvelope, ihertz, 12
;asignal poscil3 kenvelope, ihertz, giharmonics
;asignal distort asignal, gkDistortFactor * .4, giwaveshaping
aleft, aright pan2 asignal, ipan
a_signal = aleft + aright
a_damping linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_signal * a_damping
kx jspline gi_scene_radius * 2, gi_minimum_rate, gi_maximum_rate
ky jspline gi_scene_radius * 2, gi_minimum_rate, gi_maximum_rate
kz jspline gi_scene_radius * 2, gi_minimum_rate, gi_maximum_rate
kx = kx - gi_scene_radius
ky = ky - gi_scene_radius
kz = kz - gi_scene_radius
kelapsed timeinsts
printks "Buzzer         i %7.2f t %7.2f [%7.2f] d %7.2f k %7.2f f %7.2f v %7.2f kx %7.2f ky %7.2f kz %7.2f A %9.2f\n", 1.0, p1, p2, kelapsed, p3, p4, ihertz, p5, kx, ky, kz, rms(a_signal)
#ifdef USE_SPATIALIZATION
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_send init 0
a_bsignal, a_spatial_reverb_send Spatialize a_signal, kx, ky, kz
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
endin

gk_Shiner_attack init 10
gk_Shiner_decay init 10
gk_Shiner_level init -100
instr Shiner
insno = p1
istart = p2
iduration = p3
ikey = p4
ivelocity = p5
iphase = p6
ipan = p7
iamp = ampdb(ivelocity) * 4
i_attack = i(gk_Shiner_attack)
idecay = i(gk_Shiner_decay)
i_sustain = p3
xtratim i_attack + i_sustain + idecay
k_gain = ampdb(gk_Shiner_level)
kenvelope transeg 0.0, i_attack / 2.0, 1.5, iamp / 2.0, i_attack / 2.0, -1.5, iamp, i_sustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
ihertz = cpsmidinn(ikey)
;asignal gbuzz kenvelope, ihertz, 3, gkFirstHarmonic, gkDistortFactor, gisine
;asignal buzz kenvelope, ihertz, gkHarmonics, gisine
;asignal = asignal
asignal vco2 kenvelope * 4, ihertz, 12
;asignal poscil3 kenvelope, ihertz, giharmonics
;asignal distort asignal, gkDistortFactor * .4, giwaveshaping
aleft, aright pan2 asignal, ipan
adamping linseg 0, 0.03, 1, i_sustain, 1, 0.07, 0
aleft = adamping * aleft
aright = adamping * aright
asignal = aleft + aright
asignal = asignal * k_gain
kx jspline gi_scene_radius * 2, gi_minimum_rate, gi_maximum_rate
ky jspline gi_scene_radius * 2, gi_minimum_rate, gi_maximum_rate
kz jspline gi_scene_radius * 2, gi_minimum_rate, gi_maximum_rate
kx = kx - gi_scene_radius
ky = ky - gi_scene_radius
kz = kz - gi_scene_radius
kelapsed timeinsts
printks "Shiner         i %7.2f t %7.2f [%7.2f] d %7.2f k %7.2f f %7.2f v %7.2f kx %7.2f ky %7.2f kz %7.2f A %9.2f\n", 1.0, p1, p2, kelapsed, p3, p4, ihertz, p5, kx, ky, kz, rms(asignal)
absignal[] init 16
absignal, aspatialreverbsend Spatialize asignal, kx, ky, kz
outletv "outbformat", absignal
outleta "out", aspatialreverbsend
endin

gk_Blower_midi_dynamic_range init 127
gk_Blower_grainDensity init 150
gk_Blower_grainDuration init 0.25
gk_Blower_grainAmplitudeRange init 10
gk_Blower_grainFrequencyRange init .033
gk_Blower_level init -50
;                f     #  time  size   1  filcod         skiptime   format   channel
gi_Blower_sample ftgen 0, 0,    65536, 1, "hohner2.wav", 5,         0,       0
;; gi_Blower_sample ftgen 0, 0, 65536, 10, 1, .3, .1, 0, .2, .02, 0, .1, .04
gi_Blower_wintab ftgen 0, 0, 65536, 10, 1, 0, .5, 0, .33, 0, .25, 0, .2, 0, .167
instr Blower
 //////////////////////////////////////////////
 // Original by Hans Mikelson.
 // Adapted by Michael Gogins.
 //////////////////////////////////////////////
if p3 == -1 then
  p3 = 1000000
endif
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
i_level_correction = 134
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Blower_level)
; f1 0 65536 1 "hahaha.aif" 0 4 0
; f2 0 1024 7 0 224 1 800 0
; f3 0 8192 7 1 8192 -1
; f4 0 1024 7 0 512 1 512 0
; f5 0 65536 10 1 .3 .1 0 .2 .02 0 .1 .04
; f6 0 65536 10 1 0 .5 0 .33 0 .25 0 .2 0 .167
; a0 14 50
; p1 p2 p3 p4 p5 p6 p7 p8 p9 p10
; Start Dur Amp Freq GrTab WinTab FqcRng Dens Fade
; i1 0.0 6.5 700 9.00 5 4 .210 200 1.8
; i1 3.2 3.5 800 7.08 . 4 .042 100 0.8
; i1 5.1 5.2 600 7.10 . 4 .0320 100 0.9
; i1 7.2 6.6 900 8.03 . 4 .021 150 1.6
; i1 21.3 4.5 1000 9.00 . 4 .031 150 1.2
; i1 26.5 13.5 1100 6.09 . 4 .121 150 1.5
; i1 30.7 9.3 900 8.05 . 4 .014 150 2.5
; i1 34.2 8.8 700 10.02 . 4 .14 150 1.6
igrtab = gi_Blower_sample 
iHz = i_frequency
ihertz = iHz
ip4 = i_amplitude
ip5 = iHz
ip6 = igrtab
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
i_attack = 0.5
idecay = 0.5
i_sustain = p3 + i_attack + idecay
kenvelope transeg 0.0, i_attack / 2.0, 1.5, .5, i_attack / 2.0, -1.5, 1, i_sustain, 0.0, 1, idecay / 2.0, 1.5, .5, idecay / 2.0, -1.5, 0
; kamp linseg 0, ifade, 1, idur - 2 * ifade, 1, ifade, 0
kamp = kenvelope
; Amp Fqc Dense AmpOff PitchOff GrDur GrTable WinTable MaxGrDur
aoutl grain ip4, ifqc, gk_Blower_grainDensity, gk_Blower_grainAmplitudeRange, gk_Blower_grainFrequencyRange, gk_Blower_grainDuration, igrtab, gi_Blower_wintab, 5
aoutr grain ip4, ifqc, gk_Blower_grainDensity, gk_Blower_grainAmplitudeRange, gk_Blower_grainFrequencyRange, gk_Blower_grainDuration, igrtab, gi_Blower_wintab, 5
asignal = aoutl + aoutr
asignal = asignal * k_gain;
absignal[] init 16
kx jspline gi_scene_radius * 2, gi_minimum_rate, gi_maximum_rate
ky jspline gi_scene_radius * 2, gi_minimum_rate, gi_maximum_rate
kz jspline gi_scene_radius * 2, gi_minimum_rate, gi_maximum_rate
kx = kx - gi_scene_radius
ky = ky - gi_scene_radius
kz = kz - gi_scene_radius
kelapsed timeinsts
printks "Blower         i %7.2f t %7.2f [%7.2f] d %7.2f k %7.2f f %7.2f v %7.2f kx %7.2f ky %7.2f kz %7.2f A %9.2f\n", 1.0, p1, p2, kelapsed, p3, p4, ihertz, p5, kx, ky, kz, rms(asignal)
absignal, asend Spatialize asignal, kx, ky, kz
outleta "out", asend
outletv "outbformat", absignal
endin

instr Controls
gk_Droner_1 invalue "gk_Droner_1"
gk_Droner_2 invalue "gk_Droner_2"
gk_Droner_3 invalue "gk_Droner_3"
gk_Droner_4 invalue "gk_Droner_4"
gk_Droner_5 invalue "gk_Droner_5"
gk_Droner_x invalue "gk_Droner_x"
gk_Droner_y invalue "gk_Droner_y"
gk_Droner_z invalue "gk_Droner_z"
gk_LocalReverbByDistance_Wet invalue "gk_LocalReverbByDistance_Wet"
gk_LocalReverbByDistance_ReverbDecay invalue "gk_LocalReverbByDistance_ReverbDecay"
gk_LocalReverbByDistance_CutoffHz invalue "gk_LocalReverbByDistance_CutoffHz"
gk_SpatialReverb_ReverbDecay invalue "gk_SpatialReverb_ReverbDecay"
gk_SpatialReverb_CutoffHz invalue "gk_SpatialReverb_CutoffHz"
gk_SpatialReverb_Gain invalue "gk_SpatialReverb_Gain"
gk_BformatDecoder_MasterLevel invalue "gk_BformatDecoder_MasterLevel"
endin

prealloc "FMDroner",  8
prealloc "Bower",     8
prealloc "Blower",    8
prealloc "Buzzer",    8
prealloc "Shiner",    8
prealloc "Sweeper",   8

</CsInstruments>
<CsScore>

; p4 is just intonation in MIDI key numbers (numerator can be 0):
; [ ((numerator / denominator) * 12) + (octave * 12) + 24 ] 

t 0  30
;t 0 120

i "STKBeeThree"    0  60 [ (( 0 /  1) * 12) + (0 * 12) + 24 ] 63
i "STKBeeThree"    0  60 [ (( 4 /  5) * 12) + (1 * 12) + 24 ] 60
i "STKBeeThree"    0  30 [ (( 3 /  2) * 12) + (2 * 12) + 24 ] 60
i "STKBeeThree"    0  60 [ ((15 /  8) * 12) + (2 * 12) + 24 ] 57

;i "FMDroner"    0  60 [ (( 0 /  1) * 12) + (0 * 12) + 24 ] 63
;i "FMDroner"    0  60 [ (( 4 /  5) * 12) + (1 * 12) + 24 ] 60
;i "FMDroner"    0  30 [ (( 3 /  2) * 12) + (2 * 12) + 24 ] 60
;i "FMDroner"    0  60 [ ((15 /  8) * 12) + (2 * 12) + 24 ] 57

i "Bower"      30  30 [ (( 5 /  8) * 12) + (2 * 12) + 24 ] 72

i "Sweeper"    60  60 [ (( 2 /  3) * 12) + (0 * 12) + 24 ] 56
i "Sweeper"    60  60 [ ((32 / 45) * 12) + (0 * 12) + 24 ] 54
i "Sweeper"    60  30 [ ((15 /  8) * 12) + (0 * 12) + 24 ] 51

i "Buzzer"     90  30 [ (( 3 /  4) * 12) + (1 * 12) + 24 ] 60

i "Buzzer"    120  60 [ (( 0 /  1) * 12) + (0 * 12) + 24 ] 63
i "FMDroner"  120  60 [ (( 4 /  5) * 12) + (1 * 12) + 24 ] 60
i "Bower"     120  30 [ ((15 /  8) * 12) + (2 * 12) + 24 ] 60
i "FMDroner"  150  30 [ (( 0 /  1) * 12) + (2 * 12) + 24 ] 60

e 200
</CsScore>
</CsoundSynthesizer>

