gk_Blower_grainDensity init 150
gk_Blower_grainDuration init 0.2
gk_Blower_grainAmplitudeRange init 100
gk_Blower_grainFrequencyRange init .033
gk_Blower_level init 0
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
i_midi_velocity = p5
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_overall_amps = 135
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Blower_level)
; f1 0 65536 1 "hahaha.aif" 0 4 0
; f2 0 1024 7 0 224 1 800 0
; f3 0 8192 7 1 8192 -1
; f4 0 1024 7 0 512 1 512 0
; f5 0 1024 10 1 .3 .1 0 .2 .02 0 .1 .04
; f6 0 1024 10 1 0 .5 0 .33 0 .25 0 .2 0 .167
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
igrtab ftgenonce 0, 0, 65536, 10, 1, .3, .1, 0, .2, .02, 0, .1, .04
iwintab ftgenonce 0, 0, 65536, 10, 1, 0, .5, 0, .33, 0, .25, 0, .2, 0, .167
iHz = i_frequency
ihertz = iHz
ip4 = i_amplitude
ip5 = iHz
ip6 = igrtab
ip7 = iwintab
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
idecay = 0.5
isustain = p3 + iattack + idecay
kenvelope transeg 0.0, iattack / 2.0, 1.5, .5, iattack / 2.0, -1.5, 1, isustain, 0.0, 1, idecay / 2.0, 1.5, .5, idecay / 2.0, -1.5, 0
; kamp linseg 0, ifade, 1, idur - 2 * ifade, 1, ifade, 0
kamp = kenvelope
; Amp Fqc Dense AmpOff PitchOff GrDur GrTable WinTable MaxGrDur
aoutl grain ip4, ifqc, gk_Blower_grainDensity, gk_Blower_grainAmplitudeRange,  gk_Blower_grainFrequencyRange, gk_Blower_grainDuration, igrtab, iwintab, 5
aoutr grain ip4, ifqc, gk_Blower_grainDensity, gk_Blower_grainAmplitudeRange, gk_Blower_grainFrequencyRange, gk_Blower_grainDuration, igrtab, iwintab, 5
a_signal = aoutl + aoutr
i_attack = .002
i_sustain = p3
i_release = 0.01
p3 = i_attack + i_sustain + i_release
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_signal * i_amplitude * a_declicking * k_gain
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
prints "Blower         i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", p1, p2, p3, p4, p5, p7, active(p1)
endin