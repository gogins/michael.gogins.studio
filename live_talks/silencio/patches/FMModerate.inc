gk_FMModerate_level init 0
instr FMModerate
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
i_overall_amps = 82 ; To start - change to observed value.
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_FMModerate_level)
iattack = 0.002
isustain = p3
idecay = 1.5
irelease = 0.05
p3 = iattack + idecay + isustain + irelease
icarrier = 1
imodulator = 1
ifmamplitude = 8
index = 5.4
ifrequencyb = i_frequency * 1.003
icarrierb = icarrier * 1.004
aindenv transegr 0.0, iattack, -7.0, 1.0, idecay, -10.0, 0.025, isustain, 0.0, 0.025, irelease, -7.0, 0.0
aindex = aindenv * index * ifmamplitude
icosine ftgenonce 0, 0, 65536, 11, 1
aouta foscili 1.0, i_frequency, icarrier, imodulator, index, icosine
aoutb foscili 1.0, ifrequencyb, icarrierb, imodulator, index, icosine
a_signal = (aouta + aoutb) * aindenv
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
prints "FMModerate     i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p7, active(p1)
endin