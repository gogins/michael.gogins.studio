gk_Shiner_midi_dynamic_range chnexport "gk_Shiner_midi_dynamic_range", 3 ;  127
gk_Shiner_attack chnexport "gk_Shiner_attack", 3 ;  .0125
gk_Shiner_release chnexport "gk_Shiner_release", 3 ;  .0125
gk_Shiner_level chnexport "gk_Shiner_level", 3 ;  0.5

gk_Shiner_midi_dynamic_range init 20
gk_Shiner_attack init .0125
gk_Shiner_release init .0125
gk_Shiner_level init 0.5

gk_Shiner_front_to_back chnexport "gk_Shiner_front_to_back", 3 ;  0
gk_Shiner_left_to_right chnexport "gk_Shiner_left_to_right", 3 ;  0.5
gk_Shiner_bottom_to_top chnexport "gk_Shiner_bottom_to_top", 3 ;  0

gk_Shiner_front_to_back init 0
gk_Shiner_left_to_right init 0.5
gk_Shiner_bottom_to_top init 0

instr Shiner
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_Shiner_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
k_space_front_to_back = gk_Shiner_front_to_back
k_space_left_to_right = gk_Shiner_left_to_right
k_space_bottom_to_top = gk_Shiner_bottom_to_top
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 80
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Shiner_level)

iattack = i(gk_Shiner_attack)
idecay = i(gk_Shiner_release)
isustain = p3 - i(gk_Shiner_attack)

iattack = p3/3.
i_sustain = p3/3.
idecay = p3/3.
;print iattack, i_sustain, idecay
xtratim idecay
kenvelope transegr 0.0, iattack / 2.0, 1.5, .5, iattack / 2.0, -1.5, 1, i_sustain, 0.0, 1, idecay / 2.0, 1.5, .5, idecay / 2.0, -1.5, 0

ihertz = cpsmidinn(i_midi_key)
gk_Harmonics = 1 * 20
asignal vco2 kenvelope * 4, ihertz, 12
k_gain = ampdb(gk_Shiner_level) * .5
a_signal = asignal * k_gain * i_amplitude

kx, ky instrument_position -p2, 10, p1
kz init 0

#ifdef SPATIALIZE_GOGINS
absignal[] init 16
aspatialreverbsend init 0
; opcode Spatialize, a[]a, akkk
absignal, aspatialreverbsend Spatialize a_signal, kx, ky, kz
outletv "outbformat", absignal
outleta "out", aspatialreverbsend
#end

#ifdef SPATIALIZE_STEREO
a_out_left, a_out_right pan2 a_signal, ky
outleta "outleft", a_out_left
outleta "outright", a_out_right
#end
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin
