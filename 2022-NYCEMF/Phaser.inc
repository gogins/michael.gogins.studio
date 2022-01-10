gk_Phaser_attack chnexport "gk_Phaser_attack", 3 ;  .125
gk_Phaser_release chnexport "gk_Phaser_release", 3 ;  .125
gk_Phaser_ratio1 chnexport "gk_Phaser_ratio1", 3 ;  1
gk_Phaser_ratio2 chnexport "gk_Phaser_ratio2", 3 ;  1/5
gk_Phaser_index1 chnexport "gk_Phaser_index1", 3 ;  1.01
gk_Phaser_index2 chnexport "gk_Phaser_index2", 3 ;  .103
gk_Phaser_level chnexport "gk_Phaser_level", 3 ;  0.5
gk_Phaser_midi_dynamic_range chnexport "gk_Phaser_midi_dynamic_range", 3 ;  20

gk_Phaser_attack init .125
gk_Phaser_release init .125
gk_Phaser_ratio1 init 1
gk_Phaser_ratio2 init 5
gk_Phaser_index1 init 5
gk_Phaser_index2 init .05
gk_Phaser_level init 0.5
gk_Phaser_midi_dynamic_range init 20

gk_Phaser_front_to_back chnexport "gk_Phaser_front_to_back", 3 ;  0
gk_Phaser_left_to_right chnexport "gk_Phaser_left_to_right", 3 ;  0.5
gk_Phaser_bottom_to_top chnexport "gk_Phaser_bottom_to_top", 3 ;  0

gk_Phaser_front_to_back init 0
gk_Phaser_left_to_right init 0.5
gk_Phaser_bottom_to_top init 0

gi_Phaser_sine ftgen 0,0,65537,10,1

instr Phaser
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_Phaser_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
k_space_front_to_back = gk_Phaser_front_to_back
k_space_left_to_right = gk_Phaser_left_to_right
k_space_bottom_to_top = gk_Phaser_bottom_to_top
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 81
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Phaser_level)
i_attack = i(gk_Phaser_attack)
i_release = i(gk_Phaser_release)
i_sustain = 1000
xtratim i_attack + i_release
a_envelope transegr 0.0, i_attack / 2.0, 1.5, i_amplitude / 2.0, i_attack / 2.0, -1.5, i_amplitude, i_sustain, 0.0, i_amplitude, i_release / 2.0, 1.5, i_amplitude / 2.0, i_release / 2.0, -1.5, 0
a1,a2 crosspm gk_Phaser_ratio1, gk_Phaser_ratio2, gk_Phaser_index1, gk_Phaser_index2, i_frequency, gi_Phaser_sine, gi_Phaser_sine
a_signal = (a1 + a2) * k_gain * a_envelope

prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
;printks "Phaser         i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", 1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(aleft)), dbamp(rms(aright))
k_time times
k_space_front_to_back = sin(k_time / p1)
k_space_left_to_right = cos(k_time / p1)
k_space_bottom_to_top = .5

#ifdef SPATIALIZE_GOGINS
kx, ky instrument_position p2, 10, p1
kz init 0
Spatialize3D a_signal, kx, ky, kz
#end
#ifdef SPATIALIZE_IEM
a_iem_out[] init 64
k_azimuth, k_elevation, k_radius iem_cartesian_to_spherical_vst gi_size, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
i_index init floor(p1) - 1
i_azimuth =     7 + i_index * 5 + 0
i_elevation =   7 + i_index * 5 + 1
i_radius =      7 + i_index * 5 + 2
prints "i_index: %3d i_azimuth: %3d i_elevation: %3d i_radius: %3d\n", i_index, i_azimuth, i_elevation, i_radius
vstparamset gi_iem_multi_encoder, i_azimuth, k_azimuth
vstparamset gi_iem_multi_encoder, i_elevation, k_elevation
vstparamset gi_iem_multi_encoder, i_radius, k_radius
a_iem_out[i_index] = a_signal
outletv "iem_out", a_iem_out
#end
#ifdef SPATIALIZE_STEREO
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
#end
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

