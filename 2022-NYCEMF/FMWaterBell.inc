//////////////////////////////////////////////
// Original by Steven Yi.
// Adapted by Michael Gogins.
//////////////////////////////////////////////
gk_FMWaterBell_level chnexport "gk_FMWaterBell_level", 3 ; 0
gi_FMWaterBell_attack chnexport "gi_FMWaterBell_attack", 3 ; 0.002
gi_FMWaterBell_release chnexport "gi_FMWaterBell_release", 3 ; 0.01
gi_FMWaterBell_sustain chnexport "gi_FMWaterBell_sustain", 3 ; 20
gi_FMWaterBell_sustain_level chnexport "gi_FMWaterBell_sustain_level", 3 ; .1
gk_FMWaterBell_index chnexport "gk_FMWaterBell_index", 3 ; .5
gk_FMWaterBell_crossfade chnexport "gk_FMWaterBell_crossfade", 3 ; .5
gk_FMWaterBell_vibrato_depth chnexport "gk_FMWaterBell_vibrato_depth", 3 ; 0.05
gk_FMWaterBell_vibrato_rate chnexport "gk_FMWaterBell_vibrato_rate", 3 ; 6
gk_FMWaterBell_midi_dynamic_range chnexport "gk_FMWaterBell_midi_dynamic_range", 3 ; 20

gk_FMWaterBell_level init 0
gi_FMWaterBell_attack init 0.002
gi_FMWaterBell_release init 0.01
gi_FMWaterBell_sustain init 20
gi_FMWaterBell_sustain_level init .1
gk_FMWaterBell_index init .5
gk_FMWaterBell_crossfade init .5
gk_FMWaterBell_vibrato_depth init 0.05
gk_FMWaterBell_vibrato_rate init 6
gk_FMWaterBell_midi_dynamic_range init 20
gk_FMWaterBell_front_to_back init 0
gk_FMWaterBell_left_to_right init 0.5
gk_FMWaterBell_bottom_to_top init 0

gi_FMWaterBell_cosine ftgen 0, 0, 65537, 11, 1

instr FMWaterBell
i_instrument = p1
i_time = p2
i_duration = p3
; One of the envelopes in this instrument should be releasing, and use this:
i_sustain = 1000
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
i_level_correction = 75
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization * 1.6
k_gain = ampdb(gk_FMWaterBell_level)
a_signal fmbell	1, i_frequency, gk_FMWaterBell_index, gk_FMWaterBell_crossfade, gk_FMWaterBell_vibrato_depth, gk_FMWaterBell_vibrato_rate, gi_FMWaterBell_cosine, gi_FMWaterBell_cosine, gi_FMWaterBell_cosine, gi_FMWaterBell_cosine, gi_FMWaterBell_cosine ;, gi_FMWaterBell_sustain
;a_envelope linsegr 0, gi_FMWaterBell_attack, 1, i_sustain, gi_FMWaterBell_sustain_level, gi_FMWaterBell_release, 0
a_envelope linsegr 0, gi_FMWaterBell_attack, 1, i_sustain, 1, gi_FMWaterBell_release, 0
; ares transegr ia, idur, itype, ib [, idur2] [, itype] [, ic] ...
; a_envelope transegr 0, gi_FMWaterBell_attack, 12, 1, i_sustain, 12, gi_FMWaterBell_sustain_level, gi_FMWaterBell_release, 12, 0
a_signal = a_signal * i_amplitude * a_envelope * k_gain

k_time times
k_space_front_to_back = .75
k_space_left_to_right = .75
k_space_bottom_to_top = .75

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
k_time times
;printks "%-24s             t %9.4f L %9.4f R %9.4f\n", 1, nstrstr(p1), k_time, downsamp(a_out_left), downsamp(a_out_right)
#end
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin
