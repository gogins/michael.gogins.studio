// This must be initialized in the orc header before any #includes.
// gi_Pianoteq vstinit "/home/mkg/Pianoteq\ 7/x86-64bit/Pianoteq\ 7.so", 0

// To print parameters:
// validator "/Library/Audio/Plug-Ins/VST3/Pianoteq 7.vst3"

//vstinfo gi_Pianoteq 

gk_PianoOutPianoteq_level chnexport "gk_PianoOutPianoteq_level", 3 ;  0
gi_PianoOutPianoteq_print chnexport "gi_PianoOutPianoteq_print", 3 ;  1
gk_PianoOutPianoteq_front_to_back chnexport "gk_PianoOutPianoteq_front_to_back", 3 ;  0
gk_PianoOutPianoteq_left_to_right chnexport "gk_PianoOutPianoteq_left_to_right", 3 ;  0.5
gk_PianoOutPianoteq_bottom_to_top chnexport "gk_PianoOutPianoteq_bottom_to_top", 3 ;  0

gk_PianoOutPianoteq_level init 0
gi_PianoOutPianoteq_print init 0
gk_PianoOutPianoteq_front_to_back init 0
gk_PianoOutPianoteq_left_to_right init 0.5
gk_PianoOutPianoteq_bottom_to_top init 0

instr PianoOutPianoteq
vstprogset gi_Pianoteq, 1
; Sustain off.
vstparamset gi_Pianoteq, 6, 0
; Reverb switch off.
vstparamset gi_Pianoteq, 93, 0
k_gain = ampdb(gk_PianoOutPianoteq_level)
i_overall_amps = 87
i_normalization = ampdb(-i_overall_amps) * 2
i_amplitude = ampdb(80) * i_normalization
if gi_PianoOutPianoteq_print == 1 then
  vstinfo gi_Pianoteq
endif
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
ainleft init 0
ainright init 0
aoutleft, aoutright vstaudio gi_Pianoteq, ainleft, ainright
a_signal = aoutleft + aoutright
a_signal *= k_gain
a_signal *= i_amplitude
a_out_left, a_out_right pan2 a_signal, gk_PianoOutPianoteq_left_to_right
k_1 rms aoutleft
k_2 rms aoutright

printks "%-24s l %9.4f r %9.4f\n", 0.5, nstrstr(p1), k_1, k_2
;prints  "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
k_time times
k_space_front_to_back = .25
k_space_left_to_right = .25
k_space_bottom_to_top = .25

#ifdef SPATIALIZE_GOGINS
kx, ky instrument_position p2, 10, p1
kz init 0
Spatialize3D a_signal, kx, ky, kz
printks "%-24s signal %9.4f\n", 0.5, nstrstr(p1), k(a_signal)
#end

#ifdef SPATIALIZE_IEM
a_iem_out[] init 64
k_azimuth, k_elevation, k_radius iem_cartesian_to_spherical_vst gi_size, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
i_index init floor(p1) - 1
i_azimuth =     7 + i_index * 5 + 0
i_elevation =   7 + i_index * 5 + 1
i_radius =        7 + i_index * 5 + 2
prints "i_index: %3d i_azimuth: %3d i_elevation: %3d i_radius: %3d\n", i_index, i_azimuth, i_elevation, i_radius
vstparamset gi_iem_multi_encoder, i_azimuth, k_azimuth
vstparamset gi_iem_multi_encoder, i_elevation, k_elevation
vstparamset gi_iem_multi_encoder, i_radius, k_radius
a_iem_out[i_index] = a_signal
outletv "iem_out", a_iem_out
a_iem_out[i_index] = a_signal
k_iem_out rms a_iem_out[i_index]
outletv "iem_out", a_iem_out
printks "%-24s index %3d ie_out %9.4f\n", 0.5, nstrstr(p1), i_index, k_iem_out
#end

#ifdef SPATIALIZE_STEREO
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
#end

prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin
