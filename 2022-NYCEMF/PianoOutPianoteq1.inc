// This must be initialized in the orc header before any #includes.

// gi_Pianoteq vstinit "/home/mkg/Pianoteq\ 7/x86-64bit/Pianoteq\ 7.so", 0
//vstinfo gi_Pianoteq 

gk_PianoOutPianoteq_level chnexport "gk_PianoOutPianoteq_level", 3 ;  0
gi_PianoOutPianoteq_print chnexport "gi_PianoOutPianoteq_print", 3 ;  1
gk_PianoOutPianoteq_front_to_back chnexport "gk_PianoOutPianoteq_front_to_back", 3 ;  0
gk_PianoOutPianoteq_left_to_right chnexport "gk_PianoOutPianoteq_left_to_right", 3 ;  0.5
gk_PianoOutPianoteq_bottom_to_top chnexport "gk_PianoOutPianoteq_bottom_to_top", 3 ;  0

gk_PianoOutPianoteq_level init 0
gi_PianoOutPianoteq_print init 1
gk_PianoOutPianoteq_front_to_back init 0
gk_PianoOutPianoteq_left_to_right init 0.5
gk_PianoOutPianoteq_bottom_to_top init 0

instr PianoOutPianoteq
vstprogset gi_Pianoteq, 1
; Sustain off.
vstparamset gi_Pianoteq, 6, 1
; Reverb switch off.
vstparamset gi_Pianoteq, 93, 0
k_gain = ampdb(gk_PianoOutPianoteq_level)
i_overall_amps = 87
i_normalization = ampdb(-i_overall_amps) * 2
i_amplitude = ampdb(80) * i_normalization
if gi_PianoOutPianoteq_print == 1 then
  vstinfo gi_PianoOutPianoteq_print
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

absignal[] init 16
aspatialreverbsend init 0
k_space_front_to_back init 0
k_space_left_to_right init 0
k_space_bottom_to_top init 0
; opcode Spatialize, a[]a, akkk
absignal, aspatialreverbsend Spatialize a_signal, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
k_signal_rms rms a_signal
kw = k(absignal[0])
kx = k(absignal[1])
ky = k(absignal[2])
kz = k(absignal[3])
ksend = k(aspatialreverbsend)
printks "%-24s rms %9.4f W %9.4f X %9.4f Y %9.4f Z%9.4f  send %9.4f \n", 0.5, nstrstr(p1), k_signal_rms, kw, kx, ky, kz, ksend
outletv "outbformat", absignal
outleta "out", aspatialreverbsend

prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin
