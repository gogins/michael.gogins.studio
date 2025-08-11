gk_STKBowed_vibrato_level chnexport "gk_STKBosed_vibrato_level", 3
gk_STKBowed_bow_pressure chnexport "gk_STKBowed_bow_pressure", 3
gk_STKBowed_bow_position chnexport "gk_STKBowed_bow_position", 3
gk_STKBowed_vibrato_frequency chnexport "gk_STKBowed_vibrato_frequency", 3
gk_STKBowed_level chnexport "gk_STKBowed_level", 3

gk_STKBowed_vibrato_level init 2.8
gk_STKBowed_bow_pressure init 110
gk_STKBowed_bow_position init 20
gk_STKBowed_vibrato_frequency init 50.2
gk_STKBowed_level init 0

instr STKBowed
; Author: Michael Gogins
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
; Adjust the following value until "overall amps" at the end of solo performance is about -6 dB.
i_overall_amps = 74.5
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_STKBowed_level)
a_signal STKBowed i_frequency, 1.0, 1, gk_STKBowed_vibrato_level, 2, gk_STKBowed_bow_pressure, 4, gk_STKBowed_bow_position, 11, gk_STKBowed_vibrato_frequency
iattack = p3/3.
isustain = p3/3.
idecay = p3/3.
xtratim idecay
a_declicking transegr 0.0, iattack / 2.0, 1.5, i_amplitude / 2.0, iattack / 2.0, -1.5, i_amplitude, isustain, 0.0, i_amplitude, idecay / 2.0, 1.5, i_amplitude / 2.0, idecay / 2.0, -1.5, 0
a_signal = a_signal * i_amplitude * a_declicking * k_gain 

kx, ky instrument_position p2, 10, p1
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
