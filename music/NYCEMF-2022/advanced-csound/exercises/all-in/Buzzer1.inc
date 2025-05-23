gk_Buzzer_attack chnexport "gk_Buzzer_attack", 3
gk_Buzzer_release chnexport "gk_Buzzer_release", 3
gk_Buzzer_harmonics chnexport "gk_Buzzer_harmonics", 3
gk_Buzzer_level chnexport "gk_Buzzer_level", 3
gk_Buzzer_midi_dynamic_range chnexport "gk_Buzzer_midi_dynamic_range", 3

gk_Buzzer_attack init .125
gk_Buzzer_release init .25
gk_Buzzer_harmonics init 8
gk_Buzzer_level init 0
gk_Buzzer_midi_dynamic_range init 20

gk_Buzzer_front_to_back chnexport "gk_Buzzer_front_to_back", 3 ;  0
gk_Buzzer_left_to_right chnexport "gk_Buzzer_left_to_right", 3 ;  0.5
gk_Buzzer_bottom_to_top chnexport "gk_Buzzer_bottom_to_top", 3 ;  0

gk_Buzzer_front_to_back init 0
gk_Buzzer_left_to_right init 0.5
gk_Buzzer_bottom_to_top init 0

gi_Buzzer_sine ftgen 0, 0, 65537, 10, 1

instr Buzzer
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_Buzzer_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
k_space_front_to_back = gk_Buzzer_front_to_back
k_space_left_to_right = gk_Buzzer_left_to_right
k_space_bottom_to_top = gk_Buzzer_bottom_to_top
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 75
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Buzzer_level)
i_attack = p3/3.
i_sustain = p3/3.
i_release = p3/3.
xtratim  i_release
a_envelope transegr 0.0, i_attack / 2.0, 1.5, i_amplitude / 2.0, i_attack / 2.0, -1.5, i_amplitude, i_sustain, 0.0, i_amplitude, i_release / 2.0, 1.5, i_amplitude / 2.0, i_release / 2.0, -1.5, 0
a_signal buzz a_envelope, i_frequency, gk_Buzzer_harmonics, gi_Buzzer_sine
a_signal = a_signal * k_gain

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
