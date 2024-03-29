gk_Bower_midi_dynamic_range chnexport "gk_Bower_midi_dynamic_range", 3
gk_Bower_attack chnexport "gk_Bower_attack", 3
gk_Bower_release chnexport "gk_Bower_release", 3
gk_Bower_level chnexport "gk_Bower_level", 3
gk_Bower_pressure chnexport "gk_Bower_pressure", 3

gk_Bower_midi_dynamic_range init 20
gk_Bower_attack init .125
gk_Bower_release init .125
gk_Bower_level init 0
gk_Bower_pressure init 0.25

gk_Bower_front_to_back chnexport "gk_Bower_front_to_back", 3 ;  0
gk_Bower_left_to_right chnexport "gk_Bower_left_to_right", 3 ;  0.5
gk_Bower_bottom_to_top chnexport "gk_Bower_bottom_to_top", 3 ;  0

gk_Bower_front_to_back init 0
gk_Bower_left_to_right init 0.5
gk_Bower_bottom_to_top init 0

gi_Bower_sine ftgen 0,0,65537,10,1

instr Bower
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_Bower_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
k_space_front_to_back = gk_Bower_front_to_back
k_space_left_to_right = gk_Bower_left_to_right
k_space_bottom_to_top = gk_Bower_bottom_to_top
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 66
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Bower_level)
iamp = i_amplitude
iattack = p3/3.
isustain = p3/3.
idecay = p3/3.
xtratim iattack + idecay
kenvelope transegr 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
ihertz = cpsmidinn(i_midi_key)
kamp = kenvelope
kfreq = ihertz
kpres = 0.25
krat rspline 0.006,0.988,1,4
kvibf = 4.5
kvibamp = 0
iminfreq = i(kfreq) / 2
aSig wgbow kamp,kfreq,gk_Bower_pressure,krat,kvibf,kvibamp,gi_Bower_sine,iminfreq
a_signal = aSig * kenvelope * k_gain

prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
#ifdef USE_SPATIALIZATION
kx, ky instrument_position p2, 10, p1
kz init 0
Spatialize3D a_signal, kx, ky, kz
#else
a_out_left, a_out_right pan2 a_signal, p7
outleta "outleft", a_out_left
outleta "outright", a_out_right
#end
;printks "Blower         i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", 1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_out_left)), dbamp(rms(a_out_right))
endin
