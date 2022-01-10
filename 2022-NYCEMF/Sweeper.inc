gk_Sweeper_midi_dynamic_range chnexport "gk_Sweeper_midi_dynamic_range", 3 ;  127
gk_Sweeper_attack chnexport "gk_Sweeper_attack", 3 ;  .125
gk_Sweeper_release chnexport "gk_Sweeper_release", 3 ;  .25
gk_Sweeper_britel chnexport "gk_Sweeper_britel", 3 ;  0.1
gk_Sweeper_briteh chnexport "gk_Sweeper_briteh", 3 ;  2.9
gk_Sweeper_britels chnexport "gk_Sweeper_britels", 3 ;  2
gk_Sweeper_britehs chnexport "gk_Sweeper_britehs", 3 ;  1
gk_Sweeper_level chnexport "gk_Sweeper_level", 3 ;  0

gk_Sweeper_midi_dynamic_range init 20
gk_Sweeper_attack init .125
gk_Sweeper_release init .25
gk_Sweeper_britel init 0.1
gk_Sweeper_briteh init 2.9
gk_Sweeper_britels init 2
gk_Sweeper_britehs init 1
gk_Sweeper_level init 0

gk_Sweeper_front_to_back chnexport "gk_Sweeper_front_to_back", 3 ;  0
gk_Sweeper_left_to_right chnexport "gk_Sweeper_left_to_right", 3 ;  0.5
gk_Sweeper_bottom_to_top chnexport "gk_Sweeper_bottom_to_top", 3 ;  0

gk_Sweeper_front_to_back init 0
gk_Sweeper_left_to_right init 0.5
gk_Sweeper_bottom_to_top init 0

gi_Sweeper_sine ftgen 0, 0, 65537, 10, 1
gi_Sweeper_octfn ftgen 0, 0, 65537, -19, 1, 0.5, 270, 0.5

instr Sweeper
//////////////////////////////////////////////
// Original by Iain McCurdy.
// Adapted by Michael Gogins.
//////////////////////////////////////////////
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_Sweeper_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
k_space_front_to_back = gk_Sweeper_front_to_back
k_space_left_to_right = gk_Sweeper_left_to_right
k_space_bottom_to_top = gk_Sweeper_bottom_to_top
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 71.5
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Sweeper_level)

iattack = i(gk_Sweeper_attack)
irelease = i(gk_Sweeper_release)
isustain = p3
xtratim iattack + irelease
kenvelope transegr 0.0, iattack / 2.0, 1.5, i_amplitude / 2.0, iattack / 2.0, -1.5, i_amplitude, isustain, 0.0, i_amplitude, irelease / 2.0, 1.5, i_amplitude / 2.0, irelease / 2.0, -1.5, 0
ihertz = i_frequency
icps = ihertz
kamp expseg 0.001,0.02,0.2,p3-0.01,0.001
ktonemoddep jspline 0.01,0.05,0.2
ktonemodrte jspline 6,0.1,0.2
ktone poscil3 ktonemoddep, ktonemodrte, gi_Sweeper_sine
kbrite rspline gk_Sweeper_britel, gk_Sweeper_briteh, gk_Sweeper_britels, gk_Sweeper_britehs
ibasfreq init icps
ioctcnt init 3
iphs init 0
a1 hsboscil kenvelope, ktone, kbrite, ibasfreq, gi_Sweeper_sine, gi_Sweeper_octfn, ioctcnt, iphs
amod poscil3 0.25, ibasfreq*(1/3), gi_Sweeper_sine
arm = a1*amod
kmix expseg 0.001, 0.01, rnd(1), rnd(3)+0.3, 0.001
kmix=.25
a1 ntrpol a1, arm, kmix
kpanrte jspline 5, 0.05, 0.1
kpandep jspline 0.9, 0.2, 0.4
kpan poscil3 kpandep, kpanrte, gi_Sweeper_sine
;a1,a2 pan2 a1, kpan
a1,a2 pan2 a1, k_space_left_to_right
aleft delay a1, rnd(0.1)
aright delay a2, rnd(0.11)
a_signal = (aleft + aright) * k_gain

prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
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
i_radius =        7 + i_index * 5 + 2
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
