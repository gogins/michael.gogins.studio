gk_Sweeper_midi_dynamic_range chnexport "gk_Sweeper_midi_dynamic_range", 3 ;  127
gk_Sweeper_attack chnexport "gk_Sweeper_attack", 3 ;  .125
gk_Sweeper_release chnexport "gk_Sweeper_release", 3 ;  .25
gk_Sweeper_bright_min chnexport "gk_Sweeper_bright_min", 3 ;  0.1
gk_Sweeper_bright_max chnexport "gk_Sweeper_bright_max", 3 ;  2.9
gk_Sweeper_rate_min chnexport "gk_Sweeper_rate_min", 3 ;  2
gk_Sweeper_rate_max chnexport "gk_Sweeper_rate_max", 3 ;  1
gk_Sweeper_level chnexport "gk_Sweeper_level", 3 ;  0

gk_Sweeper_midi_dynamic_range init 20
gk_Sweeper_attack init .125
gk_Sweeper_release init .25
gk_Sweeper_bright_min init 0.1
gk_Sweeper_bright_max init 2.9
gk_Sweeper_rate_min init 2
gk_Sweeper_rate_max init 1
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
i_level_correction = 71
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Sweeper_level)

iattack = i(gk_Sweeper_attack)
isustain max iattack, p3 - iattack
idecay = i(gk_Sweeper_release)
xtratim idecay
k_envelope transegr 0.0, iattack / 2.0, 1.5, i_amplitude / 2.0, iattack / 2.0, -1.5, i_amplitude, isustain, 0.0, i_amplitude, idecay / 2.0, 1.5, i_amplitude / 2.0, idecay / 2.0, -1.5, 0
ihertz = i_frequency
icps = ihertz
kamp expseg 0.001,0.02,0.2,p3-0.01,0.001
k_tonality_mod_depth jspline 0.01,0.05,0.2
k_tonality_mod_rate jspline 6,0.1,0.2
k_cyclic_tonality poscil3 k_tonality_mod_depth, k_tonality_mod_rate, gi_Sweeper_sine
; kres rspline krangeMin, krangeMax, kcpsMin, kcpsMax
k_brightness rspline gk_Sweeper_bright_min, gk_Sweeper_bright_max, gk_Sweeper_rate_min, gk_Sweeper_rate_max
i_base_frequency init icps
i_octave_count init 3
i_initial_phase init 0

gS_comment init {{
I keep forgetting what the parameters do...

k_envelope --                   amplitude of note.
k_cyclic_tonality --            cyclic tonality parameter relative to 
                                i_base_frequency in logarithmic octave, range 
                                0 to 1, values > 1 can be used, and are 
                                internally reduced to frac(k_cyclic_tonality).
k_brightness --                 brightness parameter relative to i_base_frequency, 
                                achieved by weighting i_octave_count octaves. 
                                It is scaled in such a way, that a value of 0 
                                corresponds to the orignal value of i_base_frequency, 
                                1 corresponds to one octave above ibasfreq, 
                                -2 corresponds to two octaves below i_base_frequency,
                                etc. k_brightness may be fractional.
i_base_frequency --             base frequency to which tonality and brighness 
                                are relative.
gi_Sweeper_sine --              function table of the waveform, usually a sine
gi_Sweeper_octfn --             function table used for weighting the octaves, 
                                usually something like:
                                f1 0  1024  -19  1  0.5  270  0.5.
i_octave_count --               number of octaves used for brightness blending. 
                                Must be in the range 2 to 10. Default is 3.
i_initial_phase --              initial phase of the oscillator. If 
                                i_initial_phase = -1, initialization is skipped.

hsboscil takes tonality and brightness as arguments, relative to a base frequency 
(i_base_frequency). Tonality is a cyclic parameter in the logarithmic octave, 
brightness is realized by mixing multiple weighted octaves.It is useful when tone 
space is understood in a concept of polar coordinates.

Making k_cyclic_tonality a line, and k_brightness a constant, produces Risset's 
glissando.

Oscillator table iwfn is always read interpolated. Performance time requires 
about i_octave_count * oscili.

}}

a1 hsboscil k_envelope, k_cyclic_tonality, k_brightness, i_base_frequency, gi_Sweeper_sine, gi_Sweeper_octfn, i_octave_count, i_initial_phase
amod poscil3 0.25, i_base_frequency*(1/3), gi_Sweeper_sine
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
