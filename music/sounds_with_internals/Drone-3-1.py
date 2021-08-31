orc = '''
sr = 48000
ksmps = 128
nchnls = 2
0dbfs = 20000

connect "Internals_1",  "outleft",  "MVerb",        "inleft"
connect "Internals_1",  "outright", "MVerb",        "inright"
connect "Bower",        "outleft",  "MVerb",        "inleft"
connect "Bower",        "outright", "MVerb",        "inright"
connect "Phaser",       "outleft",  "MVerb",        "inleft"
connect "Phaser",       "outright", "MVerb",        "inright"
connect "Droner",       "outleft",  "MVerb",        "inleft"
connect "Droner",       "outright", "MVerb",        "inright"
connect "Sweeper",      "outleft",  "MVerb",        "inleft"
connect "Sweeper",      "outright", "MVerb",        "inright"
connect "Buzzer",       "outleft",  "MVerb",        "inleft"
connect "Buzzer",       "outright", "MVerb",        "inright"
connect "Blower",       "outleft",  "MVerb",        "inleft"
connect "Blower",       "outright", "MVerb",        "inright"
connect "Shiner",       "outleft",  "MVerb",        "inleft"
connect "Shiner",       "outright", "MVerb",        "inright"
connect "MVerb",        "outleft",  "MasterOutput", "inleft"
connect "MVerb",        "outright", "MasterOutput",	"inright"

alwayson "MVerb"
alwayson "ReverbRight"
alwayson "MasterOutput"

opcode ratio2midinn, i, iii
i_fundamental, i_numerator, i_denominator xin
i_frequency = i_fundamental * i_numerator / i_denominator
; print i_frequency
; m = 12*log2(fm/440 Hz) + 69
i_log2 = log(2)
i_midinn = 12 * (log(i_frequency / 440) / i_log2) + 69
; print i_midinn
xout i_midinn
endop

instr 101
i_instrument = p1
i_time = p2
i_duration = p3
i_fundamental = p4
i_numerator = p5
i_denominator = p6
i_midi_velocity = p7
i_pan = p8
i_ratio = i_numerator / i_denominator
i_frequency = i_fundamental * i_ratio
i_midi_key ratio2midinn i_fundamental, i_numerator, i_denominator
event_i "i", "Internals_1", 0, i_duration, i_midi_key, i_midi_velocity, 0, i_pan
endin

instr 102
i_instrument = p1
i_time = p2
i_duration = p3
i_fundamental = p4
i_numerator = p5
i_denominator = p6
i_midi_velocity = p7
i_pan = p8
i_ratio = i_numerator / i_denominator
i_frequency = i_fundamental * i_ratio
i_midi_key ratio2midinn i_fundamental, i_numerator, i_denominator
event_i "i", "Sweeper", 0, i_duration, i_midi_key, i_midi_velocity, 0, i_pan
endin

instr 103
i_instrument = p1
i_time = p2
i_duration = p3
i_fundamental = p4
i_numerator = p5
i_denominator = p6
i_midi_velocity = p7
i_pan = p8
i_ratio = i_numerator / i_denominator
i_frequency = i_fundamental * i_ratio
i_midi_key ratio2midinn i_fundamental, i_numerator, i_denominator
event_i "i", "Bower", 0, i_duration, i_midi_key, i_midi_velocity, 0, i_pan
endin

instr 104
i_instrument = p1
i_time = p2
i_duration = p3
i_fundamental = p4
i_numerator = p5
i_denominator = p6
i_midi_velocity = p7
i_pan = p8
i_ratio = i_numerator / i_denominator
i_frequency = i_fundamental * i_ratio
i_midi_key ratio2midinn i_fundamental, i_numerator, i_denominator
event_i "i", "Phaser", 0, i_duration, i_midi_key, i_midi_velocity, 0, i_pan
endin

gk_Blower_level chnexport "gk_Blower_level", 3
gk_Blower_pan chnexport "gk_Blower_pan", 3
gk_Blower_grainDensity chnexport "gk_Blower_grainDensity", 3
gk_Blower_grainDuration chnexport "gk_Blower_grainDuration", 3
gk_Blower_grainAmplitudeRange chnexport "gk_Blower_grainAmplitudeRange", 3
gk_Blower_grainFrequencyRange chnexport "gk_Blower_grainFrequencyRange", 3
gk_Blower_midi_dynamic_range init 20
gi_Blower_grtab ftgen 0, 0, 65537, 10, 1, .3, .1, 0, .2, .02, 0, .1, .04
gi_Blower_wintab ftgen 0, 0, 65537, 10, 1, 0, .5, 0, .33, 0, .25, 0, .2, 0, .167
instr Blower
//////////////////////////////////////////////
// Original by Hans Mikelson.
// Adapted by Michael Gogins.
//////////////////////////////////////////////
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_Blower_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = gk_Blower_pan
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 90
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Blower_level)
iHz = i_frequency
ihertz = iHz
ip4 = i_amplitude
ip5 = iHz
ip6 = gi_Blower_grtab
ip7 = gi_Blower_wintab
ip8 = 0.033
ip8 = .002
ip9 = 150
ip9 = 100
ip10 = 1.6
ip10 = 3
idur = p3
iamp = i_amplitude ; p4
ifqc = iHz ; cpspch(p5)
igrtab = ip6
iwintab = ip7
ifrng = ip8
idens = ip9
ifade = ip10
igdur = 0.2
iattack = 0.5
i_sustain = p3
idecay = 1.5
kenvelope transegr 0.0, iattack / 2.0, 1.5, .5, iattack / 2.0, -1.5, 1, i_sustain, 0.0, 1, idecay / 2.0, 1.5, .5, idecay / 2.0, -1.5, 0
; kamp linseg 0, ifade, 1, idur - 2 * ifade, 1, ifade, 0
kamp = kenvelope
; Amp Fqc Dense AmpOff PitchOff GrDur GrTable WinTable MaxGrDur
aoutl grain ip4, ifqc, gk_Blower_grainDensity, gk_Blower_grainAmplitudeRange, gk_Blower_grainFrequencyRange, gk_Blower_grainDuration, igrtab, iwintab, 5
aoutr grain ip4, ifqc, gk_Blower_grainDensity, gk_Blower_grainAmplitudeRange, gk_Blower_grainFrequencyRange, gk_Blower_grainDuration, igrtab, iwintab, 5
a_signal = aoutl + aoutr
i_attack = .002
i_release = 0.01
; printks2 "csound: gk_Blower_level:               %9.4f\\n", gk_Blower_level
; printks2 "csound: gk_Blower_grainDensity:        %9.4f\\n", gk_Blower_grainDensity
; printks2 "csound: gk_Blower_grainAmplitudeRange: %9.4f\\n", gk_Blower_grainAmplitudeRange
; printks2 "csound: gk_Blower_grainFrequencyRange: %9.4f\\n", gk_Blower_grainFrequencyRange
; printks2 "csound: gk_Blower_grainDuration:       %9.4f\\n", gk_Blower_grainDuration
p3 = i_attack + i_sustain + i_release
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_signal * i_amplitude * a_declicking * k_gain
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
prints  "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
; printks "Blower         i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\\n", 1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_out_left)), dbamp(rms(a_out_right))
endin

gk_Internals_1_mod_amp chnexport "gk_Internals_1_mod_amp", 3
gk_Internals_1_mod_hz chnexport "gk_Internals_1_mod_hz", 3
gk_Internals_1_level chnexport "gk_Internals_1_level", 3
gk_Internals_1_pan chnexport "gk_Internals_1_pan", 3
gS_Internals_1_mod_waveform chnexport "gS_Internals_1_mod_waveform", 3
gS_Internals_1_waveform chnexport "gS_Internals_1_waveform", 3
gk_Internals_1_k1 chnexport "gk_Internals_1_k1", 3
gk_Internals_1_k2 chnexport "gk_Internals_1_k2", 3
gk_Internals_1_k3 chnexport "gk_Internals_1_k3", 3
gk_Internals_1_k4 chnexport "gk_Internals_1_k4", 3
gk_Internals_1_k5 chnexport "gk_Internals_1_k5", 3
gk_Internals_1_k6 chnexport "gk_Internals_1_k6", 3
gk_Internals_1_k7 chnexport "gk_Internals_1_k7", 3
gk_Internals_1_k8 chnexport "gk_Internals_1_k8", 3
gi_Internals_1_sine ftgen 0, 0, 65537, 10, 1, 0, .02
instr Internals_1
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
i_phase = p6
i_pan = p7
k1 = gk_Internals_1_k1
k2 = gk_Internals_1_k2
k3 = gk_Internals_1_k3
k4 = gk_Internals_1_k4
k5 = gk_Internals_1_k5
k6 = gk_Internals_1_k6
k7 = gk_Internals_1_k7
k8 = gk_Internals_1_k8
i_amplitude = ampdb(i_midi_velocity)
i_attack =  p3 * (1 / 4) * (4 / 3)
i_sustain = p3 * (1 / 2) * (4 / 3)
i_release =   p3 * (1 / 4) * (4 / 3)
p3 = i_attack + i_sustain + i_release
ak_envelope transeg 0.0, i_attack / 2.0, 1.5, i_amplitude / 2.0, i_attack / 2.0, -1.5, i_amplitude, i_sustain, 0.0, i_amplitude, i_release / 2.0, 1.5, i_amplitude / 2.0, i_release / 2.0, -1.5, 0
i_frequency = cpsmidinn(i_midi_key)
; print i_frequency
i_waveform init 0
if strcmp(gS_Internals_1_waveform, "Sine") == 0 then
i_waveform = 0
endif
if strcmp(gS_Internals_1_waveform, "Sawtooth") == 0 then
i_waveform = 1
endif
if strcmp(gS_Internals_1_waveform, "Triangle") == 0 then
i_waveform = 2
endif
if strcmp(gS_Internals_1_waveform, "Chebyshev") == 0 then
i_waveform = 3 
endif
if i_waveform == 0 then
a_signal poscil3 1, i_frequency, gi_Internals_1_sine
endif
if i_waveform == 1 then
a_signal vco2 1, i_frequency, 8
endif
if i_waveform == 2 then
a_signal vco2 1, i_frequency, 12
endif
if i_waveform == 3 then
a_signal poscil3 1, i_frequency, gi_Internals_1_sine
a_signal chebyshevpoly a_signal, 0, k1, k2, k3, k4, k5, k6, k7, k8
endif
i_mod_waveform init 12
if strcmp(gS_Internals_1_mod_waveform, "Triangle") == 0 then
i_mod_waveform init 12
endif 
if strcmp(gS_Internals_1_mod_waveform, "Sawtooth") == 0 then
i_mod_waveform init 4
endif
if strcmp(gS_Internals_1_mod_waveform, "Square") == 0 then
i_mod_waveform init 10
endif 
; prints   "Csound: gS_Internals_1_waveform:     %s\\n", gS_Internals_1_waveform
; prints   "Csound: gS_Internals_1_mod_waveform: %s\\n", gS_Internals_1_mod_waveform
; prints   "Csound: i_mod_waveform:              %9.4f\\n", i_mod_waveform
; printks2 "Csound: gk_Internals_1_mod_amp:      %9.4f\\n", gk_Internals_1_mod_amp
; printks2 "Csound: gk_Internals_1_mod_hz:       %9.4f\\n", gk_Internals_1_mod_hz
; printks2 "Csound: gk_Internals_1_level:        %9.4f\\n", gk_Internals_1_level
; printks2 "Csound: gk_Internals_1_k1:           %9.4f\\n", gk_Internals_1_k1
; printks2 "Csound: gk_Internals_1_k2:           %9.4f\\n", gk_Internals_1_k2
; printks2 "Csound: gk_Internals_1_k3:           %9.4f\\n", gk_Internals_1_k3
; printks2 "Csound: gk_Internals_1_k4:           %9.4f\\n", gk_Internals_1_k4
; printks2 "Csound: gk_Internals_1_k5:           %9.4f\\n", gk_Internals_1_k5
; printks2 "Csound: gk_Internals_1_k6:           %9.4f\\n", gk_Internals_1_k6
; printks2 "Csound: gk_Internals_1_k7:           %9.4f\\n", gk_Internals_1_k7
; printks2 "Csound: gk_Internals_1_k8:           %9.4f\\n", gk_Internals_1_k8
k_gain = ampdb(gk_Internals_1_level)
a_modulator vco2 gk_Internals_1_mod_amp, gk_Internals_1_mod_hz, 12
a_vdelay vdelay3 a_signal, a_modulator, 4
a_vdelay = a_vdelay * ak_envelope * 10
a_output = k_gain * a_vdelay
a_left, a_right pan2 a_output, gk_Internals_1_pan
a_damping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
a_left = a_damping * a_left
a_right = a_damping * a_right
outleta "outleft", a_left
outleta "outright", a_right
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_Bower_level chnexport "gk_Bower_level", 3
gk_Bower_pan chnexport "gk_Bower_pan", 3
gi_Bower_minimum_hz chnexport "gi_Bower_minimum_hz", 3
gk_Bower_bow_pressure chnexport "gk_Bower_bow_pressure", 3
gk_Bower_bow_position chnexport "gk_Bower_bow_position", 3
gk_Bower_vibrato_hz chnexport "gk_Bower_vibrato_hz", 3
gk_Bower_vibrato_amplitude chnexport "gk_Bower_vibrato_amplitude", 3
gi_Bower_sine ftgen 0,0,65537,10,1
instr Bower
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
i_phase = p6
i_pan = p7
i_amplitude = ampdb(i_midi_velocity) * 500
i_attack =  p3 * (1 / 4) * (4 / 3)
i_sustain = p3 * (1 / 2) * (4 / 3)
i_release =   p3 * (1 / 4) * (4 / 3)
p3 = i_attack + i_sustain + i_release
k_envelope transeg 0.0, i_attack / 2.0, 1.5, i_amplitude / 2.0, i_attack / 2.0, -1.5, i_amplitude, i_sustain, 0.0, i_amplitude, i_release / 2.0, 1.5, i_amplitude / 2.0, i_release / 2.0, -1.5, 0
i_frequency = cpsmidinn(i_midi_key)
kamp = k_envelope
kfreq = i_frequency
kpres = gk_Bower_bow_pressure
; krat rspline 0.006,0.988,0.1,0.4
krat rspline 0.006,0.988,1,4
krat = gk_Bower_bow_position
kvibf = gk_Bower_vibrato_hz
kvibamp = gk_Bower_vibrato_amplitude
iminfreq = gi_Bower_minimum_hz
aSig wgbow kamp,kfreq,kpres,krat,kvibf,kvibamp,gi_Bower_sine,iminfreq
k_gain = ampdb(gk_Bower_level)
aSig = aSig * k_gain * .001
;aSig butlp aSig,2000
;aSig pareq aSig,80,6,0.707
a_left, a_right pan2 aSig, i_pan
a_damping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
a_left = a_damping * a_left
a_right = a_damping * a_right
outleta "outleft", a_left
outleta "outright", a_right
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_Phaser_attack init .125
gk_Phaser_release init .125
gk_Phaser_level chnexport "gk_Phaser_level", 3
gk_Phaser_pan chnexport "gk_Phaser_pan", 3
gk_Phaser_ratio1 chnexport "gk_Phaser_ratio1", 3
gk_Phaser_index1 chnexport "gk_Phaser_index1", 3
gk_Phaser_ratio2 chnexport "gk_Phaser_ratio2", 3
gk_Phaser_index2 chnexport "gk_Phaser_index2", 3
gS_Phaser_eq_mode chnexport "gS_Phaser_eq_mode", 3
gk_Phaser_eq_cutoff_hz chnexport "gk_Phaser_eq_cutoff_hz", 3
gk_Phaser_eq_level chnexport "gk_Phaser_eq_level", 3
gk_Phaser_eq_Q chnexport "gk_Phaser_eq_Q", 3
gk_Phaser_eq_slope chnexport "gk_Phaser_eq_slope", 3
gk_Phaser_midi_dynamic_range init 127
gi_Phaser_sine ftgen 0,0,65537,10,1
instr Phaser
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_Phaser_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = gk_Phaser_pan
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 10
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Phaser_level)
i_attack =  p3 * (1 / 4) * (4 / 3)
i_sustain = p3 * (1 / 2) * (4 / 3)
i_release = p3 * (1 / 4) * (4 / 3)
p3 = i_attack + i_sustain + i_release
a_envelope transegr 0.0, i_attack / 2.0, 1.5, i_amplitude / 2.0, i_attack / 2.0, -1.5, i_amplitude, i_sustain, 0.0, i_amplitude, i_release / 2.0, 1.5, i_amplitude / 2.0, i_release / 2.0, -1.5, 0
a1,a2 crosspm gk_Phaser_ratio1, gk_Phaser_ratio2, gk_Phaser_index1, gk_Phaser_index2, i_frequency, gi_Phaser_sine, gi_Phaser_sine
a_signal = (a1 + a2) * k_gain * a_envelope
; ar rbjeq asig, kfco, klvl, kQ, kS[, imode]
if strcmp(gS_Phaser_eq_mode, "Resonant lowpass") == 0 then
    i_mode = 0
endif
if strcmp(gS_Phaser_eq_mode, "Resonant highpass") == 0 then
    i_mode = 2
endif
if strcmp(gS_Phaser_eq_mode, "Bandpass") == 0 then
    i_mode = 4
endif
if strcmp(gS_Phaser_eq_mode, "Band-reject") == 0 then
    i_mode = 6
endif
if strcmp(gS_Phaser_eq_mode, "Peaking EQ") == 0 then
    i_mode = 8
endif
if strcmp(gS_Phaser_eq_mode, "Low shelf EQ") == 0 then
    i_mode = 10
endif
if strcmp(gS_Phaser_eq_mode, "High shelf EQ") == 0 then
    i_mode = 12
endif
a_signal rbjeq a_signal, gk_Phaser_eq_cutoff_hz, gk_Phaser_eq_level, gk_Phaser_eq_Q, gk_Phaser_eq_slope, i_mode
#ifdef USE_SPATIALIZATION
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
;printks "Phaser         i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\\n", 1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_left)), dbamp(rms(a_right))
endin

gk_Sweeper_level chnexport "gk_Sweeper_level",3
gk_Sweeper_pan chnexport "gk_Sweeper_pan",3
gk_Sweeper_britel chnexport "gk_Sweeper_britel",3
gk_Sweeper_briteh chnexport "gk_Sweeper_briteh",3
gk_Sweeper_britels chnexport "gk_Sweeper_britels",3
gk_Sweeper_britehs chnexport "gk_Sweeper_britehs",3
gi_Sweeper_sine ftgen 0, 0, 65537, 10, 1
gi_Sweeper_octfn ftgen 0, 0, 65537, -19, 1, 0.5, 270, 0.5
instr Sweeper
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
i_phase = p6
k_pan = gk_Sweeper_pan
i_amplitude = ampdb(i_midi_velocity) * .1
i_attack =  p3 * (1 / 4) * (4 / 3)
i_sustain = p3 * (1 / 2) * (4 / 3)
i_release = p3 * (1 / 4) * (4 / 3)
p3 = i_attack + i_sustain + i_release
k_envelope transeg 0.0, i_attack / 2.0, 1.5, i_amplitude / 2.0, i_attack / 2.0, -1.5, i_amplitude, i_sustain, 0.0, i_amplitude, i_release / 2.0, 1.5, i_amplitude / 2.0, i_release / 2.0, -1.5, 0
i_frequency = cpsmidinn(i_midi_key)
icps = i_frequency
kamp expseg 0.001,0.02,0.2,p3-0.01,0.001
ktonemoddep jspline 0.01,0.05,0.2
ktonemodrte jspline 6,0.1,0.2
ktone poscil3 ktonemoddep, ktonemodrte, gi_Sweeper_sine
kbrite rspline gk_Sweeper_britel, gk_Sweeper_briteh, gk_Sweeper_britels, gk_Sweeper_britehs
ibasfreq init icps
ioctcnt init 3
iphs init 0
;a1 hsboscil kamp, ktone, kbrite, ibasfreq, gi_Sweeper_sine, gi_Sweeper_octfn, ioctcnt, iphs
a1 hsboscil k_envelope, ktone, kbrite, ibasfreq, gi_Sweeper_sine, gi_Sweeper_octfn, ioctcnt, iphs
amod poscil3 0.25, ibasfreq*(1/3), gi_Sweeper_sine
arm = a1*amod
kmix expseg 0.001, 0.01, rnd(1), rnd(3)+0.3, 0.001
kmix=.25
a1 ntrpol a1, arm, kmix
;a1 pareq a1/10, 400, 15, .707
;a1 tone a1, 500
kpanrte jspline 5, 0.05, 0.1
kpandep jspline 0.9, 0.2, 0.4
kpan poscil3 kpandep, kpanrte, gi_Sweeper_sine
k_gain ampdb gk_Sweeper_level
a1 = a1 * k_gain
a1,a2 pan2 a1, kpan + gk_Sweeper_pan
a1 delay a1, rnd(0.1)
a2 delay a2, rnd(0.11)
kenv linsegr 1, 1, 0
kenv = k_envelope
a_left = a1*kenv*.02
a_right = a2*kenv*.02
a_damping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
a_left = a_damping * a_left
a_right = a_damping * a_right
outleta "outleft", a_left
outleta "outright", a_right
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_Buzzer_level chnexport "gk_Buzzer_level", 3
gk_Buzzer_pan chnexport "gk_Buzzer_pan", 3
gk_Buzzer_partials chnexport "gk_Buzzer_partials", 3
gi_Buzzer_sine ftgen 0, 0, 65537, 10, 1
instr Buzzer
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
i_phase = p6
i_pan = p7
i_amplitude = ampdb(i_midi_velocity) * 4
i_attack =  p3 * (1 / 4) * (4 / 3)
i_sustain = p3 * (1 / 2) * (4 / 3)
i_release = p3 * (1 / 4) * (4 / 3)
k_amplitude = ampdb(gk_Buzzer_level)
p3 = i_attack + i_sustain + i_release
k_envelope transeg 0.0, i_attack / 2.0, 1.5, i_amplitude / 2.0, i_attack / 2.0, -1.5, i_amplitude, i_sustain, 0.0, i_amplitude, i_release / 2.0, 1.5, i_amplitude / 2.0, i_release / 2.0, -1.5, 0
i_frequency = cpsmidinn(i_midi_key)
;a_signal gbuzz k_envelope, i_frequency, 3, gk_FirstHarmonic, gk_DistortFactor, gisine
a_signal buzz k_envelope, i_frequency, gk_Buzzer_partials, gi_Buzzer_sine
a_signal = a_signal * 3 * k_amplitude
;a_signal vco2 k_envelope, i_frequency, 12
;a_signal poscil3 k_envelope, i_frequency, giharmonics
;a_signal distort a_signal, gk_DistortFactor * .4, giwaveshaping
a_left, a_right pan2 a_signal, gk_Buzzer_pan
a_damping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
a_left = a_damping * a_left
a_right = a_damping * a_right
outleta "outleft", a_left
outleta "outright", a_right
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_Reverb_Feedback chnexport "gk_Reverb_Feedback", 3
gk_Reverb_DelayModulation chnexport "gk_Reverb_DelayModulation", 3

instr ReverbLeft
; printks2 "Csound: gk_Reverb_Feedback:         %9.4f\\n", gk_Reverb_Feedback
; printks2 "Csound: gk_Reverb_DelayModulation:  %9.4f\\n", gk_Reverb_DelayModulation
; p4 = gain of reverb. Adjust empirically
; for desired reverb time. .6 gives
; a good small "live" room sound, .8
; a small hall, .9 a large hall,
; .99 an enormous stone cavern.

; p5 = amount of random pitch modulation
; for the delay lines. 1 is the "normal"
; amount, but this may be too high for
; held pitches such as piano tones.
; Adjust to taste.

; p6 = cutoff frequency of lowpass filters
; in feedback loops of delay lines,
; in Hz. Lower cutoff frequencies results
; in a sound with more high-frequency
; damping.

; 8 delay line FDN reverb, with feedback matrix based upon
; physical modeling scattering junction of 8 lossless waveguides
; of equal characteristic impedance. Based on Julius O. Smith III,
; "A New Approach to Digital Reverberation using Closed Waveguide
; Networks," Proceedings of the International Computer Music
; Conference 1985, p. 47-53 (also available as a seperate
; publication from CCRMA), as well as some more recent papers by
; Smith and others.
; Coded by Sean Costello, October 1999
itone = 16000
a_signal inleta "input"
afilt1 init 0
afilt2 init 0
afilt3 init 0
afilt4 init 0
afilt5 init 0
afilt6 init 0
afilt7 init 0
afilt8 init 0
idel1 = (2473.000/sr)
idel2 = (2767.000/sr)
idel3 = (3217.000/sr)
idel4 = (3557.000/sr)
idel5 = (3907.000/sr)
idel6 = (4127.000/sr)
idel7 = (2143.000/sr)
idel8 = (1933.000/sr)
; k1-k8 are used to add random pitch modulation to the
; delay lines. Helps eliminate metallic overtones
; in the reverb sound.
// ares randi   xamp, xcps [, iseed] [, isize] [, ioffset]
// ares jspline xamp, kcpsMin, kcpsMax
k1 randi .001,  3.1,   .06
k2 randi .0011, 3.5,   .9
k3 randi .0017, 1.11,  .7
k4 randi .0006, 3.973, .3
k5 randi .001,  2.341, .63
k6 randi .0011, 1.897, .7
k7 randi .0017, 0.891, .9
k8 randi .0006, 3.221, .44
; apj is used to calculate "resultant junction pressure" for
; the scattering junction of 8 lossless waveguides
; of equal characteristic impedance. If you wish to
; add more delay lines, simply add them to the following
; equation, and replace the .25 by 2/N, where N is the
; number of delay lines.
apj = .25 * (afilt1 + afilt2 + afilt3 + afilt4 + afilt5 + afilt6 + afilt7 + afilt8)
adum1 delayr 1
adel1 deltap3 idel1 + k1 * gk_Reverb_DelayModulation
delayw a_signal + apj - afilt1
adum2 delayr 1
adel2 deltap3 idel2 + k2 * gk_Reverb_DelayModulation
delayw a_signal + apj - afilt2
adum3 delayr 1
adel3 deltap3 idel3 + k3 * gk_Reverb_DelayModulation
delayw a_signal + apj - afilt3
adum4 delayr 1
adel4 deltap3 idel4 + k4 * gk_Reverb_DelayModulation
delayw a_signal + apj - afilt4
adum5 delayr 1
adel5 deltap3 idel5 + k5 * gk_Reverb_DelayModulation
delayw a_signal + apj - afilt5
adum6 delayr 1
adel6 deltap3 idel6 + k6 * gk_Reverb_DelayModulation
delayw a_signal + apj - afilt6
adum7 delayr 1
adel7 deltap3 idel7 + k7 * gk_Reverb_DelayModulation
delayw a_signal + apj - afilt7
adum8 delayr 1
adel8 deltap3 idel8 + k8 * gk_Reverb_DelayModulation
delayw a_signal + apj - afilt8
; 1st order lowpass filters in feedback
; loops of delay lines.
afilt1 tone adel1 * gk_Reverb_Feedback, itone
afilt2 tone adel2 * gk_Reverb_Feedback, itone
afilt3 tone adel3 * gk_Reverb_Feedback, itone
afilt4 tone adel4 * gk_Reverb_Feedback, itone
afilt5 tone adel5 * gk_Reverb_Feedback, itone
afilt6 tone adel6 * gk_Reverb_Feedback, itone
afilt7 tone adel7 * gk_Reverb_Feedback, itone
afilt8 tone adel8 * gk_Reverb_Feedback, itone
; The outputs of the delay lines are summed
; and sent to the stereo outputs. This could
; easily be modified for a 4 or 8-channel
; sound system.
aout1 = (afilt1 + afilt3 + afilt5 + afilt7)
aout2 = (afilt2 + afilt4 + afilt6 + afilt8)
aout = aout1 + aout2
; To the master output.
outleta "output", aout
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

instr ReverbRight
; p4 = gain of reverb. Adjust empirically
; for desired reverb time. .6 gives
; a good small "live" room sound, .8
; a small hall, .9 a large hall,
; .99 an enormous stone cavern.

; p5 = amount of random pitch modulation
; for the delay lines. 1 is the "normal"
; amount, but this may be too high for
; held pitches such as piano tones.
; Adjust to taste.

; p6 = cutoff frequency of lowpass filters
; in feedback loops of delay lines,
; in Hz. Lower cutoff frequencies results
; in a sound with more high-frequency
; damping.

; 8 delay line FDN reverb, with feedback matrix based upon
; physical modeling scattering junction of 8 lossless waveguides
; of equal characteristic impedance. Based on Julius O. Smith III,
; "A New Approach to Digital Reverberation using Closed Waveguide
; Networks," Proceedings of the International Computer Music
; Conference 1985, p. 47-53 (also available as a seperate
; publication from CCRMA), as well as some more recent papers by
; Smith and others.
; Coded by Sean Costello, October 1999
itone = 16000
a_signal inleta "input"
afilt1 init 0
afilt2 init 0
afilt3 init 0
afilt4 init 0
afilt5 init 0
afilt6 init 0
afilt7 init 0
afilt8 init 0
idel1 = (2473.000/sr)
idel2 = (2767.000/sr)
idel3 = (3217.000/sr)
idel4 = (3557.000/sr)
idel5 = (3907.000/sr)
idel6 = (4127.000/sr)
idel7 = (2143.000/sr)
idel8 = (1933.000/sr)
; k1-k8 are used to add random pitch modulation to the
; delay lines. Helps eliminate metallic overtones
; in the reverb sound.
// ares randi   xamp, xcps             [, iseed] [, isize] [, ioffset]
// ares jspline xamp, kcpsMin, kcpsMax
k1 randi .001,  3.1,   .06
k2 randi .0011, 3.5,   .9
k3 randi .0017, 1.11,  .7
k4 randi .0006, 3.973, .3
k5 randi .001,  2.341, .63
k6 randi .0011, 1.897, .7
k7 randi .0017, 0.891, .9
k8 randi .0006, 3.221, .44
; apj is used to calculate "resultant junction pressure" for
; the scattering junction of 8 lossless waveguides
; of equal characteristic impedance. If you wish to
; add more delay lines, simply add them to the following
; equation, and replace the .25 by 2/N, where N is the
; number of delay lines.
apj = .25 * (afilt1 + afilt2 + afilt3 + afilt4 + afilt5 + afilt6 + afilt7 + afilt8)
adum1 delayr 1
adel1 deltap3 idel1 + k1 * gk_Reverb_DelayModulation
delayw a_signal + apj - afilt1
adum2 delayr 1
adel2 deltap3 idel2 + k2 * gk_Reverb_DelayModulation
delayw a_signal + apj - afilt2
adum3 delayr 1
adel3 deltap3 idel3 + k3 * gk_Reverb_DelayModulation
delayw a_signal + apj - afilt3
adum4 delayr 1
adel4 deltap3 idel4 + k4 * gk_Reverb_DelayModulation
delayw a_signal + apj - afilt4
adum5 delayr 1
adel5 deltap3 idel5 + k5 * gk_Reverb_DelayModulation
delayw a_signal + apj - afilt5
adum6 delayr 1
adel6 deltap3 idel6 + k6 * gk_Reverb_DelayModulation
delayw a_signal + apj - afilt6
adum7 delayr 1
adel7 deltap3 idel7 + k7 * gk_Reverb_DelayModulation
delayw a_signal + apj - afilt7
adum8 delayr 1
adel8 deltap3 idel8 + k8 * gk_Reverb_DelayModulation
delayw a_signal + apj - afilt8
; 1st order lowpass filters in feedback
; loops of delay lines.
afilt1 tone adel1 * gk_Reverb_Feedback, itone
afilt2 tone adel2 * gk_Reverb_Feedback, itone
afilt3 tone adel3 * gk_Reverb_Feedback, itone
afilt4 tone adel4 * gk_Reverb_Feedback, itone
afilt5 tone adel5 * gk_Reverb_Feedback, itone
afilt6 tone adel6 * gk_Reverb_Feedback, itone
afilt7 tone adel7 * gk_Reverb_Feedback, itone
afilt8 tone adel8 * gk_Reverb_Feedback, itone
; The outputs of the delay lines are summed
; and sent to the stereo outputs. This could
; easily be modified for a 4 or 8-channel
; sound system.
aout1 = (afilt1 + afilt3 + afilt5 + afilt7)
aout2 = (afilt2 + afilt4 + afilt6 + afilt8)
aout = aout1 + aout2
; To the master output.
outleta "output", aout
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gS_MVerb_preset chnexport "gS_MVerb_preset", 3 ; "Huge Hall"
gk_MVerb_FB chnexport "gk_MVerb_feedback", 3 ; .975
gk_MVerb_wet chnexport "gk_MVerb_wet", 3 ; .5
gk_MVerb_random chnexport "gk_MVerb_random", 3 ; 1
gk_MVerb_rslow chnexport "gk_MVerb_rslow", 3 ; 1.1
gk_MVerb_rfast chnexport "gk_MVerb_rfast", 3 ; 3.8
gk_MVerb_rmax chnexport "gk_MVerb_rmax", 3 ; .0005
gk_MVerb_print chnexport "gk_MVerb_print", 3 ; 1
gk_MVerb_DFact chnexport "gk_MVerb_DFact", 3 ; .75

gS_MVerb_preset init "Huge Hall"
gk_MVerb_feedback init .975
gk_MVerb_wet init 1
gk_MVerb_random init 1
gk_MVerb_rslow init .3
gk_MVerb_rfast init 2.1
gk_MVerb_rmax init .001
gk_MVerb_print init 1
gk_MVerb_DFact init .75

instr MVerb
//////////////////////////////////////////////
// Original csd by Jon Christopher Nelson.
// Adapted to C++ plugin by Michael Gogins.
// Compute-intensive!
//////////////////////////////////////////////
ainleft  inleta  "inleft"
ainright  inleta  "inright"
aoutleft, aoutright MVerb ainleft, ainright, gS_MVerb_preset, "wet", gk_MVerb_wet, "FB", gk_MVerb_feedback, "random", 1, "rslow", gk_MVerb_rslow, "rfast", gk_MVerb_rfast, "rmax", gk_MVerb_rmax, "print", gk_MVerb_print, "DFact", gk_MVerb_DFact
outleta  "outleft", aoutleft
outleta  "outright", aoutright
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_MasterLevel init 0
gk_MasterLevel chnexport "gk_MasterLevel", 3
instr MasterOutput
; printks2 "Csound: gk_MasterLevel:  %9.4f\\n", gk_MasterLevel
a_left inleta "inleft"
a_right inleta "inright"
k_gain = ampdb(gk_MasterLevel)
a_left *= k_gain
a_right *= k_gain
outs a_left, a_right
;fout "Drone-IV-performance.wav", 16, a_left, a_right
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin
'''

import CsoundAC
import math

model = CsoundAC.MusicModel()
lindenmayer = CsoundAC.ChordLindenmayer()
print("lindenmayer: {}".format(lindenmayer))

score_model = CsoundAC.ScoreModel()
lindenmayer = CsoundAC.ChordLindenmayer();
lindenmayer.setAxiom("(seed P 3928394)(= P 60)(= Sc Cmajor {0,2,4,5,7,9,11})(++ C)(= C {0,4,7,11})(= M {0,4,7,11})(= N[d] 3.)(Sc P)(A)")
#lindenmayer.addRule("(A)", "(A)(W N)(+ N[k] 20 R)(C Sd 5 R)(W C R)([)(+ N[k] 7)(F N 1)(A)(])(uni V 1 24)(uni N[x] 0 1)(+ N[t] 1)(W C R)(C Sd 3)(uni N[i] 1 5)(M Sc 3 0)(Sc P)(W N)(A)(- N[k] 2)(+ N[t] 1)(W N)(A)")
#lindenmayer.addRule("(A)", "(A)(W C R)(+ N[k] 2 R)(C Sd 3 R)(W Cl R)([)(+ N[k] 7 R)(F N .1)(A)(])(M Sc 4 0)(uni V 1 24)(uni N[x] 0 1)(+ N[t] 1)(T C 5 O)(W C R)(C Sd 3)(uni N[i] 1 5)(M Sc 3 0)(Sc P)(W N R)(A)(- N[k] 2 R)(+ N[t] 1)(W N R)(A)")
lindenmayer.addRule("(A)", "(A)(W C R)(+ N[k] 2 R)(+ N[v] 2)(+ Sd 2 R)(W Cl R)([)(* S[t] .9)(+ N[k] 7 R)(F N .1)(A)(])(M Sc 4 0)(uni V 1 24)(uni N[x] 0 1)(+ N[t] 1)(- N[v] 2)(T C 5 O)(W C R)(+ Sd 3)(uni N[i] 1 5)(M Sc 3 0)(Sc P)(W N R)(A)(- N[k] 2 R)(+ N[t] 1)(W N R)(A)")
lindenmayer.setIterationCount(3)
#print("lindenmayer: " + lindenmayer)
rescale = CsoundAC.Rescale()
rescale.setRescale(CsoundAC.Event.TIME, True, False, .02, 0.)
rescale.setRescale(CsoundAC.Event.INSTRUMENT, True, True, 1., 5.999)
rescale.setRescale(CsoundAC.Event.KEY, True, False, 24., 72.)
rescale.setRescale(CsoundAC.Event.VELOCITY, True, True, 50., 18.)
CsoundAC.System.setMessageLevel(15)
rescale.addChild(lindenmayer)
score_model.addChild(rescale)
score_model.generate()
score = score_model.getScore()
score.setDuration(20 * 60)
sco = score.getCsoundScore(12, False)
print("sco:")
print(sco)

##############################################################################
# TEMPLATE CODE BEGINS
# Assumptions: 
# 1. The playpen.ini file in the home directory exists and is correct.
# 2. A Csound orchestra has been defined in the global orc string.
# 3. A Csound score has been defined in the global sco string.
# 4. All suitable widgets in main_window have exactly the IDs and 
#    names of Csound control channels, which are created using chnexport in 
#    the Csound orchestra. All such nanes and ids begin with 'gk', 'gi', 
#    or 'gS'.
##############################################################################

import datetime
import inspect
import io
import logging
import os
import sys
import traceback
import warnings

warnings.filterwarnings("ignore")
logging.getLogger().setLevel(logging.DEBUG)

def log_print(message):
    # Get the previous frame in the stack, otherwise it would
    # be this function!!!
    caller = inspect.currentframe().f_back.f_code
    #~ # Dump the message + the name of this function to the log.
    logging.debug("%s in %s:%i %s" % (
        caller.co_name, 
        caller.co_filename, 
        caller.co_firstlineno,
        message, 
    ))
    
def log_exception(message):
    # Get the previous frame in the stack, otherwise it would
    # be this function!!!
    caller = inspect.currentframe().f_back.f_code
    # Dump the message + the name of this function to the log.
    logging.exception("%s in %s:%i %s" % (
        caller.co_name, 
        caller.co_filename, 
        caller.co_firstlineno,
        message, 
    ))

import ctcsound
csound = ctcsound.Csound()
csound_is_performing = False
log_print("Global Csound instance: {} CSOUND *: 0x{:x}.".format(csound, int(csound.csound())))
    
import gi
gi.require_version('Gdk', '3.0')
from gi.repository import Gdk
from gi.repository import GObject
from gi.repository import GLib

# Read user settings.
settings = GLib.KeyFile.new()
home_directory = os.environ["HOME"]
playpen_ini_filepath = os.path.join(home_directory, "playpen.ini")
GLib.KeyFile.load_from_file(settings, playpen_ini_filepath, GLib.KeyFileFlags.NONE)
metadata_author = settings.get_value("metadata", "author")
metadata_publisher = settings.get_value("metadata", "publisher")
metadata_year = settings.get_value("metadata", "year")
metadata_notes = settings.get_value("metadata", "notes")
metadata_license=settings.get_value("metadata", "license")
csound_audio_output = settings.get_value("csound", "audio-output")
print("csound_audio_output: " + csound_audio_output)
soundfile_editor=settings.get_value("playpen", "soundfile-editor")
gnome_theme=settings.get_value("playpen", "gnome-theme")
editor_scheme = settings.get_value("playpen", "editor-scheme")

gi.require_version("Gtk", "3.0")
from gi.repository import Gtk 

# Override some global Gnome settings with playpen.ini values.
gnome_settings = Gtk.Settings.get_default()
gnome_settings.set_property("gtk-theme-name", gnome_theme)

piece_filepath = sys.argv[0]
piece_basename = os.path.splitext(piece_filepath)[0]
ui_filepath = piece_basename + ".ui"
ui_channels_filepath = ui_filepath + ".channels"
output_soundfile_filepath = piece_filepath + ".wav"
piece_archived_csd_filepath = piece_filepath + ".csd"
log_print("piece_filepath:              {}".format(piece_filepath))
log_print("ui_filepath:                 {}".format(ui_filepath))
log_print("ui_channels_filepath:        {}".format(ui_channels_filepath))
log_print("output_soundfile_filepath:   {}".format(output_soundfile_filepath))
log_print("piece_archived_csd_filepath: {}".format(piece_archived_csd_filepath))
widgets_for_channels = dict()
values_for_channels = dict()

def create_csd_text(options, license, orc, sco):
    string_file = io.StringIO()
    string_file.write("<CsoundSynthesizer>\n")
    string_file.write("<CsOptions>\n")
    string_file.write(options)
    string_file.write("\n")
    string_file.write("</CsOptions>\n")
    string_file.write("<CsLicense>\n")
    string_file.write(license)
    string_file.write("</CsLicense>\n")
    string_file.write("<CsInstruments>\n")
    string_file.write(orc)
    string_file.write("\n")
    string_file.write("; Preset channel values:\n\n")
    global values_for_channels
    for name, value in values_for_channels.items():
        if isinstance(value, str):
            line = '{} init "{}"\n'.format(name, value)
        else:
            line = "{} init {}\n".format(name, value)
        string_file.write(line)
    string_file.write("</CsInstruments>\n")
    string_file.write("<CsScore>\n")
    string_file.write(sco)
    string_file.write("\n")
    string_file.write("</CsScore>\n")
    string_file.write("</CsoundSynthesizer>\n")
    return string_file.getvalue()

def on_destroy(source):
    try:
        csound.stop()
        csound.cleanup()
        csound.reset()
        Gtk.main_quit()
    except:
        log_exception("Shutting down.")
    
def save_ui(button = None):
    try:
        global ui_channels_filepath
        global widgets_for_channels
        global values_for_channels
        log_print("ui_channels_filepath: {}".format(ui_channels_filepath))
        log_print("widgets_for_channels size: {}".format(len(widgets_for_channels)))
        with open(ui_channels_filepath, "w") as file:
            for channel, widget in widgets_for_channels.items():
                channel_value = get_control_value(widget)
                values_for_channels[channel] = channel_value
                log_print("channel: {} value: {}".format(widget.get_name(), channel_value))
                if isinstance(channel_value, str):
                    file.write('{} init "{}"\n'.format(widget.get_name(), channel_value))
                else:
                    file.write("{} init {}\n".format(widget.get_name(), channel_value))
    except:
        log_exception("Failed to save UI.")
    
def on_play_button_clicked(button):
    try:
        global csound_is_performing
        global piece_archived_csd_filepath
        csound_is_performing = False
        csd_text = create_csd_text("-+msg_color=0 -d -m195 -f -RWo" + csound_audio_output, "", orc, sco)
        with open(piece_archived_csd_filepath, "w", encoding="utf-8") as csd_file:
            csd_file.write(csd_text)
            log_print("Archived csd: {}.".format(piece_archived_csd_filepath))
        csound.stop()
        csound.cleanup()
        csound.reset()
        csound.compileCsdText(csd_text)
        csound.start()
        load_ui()
        log_print("Restoring {} channels...".format(len(values_for_channels)))
        for name, value in values_for_channels.items():
            log_print("initialize channel: {} value {} {}".format(name, value, type(value)))
            if isinstance(value, str):
                csound.setStringChannel(name, value)
            else:
                csound.setControlChannel(name, value)
        csound_is_performing = True
        start_time = float(start_time_entry.get_text())
        log_print("start_time: {}".format(start_time))
        kperiod_count = 0
        while csound.performKsmps() == 0:
            kperiod_count = kperiod_count + 1
            # Keep the UI responsive during performance.
            Gtk.main_iteration_do(False)
            if start_time > 0:
                csound.setScoreOffsetSeconds(start_time)
                start_time = 0
            if kperiod_count % 100:
                score_seconds = csound.scoreTime()
                score_time_label.set_text("{0:9.6f} / {1}".format(score_seconds, datetime.timedelta(score_seconds/86400.)))
    except:
        print(traceback.format_exc())
        
def post_process():
    try:
        global piece_filepath
        global output_soundfile_filepath
        cwd = os.getcwd()
        print('cwd:                    ' + cwd)
        author = metadata_author #'Michael Gogins'
        year = metadata_year #'2021'
        license = metadata_license #'ASCAP'
        publisher = metadata_publisher #'Irreducible Productions, ASCAP'
        notes = metadata_notes #'Electroacoustic Music'

        directory, basename = os.path.split(piece_filepath)
        rootname = os.path.splitext(basename)[0].split('.')[0]
        soundfile_name = output_soundfile_filepath
        title = rootname.replace("-", " ").replace("_", " ")
        label = '{} -- {}'.format(author, title).replace(" ", "_")
        master_filename = '{}.normalized.wav'.format(label)
        spectrogram_filename = '%s.png' % label
        cd_quality_filename = '%s.cd.wav' % label
        mp3_filename = '%s.mp3' % label
        mp4_filename = '%s.mp4' % label
        flac_filename = '%s.flac' % label
        print('Basename:               ' + basename)
        print('Original soundfile:     ' + soundfile_name)
        print('Author:                 ' + author)
        print('Title:                  ' + title)
        print('Year:                   ' + year)
        str_copyright          = 'Copyright %s by %s' % (year, author)
        print('Copyright:              ' + str_copyright)
        print('Licence:                ' + license)
        print('Publisher:              ' + publisher)
        print('Notes:                  ' + notes)
        print('Master filename:        ' + master_filename)
        print('Spectrogram filename:   ' + spectrogram_filename)
        print('CD quality filename:    ' + cd_quality_filename)
        print('MP3 filename:           ' + mp3_filename)
        print('MP4 filename:           ' + mp4_filename)
        print('FLAC filename:          ' + flac_filename)
        bext_description       = notes
        bext_originator        = author
        bext_orig_ref          = basename
        #bext_umid              = xxx
        #bext_orig_date         = xxx
        #bext_orig_time         = xxx
        #bext_coding_hist       = xxx
        #bext_time_ref          = xxx
        str_comment            = notes
        str_title              = title
        str_artist             = author
        str_date               = year
        str_license            = license
        sox_normalize_command = '''sox -S "%s" "%s" gain -n -3''' % (soundfile_name, master_filename + 'untagged.wav')
        print('sox_normalize command:  ' + sox_normalize_command)
        os.system(sox_normalize_command)
        tag_wav_command = '''sndfile-metadata-set "%s" --bext-description "%s" --bext-originator "%s" --bext-orig-ref "%s" --str-comment "%s" --str-title "%s" --str-copyright "%s" --str-artist  "%s" --str-date "%s" --str-license "%s" "%s"''' % (master_filename + 'untagged.wav', bext_description, bext_originator, bext_orig_ref, str_comment, str_title, str_copyright, str_artist, str_date, str_license, master_filename)
        print('tag_wav_command:        ' + tag_wav_command)
        os.system(tag_wav_command)
        sox_spectrogram_command = '''sox -S "%s" -n spectrogram -o "%s" -t"%s" -c"%s"''' % (master_filename, spectrogram_filename, label, str_copyright + ' (%s' % publisher)
        print('sox_spectrogram_command:' + sox_spectrogram_command)
        os.system(sox_spectrogram_command)
        sox_cd_command = '''sox -S "%s" -b 16 -r 44100 "%s"''' % (master_filename, cd_quality_filename + 'untagged.wav')
        print('sox_cd_command:         ' + sox_cd_command)
        os.system(sox_cd_command)
        tag_wav_command = '''sndfile-metadata-set "%s" --bext-description "%s" --bext-originator "%s" --bext-orig-ref "%s" --str-comment "%s" --str-title "%s" --str-copyright "%s" --str-artist  "%s" --str-date "%s" --str-license "%s" "%s"''' % (cd_quality_filename + 'untagged.wav', bext_description, bext_originator, bext_orig_ref, str_comment, str_title, str_copyright, str_artist, str_date, str_license, cd_quality_filename)
        print('tag_wav_command:        ' + tag_wav_command)
        os.system(tag_wav_command)
        mp3_command = '''lame --add-id3v2 --tt "%s" --ta "%s" --ty "%s" --tn "%s" --tg "%s"  "%s" "%s"''' % (title, "Michael Gogins", year, notes, "Electroacoustic", master_filename, mp3_filename)
        print('mp3_command:            ' + mp3_command)
        os.system(mp3_command)
        sox_flac_command = '''sox -S "%s" "%s"''' % (master_filename, flac_filename)
        print('sox_flac_command:       ' + sox_flac_command)
        os.system(sox_flac_command)
        mp4_command = '''%s -r 1 -i "%s" -i "%s" -codec:a aac -strict -2 -b:a 384k -c:v libx264 -b:v 500k "%s"''' % ('ffmpeg', os.path.join(cwd, spectrogram_filename), os.path.join(cwd, master_filename), os.path.join(cwd, mp4_filename))
        mp4_metadata =  '-metadata title="%s" ' % title
        mp4_metadata += '-metadata date="%s" ' % year
        mp4_metadata += '-metadata genre="%s" ' % notes
        mp4_metadata += '-metadata copyright="%s" ' % str_copyright
        mp4_metadata += '-metadata composer="%s" ' % author
        mp4_metadata += '-metadata artist="%s" ' % author
        mp4_metadata += '-metadata publisher="%s" ' % publisher
        mp4_command = '''"%s" -y -loop 1 -framerate 2 -i "%s" -i "%s" -c:v libx264 -preset medium -tune stillimage -crf 18 -codec:a aac -strict -2 -b:a 384k -r:a 48000 -shortest -pix_fmt yuv420p -vf "scale=trunc(iw/2)*2:trunc(ih/2)*2" %s "%s"''' % ('ffmpeg', os.path.join(cwd, spectrogram_filename), os.path.join(cwd, master_filename), mp4_metadata, os.path.join(cwd, mp4_filename))
        mp4_command = mp4_command.replace('\\', '/')
        print('mp4_command:            ' + mp4_command)
        os.system(mp4_command)
        os.system('del *wavuntagged.wav')
        os.system('{} {}'.format(soundfile_editor, master_filename))
        print("")
    except:
        print(traceback.format_exc())
        
def on_render_button_clicked(button):
    try:
        global piece_filepath
        global values_for_channels
        global csound_is_performing
        csound_is_performing = False
        csd_text = create_csd_text("-+msg_color=0 -d -m195 -f -RWo" + output_soundfile_filepath, "", orc, sco)
        csound.stop()
        csound.cleanup()
        csound.reset()
        csound.compileCsdText(csd_text)
        csound.start()
        load_ui()
        log_print("Restoring {} channels...".format(len(values_for_channels)))
        for name, value in values_for_channels.items():
            log_print("initialize channel: {} value {} {}".format(name, value, type(value)))
            if isinstance(value, str):
                csound.setStringChannel(name, value)
            else:
                csound.setControlChannel(name, value)
        csound_is_performing = True
        start_time = float(start_time_entry.get_text())
        log_print("start_time: {}".format(start_time))
        kperiod_count = 0
        while csound.performKsmps() == 0:
            kperiod_count = kperiod_count + 1
            # Keep the UI responsive during performance.
            Gtk.main_iteration_do(False)
            if start_time > 0:
                csound.setScoreOffsetSeconds(start_time)
                start_time = 0
            if kperiod_count % 100:
                score_seconds = csound.scoreTime()
                score_time_label.set_text("{0:9.6f} / {1}".format(score_seconds, datetime.timedelta(score_seconds/86400.)))
        csound.stop()
        csound.cleanup()
        csound.reset()
        post_process()
    except:
        print(traceback.format_exc())
        
def on_stop_button_clicked(button):
    try:
        global csound_is_performing
        csound_is_performing = False
        csound.stop()
        csound.cleanup()
        csound.reset()
        print("Csound has been stopped and reset.")
    except:
        print(traceback.format_exc())
        
def get_control_value(control):
    channel_value = 0
    if isinstance(control, Gtk.Switch):
        channel_value = control.get_state()
    elif isinstance(control, Gtk.ComboBox):
        channel_value = control.get_active_id()
    elif isinstance(control, Gtk.ToggleButton):
        channel_value = control.get_active()
    elif isinstance(control, Gtk.Scale):
        channel_value = control.get_value()
    #~ elif isinstance(control, Gtk.SpinButton):
        #~ channel_value = control.get_value()
    elif isinstance(control, Gtk.Editable):
        channel_value = control.get_text()
    #log_print("control: {} value: {}".format(control.get_name(), channel_value))
    return channel_value
    
def set_control_value(control, value):
    value = value.strip().replace('"', '')
    #log_print("control: {}{} value: {}".format(control.get_name(), type(control), value))
    if isinstance(control, Gtk.Switch):
        control.set_state(float(value))
    elif isinstance(control, Gtk.ComboBox):
        result = control.set_active_id(value)
    elif isinstance(control, Gtk.ToggleButton):
        control.set_active(float(value))
    elif isinstance(control, Gtk.Scale):
        control.set_value(float(value))
    #~ elif isinstance(control, Gtk.SpinButton):
        #~ channel_value = control.get_value()
    elif isinstance(control, Gtk.Editable):
        control.set_text(value)
         
# Please note, the order of conditions matters; some subclasses do 
# not handle superclass signals.

def on_control_change(control, data=-1 ,user_data=None):
    try:
        global values_for_channels
        global csound_is_performing
        global csound
        channel_name = control.get_name()
        channel_value = get_control_value(control)
        #log_print("channel: {} value: {}".format(channel_name, channel_value))
        # Prevent premature definition of control channels.
        if csound_is_performing == False:
            pass
        else:
            if isinstance(control, Gtk.ToggleButton):
                #log_print("ToggleButton:  setControlChannel({}, {}, ({}))".format(channel_name, channel_value, type(channel_value)))
                csound.setControlChannel(channel_name, channel_value)
            elif isinstance(control, Gtk.ComboBox):
                channel_value = control.get_active_id()
                #log_print("Combo box:     SetStringChannel({}, {}, ({}))".format(channel_name, channel_value, type(channel_value)))
                csound.setStringChannel(channel_name, channel_value)
            elif isinstance(control, Gtk.Button):
                channel_value = float(data)
                #log_print("Button:        setControlChannel({}, {}, ({}))".format(channel_name, channel_value, type(channel_value)))
                csound.setControlChannel(channel_name, channel_value)
            elif isinstance(control, Gtk.MenuItem):
                channel_value = data
                #log_print("MenuItem:      setControlChannel({}, {}, ({}))".format(channel_name, channel_value, type(channel_value)))
                csound.setControlChannel(channel_name, channel_value)
            elif isinstance(control, Gtk.Scale):
                #log_print("Scale:         setControlChannel({}, {}, ({}))".format(channel_name, channel_value, type(channel_value)))
                csound.setControlChannel(channel_name, channel_value)
            #~ elif isinstance(control, Gtk.SpinButton):
                #~ channel_value = control.get_value()
                #~ csound.SetControlChannel(channel_name, channel_value)
            elif isinstance(control, Gtk.Editable):
                #channel_value = control.get_text()
                log_print("Editable:      SetStringChannel({}, {}, ({}))".format(channel_name, channel_value, type(channel_value)))
                csound.setStringChannel(channel_name, channel_value)
        values_for_channels[channel_name] = channel_value
    except:
        print(traceback.format_exc())
        
'''
For only those widgets and those signals that are used here to control Csound 
performances using the Csound control channels, connect the on_control_changed 
signal to its callback. Also, associate the actual widget with its name and 
its current value.
'''
def connect_controls(container):
    global widgets_for_channels
    global values_for_channels
    # A rare GTK stupidity, all widgets should have get_children, none should 
    # have get_child.
    children = []
    try:
        children = container.get_children()
    except:
        try:
            child = container.get_child()
            children.append(child)
        except:
            pass
    for child in children:
        log_print("child: {} {}".format(child, child.get_name()))
        channel_name = child.get_name()
        # Valid channels start with gk, gi, or gS.
        if channel_name[:2] not in ["gk", "gi", "gS"]:
            pass # log_print("  {} is not a Csound control channel, skipping...".format(channel_name))
        else:
            channel_value = get_control_value(child)
            if isinstance(child, Gtk.ComboBox):
                child.connect("changed", on_control_change, 1.)
                widgets_for_channels[channel_name] = child
                values_for_channels[channel_name] = channel_value
                log_print("Connected GTK widget '{}' to Csound control channel '{}'".format(type(child).__name__, channel_name))
            if isinstance(child, Gtk.Button):
                child.connect("pressed", on_control_change, 1.)
                child.connect("released", on_control_change, 0.)            
                widgets_for_channels[channel_name] = child
                values_for_channels[channel_name] = channel_value
                log_print("Connected GTK widget '{}' to Csound control channel '{}'".format(type(child).__name__, channel_name))
            if isinstance(child, Gtk.MenuItem):
                child.connect("select", on_control_change, 1.)  
                child.connect("deselect", on_control_change, 0.)
                widgets_for_channels[channel_name] = child
                values_for_channels[channel_name] = channel_value
                log_print("Connected GTK widget '{}' to Csound control channel '{}'".format(type(child).__name__, channel_name))
            if isinstance(child, Gtk.Scale):
                handler_id = child.connect("value-changed", on_control_change)
                widgets_for_channels[channel_name] = child
                values_for_channels[channel_name] = channel_value
                log_print("Connected GTK widget '{}' to Csound control channel '{}'".format(type(child).__name__, channel_name))
            if isinstance(child, Gtk.ScaleButton):
                child.connect("value-changed", on_control_change, -1.)
                widgets_for_channels[channel_name] = child
                values_for_channels[channel_name] = channel_value
                log_print("Connected GTK widget '{}' to Csound control channel '{}'".format(type(child).__name__, channel_name))
            if isinstance(child, Gtk.Switch):
                child.connect("state-set", on_control_change, -1.)
                widgets_for_channels[channel_name] = child
                values_for_channels[channel_name] = channel_value
                log_print("Connected GTK widget '{}' to Csound control channel '{}'".format(type(child).__name__, channel_name))
            if isinstance(child, Gtk.Editable):
                child.connect("activate", on_control_change, -1.)
                widgets_for_channels[channel_name] = child
                values_for_channels[channel_name] = channel_value
                log_print("Connected GTK widget '{}' to Csound control channel '{}'".format(type(child).__name__, channel_name))
            if isinstance(child, Gtk.SpinButton):
                child.connect("value-changed", on_control_change, -1)
                widgets_for_channels[channel_name] = child
                values_for_channels[channel_name] = channel_value
                log_print("Connected GTK widget '{}' to Csound control channel '{}'".format(type(child).__name__, channel_name))
        connect_controls(child)                

def load_ui(source=None):
    try:
        global ui_filepath
        global ui_channels_filepath
        global widgets_for_channels
        global values_for_channels
        log_print("Loading UI: {}".format(ui_filepath))
        if os.path.exists(ui_filepath) == True:
            with open(ui_filepath, "r") as file:
                ui_text = file.read()
            result = builder.add_from_string(ui_text)
            main_window = builder.get_object("main_window")
            log_print("main_window: {}".format(main_window))
            log_print("widgets_for_channels size: {}".format(len(widgets_for_channels)))
            if os.path.exists(ui_channels_filepath) == True:
                with open(ui_channels_filepath, "r", encoding="utf-8") as file:
                    for line in file:
                        channel, equals, value = line.split(maxsplit=2)
                        if channel in widgets_for_channels:
                            widget = widgets_for_channels[channel]
                            if widget:
                                set_control_value(widget, value)
            else:
                log_print("UI file not found, not defining controls.")
    except:
        log_print("Error: failed to load user-defined controls layout.")
        print(traceback.format_exc())
  
builder = Gtk.Builder()
builder.add_from_file(ui_filepath)
main_window = builder.get_object("main_window")
main_window.connect("destroy", on_destroy)
save_button = builder.get_object("save_button")
save_button.connect("clicked", save_ui)
restore_button = builder.get_object("restore_button")
restore_button.connect("clicked", load_ui)
play_button = builder.get_object("play_button")
play_button.connect("clicked", on_play_button_clicked)
render_button = builder.get_object("render_button")
render_button.connect("clicked", on_render_button_clicked)
stop_button = builder.get_object("stop_button")
stop_button.connect("clicked", on_stop_button_clicked)
start_time_entry = builder.get_object("start_time_entry")
score_time_label = builder.get_object("score_time")
level_slider = builder.get_object("gk_MasterOutput_level")
connect_controls(main_window)
main_window.show_all() 
load_ui()
Gtk.main()
