<CsoundSynthesizer>
<CsOptions>
--midi-key=4 --midi-velocity=5 -m168 -j1 -odac
</CsOptions>
<CsInstruments>

    
sr = 48000
ksmps = 128
nchnls = 2
0dbfs = 12

; Ensure the same random stream for each rendering.
; rand, randh, randi, rnd(x) and birnd(x) are not affected by seed.

seed 574382

;gi_Protoverb vstinit "/home/mkg/.u-he/Protoverb/Protoverb.64.avx.so", 1
gi_Mverb2020 vstinit "/home/mkg/.local/lib/Mverb2020.so", 1
;gi_ReverbDragonfly vstinit "/home/mkg/.local/lib/DragonflyHallReverb-vst.so", 1
gi_Pianoteq vstinit "/home/mkg/Pianoteq\ 7/x86-64bit/Pianoteq\ 7.so", 1

gi_Organteq vstinit "/home/mkg/Organteq\ 1/x86-64bit/Organteq\ 1.lv2/Organteq_1.so", 0

alwayson "OrganOutOrganteq"
alwayson "PianoOutPianoteq"
alwayson "Mverb2020"
alwayson "ReverbSC"
; alwayson "ReverbDragonfly"
; alwayson "MVerb"
; alwayson "ReverbBabo"
; alwayson "NReverb"
alwayson "MasterOutput"

connect "Droner", "outleft", "Mverb2020", "inleft"
connect "Droner", "outright", "Mverb2020", "inright"
connect "Droner1", "outleft", "Mverb2020", "inleft"
connect "Droner1", "outright", "Mverb2020", "inright"
connect "Droner2", "outleft", "Mverb2020", "inleft"
connect "Droner2", "outright", "Mverb2020", "inright"
connect "Droner3", "outleft", "Mverb2020", "inleft"
connect "Droner3", "outright", "Mverb2020", "inright"
connect "Droner4", "outleft", "Mverb2020", "inleft"
connect "Droner4", "outright", "Mverb2020", "inright"
connect "PianoOutPianoteq", "outleft", "Mverb2020", "inleft"
connect "PianoOutPianoteq", "outright", "Mverb2020", "inright"
connect "OrganOutOrganteq", "outleft", "Mverb2020", "inleft"
connect "OrganOutOrganteq", "outright", "Mverb2020", "inright"
connect "Mverb2020", "outleft", "ReverbSC", "inleft"
connect "Mverb2020", "outright", "ReverbSC", "inright"
connect "ReverbSC", "outleft", "MasterOutput", "inleft"
connect "ReverbSC", "outright", "MasterOutput", "inright"

/**
 * Stereo delay line with modulated feedback. The modulation 
 * is controlled by the logistic equation.
 */
opcode feedback_delay_modulation, aa, aakkkkk
a_inleft, a_inright, k_delay_seconds, k_delay_feedback, k_modulation_depth, k_modulation_hz, k_modulation_coefficient xin
k_y init .5
k_y1 init .5
// Frequency of the logistic "oscillator."
k_trigger metro k_modulation_hz
if k_trigger == 1 then
k_y1 = k_y
// Logistic equation.
k_y = k_y1 * k_modulation_coefficient * 4. * (1 - k_y1)
endif
// Sawtooth wave for delay time.
// ares vco2 kamp, kcps [, imode] [, kpw] [, kphs] [, inyx]
a_modulator vco2 k_modulation_depth, k_modulation_hz * k_y
a_inleft = a_inleft * k_delay_feedback
a_inright = a_inright * k_delay_feedback
a_modulated_delay_seconds = k_delay_seconds * a_modulator
// aout1, aout2 vdelayxws ain1, ain2, adl, imd, iws [, ist]
a_outleft, a_outright vdelayxws a_inleft, a_inright, a_modulated_delay_seconds, 100, 100
a_outleft1 = a_outleft
a_outright1 = a_outright
xout a_outleft, a_outright
endop

gk_Droner_partial1 init .5
gk_Droner_partial2 init .6
gk_Droner_partial3 init .2
gk_Droner_partial4 init .4
gk_Droner_partial5 init .1
gk_Droner_partial6 init .05
gk_Droner_partial7 init .1
gk_Droner_partial8 init 0
gk_Droner_partial9 init 0
gk_Droner_partial10 init 0
gk_Droner_level init 9
gi_Droner_waveform init 0
instr Droner, Droner1, Droner2, Droner3, Droner4
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
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_overall_amps = -20 + 98 + 4
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Droner_level)
k1 = gk_Droner_partial1
k2 = gk_Droner_partial2
k3 = gk_Droner_partial3
k4 = gk_Droner_partial4
k5 = gk_Droner_partial5
k6 = gk_Droner_partial6
k7 = gk_Droner_partial7
k8 = gk_Droner_partial8
k9 = gk_Droner_partial9
k10 = gk_Droner_partial10
iwaveform = gi_Droner_waveform
iattack = .5
idecay = .5
xtratim iattack + idecay
isustain = p3
aenvelope transegr 0.0, iattack / 2.0, 1.5, 1 / 2.0, iattack / 2.0, -1.5, 1, isustain, 0.0, 1, idecay / 2.0, 1.5, 1 / 2.0, idecay / 2.0, -1.5, 0
ihertz = cpsmidinn(i_midi_key)
isine ftgenonce 0, 0, 65537, 10, 1, 0, .02
if iwaveform == 0 goto i_waveform_0
if iwaveform == 1 goto i_waveform_1
if iwaveform == 2 goto i_waveform_2
i_waveform_0:
asignal poscil3 1, ihertz, isine
goto i_waveform_endif
i_waveform_1:
asignal vco2 1, ihertz, 8 ; integrated saw
goto i_waveform_endif
i_waveform_2:
asignal vco2 1, ihertz, 12 ; triangle
i_waveform_endif:
asignal chebyshevpoly asignal, 0, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10
adeclicking linsegr 0, .004, 1, p3 - .014, 1, .1, 0
a_signal = asignal * adeclicking
a_signal = a_signal * i_amplitude * k_gain * 1.4
#ifdef USE_SPATIALIZATION
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right

// a_inleft, a_inright, k_delay_seconds, k_delay_feedback, k_modulation_depth, k_modulation_hz, k_modulation_coefficient
a_out_left_1, a_out_right_1 feedback_delay_modulation a_out_left, a_out_right, .5, .5, .1, .5, .979

outleta "outleft", a_out_left_1
outleta "outright", a_out_right_1
#endif
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_OrganNoteOrganteq_midi_dynamic_range init 127
; These are the pedalboard and three manuals.
instr Pedale, Positif, Grand_Orgue, Recit
if p3 == -1 then
  p3 = 1000000
endif
i_instrument = p1
i_midi_channel = i_instrument - 1
i_time = p2
i_duration = p3 
p3 = i_duration
i_midi_key = p4
i_midi_dynamic_range = i(gk_OrganNoteOrganteq_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_instrument = p1
i_homogeneity = p11
instances active p1
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
vstnote gi_Organteq, i_midi_channel, i_midi_key, i_midi_velocity, i_duration
endin

gk_PianoNotePianoteq_midi_dynamic_range init 127
instr 11,12,13,14
if p3 == -1 then
  p3 = 1000000
endif
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_PianoNotePianoteq_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
i_homogeneity = p11
instances active p1
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
i_pitch_correction = 44100 / sr
; prints "Pitch factor:   %9.4f\n", i_pitch_correction
vstnote gi_Pianoteq, 0, i_midi_key, i_midi_velocity, i_duration
endin

// This must be initialized in the orc header before any #includes.

gk_PianoOutPianoteq_level init 0
gi_PianoOutPianoteq_print init 1
gk_PianoOutPianoteq_front_to_back init 0
gk_PianoOutPianoteq_left_to_right init 0.5
gk_PianoOutPianoteq_bottom_to_top init 0

instr PianoOutPianoteq
; Should be "D4 Daily Practice".
vstprogset gi_Pianoteq, 0
; Sustain off.
vstparamset gi_Pianoteq, 0, 0
; Reverb off.
vstparamset gi_Pianoteq, 72, 0
k_gain = ampdb(gk_PianoOutPianoteq_level)
i_overall_amps = 89
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
; printks "vstaudio:       %9.4f   %9.4f\n", 0.5, aoutleft, aoutright
#ifdef USE_SPATIALIZATION
a_signal = a_out_left + a_out_right
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, gk_PianoOutPianoteq_front_to_back, gk_PianoOutPianoteq_left_to_right, gk_PianoOutPianoteq_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
; printks "PianoOutPt     L %9.4f R %9.4f l %9.4f\\n", 0.5, a_out_left, a_out_right, gk_Piano_level
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin


gk_OrganOutOrganteq_level init 0
gi_OrganOutOrganteq_print init 1
gk_OrganOutOrganteq_front_to_back init 0
gk_OrganOutOrganteq_left_to_right init 0.5
gk_OrganOutOrganteq_bottom_to_top init 0
instr OrganOutOrganteq
; By default, Organteq .fxp  preset files are saved thus:
; /home/mkg/.local/share/Modartt/Organteq/Presets/My Presets/Church (copy).fxp
; However, vst4cs doesn't load .fxp files, only .fxb preset bank files.
; However again, Organtec doesn't save .fxb files, only .fxp files.
; The vst4cs program change opcode does not seem to work except with loaded .fxb files.
; vstprogset gi_Organteq, 3
; vstmidiout gi_Organteq, 192, 5, 15, 0
; vstmidiout gi_Organteq, 192, 6, 15, 0
; So, the only thing to do is to set each parameter right here. Unfortunately,
; not all the parameters in the GUI are available from code.
; Reverb control.
vstparamset gi_Organteq, 4, 0
; Tutti (test), it works.
; vstparamset gi_Organteq, 6, 1

; Set up all stops...

; Keyboard 1 -- Pedale

vstparamset gi_Organteq, 33, 1
vstparamset gi_Organteq, 34, 0
vstparamset gi_Organteq, 35, 0
vstparamset gi_Organteq, 36, 0
vstparamset gi_Organteq, 37, 1
vstparamset gi_Organteq, 38, 0
vstparamset gi_Organteq, 39, 0
vstparamset gi_Organteq, 40, 0
vstparamset gi_Organteq, 41, 0
vstparamset gi_Organteq, 42, 0

; Keyboard 2 -- Positif

vstparamset gi_Organteq, 43, 0
vstparamset gi_Organteq, 44, 1
vstparamset gi_Organteq, 45, 0
vstparamset gi_Organteq, 46, 0
vstparamset gi_Organteq, 47, 1
vstparamset gi_Organteq, 48, 0
vstparamset gi_Organteq, 49, 0
vstparamset gi_Organteq, 50, 0
vstparamset gi_Organteq, 51, 1
vstparamset gi_Organteq, 52, 0

; Keyboard 3 -- Grand Orgue

vstparamset gi_Organteq, 53, 0
vstparamset gi_Organteq, 54, 1
vstparamset gi_Organteq, 55, 1
vstparamset gi_Organteq, 56, 0
vstparamset gi_Organteq, 57, 0 
vstparamset gi_Organteq, 58, 0
vstparamset gi_Organteq, 59, 0
vstparamset gi_Organteq, 60, 0
vstparamset gi_Organteq, 61, 0
vstparamset gi_Organteq, 62, 0

; Keyboard 4 - Recit 

vstparamset gi_Organteq, 63, 1
vstparamset gi_Organteq, 64, 1
vstparamset gi_Organteq, 65, 0
vstparamset gi_Organteq, 66, 0
vstparamset gi_Organteq, 67, 0
vstparamset gi_Organteq, 68, 0
vstparamset gi_Organteq, 69, 0
vstparamset gi_Organteq, 70, 1
vstparamset gi_Organteq, 71, 0
vstparamset gi_Organteq, 72, 0

k_gain = ampdb(gk_OrganOutOrganteq_level)
i_overall_amps = 89
i_normalization = ampdb(-i_overall_amps) * 2
i_amplitude = ampdb(80) * i_normalization
if gi_OrganOutOrganteq_print == 1 then
  vstinfo gi_Organteq
endif
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
ainleft init 0
ainright init 0
aoutleft, aoutright vstaudio gi_Organteq, ainleft, ainright
a_signal = aoutleft + aoutright
a_signal *= k_gain
a_signal *= i_amplitude
a_out_left, a_out_right pan2 a_signal, gk_OrganOutOrganteq_left_to_right
; printks "vstaudio:       %9.4f   %9.4f\\n", 0.5, aoutleft, aoutright
#ifdef USE_SPATIALIZATION
a_signal = a_out_left + a_out_right
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, gk_OrganOutOrganteq_front_to_back, gk_OrganOutOrganteq_left_to_right, gk_OrganOutOrganteq_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
; printks "OrganOutPt     L %9.4f R %9.4f l %9.4f\\n", 0.5, a_out_left, a_out_right, gk_Organ_level
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_Mverb2020_level init 0
gk_Mverb2020_Mix init .9
gk_Mverb2020_Pre_delay init 0.5
gk_Mverb2020_Early_late_mix init 0.5
gk_Mverb2020_Size init .9
gk_Mverb2020_Density init 0.75
gk_Mverb2020_Bandwith_Frequency init 0.75
gk_Mverb2020_Decay init 0.98
gk_Mverb2020_Damping_Frequency init 0.66665
gk_Mverb2020_Gain init 1
gi_Mverb2020_Program init 4
instr Mverb2020
;vstprogset gi_Mverb2020, gi_Mverb2020_Program
vstparamset gi_Mverb2020, 1, gk_Mverb2020_Mix
vstparamset gi_Mverb2020, 1, gk_Mverb2020_Pre_delay
vstparamset gi_Mverb2020, 2, gk_Mverb2020_Early_late_mix
vstparamset gi_Mverb2020, 3, gk_Mverb2020_Size
vstparamset gi_Mverb2020, 4, gk_Mverb2020_Density
vstparamset gi_Mverb2020, 5, gk_Mverb2020_Bandwith_Frequency
vstparamset gi_Mverb2020, 6, gk_Mverb2020_Decay
vstparamset gi_Mverb2020, 7, gk_Mverb2020_Damping_Frequency
vstparamset gi_Mverb2020, 8, gk_Mverb2020_Gain
k_gain = ampdb(gk_Mverb2020_level)
ainleft inleta "inleft"
ainright inleta "inright"
aoutleft, aoutright vstaudio gi_Mverb2020, ainleft, ainright
outleta "outleft", aoutleft
outleta "outright", aoutright
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gS_MVerb_preset init "Huge Hall"
gk_MVerb_FB init .975
gk_MVerb_wet init .5
gk_MVerb_random init 1
gk_MVerb_rslow init 1.1
gk_MVerb_rfast init 3.8
gk_MVerb_rmax init .0005
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
aoutleft, aoutright MVerb ainleft, ainright, gS_MVerb_preset; , "wet", gk_MVerb_wet, "FB", gk_MVerb_feedback, "random", 1, "rslow", gk_MVerb_rslow, "rfast", gk_MVerb_rfast, "rmax", gk_MVerb_rmax, "print", gk_MVerb_print, "DFact", gk_MVerb_DFact
outleta  "outleft", aoutleft
outleta  "outright", aoutright
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

instr ReverbBabo
ainleft  inleta  "inleft"
ainright  inleta  "inright"
ixsize  = 10  ; width  of the resonator
iysize  = 20  ; depth  of the resonator
izsize  = 8  ; height of the resonator
idiff   = 1  ; diffusion coefficient
;iexpert = p14  ; power user values stored in this function
; Two babos are required, one for left input and one for right input; outputs 
; are stereo and must be summed.
aleftleftout, aleftrightout babo ainleft*0.7, -4, -8, 2, ixsize, iysize, izsize, idiff ;, iexpert
arightleftout, arightrightout babo ainright*0.7, 4, -8, 2, ixsize, iysize, izsize, idiff;, iexpert
aoutleft = aleftleftout + arightleftout
aoutright = aleftrightout + arightrightout
outleta  "outleft", aoutleft
outleta  "outright", aoutright
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_ReverbSC_feedback init 0.995
gk_ReverbSC_wet init 1.
gi_ReverbSC_delay_modulation init 0.1
gk_ReverbSC_frequency_cutoff init 10000
instr ReverbSC
gk_ReverbSC_dry = 1.0 - gk_ReverbSC_wet
aleftin init 0
arightin init 0
aleftout init 0
arightout init 0
aleftin inleta "inleft"
arightin inleta "inright"
aleftout, arightout reverbsc aleftin, arightin, gk_ReverbSC_feedback, gk_ReverbSC_frequency_cutoff, sr, gi_ReverbSC_delay_modulation
aleftoutmix = aleftin * gk_ReverbSC_dry + aleftout * gk_ReverbSC_wet
arightoutmix = arightin * gk_ReverbSC_dry + arightout * gk_ReverbSC_wet
outleta "outleft", aleftoutmix
outleta "outright", arightoutmix
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_NReverb_wet init 0.5
gk_NReverb_time init 4
gk_NReverb_khdif init .75
gi_NReverb_nCombs init 8
gi_NReverb_fCombs ftgen 0, 0, 16, -2, -1116, -1188, -1277, -1356, -1422, -1491, -1557, -1617, .8, .79, .78, .77, .76, .75, .74, .73
gi_NReverb_nAllPass init 4
gi_NReverb_fAllPass ftgen 0, 0, 8, -2, -556, -441, -341, -225, .7, .72, .74, .76
instr NReverb
print gi_NReverb_nCombs, gi_NReverb_fCombs, gi_NReverb_nAllPass, gi_NReverb_fAllPass
ainleft inleta  "inleft"
ainright inleta  "inright"
gk_NReverb_dry = 1.0 - gk_NReverb_wet
; ares    nreverb asig,     ktime,           khdif      [, iskip] [,inumCombs]          [, ifnCombs]         [, inumAlpas]           [, ifnAlpas]
aleftout  nreverb ainleft,  gk_NReverb_time, gk_NReverb_khdif, 0, gi_NReverb_nCombs, gi_NReverb_fCombs, gi_NReverb_nAllPass, gi_NReverb_fAllPass
arightout nreverb ainright, gk_NReverb_time, gk_NReverb_khdif, 0, gi_NReverb_nCombs, gi_NReverb_fCombs, gi_NReverb_nAllPass, gi_NReverb_fAllPass
aleftoutmix = ainleft * gk_NReverb_dry + aleftout * gk_NReverb_wet
arightoutmix = ainright * gk_NReverb_dry + arightout * gk_NReverb_wet
outleta "outleft", aleftoutmix
outleta "outright", arightoutmix
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_MasterOutput_level init 0
gS_MasterOutput_filename init "check.wav"
instr MasterOutput
aleft inleta "inleft"
aright inleta "inright"
k_gain = ampdb(gk_MasterOutput_level)
printks2 "Master gain: %f\\n", k_gain
iamp init 1
aleft butterlp aleft, 18000
aright butterlp aright, 18000
outs aleft * k_gain, aright * k_gain
; We want something that will play on my phone.
i_amplitude_adjustment = 1; ampdbfs(-3) / 32767
i_filename_length strlen gS_MasterOutput_filename
if i_filename_length > 0 goto filename_exists
goto filename_endif
filename_exists:
prints sprintf("Output filename: %s\\n", gS_MasterOutput_filename)
fout gS_MasterOutput_filename, 18, aleft * i_amplitude_adjustment, aright * i_amplitude_adjustment
filename_endif:
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

</CsInstruments>
<CsScore>
f 0 620
i 3 0 34.5809 68 60 0 0 0 0 4095 1 
i 3.24975 0 34.5809 57 60 0 0 0 0 4095 1 
i 3.4995 0 34.5809 47 60 0 0 0 0 4095 1 
i 3.74925 0 34.5809 37 60 0 0 0 0 4095 1 
i 3.999 0 34.5809 75 60 0 0 0 0 4095 1 
i 3 21.34624 34.5809 45 60 0 0 0 0 4095 1 
i 3.24975 21.34624 34.5809 34 60 0 0 0 0 4095 1 
i 3.4995 21.34624 34.5809 36 60 0 0 0 0 4095 1 
i 3.74925 21.34624 34.5809 38 60 0 0 0 0 4095 1 
i 3.999 21.34624 34.5809 41 60 0 0 0 0 4095 1 
i 3 46.96172 34.5809 56 60 0 0 0 0 4095 1 
i 3.24975 46.96172 34.5809 33 60 0 0 0 0 4095 1 
i 3.4995 46.96172 34.5809 35 60 0 0 0 0 4095 1 
i 3.74925 46.96172 34.5809 37 60 0 0 0 0 4095 1 
i 3.999 46.96172 34.5809 40 60 0 0 0 0 4095 1 
i 3 64.03871 34.5809 56 60 0 0 0 0 4095 1 
i 3.24975 64.03871 34.5809 92 60 0 0 0 0 4095 1 
i 3.4995 64.03871 34.5809 32 60 0 0 0 0 4095 1 
i 3.74925 64.03871 34.5809 40 60 0 0 0 0 4095 1 
i 3.999 64.03871 34.5809 44 60 0 0 0 0 4095 1 
i 3 89.65419 34.5809 44 60 0 0 0 0 4095 1 
i 3.24975 89.65419 34.5809 80 60 0 0 0 0 4095 1 
i 3.4995 89.65419 34.5809 32 60 0 0 0 0 4095 1 
i 3.74925 89.65419 34.5809 38 60 0 0 0 0 4095 1 
i 3.999 89.65419 34.5809 38 60 0 0 0 0 4095 1 
i 3 120.3928 34.5809 43 60 0 0 0 0 4095 1 
i 3.24975 120.3928 34.5809 57 60 0 0 0 0 4095 1 
i 3.4995 120.3928 34.5809 34 60 0 0 0 0 4095 1 
i 3.74925 120.3928 34.5809 37 60 0 0 0 0 4095 1 
i 3.999 120.3928 34.5809 39 60 0 0 0 0 4095 1 
i 3 140.8852 34.5809 43 60 0 0 0 0 4095 1 
i 3.24975 140.8852 34.5809 45 60 0 0 0 0 4095 1 
i 3.4995 140.8852 34.5809 35 60 0 0 0 0 4095 1 
i 3.74925 140.8852 34.5809 36 60 0 0 0 0 4095 1 
i 3.999 140.8852 34.5809 39 60 0 0 0 0 4095 1 
i 3 157.9621 34.5809 91 60 0 0 0 0 4095 1 
i 3.24975 157.9621 34.5809 69 60 0 0 0 0 4095 1 
i 3.4995 157.9621 34.5809 35 60 0 0 0 0 4095 1 
i 3.74925 157.9621 34.5809 37 60 0 0 0 0 4095 1 
i 3.999 157.9621 34.5809 40 60 0 0 0 0 4095 1 
i 3 178.4545 34.5809 67 60 0 0 0 0 4095 1 
i 3.24975 178.4545 34.5809 46 60 0 0 0 0 4095 1 
i 3.4995 178.4545 34.5809 36 60 0 0 0 0 4095 1 
i 3.74925 178.4545 34.5809 38 60 0 0 0 0 4095 1 
i 3.999 178.4545 34.5809 39 60 0 0 0 0 4095 1 
i 3 192.1161 34.5809 91 60 0 0 0 0 4095 1 
i 3.24975 192.1161 34.5809 81 60 0 0 0 0 4095 1 
i 3.4995 192.1161 34.5809 83 60 0 0 0 0 4095 1 
i 3.74925 192.1161 34.5809 85 60 0 0 0 0 4095 1 
i 3.999 192.1161 34.5809 78 60 0 0 0 0 4095 1 
i 3 217.7316 34.5809 31 60 0 0 0 0 4095 1 
i 3.24975 217.7316 34.5809 57 60 0 0 0 0 4095 1 
i 3.4995 217.7316 34.5809 83 60 0 0 0 0 4095 1 
i 3.74925 217.7316 34.5809 60 60 0 0 0 0 4095 1 
i 3.999 217.7316 34.5809 74 60 0 0 0 0 4095 1 
i 3 248.4702 34.5809 30 60 0 0 0 0 4095 1 
i 3.24975 248.4702 34.5809 68 60 0 0 0 0 4095 1 
i 3.4995 248.4702 34.5809 35 60 0 0 0 0 4095 1 
i 3.74925 248.4702 34.5809 36 60 0 0 0 0 4095 1 
i 3.999 248.4702 34.5809 77 60 0 0 0 0 4095 1 
i 3 268.9626 34.5809 65 60 0 0 0 0 4095 1 
i 3.24975 268.9626 34.5809 66 60 0 0 0 0 4095 1 
i 3.4995 268.9626 34.5809 55 60 0 0 0 0 4095 1 
i 3.74925 268.9626 34.5809 77 60 0 0 0 0 4095 1 
i 3.999 268.9626 34.5809 65 60 0 0 0 0 4095 1 
i 3 299.7012 34.5809 41 60 0 0 0 0 4095 1 
i 3.24975 299.7012 34.5809 65 60 0 0 0 0 4095 1 
i 3.4995 299.7012 34.5809 38 60 0 0 0 0 4095 1 
i 3.74925 299.7012 34.5809 89 60 0 0 0 0 4095 1 
i 3.999 299.7012 34.5809 53 60 0 0 0 0 4095 1 
i 3 336.5874 34.5809 67 60 0 0 0 0 4095 1 
i 3.24975 336.5874 34.5809 67 60 0 0 0 0 4095 1 
i 3.4995 336.5874 34.5809 52 60 0 0 0 0 4095 1 
i 3.74925 336.5874 34.5809 52 60 0 0 0 0 4095 1 
i 3.999 336.5874 34.5809 52 60 0 0 0 0 4095 1 
i 3 361.1783 34.5809 54 60 0 0 0 0 4095 1 
i 3.24975 361.1783 34.5809 33 60 0 0 0 0 4095 1 
i 3.4995 361.1783 34.5809 58 60 0 0 0 0 4095 1 
i 3.74925 361.1783 34.5809 37 60 0 0 0 0 4095 1 
i 3.999 361.1783 34.5809 52 60 0 0 0 0 4095 1 
i 3 381.6707 34.5809 52 60 0 0 0 0 4095 1 
i 3.24975 381.6707 34.5809 31 60 0 0 0 0 4095 1 
i 3.4995 381.6707 34.5809 88 60 0 0 0 0 4095 1 
i 3.74925 381.6707 34.5809 76 60 0 0 0 0 4095 1 
i 3.999 381.6707 34.5809 40 60 0 0 0 0 4095 1 
i 3 406.2616 34.5809 91 60 0 0 0 0 4095 1 
i 3.24975 406.2616 34.5809 45 60 0 0 0 0 4095 1 
i 3.4995 406.2616 34.5809 59 60 0 0 0 0 4095 1 
i 3.74925 406.2616 34.5809 51 60 0 0 0 0 4095 1 
i 3.999 406.2616 34.5809 41 60 0 0 0 0 4095 1 
i 3 422.6555 34.5809 66 60 0 0 0 0 4095 1 
i 3.24975 422.6555 34.5809 44 60 0 0 0 0 4095 1 
i 3.4995 422.6555 34.5809 70 60 0 0 0 0 4095 1 
i 3.74925 422.6555 34.5809 38 60 0 0 0 0 4095 1 
i 3.999 422.6555 34.5809 40 60 0 0 0 0 4095 1 
i 3 439.7325 34.5809 78 60 0 0 0 0 4095 1 
i 3.24975 439.7325 34.5809 56 60 0 0 0 0 4095 1 
i 3.4995 439.7325 34.5809 70 60 0 0 0 0 4095 1 
i 3.74925 439.7325 34.5809 36 60 0 0 0 0 4095 1 
i 3.999 439.7325 34.5809 40 60 0 0 0 0 4095 1 
i 3 460.2248 34.5809 77 60 0 0 0 0 4095 1 
i 3.24975 460.2248 34.5809 68 60 0 0 0 0 4095 1 
i 3.4995 460.2248 34.5809 58 60 0 0 0 0 4095 1 
i 3.74925 460.2248 34.5809 36 60 0 0 0 0 4095 1 
i 3.999 460.2248 34.5809 40 60 0 0 0 0 4095 1 
i 3 473.8864 34.5809 64 60 0 0 0 0 4095 1 
i 3.24975 473.8864 34.5809 45 60 0 0 0 0 4095 1 
i 3.4995 473.8864 34.5809 63 60 0 0 0 0 4095 1 
i 3.74925 473.8864 34.5809 40 60 0 0 0 0 4095 1 
i 3.999 473.8864 34.5809 64 60 0 0 0 0 4095 1 
i 3 494.3788 34.5809 30 60 0 0 0 0 4095 1 
i 3.24975 494.3788 34.5809 90 60 0 0 0 0 4095 1 
i 3.4995 494.3788 34.5809 81 60 0 0 0 0 4095 1 
i 3.74925 494.3788 34.5809 42 60 0 0 0 0 4095 1 
i 3.999 494.3788 34.5809 54 60 0 0 0 0 4095 1 
i 3 518.9697 34.5809 91 60 0 0 0 0 4095 1 
i 3.24975 518.9697 34.5809 82 60 0 0 0 0 4095 1 
i 3.4995 518.9697 34.5809 85 60 0 0 0 0 4095 1 
i 3.74925 518.9697 34.5809 62 60 0 0 0 0 4095 1 
i 3.999 518.9697 34.5809 42 60 0 0 0 0 4095 1 
i 3 535.3636 34.5809 31 60 0 0 0 0 4095 1 
i 3.24975 535.3636 34.5809 81 60 0 0 0 0 4095 1 
i 3.4995 535.3636 34.5809 58 60 0 0 0 0 4095 1 
i 3.74925 535.3636 34.5809 37 60 0 0 0 0 4095 1 
i 3.999 535.3636 34.5809 40 60 0 0 0 0 4095 1 
i 3 549.0252 34.5809 44 60 0 0 0 0 4095 1 
i 3.24975 549.0252 34.5809 32 60 0 0 0 0 4095 1 
i 3.4995 549.0252 34.5809 80 60 0 0 0 0 4095 1 
i 3.74925 549.0252 34.5809 76 60 0 0 0 0 4095 1 
i 3.999 549.0252 34.5809 40 60 0 0 0 0 4095 1 
i 3 565.4191 34.5809 92 60 0 0 0 0 4095 1 
i 3.24975 565.4191 34.5809 82 60 0 0 0 0 4095 1 
i 3.4995 565.4191 34.5809 48 60 0 0 0 0 4095 1 
i 3.74925 565.4191 34.5809 37 60 0 0 0 0 4095 1 
i 3.999 565.4191 34.5809 40 60 0 0 0 0 4095 1 

</CsScore>
</CsoundSynthesizer>
