<CsoundSyntheizer>
<CsLicense>

# snow-voice

snow-voice

Copyright (C) 2023 by Michael Gogins
[Attribution-NonCommercial-ShareAlike 3.0 Unported (CC BY-NC-SA 3.0)](http://creativecommons.org/licenses/by-nc-sa/3.0/)

The score of this piece is generated in this Csound file using my 
csound-cxx-opcodes, which compile C++ source code embedded in this file to a 
dynamic link library, and invoke the code in that library to generate the 
Csound score as Csound runs. This is a "pseudo-tonal" piece. The score is 
generated using my CsoundAC library for algorithmic composition. The basic 
structure of the piece is a recurrent iterated function system of notes; but 
the system also generates root progressions and modulations _for_ the notes.

All of the sound is synthesized using the Csound instruments in this file, 
except for the piano sound, which is synthesized by Modartt's Pianoteq plugin, 
accessed via my csound-vst3-opcodes.

The generated score is displayed in a Web page (also embedded in this file) 
using my csound-webserver-opcodes as a three-dimensional piano roll, along 
with controls for adjusting the levels and parameters of the instruments.

# Biography

I was born in 1950 in Salt Lake City, Utah, and lived there till 1973, a 
wonderful place to grow up with many trips to mountains, desert, and unlocked 
university labs. My father was an inventor, my mother was a fine artist and 
commercial artist. I have pursued poetry, photography, music performance, and 
music composition. I have lived in Salt Lake City, Los Angeles, New York, 
Seattle, and now New York again. I have a B.A. in comparative religion from 
the University of Washington, 1984.

At the same time as I was studying comparative religion, I was taking seminars 
in computer music with John Rahn. Computer music gradually became my major 
interest. It also enabled me to make a living as a software engineer. In the 
late 1980s, I benefited greatly from Brad Garton's openness to non-student 
participation in the woof user group and concerts at the Columbia-Princeton 
Electronic Music Center, now the Columbia Computer Music Center.

Currently, I contribute code to Csound, maintain the csound VST3 opcodes for 
hosting VST plugins in Csound, maintain the Csound for Android app, and 
maintain the CsoundAC package incorporating various facilities for algorithmic 
composition in C++, Python, and HTML5. I write articles and papers and give 
talks on computer music, and I create computer music. I have a special 
interest in algorithmic composition. I am currently working to bring new 
developments in mathematical music theory into algorithmic composition 
software. I am married to Heidi Rogers, who before she retired ran Framk Music 
Company, a classical sheet music store in New York. We live on the Upper West 
Side of Manhattan, and in the Catskills.

</CsLicense>
<CsOptions>
--m-amps=1 --m-range=1 --m-dB=1 --m-benchmarks=1 --m-warnings=0 -+msg_color=0 -d -odac 
</CsOptions>
<CsInstruments>

//////////////////////////////////////////////////////////////////////////////
// Change to sr=96000 with ksmps=1 for final rendering to soundfile.
//////////////////////////////////////////////////////////////////////////////

sr = 48000
ksmps = 128
nchnls = 2
0dbfs = 100

//////////////////////////////////////////////////////////////////////////////
// This random seed ensures that the same random stream  is used for each 
// rendering. Note that rand, randh, randi, rnd(x) and birnd(x) are not 
// affected by seed.
//////////////////////////////////////////////////////////////////////////////

seed 88818145

//////////////////////////////////////////////////////////////////////////////
// Turn printing of VST plugin parameters off and on globally.
//////////////////////////////////////////////////////////////////////////////

gi_vstinfo init 0

//////////////////////////////////////////////////////////////////////////////
// We will load plugins from different locations on different operating 
// systems.
//////////////////////////////////////////////////////////////////////////////

gS_os, gS_macros cxx_os
prints "Operating system: %s\n", gS_os

gi_base init 0

opcode instrument_position, kk, iii
i_onset, i_radius, i_rate xin
k_pan = (p1 / 10) + (1 / 10)
if floor(p1) == 1 then
k_pan = .4
endif
if floor(p1) == 2 then
k_pan = .6
endif

xout 0, k_pan
endop

connect "PianoOutPianoteq", "outleft", "ReverbSC", "inleft"
connect "PianoOutPianoteq", "outright", "ReverbSC", "inright"
connect "Harpsichord", "outleft", "ReverbSC", "inleft"
connect "Harpsichord", "outright", "ReverbSC", "inright"
connect "Plucked", "outleft", "ReverbSC", "inleft"
connect "Plucked", "outright", "ReverbSC", "inright"
connect "SeidelHarmOsc", "outleft", "ReverbSC", "inleft"
connect "SeidelHarmOsc", "outright", "ReverbSC", "inright"
connect "ZakianFlute", "outleft", "ReverbSC", "inleft"
connect "ZakianFlute", "outright", "ReverbSC", "inright"
connect "Sweeper", "outleft", "ReverbSC", "inleft"
connect "Sweeper", "outright", "ReverbSC", "inright"
connect "FMWaterBell", "outleft", "ReverbSC", "inleft"
connect "FMWaterBell", "outright", "ReverbSC", "inright"

connect "ReverbSC", "outleft", "MasterOutput", "inleft"
connect "ReverbSC", "outright", "MasterOutput", "inright"

//////////////////////////////////////////////////////////////////////////////
// These are all the Csound instruments and effects used in this piece.
//////////////////////////////////////////////////////////////////////////////

if strcmp(gS_os, "Linux") == 0 then
gi_Pianoteq vstinit "/home/mkg/Pianoteq\ 7/x86-64bit/Pianoteq\ 7.so", gi_vstinfo
endif
if strcmp(gS_os, "macOS") == 0 then
gi_Pianoteq vst3init "/Library/Audio/Plug-Ins/VST3/Pianoteq\ 7.vst3", "Pianoteq 7", 0
endif

gk_PianoNotePianoteq_midi_dynamic_range chnexport "gk_PianoNotePianoteq_midi_dynamic_range", 3 ;  20

gk_PianoNotePianoteq_midi_dynamic_range init 20

instr PianoNotePianoteq
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
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
i_pitch_correction = 44100 / sr
; prints "Pitch factor:   %9.4f\n", i_pitch_correction
i_result vst3note gi_Pianoteq, 0, i_midi_key, i_midi_velocity, i_duration
endin

gk_Harpsichord_level chnexport "gk_Harpsichord_level", 3
gk_Harpsichord_pick chnexport "gk_Harpsichord_pick", 3
gk_Harpsichord_reflection chnexport "gk_Harpsichord_reflection", 3
gk_Harpsichord_pluck chnexport "gk_Harpsichord_pluck", 3
gk_Harpsichord_midi_dynamic_range chnexport "gk_Harpsichord_midi_dynamic_range", 3
gk_Harpsichord_space_left_to_right chnexport "gk_Harpsichord_space_left_to_right", 3

gk_Harpsichord_level init 0
gk_Harpsichord_pick init .075
gk_Harpsichord_reflection init .5
gk_Harpsichord_pluck init .75
gk_Harpsichord_midi_dynamic_range init 20
gk_Harpsichord_space_left_to_right init .5

gi_Harpsichord_harptable ftgen 0, 0, 65537, 7, -1, 1024, 1, 1024, -1

instr Harpsichord
i_instrument = p1
i_time = p2
; Make indefinite notes last no longer than the physical decay.
i_physical_decay = 40
if p3 == -1 then
i_duration = i_physical_decay
else
i_duration = p3
endif
i_midi_key = p4
i_midi_dynamic_range = i(gk_Harpsichord_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
if p7 == 0 then
k_space_left_to_right = gk_Harpsichord_space_left_to_right
else
k_space_left_to_right = p7
endif
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 66
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Harpsichord_level)
iHz = cpsmidinn(i_midi_key)
kHz = k(iHz)
a_physical_envelope transeg 1.0, i_physical_decay, -25.0, 0.0
apluck pluck i_amplitude * k_gain, kHz, iHz, 0, 1
aharp poscil a_physical_envelope, kHz, gi_Harpsichord_harptable
aharp2 balance apluck, aharp
a_signal	= (apluck + aharp2)
i_attack = .0005
i_sustain = p3
i_release = 0.01
; The end of the note must be extended _past_ the end of the release segment.
xtratim 1
i_declick_attack init .0008
i_declick_release init .01
a_declicking_envelope cossegr 0, i_declick_attack, 1,  i_duration, 1,  i_declick_release, 0
a_envelope = a_declicking_envelope
a_filtered_envelope tonex a_envelope, 40, 4
a_signal = a_signal * i_amplitude * a_filtered_envelope * k_gain 


a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
;printks "Harpsichord      %9.4f   %9.4f\n", 0.5, a_out_left, a_out_right
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_Plucked_midi_dynamic_range chnexport "gk_Plucked_midi_dynamic_range", 3
gk_Plucked_midi_dynamic_range init 30
gk_Plucked_space_left_to_right chnexport "gk_Plucked_space_left_to_right", 3
gk_Plucked_space_left_to_right init .5
gk_Plucked_level chnexport "gk_Plucked_level", 3
gk_Plucked_level init 0

gi_Plucked_sine ftgen 0, 0, 65537, 10, 1

instr Plucked
; Author: Michael Gogins
i_instrument = p1
i_time = p2
; Make indefinite notes last no longer than the physical decay.
i_physical_decay = 20
if p3 == -1 then
i_duration = i_physical_decay
else
i_duration = p3
endif
i_midi_key = p4
i_midi_dynamic_range = i(gk_Plucked_midi_dynamic_range)
i_midi_velocity = p5 ;* i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
i_midi_velocity ampmidid i_midi_velocity, i_midi_dynamic_range
k_space_front_to_back = p6
if p7 == 0 then
k_space_left_to_right = gk_Plucked_space_left_to_right
else
k_space_left_to_right = p7
endif
k_space_bottom_to_top = p8
i_phase = p9
i_detune_cents = 1.5
i_detune = i_detune_cents / 100
i_frequency1 = cpsmidinn(i_midi_key - i_detune)
i_frequency2 = cpsmidinn(i_midi_key)
i_frequency3 = cpsmidinn(i_midi_key + i_detune)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_overall_amps = 26
i_normalization = ampdb(-(i_overall_amps)) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Plucked_level)
asignal1 wgpluck2 0.1, 1.0, i_frequency1, 0.25, 0.222
asignal2 wgpluck2 0.1, 1.0, i_frequency2, 0.20, 0.223
asignal3 wgpluck2 0.1, 1.0, i_frequency3, 0.23, 0.225
a_signal = (asignal1 + asignal2 + asignal3)
; As with most instruments that are based upon an impulse delivered to a 
; resonator, there are two envelopes, one for the physical decay with a 
; fixed release ending at zero, and one with a release segment to remove 
; clicks from the attack and release.
;
; As with most software instruments that are modeled on an impulse exciting a 
; resonator, there should be two envelopes. The "physical" envelope must have a 
; fixed decay ending at zero.
i_declick_minimum = .001
i_attack = .001 / i_frequency2 + i_declick_minimum
i_exponent = 7
a_physical_envelope transeg 0,   i_attack, i_exponent,  1,   i_physical_decay, -i_exponent,  0
; The de-clicking envelope must have attack and release segments that damp 
; artifacts in the signal. The duration of these segments depends on 
; the behavior of the instrument, and may vary as a function of frequency.
i_declick_attack = i_attack
i_declick_release = i_declick_minimum * 2
; The end of the note must be extended _past_ the end of the release segment.
xtratim 1
a_declicking_envelope cossegr 0, i_declick_attack, 1,  i_duration, 1,  i_declick_release, 0
; The envelope of the instrument is the product of the physical envelope times 
; the declicking envelope. 
a_envelope = a_physical_envelope * a_declicking_envelope
; That envelope is then low-pass filtered to remove most discontinuities.
a_filtered_envelope tonex a_envelope, 40, 4
a_signal = a_signal * i_amplitude * a_filtered_envelope * k_gain *.001

a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gi_SeidelHarmOsc_tabsz init 2^16

gi_SeidelHarmOsc_Sin     ftgen 0, 0, gi_SeidelHarmOsc_tabsz, 9, 1,1,0
gi_SeidelHarmOsc_Tri     ftgen 0, 0, gi_SeidelHarmOsc_tabsz, 9, 1,1,0,  3,0.333,180,  5,0.2,0,  7,0.143,180, 9,0.111,0, 11,0.091,180, 13,0.077,0, 15,0.067,180, 17,0.059,0, 19,0.053,180, 21,0.048,0, 23,0.043,180, 25,0.04,0, 27,0.037,180, 29,0.034,0, 31,0.032,180
gi_SeidelHarmOsc_Saw     ftgen 0, 0, gi_SeidelHarmOsc_tabsz, 7, 0, gi_SeidelHarmOsc_tabsz/2, 1, 0, -1, gi_SeidelHarmOsc_tabsz/2, 0
gi_SeidelHarmOsc_Square  ftgen 0, 0, gi_SeidelHarmOsc_tabsz, 7, 1, gi_SeidelHarmOsc_tabsz/2, 1, 0, -1, gi_SeidelHarmOsc_tabsz/2, -1
gi_SeidelHarmOsc_Prime   ftgen 0, 0, gi_SeidelHarmOsc_tabsz, 9, 1,1,0,  2,0.5,0,  3,0.3333,0,  5,0.2,0,   7,0.143,0,  11,0.0909,0,  13,0.077,0,   17,0.0588,0,  19,0.0526,0, 23,0.0435,0, 27,0.037,0, 31,0.032,180
gi_SeidelHarmOsc_Fib     ftgen 0, 0, gi_SeidelHarmOsc_tabsz, 9, 1,1,0,  2,0.5,0,  3,0.3333,0,  5,0.2,0,   8,0.125,0,  13,0.0769,0,  21,0.0476,0,  34,0.0294,0 ;,  55,0.0182,0,  89,0.0112,0, 144,0.0069,0

gi_SeidelHarmOsc_NumTables = 5
gi_SeidelHarmOsc_List ftgen 1000, 0, gi_SeidelHarmOsc_NumTables, -2, gi_SeidelHarmOsc_Tri, gi_SeidelHarmOsc_Saw, gi_SeidelHarmOsc_Square, gi_SeidelHarmOsc_Prime, gi_SeidelHarmOsc_Fib, gi_SeidelHarmOsc_Sin
gi_SeidelHarmOsc_Morf ftgen 1001, 0, gi_SeidelHarmOsc_tabsz, 10, 1

gi_SeidelHarmOsc_lforabsz init 2^13
gi_SeidelHarmOsc_LfoTri ftgen 0, 0, gi_SeidelHarmOsc_lforabsz, 7, 0, gi_SeidelHarmOsc_lforabsz/4, 1, gi_SeidelHarmOsc_lforabsz/2, -1, gi_SeidelHarmOsc_lforabsz/4, 0

gk_SeidelHarmOsc_level chnexport "gk_SeidelHarmOsc_level", 3 ;  0
gi_SeidelHarmOsc_attack chnexport "gi_SeidelHarmOsc_attack", 3 ;  0.003
gi_SeidelHarmOsc_petals chnexport "gi_SeidelHarmOsc_petals", 3 ;  2.99
gi_SeidelHarmOsc_release chnexport "gi_SeidelHarmOsc_release", 3 ;  0.01
gk_SeidelHarmOsc_midi_dynamic_range chnexport "gk_SeidelHarmOsc_midi_dynamic_range", 3 ;  20

gk_SeidelHarmOsc_level init 0
gi_SeidelHarmOsc_attack init 0.003
gi_SeidelHarmOsc_petals init 2.99
gi_SeidelHarmOsc_release init 0.01
gk_SeidelHarmOsc_midi_dynamic_range init 20

gk_SeidelHarmOsc_P1 chnexport "gk_SeidelHarmOsc_P1", 3
gk_SeidelHarmOsc_IN1CON chnexport "gk_SeidelHarmOsc_IN1CON", 3
gk_SeidelHarmOsc_IN1C1 chnexport "gk_SeidelHarmOsc_IN1C1", 3

gk_SeidelHarmOsc_P1 init 0.1
gk_SeidelHarmOsc_IN1CON init 0.1
gk_SeidelHarmOsc_IN1C1 init 1

gk_SeidelHarmOsc_P2 chnexport "gk_SeidelHarmOsc_P2", 3
gk_SeidelHarmOsc_IN2C1 chnexport "gk_SeidelHarmOsc_IN2C1", 3
gk_SeidelHarmOsc_IN2CON chnexport "gk_SeidelHarmOsc_IN2CON", 3

gk_SeidelHarmOsc_P2 init 0.1
gk_SeidelHarmOsc_IN2C1 init 1
gk_SeidelHarmOsc_IN2CON init 0.1

gk_SeidelHarmOsc_P3 chnexport "gk_SeidelHarmOsc_P3", 3
gk_SeidelHarmOsc_IN3C1 chnexport "gk_SeidelHarmOsc_IN3C1", 3
gk_SeidelHarmOsc_IN3CON chnexport "gk_SeidelHarmOsc_IN3CON", 3

gk_SeidelHarmOsc_P3 init 0.1
gk_SeidelHarmOsc_IN3C1 init 1
gk_SeidelHarmOsc_IN3CON init 0.1

gk_SeidelHarmOsc_P4 chnexport "gk_SeidelHarmOsc_P4", 3
gk_SeidelHarmOsc_IN4C1 chnexport "gk_SeidelHarmOsc_IN4C1", 3
gk_SeidelHarmOsc_IN4CON chnexport "gk_SeidelHarmOsc_IN4CON", 3

gk_SeidelHarmOsc_P4 init 0.1
gk_SeidelHarmOsc_IN4C1 init 1
gk_SeidelHarmOsc_IN4CON init 0.1

gi_SeidelHarmOsc_pitch_bend_table ftgen 0, 0, 1024, -7, 1, 1024, 1 
gi_SeidelHarmOsc_sine ftgen 0, 0, 65537, 10, 1

instr SeidelHarmOsc
i_instrument = p1
i_time = p2
i_sustain = p3
xtratim gi_SeidelHarmOsc_attack + gi_SeidelHarmOsc_release
i_midi_key = p4
i_midi_dynamic_range = i(gk_SeidelHarmOsc_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
// Spatial location is specified in Ambisonic coordinates.
k_space_front_to_back = p6
// AKA stereo pan.
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
// Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 87
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_SeidelHarmOsc_level)
; prints("p1=%f, p2=%f, p3=%f, p4=%f\n", p1, p2, p3, p4)
;;;kfreq chnget sprintf("FREQ%d", p4)
kfreq init i_frequency

kin1con init 0
kin2con init 0
kin3con init 0
kin4con init 0

koff  init 0.001
koff1 init 0.001
koff2 init 2 * 0.001
koff3 init 3 * 0.001
koff4 init 5 * 0.001
koffa = scale2(gk_SeidelHarmOsc_P1, 0.001, 0.1, -10, 10)
if (gk_SeidelHarmOsc_IN1CON == 1) then
koffb = scale2(gk_SeidelHarmOsc_IN1C1, 0.001, 0.1, 0, 10)
koff = koffa + koffb
else
koff = koffa
endif
koff1 = koff
koff2 = 2 * koff
koff3 = 3 * koff
koff4 = 5 * koff

itbl = gi_SeidelHarmOsc_Morf
kndx init 0
kndxa = scale2(gk_SeidelHarmOsc_P2, 0, gi_SeidelHarmOsc_NumTables-1.01, -10, 10)
if (gk_SeidelHarmOsc_IN2CON == 1) then
kndxb = scale2(gk_SeidelHarmOsc_IN2C1, 0, gi_SeidelHarmOsc_NumTables-1.01, 0, 10)
kndx = kndxa + kndxb
else
kndx = kndxa
endif
ftmorf kndx, gi_SeidelHarmOsc_List, gi_SeidelHarmOsc_Morf

kamp init 0.8/9

; a1 oscil3 kamp, kfreq, itbl
a2 oscil3 kamp, kfreq+koff1, itbl
a3 oscil3 kamp, kfreq+koff2, itbl
a4 oscil3 kamp, kfreq+koff3, itbl
a5 oscil3 kamp, kfreq+koff4, itbl
a6 oscil3 kamp, kfreq-koff1, itbl
a7 oscil3 kamp, kfreq-koff2, itbl
a8 oscil3 kamp, kfreq-koff3, itbl
a9 oscil3 kamp, kfreq-koff4, itbl

kdst init 0
kdsta = scale2(gk_SeidelHarmOsc_P3, 0, 10, -10, 10)
if (gk_SeidelHarmOsc_IN3CON == 1) then
kdstb = scale2(gk_SeidelHarmOsc_IN3C1, 0, 10, 0, 10)
kdst = kdsta + kdstb
else
kdst = kdsta
endif

aL = a2+a4+a6+a8
aR = a3+a5+a7+a9
aoutL = aL + distort1(aL, kdst, 0.1, 0, 0)
aoutR = aR + distort1(aR, kdst, 0.1, 0, 0)

kpana = scale2(gk_SeidelHarmOsc_P4, 0, 7, -10, 10)
if (gk_SeidelHarmOsc_IN4CON == 1) then
kpanb = scale2(gk_SeidelHarmOsc_IN4C1, 0, 5, 0, 10)
kpan = kpana + kpanb
else
kpan = kpana
endif
if (kpan == 0) then
kext = 0
else
kext = 0.1
endif
klfoL oscili 0.49, kpan-kext, gi_SeidelHarmOsc_LfoTri, 90
klfoR oscili 0.49, kpan+kext, gi_SeidelHarmOsc_LfoTri, 270
klfoL += 0.5
klfoR += 0.5
aoutL1, aoutR1 pan2 aoutL, klfoL
aoutL2, aoutR2 pan2 aoutR, klfoR
aoutL = aoutL1+aoutL2
aoutR = aoutR1+aoutR2

aenv madsr 0.07,0,1,0.7
a_out_left = aoutL * aenv * k_gain * i_amplitude
a_out_right = aoutR * aenv * k_gain * i_amplitude
outleta "outleft", a_out_left
outleta "outright", a_out_right
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_ZakianFlute_midi_dynamic_range chnexport "gk_ZakianFlute_midi_dynamic_range", 3 ;  20
gk_ZakianFlute_level chnexport "gk_ZakianFlute_level", 3 ;  0
gk_ZakianFlute_pan chnexport "gk_ZakianFlute_pan", 3 ;  .5
gi_ZakianFLute_seed chnexport "gi_ZakianFLute_seed", 3 ;  .5
gi_ZakianFLute_space_left_to_front chnexport "gi_ZakianFLute_space_left_to_front", 3 ;  .5

gk_ZakianFlute_midi_dynamic_range init 20
gk_ZakianFlute_level init 0
gk_ZakianFlute_pan init .5
gi_ZakianFLute_seed init .5
gi_ZakianFLute_space_left_to_front init .5

gi_ZakianFLute_f2  ftgen 0, 0, 16, -2, 40, 40, 80, 160, 320, 640, 1280, 2560, 5120, 10240, 10240
gi_ZakianFlute_f26 ftgen 0, 0, 65537, -10, 2000, 489, 74, 219, 125, 9, 33, 5, 5
gi_ZakianFlute_f27 ftgen 0, 0, 65537, -10, 2729, 1926, 346, 662, 537, 110, 61, 29, 7
gi_ZakianFlute_f28 ftgen 0, 0, 65537, -10, 2558, 2012, 390, 361, 534, 139, 53, 22, 10, 13, 10
gi_ZakianFlute_f29 ftgen 0, 0, 65537, -10, 12318, 8844, 1841, 1636, 256, 150, 60, 46, 11
gi_ZakianFlute_f30 ftgen 0, 0, 65537, -10, 1229, 16, 34, 57, 32
gi_ZakianFlute_f31 ftgen 0, 0, 65537, -10, 163, 31, 1, 50, 31
gi_ZakianFlute_f32 ftgen 0, 0, 65537, -10, 4128, 883, 354, 79, 59, 23
gi_ZakianFlute_f33 ftgen 0, 0, 65537, -10, 1924, 930, 251, 50, 25, 14
gi_ZakianFlute_f34 ftgen 0, 0, 65537, -10, 94, 6, 22, 8
gi_ZakianFlute_f35 ftgen 0, 0, 65537, -10, 2661, 87, 33, 18
gi_ZakianFlute_f36 ftgen 0, 0, 65537, -10, 174, 12
gi_ZakianFlute_f37 ftgen 0, 0, 65537, -10, 314, 13
gi_ZakianFlute_wtsin ftgen 0, 0, 65537, 10, 1

instr ZakianFlute
; Author: Lee Zakian
; Adapted by: Michael Gogins
i_instrument = p1
i_time = p2
if p3 == -1 then
i_duration = 1000
else
i_duration = p3
endif
i_midi_key = p4
i_midi_velocity = p5
k_space_front_to_back = p6
if p7 == 0 then
k_space_left_to_right = gi_ZakianFLute_space_left_to_front
else
k_space_left_to_right = p7
endif
k_space_bottom_to_top = p8
i_phase = p9
i_overall_amps = 65.2
i_normalization = ampdb(-i_overall_amps) / 2
i_midi_dynamic_range = i(gk_ZakianFlute_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_ZakianFlute_level)
;;;xtratim iattack + irelease
iHz = cpsmidinn(i_midi_key)
kHz = k(iHz)
// Bug?
aenvelope transeg 1.0, 20.0, -10.0, 0.05
///aenvelope transegr 1.0, 20.0, -10.0, 0.05
ip3 = 3;;; (p3 < 3.0 ? p3 : 3.0)
; parameters
; p4 overall amplitude scaling factor
ip4 init i_amplitude
; p5 pitch in Hertz (normal pitch range: C4-C7)
ip5 init iHz
; p6 percent vibrato depth, recommended values in range [-1., +1.]
ip6 init 0.5
; 0.0 -> no vibrato
; +1. -> 1% vibrato depth, where vibrato rate increases slightly
; -1. -> 1% vibrato depth, where vibrato rate decreases slightly
; p7 attack time in seconds
; recommended value: .12 for slurred notes, .06 for tongued notes
; (.03 for short notes)
ip7 init .08
; p8 decay time in seconds
; recommended value: .1 (.05 for short notes)
ip8 init .08
; p9 overall brightness / filter cutoff factor
; 1 -> least bright / minimum filter cutoff frequency (40 Hz)
; 9 -> brightest / maximum filter cutoff frequency (10,240Hz)
ip9 init 5
; initial variables
iampscale = ip4 ; overall amplitude scaling factor
ifreq = ip5 ; pitch in Hertz
ivibdepth = abs(ip6*ifreq/100.0) ; vibrato depth relative to fundamental frequency
iattack = ip7 * (1.1 - .2*gi_ZakianFLute_seed) ; attack time with up to +-10% random deviation
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947) ; reset gi_ZakianFLute_seed
idecay = ip8 * (1.1 - .2*gi_ZakianFLute_seed) ; decay time with up to +-10% random deviation
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
ifiltcut tablei ip9, gi_ZakianFLute_f2 ; lowpass filter cutoff frequency
iattack = (iattack < 6/kr ? 6/kr : iattack) ; minimal attack length
idecay = (idecay < 6/kr ? 6/kr : idecay) ; minimal decay length
isustain = i_duration - iattack - idecay
;;;p3 = (isustain < 5/kr ? iattack+idecay+5/kr : i_duration) ; minimal sustain length
isustain = (isustain < 5/kr ? 5/kr : isustain)
iatt = iattack/6
isus = isustain/4
idec = idecay/6
iphase = gi_ZakianFLute_seed ; use same phase for all wavetables
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
; vibrato block
; kvibdepth linseg .1, .8*p3, 1, .2*p3, .7
kvibdepth linseg .1, .8*ip3, 1, isustain, 1, .2*ip3, .7
kvibdepth = kvibdepth* ivibdepth ; vibrato depth
kvibdepthr randi .1*kvibdepth, 5, gi_ZakianFLute_seed ; up to 10% vibrato depth variation
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
kvibdepth = kvibdepth + kvibdepthr
ivibr1 = gi_ZakianFLute_seed ; vibrato rate
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
ivibr2 = gi_ZakianFLute_seed
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
if ip6 < 0 goto vibrato1
kvibrate linseg 2.5+ivibr1, isustain, 4.5+ivibr2 ; if p6 positive vibrato gets faster
goto vibrato2
vibrato1:
ivibr3 = gi_ZakianFLute_seed
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
kvibrate linseg 3.5+ivibr1, .1, 4.5+ivibr2,isustain-.1, 2.5+ivibr3 ; if p6 negative vibrato gets slower
vibrato2:
kvibrater randi .1*kvibrate, 5, gi_ZakianFLute_seed ; up to 10% vibrato rate variation
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
kvibrate = kvibrate + kvibrater
kvib oscili kvibdepth, kvibrate, gi_ZakianFlute_wtsin
ifdev1 = -.03 * gi_ZakianFLute_seed ; frequency deviation
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
ifdev2 = .003 * gi_ZakianFLute_seed
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
ifdev3 = -.0015 * gi_ZakianFLute_seed
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
ifdev4 = .012 * gi_ZakianFLute_seed
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
kfreqr linseg ifdev1, iattack, ifdev2, isustain, ifdev3, idecay, ifdev4
kfreq = kHz * (1 + kfreqr) + kvib
if ifreq < 427.28 goto range1 ; (cpspch(8.08) + cpspch(8.09))/2
if ifreq < 608.22 goto range2 ; (cpspch(9.02) + cpspch(9.03))/2
if ifreq < 1013.7 goto range3 ; (cpspch(9.11) + cpspch(10.00))/2
goto range4
; wavetable amplitude envelopes
range1: ; for low range tones
kamp1 linseg 0, iatt, 0.002, iatt, 0.045, iatt, 0.146, iatt, \
0.272, iatt, 0.072, iatt, 0.043, isus, 0.230, isus, 0.000, isus, \
0.118, isus, 0.923, idec, 1.191, idec, 0.794, idec, 0.418, idec, \
0.172, idec, 0.053, idec, 0
kamp2 linseg 0, iatt, 0.009, iatt, 0.022, iatt, -0.049, iatt, \
-0.120, iatt, 0.297, iatt, 1.890, isus, 1.543, isus, 0.000, isus, \
0.546, isus, 0.690, idec, -0.318, idec, -0.326, idec, -0.116, idec, \
-0.035, idec, -0.020, idec, 0
kamp3 linseg 0, iatt, 0.005, iatt, -0.026, iatt, 0.023, iatt, \
0.133, iatt, 0.060, iatt, -1.245, isus, -0.760, isus, 1.000, isus, \
0.360, isus, -0.526, idec, 0.165, idec, 0.184, idec, 0.060, idec, \
0.010, idec, 0.013, idec, 0
iwt1 = gi_ZakianFlute_f26 ; wavetable numbers
iwt2 = gi_ZakianFlute_f27
iwt3 = gi_ZakianFlute_f28
inorm = 3949
goto end
range2: ; for low mid-range tones
kamp1 linseg 0, iatt, 0.000, iatt, -0.005, iatt, 0.000, iatt, \
0.030, iatt, 0.198, iatt, 0.664, isus, 1.451, isus, 1.782, isus, \
1.316, isus, 0.817, idec, 0.284, idec, 0.171, idec, 0.082, idec, \
0.037, idec, 0.012, idec, 0
kamp2 linseg 0, iatt, 0.000, iatt, 0.320, iatt, 0.882, iatt, \
1.863, iatt, 4.175, iatt, 4.355, isus, -5.329, isus, -8.303, isus, \
-1.480, isus, -0.472, idec, 1.819, idec, -0.135, idec, -0.082, idec, \
-0.170, idec, -0.065, idec, 0
kamp3 linseg 0, iatt, 1.000, iatt, 0.520, iatt, -0.303, iatt, \
0.059, iatt, -4.103, iatt, -6.784, isus, 7.006, isus, 11, isus, \
12.495, isus, -0.562, idec, -4.946, idec, -0.587, idec, 0.440, idec, \
0.174, idec, -0.027, idec, 0
iwt1 = gi_ZakianFlute_f29
iwt2 = gi_ZakianFlute_f30
iwt3 = gi_ZakianFlute_f31
inorm = 27668.2
goto end
range3: ; for high mid-range tones
kamp1 linseg 0, iatt, 0.005, iatt, 0.000, iatt, -0.082, iatt, \
0.36, iatt, 0.581, iatt, 0.416, isus, 1.073, isus, 0.000, isus, \
0.356, isus, .86, idec, 0.532, idec, 0.162, idec, 0.076, idec, 0.064, \
idec, 0.031, idec, 0
kamp2 linseg 0, iatt, -0.005, iatt, 0.000, iatt, 0.205, iatt, \
-0.284, iatt, -0.208, iatt, 0.326, isus, -0.401, isus, 1.540, isus, \
0.589, isus, -0.486, idec, -0.016, idec, 0.141, idec, 0.105, idec, \
-0.003, idec, -0.023, idec, 0
kamp3 linseg 0, iatt, 0.722, iatt, 1.500, iatt, 3.697, iatt, \
0.080, iatt, -2.327, iatt, -0.684, isus, -2.638, isus, 0.000, isus, \
1.347, isus, 0.485, idec, -0.419, idec, -.700, idec, -0.278, idec, \
0.167, idec, -0.059, idec, 0
iwt1 = gi_ZakianFlute_f32
iwt2 = gi_ZakianFlute_f33
iwt3 = gi_ZakianFlute_f34
inorm = 3775
goto end
range4: ; for high range tones
kamp1 linseg 0, iatt, 0.000, iatt, 0.000, iatt, 0.211, iatt, \
0.526, iatt, 0.989, iatt, 1.216, isus, 1.727, isus, 1.881, isus, \
1.462, isus, 1.28, idec, 0.75, idec, 0.34, idec, 0.154, idec, 0.122, \
idec, 0.028, idec, 0
kamp2 linseg 0, iatt, 0.500, iatt, 0.000, iatt, 0.181, iatt, \
0.859, iatt, -0.205, iatt, -0.430, isus, -0.725, isus, -0.544, isus, \
-0.436, isus, -0.109, idec, -0.03, idec, -0.022, idec, -0.046, idec, \
-0.071, idec, -0.019, idec, 0
kamp3 linseg 0, iatt, 0.000, iatt, 1.000, iatt, 0.426, iatt, \
0.222, iatt, 0.175, iatt, -0.153, isus, 0.355, isus, 0.175, isus, \
0.16, isus, -0.246, idec, -0.045, idec, -0.072, idec, 0.057, idec, \
-0.024, idec, 0.002, idec, 0
iwt1 = gi_ZakianFlute_f35
iwt2 = gi_ZakianFlute_f36
iwt3 = gi_ZakianFlute_f37
inorm = 4909.05
goto end
end:
kampr1 randi .02*kamp1, 10, gi_ZakianFLute_seed ; up to 2% wavetable amplitude variation
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
kamp1 = kamp1 + kampr1
kampr2 randi .02*kamp2, 10, gi_ZakianFLute_seed ; up to 2% wavetable amplitude variation
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
kamp2 = kamp2 + kampr2
kampr3 randi .02*kamp3, 10, gi_ZakianFLute_seed ; up to 2% wavetable amplitude variation
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
kamp3 = kamp3 + kampr3
awt1 poscil kamp1, kfreq, iwt1, iphase ; wavetable lookup
awt2 poscil kamp2, kfreq, iwt2, iphase
awt3 poscil kamp3, kfreq, iwt3, iphase
asig = awt1 + awt2 + awt3
asig = asig*(iampscale/inorm)
kcut linseg 0, iattack, ifiltcut, isustain, ifiltcut, idecay, 0 ; lowpass filter for brightness control
afilt tone asig, kcut
a_signal balance afilt, asig
i_attack = .002
i_sustain = i_duration
i_release = 0.01
i_declick_attack = i_attack
i_declick_release = i_declick_attack * 2
; The end of the note must be extended _past_ the end of the release segment.
xtratim 1
a_declicking_envelope cossegr 0, i_declick_attack, 1,  i_duration, 1,  i_declick_release, 0
; That envelope is then low-pass filtered to remove most discontinuities.
a_filtered_envelope tonex a_declicking_envelope, 40, 4
a_signal = a_signal * i_amplitude * a_filtered_envelope * k_gain 

prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
a_signal *= .7
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
endin

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
gk_Sweeper_britel init .01
gk_Sweeper_briteh init 5
gk_Sweeper_britels init .5
gk_Sweeper_britehs init .5
gk_Sweeper_level init 0
gk_Sweeper_space_left_to_right chnexport "gk_Sweeper_space_left_to_right", 3
gk_Sweeper_space_left_to_right init .5

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
k_space_front_to_back = p6
if p7 ==0 then
k_space_left_to_right = gk_Sweeper_space_left_to_right
else
k_space_left_to_right = p7
endif
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 34.2
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Sweeper_level)

iattack = i(gk_Sweeper_attack)
irelease = i(gk_Sweeper_release)
isustain = p3
kenvelope transegr 0.0, iattack / 2.0, 1.5, i_amplitude / 2.0, iattack / 2.0, -1.5, i_amplitude, isustain, 0.0, i_amplitude, irelease / 2.0, 1.5, i_amplitude / 2.0, irelease / 2.0, -1.5, 0
ihertz = i_frequency
icps = ihertz
kamp expseg 0.001,0.02,0.2,p3-0.01,0.001
ktonemoddep jspline 0.01,0.05,0.2
ktonemodrte jspline 6,0.1,0.2
ktone poscil3 ktonemoddep, ktonemodrte, gi_Sweeper_sine
; kres rspline krangeMin, krangeMax, kcpsMin, kcpsMax
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
a_signal = (aleft + aright)

; As with most software instruments that are modeled on an impulse exciting a 
; resonator, there should be two envelopes. The "physical" envelope must have a 
; fixed decay ending at zero.
i_declick_minimum = .003
i_attack = .001 / i_frequency + i_declick_minimum
i_declick_attack = i_attack
i_declick_release = i_declick_minimum * 2
; The end of the note must be extended _past_ the end of the release segment.
xtratim 1
a_declicking_envelope cossegr 0, i_declick_attack, 1,  i_duration, 1,  i_declick_release, 0
; The envelope of the instrument is the product of the physical envelope times 
; the declicking envelope. 
a_envelope = a_declicking_envelope
; That envelope is then low-pass filtered to remove most discontinuities.
a_filtered_envelope tonex a_envelope, 40, 4
a_signal = a_signal * i_amplitude * a_filtered_envelope * k_gain *.001

prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
outs a_out_left, a_out_right
endin

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

gk_FMWaterBell_space_left_to_right chnexport "gk_FMWaterBell_space_left_to_right", 3
gk_FMWaterBell_space_left_to_right init .5

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
if p7 ==0 then
k_space_left_to_right = gk_FMWaterBell_space_left_to_right
else
k_space_left_to_right = p7
endif
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 80
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization * 1.6
k_gain = ampdb(gk_FMWaterBell_level)
i_releasing_attack = 3 / min(i_frequency, 256)
i_releasing_release = .01
a_signal fmbell	1, i_frequency, gk_FMWaterBell_index, gk_FMWaterBell_crossfade, gk_FMWaterBell_vibrato_depth, gk_FMWaterBell_vibrato_rate, gi_FMWaterBell_cosine, gi_FMWaterBell_cosine, gi_FMWaterBell_cosine, gi_FMWaterBell_cosine, gi_FMWaterBell_cosine ;, gi_FMWaterBell_sustain
a_envelope transeg 0, gi_FMWaterBell_attack, 6,  1, gi_FMWaterBell_sustain, -6,  0
a_declicking cossegr 0, i_releasing_attack, 1, gi_FMWaterBell_sustain - 1, 1, i_releasing_release, 0
;;;a_signal = a_signal * i_amplitude * a_envelope * a_declicking * k_gain
a_signal = a_signal * i_amplitude * a_envelope * a_declicking * k_gain

a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

// This must be initialized in the orc header before any #includes.

// gi_Pianoteq vst3init "/home/mkg/Pianoteq\ 7/x86-64bit/Pianoteq\ 7.so", 0
// vst3info gi_Pianoteq 

gk_PianoOutPianoteq_level chnexport "gk_PianoOutPianoteq_level", 3 ;  0
gi_PianoOutPianoteq_print chnexport "gi_PianoOutPianoteq_print", 3 ;  1
gk_PianoOutPianoteq_front_to_back chnexport "gk_PianoOutPianoteq_front_to_back", 3 ;  0
gk_PianoOutPianoteq_left_to_right chnexport "gk_PianoOutPianoteq_left_to_right", 3 ;  0.5
gk_PianoOutPianoteq_bottom_to_top chnexport "gk_PianoOutPianoteq_bottom_to_top", 3 ;  0

gk_PianoOutPianoteq_level init 0
gk_PianoOutPianoteq_front_to_back init 0
gk_PianoOutPianoteq_left_to_right init 0.5
gk_PianoOutPianoteq_bottom_to_top init 0

instr PianoOutPianoteq
; i_result vst3progset gi_Pianoteq, 3
; Sustain off.
;vst3paramset gi_Pianoteq, 6, 1
; Reverb switch off.
vst3paramset gi_Pianoteq, 93, 0
k_gain = ampdb(gk_PianoOutPianoteq_level)
i_overall_amps = 87
i_normalization = ampdb(-i_overall_amps) * 2
i_amplitude = ampdb(80) * i_normalization
if gi_PianoOutPianoteq_print == 1 then
  vst3info gi_PianoOutPianoteq_print
endif
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
ainleft init 0
ainright init 0

aoutleft, aoutright vst3audio gi_Pianoteq
a_signal = aoutleft + aoutright
a_signal *= k_gain
a_signal *= i_amplitude
a_out_left, a_out_right pan2 a_signal, gk_PianoOutPianoteq_left_to_right
; printks "vst3audio:      %9.4f   %9.4f\n", 0.5, aoutleft, aoutright

kx = gk_PianoOutPianoteq_front_to_back
ky = gk_PianoOutPianoteq_left_to_right
kz = gk_PianoOutPianoteq_bottom_to_top

a_out_left, a_out_right pan2 a_signal, ky
outleta "outleft", a_out_left
outleta "outright", a_out_right
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_PianoOutPianoteq_front_to_back init -3
gk_PianoOutPianoteq_left_to_right init .4
gk_PianoOutPianoteq_bottom_to_top init 3

alwayson "PianoOutPianoteq"

#include "ReverbSC.inc"
alwayson "ReverbSC"

#include "MasterOutput.inc"
alwayson "MasterOutput"

gk_FMWaterBell_front_to_back init -3
gk_FMWaterBell_left_to_right init .6
gk_FMWaterBell_bottom_to_top init -3

//////////////////////////////////////////////////////////////////////////////
// Define the initial values of all global variables/control channels.
//////////////////////////////////////////////////////////////////////////////


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
gk_MasterOutput_level init 34.43789784288879
gk_Spatialize_SpeakerRigRadius init 0
gk_LocalReverbByDistance_ReverbDecay init 0
gk_LocalReverbByDistance_Wet init 0
gk_SpatialReverb_ReverbDecay init 0
gi_instrument_position_rate init 0
gk_BformatDecoder2_MasterLevel init 0
gk_ReverbSC_feedback init 0.71
gk_ReverbSC_wet init 0.5
gi_ReverbSC_delay_modulation init 0.0075
gk_ReverbSC_frequency_cutoff init 15000
gk_PianoOutPianoteq_level init 1.5
gi_FMWaterBell_attack init 0.002936276551436901
gi_FMWaterBell_release init 0.022698875468554768
gi_FMWaterBell_exponent init 0
gi_FMWaterBell_sustain init 5.385256143273636
gi_FMWaterBell_sustain_level init 0.08267388588088297
gk_FMWaterBell_crossfade init 0.1234039047697504
gk_FMWaterBell_index init 1.1401499375260309
gk_FMWaterBell_vibrato_depth init 0.28503171595683335
gk_FMWaterBell_vibrato_rate init 2.4993821566850647
gk_FMWaterBell_level init -9
gk_Phaser_ratio1 init 0.5
gk_Phaser_ratio2 init 2
gk_Phaser_index1 init 0.46410256573457687
gk_Phaser_index2 init 0.8551589334803189
gk_Phaser_level init -30
gk_Plucked_level init 16
gk_SeidelHarmOsc_level init -10
gi_SeidelHarmOsc_attack init 0.003
gi_SeidelHarmOsc_petals init 2.333333
gi_SeidelHarmOsc_release init 0.21521517008645225
gk_STKBowed_vibrato_level init 0
gk_STKBowed_bow_pressure init 106.85582357509631
gk_STKBowed_bow_position init 21.81769218869982
gk_STKBowed_vibrato_frequency init 42.6065254256644
gk_STKBowed_level init 12
gk_Droner_partial1 init 0.4664927441708788
gk_Droner_partial2 init 0.16386760150008153
gk_Droner_partial3 init 0.13777922713190935
gk_Droner_partial4 init 0.4664927441708788
gk_Droner_partial5 init 0.15343225175281267
gk_Droner_level init 36
gk_Sweeper_britel init 0.01
gk_Sweeper_briteh init 2.5514444322021332
gk_Sweeper_britels init 0.08
gk_Sweeper_britehs init 0.03
gk_Sweeper_level init -26
gk_Buzzer_harmonics init 4
gk_Buzzer_level init 12
gk_Shiner_level init 15
gk_Blower_grainDensity init 132.3332789825534
gk_Blower_grainDuration init 0.2854231208217838
gk_Blower_grainAmplitudeRange init 174.0746779716289
gk_Blower_grainFrequencyRange init 62.82406652535464
gk_Blower_level init 4
gk_ZakianFlute_level init -24
gk_BandedWG_level init 9.05200375059259
gk_FilteredSines_level init 40
gk_Harpsichord_level init -12
gk_Xing_level init 52
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

gS_html init {{<!DOCTYPE html>
<html>
<head>
    <title>snow-voice-44</title>
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <!--
//////////////////////////////////////////////////////////////////////////////
// Override the CSS style of the numerical values shown for dat-gui sliders.
//////////////////////////////////////////////////////////////////////////////    
    -->
    <style type="text/css">
    input[type='text']{
        font-size: 9pt;
        height: 100%;
        width: 100%;
        vertical-align: middle;
    }
    input[type='range'] {
        font-size: 9pt;
        -webkit-appearance: none;
        box-shadow: inset 0 0 3px #333;
        background-color: gray;
        height: 10px;
        width: 100%;
        vertical-align: middle;
    }
    input[type=range]::-webkit-slider-thumb {
        -webkit-appearance: none;
        border: none;
        height: 12px;
        width: 12px;
        border-radius: 50%;
        box-shadow: inset 0 0 5px #234;
        background: chartreuse;
        margin-top: -4px;
        border-radius: 10px;
    }
    textarea {
        min-width: 100%;
        max-width: 100%;
        min-height: 100%;
        max-height: 100%;
        -webkit-box-sizing: border-box; /* Safari/Chrome, other WebKit */
        -moz-box-sizing: border-box;    /* Firefox, other Gecko */
        box-sizing: border-box;         /* Opera/IE 8+ */
    }
    </style>
    <!--
    //////////////////////////////////////////////////////////////////////////
    // All dependencies that are in some sense standard and widely used, are 
    // loaded from content delivery networks.
    //////////////////////////////////////////////////////////////////////////  
    -->
    <script src="https://code.jquery.com/jquery-3.6.0.js" integrity="sha256-H+K7U5CnXl1h5ywQfKtSj8PCmoN9aaq30gDh27Xc0jk=" crossorigin="anonymous"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/dat-gui/0.7.7/dat.gui.js"></script>    
    <script src="https://cdnjs.cloudflare.com/ajax/libs/three.js/r128/three.js" integrity="sha512-NLtnLBS9Q2w7GKK9rKxdtgL7rA7CAS85uC/0xd9im4J/yOL4F9ZVlv634NAM7run8hz3wI2GabaA6vv8vJtHiQ==" crossorigin="anonymous" referrerpolicy="no-referrer"></script>    
    <!--
    //////////////////////////////////////////////////////////////////////////
    // All other dependencies are incorporated into this repository, and are 
    // loaded as local files.
    //////////////////////////////////////////////////////////////////////////  
    -->
    <script src="silencio/js/TrackballControls.js"></script>
    <script src="PianoRoll3D.js"></script>    
    <script src="csound_jsonrpc_stub.js"></script>
</head>
<body style="background-color:black;box-sizing:border-box;padding:10px;:fullscreen">
    <div>
        <canvas id = 'piano_roll' class='canvas' style="width:98vw;height:98vh;"/>
        </canvas>
        <textarea id="csound_diagnostics" style="position:absolute;top:1vh;left:1vw;color:#87CEEBC0;background-color:transparent;border:none;text-align:left;overflow:auto;padding:0;margin:0;border-collapse:collapse;font-family:Courier, sans-serif;font-size:7pt;">
        </textarea>
    </div>
    <script>
        //////////////////////////////////////////////////////////////////////
        // This is the JSON-RPC proxy of the instance of Csound that is 
        // performing this piece.
        //////////////////////////////////////////////////////////////////////
        csound = new Csound(origin);
        let message_callback_ = function(message) {
            let notifications_textarea = document.getElementById("csound_diagnostics");
            let existing_notifications = notifications_textarea.value;
            notifications_textarea.value = existing_notifications + message;
            notifications_textarea.scrollTop = notifications_textarea.scrollHeight;
        }; 
        csound.SetMessageCallback(message_callback_, true);
        //////////////////////////////////////////////////////////////////////
        // This hooks up JavaScript code for displaying a 3-dimensional piano 
        // roll display of the algorithmically generated score.
        //////////////////////////////////////////////////////////////////////
        var canvas = document.getElementById("piano_roll");
        var piano_roll = new PianoRoll.PianoRoll3D(canvas);
        let score_time = 0;
        let video_frame = 0;
        csound.SetEventSourceCallback("score_display", function(data) {
            console.log("score_display callback...");
            console.log(typeof data);
            piano_roll.fromJson(data);        
        });
        //////////////////////////////////////////////////////////////////////
        // The piano roll display is animated (a) to show a red ball that 
        // follows the score, and (b) to enable the use of the mouse or 
        // trackball to rotate, and zoom around in, the piano roll display.
        //////////////////////////////////////////////////////////////////////
        async function animate() {
            //////////////////////////////////////////////////////////////////
            // By rendering the visual display only on every nth video frame, 
            // more time is given to Csound's audio rendering.
            //////////////////////////////////////////////////////////////////
            if (video_frame % 60 == 0) {
                score_time = await csound.GetScoreTime(function(id, result) {
                    score_time = result; 
                });
                if (typeof piano_roll !== "undefined") {
                    if (typeof piano_roll.controls !== "undefined") {
                        piano_roll.controls.update()
                        piano_roll.render3D()
                        piano_roll.progress3D(score_time);
                    }
                }
            }
            video_frame = video_frame + 1;
            requestAnimationFrame(animate)
        }
        
        /**
         * Placeholder or stub, later replaced with actual function.
         */
        var save_controls = function() {
        }

        var recenter = function() {
            piano_roll.draw3D(canvas);
        }
        
        var toggle_messages = function() {
            if (csound_diagnostics.style.display === "none") {
                csound_diagnostics.style.display = "block";
            } else {
                csound_diagnostics.style.display = "none";
            }
        }
        
        //////////////////////////////////////////////////////////////////////
        // These are the controllers and commands that are initialized in the 
        // dat.gui controls. The controllers must have the same names and 
        // types as the Csound orchestra's control channels/global variables, 
        // but the initial values actually come from the CSound channels.
        //////////////////////////////////////////////////////////////////////
        var parameters = {
            gk_MasterOutput_level: 23.199086906897115,
            gk_ReverbSC_feedback:  0.72,
            gk_ReverbSC_wet:  0.5,
            gi_ReverbSC_delay_modulation:  0.0075,
            gk_ReverbSC_frequency_cutoff:  15000,
            gk_PianoOutPianoteq_level:  10.035980495554469,
            gi_FMWaterBell_attack:  0.002936276551436901,
            gi_FMWaterBell_release:  0.022698875468554768,
            gi_FMWaterBell_exponent:  0,
            gi_FMWaterBell_sustain:  5.385256143273636,
            gi_FMWaterBell_sustain_level:  0.08267388588088297,
            gk_FMWaterBell_crossfade:  0.1234039047697504,
            gk_FMWaterBell_index:  1.1401499375260309,
            gk_FMWaterBell_vibrato_depth:  0.28503171595683335,
            gk_FMWaterBell_vibrato_rate:  2.4993821566850647,
            gk_FMWaterBell_level:  -14.991627040173022,
            gk_Plucked_level:  2,
            gk_SeidelHarmOsc_level:  0.6506276696566573,
            gi_SeidelHarmOsc_attack: .003,
            gi_SeidelHarmOsc_petals: 2.99,
            gi_SeidelHarmOsc_release: .01,
            gk_Sweeper_briteh: 5,
            gk_Sweeper_britel: .5,
            gk_Sweeper_britels: .5,
            gk_Sweeper_britehs: .5,
            gk_Sweeper_level: 0,
            gk_ZakianFlute_level:  -19.997148547318524,
            gk_Harpsichord_level:  9,
            save_controls: save_controls,
            toggle_messages: toggle_messages,
            recenter: recenter
        };
        
        var number_format = new Intl.NumberFormat('en-US', {minimumFractionDigits: 3, maximumFractionDigits: 3 });

        //////////////////////////////////////////////////////////////////////
        // The use of jQuery _greatly_ simplifies using JavaScript event 
        // handlers for HTML elements.
        //
        // jQuery can only set itself up to handle events for elements when 
        // the page has been loaded.
        //
        // We do NOT use dat.gui's persistence mechanism based on HTML5 local 
        // storage.
        //////////////////////////////////////////////////////////////////////
        window.onload = async function() {
            gui = new dat.GUI({width: 500});
            gui.remember(parameters);
            gui.add(parameters, 'save_controls').name('Save controls as Csound values [Ctrl-S]');
            gui.add(parameters, 'toggle_messages').name('Toggle Csound diagnostics');
            gui.add(parameters, 'recenter').name('Re-center piano roll [Ctrl-C]');
            
            var Pianoteq = gui.addFolder('01 Pianoteq');
            add_slider(Pianoteq, 'gk_PianoOutPianoteq_level', -60, 60.);
            
            var Harpsichord = gui.addFolder('02 Harpsichord');
            add_slider(Harpsichord, 'gk_Harpsichord_level', -60, 60.);
            
            var Plucked = gui.addFolder('03 Plucked');
            add_slider(Plucked, 'gk_Plucked_level', -60, 60.);
            
            var SeidelHarmOsc = gui.addFolder('05 SeidelHarmOsc');
            add_slider(SeidelHarmOsc, 'gi_SeidelHarmOsc_attack', 0., 1.);
            add_slider(SeidelHarmOsc, 'gi_SeidelHarmOsc_petals', 1., 10.);
            add_slider(SeidelHarmOsc, 'gi_SeidelHarmOsc_release', .005, 1.);
            add_slider(SeidelHarmOsc, 'gk_SeidelHarmOsc_level', -60, 60.);
 
            var Flute = gui.addFolder('05 Zakian Flute');
            add_slider(Flute, 'gk_ZakianFlute_level', -60, 60.);

            var Sweeper = gui.addFolder('06 Sweeper');
            add_slider(Sweeper, 'gk_Sweeper_britel', 0, 4);
            add_slider(Sweeper, 'gk_Sweeper_briteh', 0, 4);
            add_slider(Sweeper, 'gk_Sweeper_britels', 0, 4);
            add_slider(Sweeper, 'gk_Sweeper_britehs', 0, 4);
            add_slider(Sweeper, 'gk_Sweeper_level', -60, 60.);

            var FMWaterBell = gui.addFolder('07 FMWaterBell');
            add_slider(FMWaterBell, 'gi_FMWaterBell_attack', 0, .1);
            add_slider(FMWaterBell, 'gi_FMWaterBell_release', 0, .1);
            add_slider(FMWaterBell, 'gi_FMWaterBell_exponent', -30, 30);
            add_slider(FMWaterBell, 'gi_FMWaterBell_sustain', 0, 20);
            add_slider(FMWaterBell, 'gi_FMWaterBell_sustain_level', 0, 1.);
            add_slider(FMWaterBell, 'gk_FMWaterBell_crossfade', 0, 1.);
            add_slider(FMWaterBell, 'gk_FMWaterBell_index', 0, 15.);
            add_slider(FMWaterBell, 'gk_FMWaterBell_vibrato_depth', 0, 10.);
            add_slider(FMWaterBell, 'gk_FMWaterBell_vibrato_rate', 0, 10.);
            add_slider(FMWaterBell, 'gk_FMWaterBell_level',-60, 60);

            var Master = gui.addFolder('Master');
            add_slider(Master, 'gk_ReverbSC_feedback', 0, 1.);
            add_slider(Master, 'gk_ReverbSC_wet', 0, 1.);
            add_slider(Master, 'gi_ReverbSC_delay_modulation', 0, 4.);
            add_slider(Master, 'gk_ReverbSC_frequency_cutoff', 0., 20000.);
            add_slider(Master, 'gk_MasterOutput_level', -60, 60.);
            
            $('input').on('input', function(event) {
                var slider_value = parseFloat(event.target.value);
                csound.SetControlChannel(event.target.id, slider_value, function(id, result){}, function(id,message){});
                var output_selector = '#' + event.target.id + '_output';
                var formatted = number_format.format(slider_value);
                $(output_selector).val(formatted);
            });
            //////////////////////////////////////////////////////////////////////
            // Initializes the values of HTML controls with the values of the 
            // Csound control channels/variables with the same names.
            //////////////////////////////////////////////////////////////////////
            console.log("Updating widgets with Csound control values...");
            for (const [key, value] of Object.entries(parameters)) {
                if (typeof value !== 'function') {
                    let value_ = await csound.GetControlChannel(key);
                    parameters[key] = value_;
                    let initialization_message = `Initialized gui: ${key} = ${value_}\n`;
                    console.log(initialization_message);
                    csound.Message(initialization_message);
                    let input_selector = '#' + key;
                    let formatted = number_format.format(value_);
                    $(input_selector).val(formatted);
                    let output_selector = '#' + key + '_output';
                    $(output_selector).val(formatted);
                }
            };
            var saved_gui_parameters = gui.getSaveObject();
            console.log(saved_gui_parameters);
            gui.remember(saved_gui_parameters);
            console.log("Updated widgets with Csound control values.");
            //////////////////////////////////////////////////////////////////////
            // When the user clicks on the "Save control values" command, the 
            // current state of the control parameters is printed to the terminal
            // in the form of Csound orchestra code. This can be copied from the 
            // terminal, and pasted over the existing initial control channel 
            // values in the Csound orchestra. Currently, Web browsers do not 
            // permit writing to the user's filesystem except in the Downloads 
            // directory.
            //////////////////////////////////////////////////////////////////////
            parameters.save_controls = function() {
                console.log("Saving control values...");
                let delimiter = ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\\n";
                let text = delimiter;
                for (const [key, value] of Object.entries(parameters)) {
                    if (typeof value !== 'function') {
                        console.log(`parameter: ${key} = ${value}`);
                        let line = `${key} init ${value}\n`;
                        text = text + line;
                    }
                };
                text = text + delimiter;
                navigator.clipboard.writeText(text);
                console.log("Saved control values:\\n" + text);
            };
            //////////////////////////////////////////////////////////////////
            // The 'Ctrl-H' key hides and unhides everything on the Web page 
            // except for the piano roll score display.
            //////////////////////////////////////////////////////////////////    
            document.addEventListener("keydown", function (e) {
                console.log(e);
                let e_char = String.fromCharCode(e.keyCode || e.charCode);
                if (e.ctrlKey === true) {
                    if (e_char === 'H') {
                        gui.closed = true;
                        gui.closed = false;
                        toggle_messages();
                    } else if (e_char === 'S') {
                        parameters.save_controls();
                    }
                }
            });
            requestAnimationFrame(animate)
        };
        
        var gk_update = function(name, value) {
            var numberValue = parseFloat(value);
            csound.SetControlChannel(name, numberValue, function(id, value) {}, function(id, value) {});
        }

        var add_slider = function(gui_folder, token, minimum, maximum, name) {
            var on_parameter_change = function(value) {
                gk_update(token, value);
            };
            gui_folder.add(parameters, token, minimum, maximum).onChange(on_parameter_change).listen();
        };
        
        window.addEventListener("unload", function(event) { 
            parameters.save_controls();
        });
    
</script>
</body>
</html>
}}

if strcmp(gS_os, "Linux") == 0 then
gi_webserver webserver_create "/home/mkg/michael.gogins.studio/music/NYCEMF-2023/", 8080, 0
endif
if strcmp(gS_os, "macOS") == 0 then
gi_webserver webserver_create "/Users/michaelgogins/michael.gogins.studio/music/NYCEMF-2023/", 8080, 0
endif
// The following event source has to be created before we actually send it a 
// score to display.
webserver_send gi_webserver, "score_display", ""
webserver_open_html gi_webserver, gS_html

//////////////////////////////////////////////////////////////////////////////
// The following C++ code defines and executes a score generator that 
// implements the "multiple copy reducing machine" algorithm for computing a 
// recurrent iterated function system (RIFS).
//
// Only a limited number of iterations are computed. On the final iteration, 
// each point in the attractor of the RIFS is translated to a single note of 
// music.
//
// This code uses my CsoundAC library, the Eigen 3 header file-only library, 
// and the C++ standard library.
//////////////////////////////////////////////////////////////////////////////

S_score_generator_code init {{

#include <Eigen/Dense>
#include <csignal>
#include <csound.h>
#include <csdl.h>
#include <dlfcn.h>
#include <iostream>
#include <cstdio>
#include <random>
#include <vector>
#include <Composition.hpp>
#include <functional>
#include <memory>
#include <MusicModel.hpp>
#include <random>
#include <ScoreNode.hpp>
#include <VoiceleadingNode.hpp>

//////////////////////////////////////////////////////////////////////////////
// This symbol is used by a number of C++ libraries, but is not defined in the
// startup code. Therefore, we may need to define it here.
//////////////////////////////////////////////////////////////////////////////

/// void* __dso_handle = (void *)&__dso_handle;

static bool diagnostics_enabled = false;

extern "C" { 

struct Cursor
{
    csound::Event note;
    csound::Chord chord;
    csound::Scale scale;
};

auto generator = [] (const Cursor &cursor, int depth, int target_depth, csound::Score &score)
{
    Cursor result = cursor;
    return result;
};

//////////////////////////////////////////////////////////////////////////////
// Computes a deterministic, finite recurrent iterated function system by
// recursively applying a set of generators (transformations) to a pen
// that represents the position of a "pen" on a "score." The entries in
// the transitions matrix represent open or closed paths of recurrence through
// the tree of calls. Because the pen is passed by value, it is in effect
// copied at each call of a generator and at each layer of recursion.
//////////////////////////////////////////////////////////////////////////////
void recurrent(std::vector< std::function<Cursor(const Cursor &, int, int, csound::Score &)> > &generators,
        Eigen::MatrixXd &transitions,
        int depth,
        int target_depth,
        int transformationIndex,
        const Cursor pen,
        csound::Score &score) {
    depth = depth + 1;
    if (depth == target_depth) {
        score.append(pen.note);    
        return;
    }
    for (int transitionIndex = 0, transitionN = transitions.rows(); transitionIndex < transitionN; ++transitionIndex) {
        if (transitions(transformationIndex, transitionIndex)) {
            auto newCursor = generators[transitionIndex](pen, depth, target_depth, score);
            recurrent(generators, transitions, depth, target_depth, transitionIndex, newCursor, score);
        }
    }
}

extern "C" {
    void (*webserver_send_message_)(CSOUND *csound, int webserver_handle, const char *channel_name, const char *message);
};

//////////////////////////////////////////////////////////////////////////////
// This is the entry point for this C++ module. It will be called by Csound 
// immediately once the module has been compiled and linked.
//////////////////////////////////////////////////////////////////////////////

// Try scales/chords not chord transformations.
// Try FM of time and pitch scales.
// The last quarter of the piece gets just one chord change, this will not do.

extern "C" int score_generator(CSOUND *csound) {
    // Turn off Csound-installed signal handlers.
    std::signal(SIGTERM, SIG_DFL);
    std::signal(SIGABRT, SIG_DFL);
    std::signal(SIGSEGV, SIG_DFL);

    // Load: extern "C" void webserver_send_message(CSOUND *csound, int webserver_handle, const char *channel_name, const char *message);
    // Library handle 0 means: search all symbols in the process.
    // Note: for this to work, libcsound_webserver.so (or the equivalent 
    // filename on other platforms) must be in the link library list for THIS 
    // code in order to be loaded, linked, and resolved.
    #ifdef __MACH__
    auto library_handle = dlopen("/Users/michaelgogins/csound-webserver-opcodes/build/libcsound_webserver.dylib", RTLD_NOW | RTLD_GLOBAL);
    #endif
    #ifdef __linux__
    auto library_handle = dlopen("/home/mkg/csound-webserver-opcodes/build/libcsound_webserver.so", RTLD_NOW | RTLD_GLOBAL);
    #endif
    webserver_send_message_ = (void (*)(CSOUND *, int, const char *, const char *)) csound->GetLibrarySymbol(library_handle, "webserver_send_message");
    csound->Message(csound, "webserver_send_message_: %p\\n", webserver_send_message_);
#if (defined(__linux__) || defined(__MACH__))
    if (webserver_send_message_ == 0) {
        csound->Message(csound, "dlerror: %s\\n", dlerror());
    }
#endif
    int result = OK;
    std::mt19937 mersenneTwister;
    std::uniform_real_distribution<> randomvariable(.05,.95);
    
    // Search for best velocity scaling:
    
    auto velocity_scaling = 1;
    // Better than 1.02: velocity_scaling = 1.06;
    // Not as good as 1.02: velocity_scaling = 1.08;
    /// original: velocity_scaling = 1.02;
    velocity_scaling = 1.04;
    // Not good: velocity_scaling = 0.96;
    
    // Search for best dynamic range:
    
    auto dynamic_range = 20; // Original.
    dynamic_range = 10; // Not much different from 20?? but not as good I think.
    dynamic_range = 30; // Better than 20.
    dynamic_range = 60; // Better thab 30.
    
    // Search for best duration fraction:
    
    auto duration_fraction = 150.; // Original.
    duration_fraction = 75.; // Slower, sparser than 150; harmony not as good.
    duration_fraction = 37.; // Not as good as 75.
    duration_fraction = 112.; // Not as good as 150.
    duration_fraction = 300.; // More interesting than original, but also too busy.
    duration_fraction = 200.; // ?
    duration_fraction = 175.; // Reveals more of the other voices; harmony muddier.
    duration_fraction = 137.;
    
    csound::ScoreModel model;
    std::map<double, csound::Chord> chordsForTimes;
    csound::Chord modality;
    Cursor pen;
    pen.scale = csound::Scale("F major");
    std::cout << "pen.scale: " << pen.scale.name() << std::endl;
    pen.chord = pen.scale.chord(1, 4);
    std::cout << "pen.chord: " << pen.chord.eOP().name() << std::endl;
    modality = pen.chord;
    pen.note = csound::Event{1,40,144,1,1,1,0,0,0,0,1};
    std::vector<std::function<Cursor(const Cursor &, int, int, csound::Score &)>> generators;
    generators.push_back([&chordsForTimes, &modality](const Cursor &pen_, int depth, int target_depth, csound::Score &score) {
        Cursor pen = pen_;
        ///if (depth == 5) {
        if (depth == 5) {
            pen.chord = pen.scale.transpose_degrees(pen.chord, 2);
            chordsForTimes[pen.note.getTime()] = pen.chord;
        }
        pen.note[csound::Event::TIME] = (pen.note[csound::Event::TIME]                * 1./4.)      + (0.)      * 100.;
        pen.note[csound::Event::KEY] =  (pen.note[csound::Event::KEY]                 * 1./2.09)    + (.5)      * 100.;
        
        pen.note[csound::Event::VELOCITY] =  (pen.note[csound::Event::VELOCITY]       * (1));
        pen.note[csound::Event::DURATION] =  (pen.note[csound::Event::DURATION]       * .97);
        return pen;
    });
    generators.push_back([&chordsForTimes, &modality](const Cursor &pen_, int depth, int target_depth, csound::Score &score) {
        Cursor pen = pen_;
        pen.note[csound::Event::TIME] = (pen.note[csound::Event::TIME]                * 1./4.)      + (1./4.)   * 100.;
        pen.note[csound::Event::KEY] =  (pen.note[csound::Event::KEY]                 * 1./2.06)    + (.5)      * 100.;
        return pen;
    });
    generators.push_back([&chordsForTimes, &modality](const Cursor &pen_, int depth, int target_depth, csound::Score &score) {
        Cursor pen = pen_;
        ///if (depth == 2) {
        if (depth == 2) {
            pen.chord = pen.scale.transpose_degrees(pen.chord, 5);
            chordsForTimes[pen.note.getTime()] = pen.chord;
        }
        pen.note[csound::Event::TIME] = (pen.note[csound::Event::TIME]                * 1./5.)      + (2./4.)   * 100.;
        pen.note[csound::Event::KEY] =  (pen.note[csound::Event::KEY]                 * 1./2.03)    + (.5)      * 110.;
        return pen;
    });
    generators.push_back([&chordsForTimes, &modality](const Cursor &pen_, int depth, int target_depth, csound::Score &score) {
        Cursor pen = pen_;
        ///if (depth == 2) {
        if (depth == 2) {
            pen.chord = pen.scale.transpose_degrees(pen.chord, -2);
            chordsForTimes[pen.note.getTime()] = pen.chord;
        }
        pen.note[csound::Event::TIME] =         (pen.note[csound::Event::TIME]        * 1./4.)    + ( 3./4.) * 100.;
        pen.note[csound::Event::KEY] =          (pen.note[csound::Event::KEY]         * 1./2.)    + ( .5)    * 118.;
        
        pen.note[csound::Event::INSTRUMENT] =   (pen.note[csound::Event::INSTRUMENT]  * 0.5)      + (90.);
        return pen;
    });
    
    generators.push_back([&chordsForTimes, &modality](const Cursor &pen_, int depth, int target_depth, csound::Score &score) {
        Cursor pen = pen_;
        if (depth == 4) {
            pen.chord = pen.scale.transpose_degrees(pen.chord, 3);
            chordsForTimes[pen.note.getTime()] = pen.chord;
        }
        if (depth == 1) {
            pen.chord = pen.scale.transpose_degrees(pen.chord, -2);
            chordsForTimes[pen.note.getTime()] = pen.chord;
        }
        pen.note[csound::Event::TIME] =         (pen.note[csound::Event::TIME]        * 1./3)       + (0.)     * 100.;
        pen.note[csound::Event::KEY] =          (pen.note[csound::Event::KEY]         * 1./2.09)    + (-.5)    * 100.;
        return pen;
    });
    generators.push_back([&chordsForTimes, &modality, &velocity_scaling](const Cursor &pen_, int depth, int target_depth, csound::Score &score) {
        Cursor pen = pen_;
        if (depth == 2) {
          auto modulations = pen.scale.modulations(pen.chord);
          auto modulations_count = modulations.size();
          auto random_index = std::floor(std::rand() % modulations_count);
          pen.scale = modulations[random_index];
          std::cout << "new scale (" << random_index << " of " << modulations_count << "): " << pen.scale.name() << std::endl;
          //  pen.chord = pen.scale.transpose_degrees(pen.chord, -2);
          chordsForTimes[pen.note.getTime()] = pen.chord;
        }
        pen.note[csound::Event::TIME] =         (pen.note[csound::Event::TIME]        * 1./3.)      + (1./3.) * 101.;
        pen.note[csound::Event::KEY] =          (pen.note[csound::Event::KEY]         * 1./2.06)    + (-.5 * 100.);
        
        pen.note[csound::Event::DURATION] =     (pen.note[csound::Event::DURATION]    * 1.02);
        pen.note[csound::Event::INSTRUMENT] =   (pen.note[csound::Event::INSTRUMENT]  * 0.75)     + (-3.);
        pen.note[csound::Event::VELOCITY] =     (pen.note[csound::Event::VELOCITY]    * velocity_scaling);
        return pen;
    });
    generators.push_back([&chordsForTimes, &modality](const Cursor &pen_, int depth, int target_depth, csound::Score &score) {
        Cursor pen = pen_;
        if (depth == 2) {
          auto modulations = pen.scale.modulations(pen.chord);
          auto modulations_count = modulations.size();
          auto random_index = std::floor(std::rand() % modulations_count);
          pen.scale = modulations[random_index];
          std::cout << "new scale: " << pen.scale.name() << std::endl;
          pen.chord = pen.scale.transpose_degrees(pen.chord, -2);
          chordsForTimes[pen.note.getTime()] = pen.chord;
        }
        pen.note[csound::Event::TIME] = (pen.note[csound::Event::TIME]                * 1./3.)    + (2./3.) * 102.;
        pen.note[csound::Event::KEY] =  (pen.note[csound::Event::KEY]                 * 1./2.01)    + ((-.7) * 100);
        
        pen.note[csound::Event::INSTRUMENT] =  (pen.note[csound::Event::INSTRUMENT]   * 0.75)     + (-3.);
        return pen;
     });
    // Generate the transition matrix.
    Eigen::MatrixXd transitions = Eigen::MatrixXd::Ones(generators.size(), generators.size());
    // Zero some paths of recurrence.
    transitions(2, 0) = 0;
    transitions(4, 0) = 0;
    transitions(3, 6) = 0;
    transitions(5, 6) = 0;
    std::cout << "transitions:" << std::endl << transitions << std::endl;
    csound::Score score;
    //////////////////////////////////////////////////////////////////////////////
    // Before iterating, ensure that the score does start and end with a chord.
    //////////////////////////////////////////////////////////////////////////////
    chordsForTimes[ 1000.] = pen.chord;
    chordsForTimes[-1000.] = pen.chord;
    recurrent(generators, transitions, 0, 7, 0, pen, score);
    std::cout << "Generated duration:     " << score.getDuration() << std::endl;
    //////////////////////////////////////////////////////////////////////////////
    // We apply the chords that were generated along WITH the notes, TO the notes.
    // This creates algorithmically generated chord progressions and modulations.
    //////////////////////////////////////////////////////////////////////////////
    score.sort();
    score.rescale(csound::Event::KEY, true, 27.0, true,  72.0);
    score.temper(12.);
    std::cout << "Generated notes:        " << score.size() << std::endl;
    double endTime = score.back().getTime();
    std::cout << "Chord segments:         " << chordsForTimes.size() << std::endl;
    int size = 0;
    int segment_count = 0;
    for (auto it = chordsForTimes.rbegin(); it != chordsForTimes.rend(); ++it, ++segment_count) {
        auto startTime = it->first;
        auto &chord = it->second;
        auto segment = csound::slice(score, startTime, endTime);
        size += segment.size();
        if (segment.size() > 0) {
            std::fprintf(stderr, "From %9.4f to %9.4f apply %s to %d notes.\\n", startTime, endTime, chord.eOP().name().c_str(), segment.size());
        }
        csound::apply(score, chord, startTime, endTime, true);
        endTime = startTime;
    }
    std::cout << "Conformed notes:        " << size << std::endl;
    score.rescale(csound::Event::INSTRUMENT,    true,  1.0, true,   6.99);
    score.rescale(csound::Event::VELOCITY,      true, 40.0, true,   dynamic_range);
    score.rescale(csound::Event::PAN,           true,  0.0, true,   0.0);
    std::cout << "Move to origin duration:" << score.getDuration() << std::endl;
    score.findScale();
    score.setDuration(610.);
    // Re-assign pans.
    for (int i = 0, n = score.size(); i < n; ++i) {
        auto insno = std::floor(score[i].getInstrument());
        auto pan = .5;
        if        (insno == 1) {
            pan == .5;
        } else if (insno == 2) {
            pan = 2./8.;
        } else if (insno == 3) {
            pan = 5./8.;
        } else if (insno == 4) {
            pan = 1./8.;
        } else if (insno == 7) {
            pan = 7./8.;
        } else if (insno == 6) {
            pan = 6./8.;
        } else if (insno == 5) {
            pan = 3./8.;
        };
        score[i].setPan((randomvariable(mersenneTwister) * .01) + pan);
        score[i].setDepth(randomvariable(mersenneTwister));
        score[i].setPhase(randomvariable(mersenneTwister));
        auto duration = score[i].getDuration() / duration_fraction;
        score[i].setDuration(duration);
    }
    score.tieOverlappingNotes(true);
    score.rescale(csound::Event::TIME,          true,  2.0, false,  0.0);
    
    // Move the last piano note to sound like an ending.
    auto score_size = score.size();
    auto index = score_size - 1;
    auto key = score[index].getKey() + 1;
    score[index].setKey(key);
    index = score_size - 6;
    key = score[index].getKey() + 1;
    score[index].setKey(key);
    
    std::cout << "score:" << std::endl << score.getCsoundScore() << std::endl;
    std::cout << "Final duration:         " << score.getDuration() << std::endl;
    //////////////////////////////////////////////////////////////////////////
    // Using the EVTBLK struct for each note is more efficient than using a 
    // string for each note, or for the entire score.
    //////////////////////////////////////////////////////////////////////////
    EVTBLK evtblk;
    // Multiply duration by some function of time so later notes get longer?
    std::memset(&evtblk, 0, sizeof(EVTBLK));
    for (const auto &note : score) {
        evtblk.strarg = nullptr;
        evtblk.scnt = 0;
        evtblk.opcod = 'i';
        evtblk.pcnt = 9;
        evtblk.p[1] = std::floor(note.getInstrument());
        evtblk.p[2] = note.getTime();
        evtblk.p[3] = note.getDuration();
        evtblk.p[4] = note.getKey();
        evtblk.p[5] = note.getVelocity();
        evtblk.p[6] = note.getDepth();
        evtblk.p[7] = note.getPan();
        evtblk.p[8] = note.getHeight();
        int result = csound->insert_score_event(csound, &evtblk, 0.);
    }
    //////////////////////////////////////////////////////////////////////////
    // This translates the just-generated CsoundAC Score to a textual JSON 
    // form.
    //////////////////////////////////////////////////////////////////////////
    auto json_score = score.toJson();
    //////////////////////////////////////////////////////////////////////////
    // The Web page has already defined a canvas with a PianoRoll3D attached.
    // Here, the PianoRoll3D instance is called to send our JSON score to 
    // the Web page, which will render it as an animated, three-dimensional 
    // piano roll.
    //////////////////////////////////////////////////////////////////////////
    webserver_send_message_(csound, 0, "score_display", json_score.c_str());
    return result;
};

};

}}

//////////////////////////////////////////////////////////////////////////////
// This compiles the above C++ module and then calls its entry point function.
//////////////////////////////////////////////////////////////////////////////

if strcmp(gS_os, "Linux") == 0 then
i_result cxx_compile "score_generator", S_score_generator_code, "g++ -v -g -O2 -std=c++17 -shared -fPIC -DUSE_DOUBLE -I. -I/usr/local/include/csound -I/home/mkg/csound/interfaces -I/usr/include/eigen3 -I/home/mkg/csound-ac/CsoundAC -lCsoundAC -lpthread -lm", "libcsound_webserver.so libCsoundAC.so"
endif
if strcmp(gS_os, "macOS") == 0 then
i_result cxx_compile "score_generator", S_score_generator_code, "g++ -v -g -O2 -std=c++17 -shared -fPIC -DUSE_DOUBLE -I. -I/usr/local/include/csound -I/Users/michaelgogins/csound/interfaces -I/usr/include/eigen3 -I/System/Volumes/Data/opt/homebrew/include/eigen3 -I/Library/Frameworks/CsoundLib64.framework/Versions/6.0/Headers -I/opt/homebrew/Cellar/boost/1.81.0_1/include -I/home/mkg/csound-ac/CsoundAC -lCsoundAC -lpthread -lm", "/Users/michaelgogins/csound-webserver-opcodes/build/libcsound_webserver.dylib libCsoundAC.dylib"
endif

instr Exit
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
exitnow
endin

</CsInstruments>
<CsScore>
; f 0 does not work here; we actually need to schedule an instrument that 
; turns off Csound.
i "Exit" [465]
</CsScore>
</CsoundSynthesizer>
