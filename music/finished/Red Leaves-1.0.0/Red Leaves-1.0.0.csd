<CsoundSyntheizer>
<CsLicense>

R E D   L E A V E S   V E R S I O N   8 . 4 . 3 . 5 . stereo . csd

Michael Gogins, 2022

This piece is another in my "Leaves" series of pieces of electroacoustic 
concert music, algorithmically composed, available on electronic distribution. 
This is the first piece in the series that I have attempted to spatialize. 
(Although this version of the piece is in ordinary stereo.) The complete 
source code for this composition is available at 
https://github.com/gogins/michael.gogins.studio/blob/master/2022-NYCEMF/Red_Leaves_v8.4.3.5.stereo.csd. 

This piece is algorithmically composed using a deterministic iterated 
function system (IFS), implemented using my CsoundAC library for algorithmic 
composition. The C++ code for the score generator is embedded in the Csound 
.csd file and compiled and run using my csound-cxx-opcodes, which allow  
embedding C++ source code directly into the Csound orchestra. The IFS uses 
operators upon chord transposition and upon the K and Q operators of the 
generalized contextual group of Fiore and Satyendra to generate chord 
progresssions, and uses operators upon instrument channel, time, pitch, and 
loudness to generate the actual notes. The piece uses a Web page to display a 
three-dimensional piano roll view of the score, with interactive controls to 
tweak the sounds and balance of the Csound instruments. The HTML5 source code 
for this page is embedded in the Csound .csd file and run using my 
csound-webserver-opcodes, which embed allow embedding HTML and JavaScript code 
directly into the Csound orchestra. The Csound instruments in this piece 
borrow and adapt instruments by Perry Cook, Ian McCurdy, Hans Mikelson, Steven 
Yi, and Lee Zakian, as well as myself. Spatialization is implemented using an 
Ambisonic spatializer originally written by Jan Jacob Hoffman and re-written 
by me, with Doppler effects and other distance cues. The plugin Csound opcodes 
used in this piece are available on GitHub at https://github.com/gogins, 
except for the vst4cs opcodes, which can be downloaded from 
https://michaelgogins.tumblr.com/csound_extended. 

</CsLicense>
<CsOptions>
-j4 --m-amps=1 --m-range=1 --m-dB=1 --m-benchmarks=1 --m-warnings=0 -+msg_color=0 -d -odac 
</CsOptions>
<CsInstruments>

//////////////////////////////////////////////////////////////////////////////
// Define just one of these to specify the spatialization system.
//////////////////////////////////////////////////////////////////////////////
#define SPATIALIZE_STEREO #1#
;#define SPATIALIZE_GOGINS #4#

//////////////////////////////////////////////////////////////////////////////
// Change to sr=96000 with ksmps=1 for final rendering to soundfile.
//////////////////////////////////////////////////////////////////////////////
sr = 48000
ksmps = 128
nchnls = 2
nchnls_i = 1
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
/// gi_vst3info init 0

//////////////////////////////////////////////////////////////////////////////
// We will load plugins from different locations on different operating 
// systems.
//////////////////////////////////////////////////////////////////////////////
gS_os, gS_macros cxx_os
prints "Operating system: %s\n", gS_os

gi_size init 20

gi_pi init 3.141592653589793

// Converts IEM Cartesian coordinates to IEM spherical coordinates.
// Azimuth is displayed as degrees in [-180, 180] with 90 at left, -90 at 
// right, -180 or 180 at back, 0 at front.
// Elevation is displayed in [-90, 90] with -90 at bottom and 90 at top.
// Radius is displayed in [0, maximum_radius].
// Cartesian coordinates are y is left to right [-1, 1], x is front to back [1, -1],
// z is bottom to top [-1, 1].
// To normalize to the VST range of [0,1], all calculations must agree on a 
// maximum radius (half the room size, or range of positions).
/*
    static void cartesianToSpherical (const Type x, const Type y, const Type z, Type& azimuthInRadians, Type& elevationInRadians, Type& radius)
    {
        const float xSquared = x * x;
        const float ySquared = y * y;
        radius = sqrt(xSquared + ySquared + z * z);
        azimuthInRadians = atan2(y, x);
        elevationInRadians = atan2(z, sqrt(xSquared + ySquared));
    };

*/
opcode iem_cartesian_to_spherical, kkk, kkk
k_x, k_y, k_z xin
k_radius = sqrt(k_x^2 + k_y^2 + k_z^2)
k_azimuth taninv2 k_y, k_x
k_elevation taninv2 k_z, sqrt(k_x^2 + k_y^2)
xout k_azimuth, k_elevation, k_radius
endop

// Converts IEM spherical coordinates to VST parameter ranges.
opcode iem_normalize_spherical_coordinates, kkk, ikkk
i_maximum_radius, k_azimuth, k_elevation, k_radius xin
k_normalized_azimuth = -1 * ((k_azimuth + gi_pi) / (2 * gi_pi))
k_normalized_elevation = (k_elevation + (gi_pi / 2)) / gi_pi
k_normalized_radius = k_radius / i_maximum_radius 
xout k_normalized_azimuth, k_normalized_elevation, k_normalized_radius
endop

// Converts IEM Cartesian coordinates to IEM 
// spherical coordinates that are normalized to VST parameter ranges.
opcode iem_cartesian_to_spherical_vst,kkk,ikkk
i_maximum_radius, k_x, k_y, k_z xin
k_x *= (i_maximum_radius / 2)
k_y *= (i_maximum_radius / 2)
k_z *= (i_maximum_radius / 2)
k_azimuth, k_elevation, k_radius iem_cartesian_to_spherical k_x, k_y, k_z
k_azimuth_vst, k_elevation_vst, k_radius_vst iem_normalize_spherical_coordinates i_maximum_radius, k_azimuth, k_elevation, k_radius
S_template init {{%-24.24s i: %3d t: %9.4f max radius: %9.4f
  => Cartesian:  x: %9.4f y: %9.4f z: %9.4f
  => Spherical:  a: %9.4f e: %9.4f r: %9.4f
  =>             a: %9.4f e: %9.4f r: %9.4f
  => Normalized: a: %9.4f e: %9.4f r: %9.4f
}}
k_time times
;printks S_template, .5, nstrstr(p1), int(p1), k_time, i_maximum_radius, k_x, k_y, k_z, k_azimuth, k_elevation, k_radius, k_azimuth * 180 / gi_pi, k_elevation * 180 / gi_pi, k_radius, k_azimuth_vst, k_elevation_vst, k_radius_vst
xout k_azimuth_vst, k_elevation_vst, k_radius_vst
endop

#ifdef SPATIALIZE_GOGINS

gi_base init 5

connect "Blower",           "outbformat",   "BformatDecoder2",  "inbformat"
connect "Blower",           "out",          "SpatialReverb",    "in"
connect "STKBowed",         "outbformat",   "BformatDecoder2",  "inbformat"
connect "STKBowed",         "out",          "SpatialReverb",    "in"
connect "Buzzer",           "outbformat",   "BformatDecoder2",  "inbformat"
connect "Buzzer",           "out",          "SpatialReverb",    "in"
connect "Droner",           "outbformat",   "BformatDecoder2",  "inbformat"
connect "Droner",           "out",          "SpatialReverb",    "in"
connect "FMWaterBell",      "outbformat",   "BformatDecoder2",  "inbformat"
connect "FMWaterBell",      "out",          "SpatialReverb",    "in"
connect "Phaser",           "outbformat",   "BformatDecoder2",  "inbformat"
connect "Phaser",           "out",          "SpatialReverb",    "in"
connect "PianoOutPianoteq", "outbformat",   "BformatDecoder2",  "inbformat"
connect "PianoOutPianoteq", "out",          "SpatialReverb",    "in"
connect "Sweeper",          "outbformat",   "BformatDecoder2",  "inbformat"
connect "Sweeper",          "out",          "SpatialReverb",    "in"
connect "Shiner",           "outbformat",   "BformatDecoder2",  "inbformat"
connect "Shiner",           "out",          "SpatialReverb",    "in"
connect "ZakianFlute",      "outbformat",   "BformatDecoder2",  "inbformat"
connect "ZakianFlute",      "out",          "SpatialReverb",    "in"
connect "SpatialReverb",    "outbformat",   "BformatDecoder2",  "inbformat"

#include "Spatialize1.inc"

gk_BformatDecoder_MasterLevel init 0
gk_BformatDecoder2_MasterLevel init 12
gk_BformatDecoder_SpeakerRig init 1
gk_BformatDecoder2_SpeakerRig init 31
gk_Spatialize_SpeakerRigRadius init 5.0
gk_SpatialReverb_ReverbDecay init 0.76
gk_SpatialReverb_CutoffHz init sr
gk_SpatialReverb_RandomDelayModulation init .002
gk_LocalReverbByDistance_Wet init 0.5
; This is a fraction of the speaker rig radius.
gk_LocalReverbByDistance_FrontWall init 0.9
gk_LocalReverbByDistance_ReverbDecay init 0.6
gk_LocalReverbByDistance_CutoffHz init 20000
gk_LocalReverbByDistance_RandomDelayModulation init .002
gk_Spatialize_Verbose init 0

alwayson "PianoOutPianoteq"
alwayson "SpatialReverb"
alwayson "BformatDecoder2"

gi_instrument_position chnexport "gi_instrument_position", 3 ;  0
gi_instrument_position init 50
opcode instrument_position, kk, iii
i_onset, i_radius, i_rate xin
i_rate = (i_rate * gi_instrument_position)
k_time times
// Depth.
k_x = gk_Spatialize_SpeakerRigRadius * cos(i_onset + ((k_time - i_onset) * i_rate)) - 5
// Pan.
k_y = gk_Spatialize_SpeakerRigRadius * sin(i_onset + ((k_time - i_onset) * i_rate))
xout k_x, k_y
endop

#end

#ifdef SPATIALIZE_STEREO

gi_base init 0

opcode Instrument_position, kk, iii
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

connect "Blower", "outleft", "ReverbSC", "inleft"
connect "Blower", "outright", "ReverbSC", "inright"
connect "STKBowed", "outleft", "ReverbSC", "inleft"
connect "STKBowed", "outright", "ReverbSC", "inright"
connect "Buzzer", "outleft", "ReverbSC", "inleft"
connect "Buzzer", "outright", "ReverbSC", "inright"
connect "Droner", "outleft", "ReverbSC", "inleft"
connect "Droner", "outright", "ReverbSC", "inright"
connect "FMWaterBell", "outleft", "ReverbSC", "inleft"
connect "FMWaterBell", "outright", "ReverbSC", "inright"
connect "Phaser", "outleft", "ReverbSC", "inleft"
connect "Phaser", "outright", "ReverbSC", "inright"
connect "PianoOutPianoteq", "outleft", "ReverbSC", "inleft"
connect "PianoOutPianoteq", "outright", "ReverbSC", "inright"
connect "Sweeper", "outleft", "ReverbSC", "inleft"
connect "Sweeper", "outright", "ReverbSC", "inright"
connect "Shiner", "outleft", "ReverbSC", "inleft"
connect "Shiner", "outright", "ReverbSC", "inright"
connect "ZakianFlute", "outleft", "ReverbSC", "inleft"
connect "ZakianFlute", "outright", "ReverbSC", "inright"
connect "ReverbSC", "outleft", "MasterOutput", "inleft"
connect "ReverbSC", "outright", "MasterOutput", "inright"

#end

//////////////////////////////////////////////////////////////////////////////
// These are all the Csound Instruments and effects used in this piece.
//////////////////////////////////////////////////////////////////////////////

if strcmp(gS_os, "Linux") == 0 then
gi_vstinfo init 1
gi_Pianoteq vstinit "/home/mkg/Pianoteq\ 7/x86-64bit/Pianoteq\ 7.so", gi_vstinfo
endif
if strcmp(gS_os, "macOS") == 0 then
gi_Pianoteq vst3init "/Library/Audio/Plug-Ins/VST3/Pianoteq\ 8.vst3", "Pianoteq 8", 1
endif

#include "PianoNotePianoteqVst3.inc"
#include "FMWaterBell1.inc" // Normalized.
#include "Phaser1.inc"      // Normalized.
#include "Droner1.inc"      // Normalized.
#include "Sweeper1.inc"     // Normalized.
#include "Buzzer1.inc"      // Normalized.
#include "Shiner1.inc"      // Normalized.
#include "Blower1.inc"      // Normalized.
#include "ZakianFlute1.inc" // Normalized.
#include "STKBowed1.inc"    // Normalized.

#include "PianoOutPianoteqVst3.inc"
gk_PianoOutPianoteq_front_to_back init -3
gk_PianoOutPianoteq_left_to_right init .4
gk_PianoOutPianoteq_bottom_to_top init 3

alwayson "PianoOutPianoteq"

#ifdef SPATIALIZE_STEREO

#include "ReverbSC.inc"
alwayson "ReverbSC"

gk_MasterOutput_level chnexport "gk_MasterOutput_level", 3 ; 0
gk_Performance_seconds chnexport "gk_Performance_seconds", 3 ; 0
gk_MasterOutput_level init 0
gk_Performance_seconds init 30

chn_k "gk_MasterOutput_output_level_left", 3
chn_k "gk_MasterOutput_output_level_right", 3

instr MasterOutputOff
i_MasterOutput_insno nstrnum "MasterOutput"
k_triggered init 0
if gk_Performance_seconds > 0 && k_triggered == 0 then
k_time times
if k_time >= gk_Performance_seconds then
turnoff2 i_MasterOutput_insno, 0, 1
k_triggered = 1
printks "Turning off insno: %d  gk_Performance_seconds: %9.4f  k_time: %9.4f...\n", 1, i_MasterOutput_insno, gk_Performance_seconds, k_time
endif
endif
endin

instr MasterOutput
aleft inleta "inleft"
aright inleta "inright"
k_gain = ampdb(gk_MasterOutput_level)
printks2 "Master gain: %f\n", k_gain
iamp init 1
aleft butterlp aleft, 18000
aright butterlp aright, 18000
a_out_left = aleft * k_gain
a_out_right = aright * k_gain
i_duration = p3

; Whether to write a soundfile is configurable.
; In NW.js with the launcher page, this soundfile will be written to the 
; `csound-nwjs/nwjs-launcher/` directory.

gk_cloud5_record chnget "gk_cloud5_record"
gS_cloud5_soundfile_name chnget "gS_cloud5_soundfile_name"

if gk_cloud5_record == 1 then 
gS_cloud5_soundfile_name chnget "gS_cloud5_soundfile_name" 
fout gS_cloud5_soundfile_name, 19, a_out_left, a_out_right
prints sprintf("MasterOutput recording to output filename: %s\n", gS_cloud5_soundfile_name)
else 
prints "Not recording.\n"
endif

prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
gk_MasterOutput_output_level_left = dbfsamp(rms(a_out_left))
gk_MasterOutput_output_level_right = dbfsamp(rms(a_out_right))
chnset gk_MasterOutput_output_level_left, "gk_MasterOutput_output_level_left"
chnset gk_MasterOutput_output_level_right, "gk_MasterOutput_output_level_right"
// printks "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", 1, nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1), gk_MasterOutput_output_level_left, gk_MasterOutput_output_level_right
// Use a fade-in, fade-out envelope.
// kres cossegr ia, idur1, ib [, idur2] [, ic] [...], irel, iz
k_envelope linsegr 0, 6, 1, 1000000000, 1, 6, 0
outs a_out_left * k_envelope, a_out_right * k_envelope
endin
alwayson "MasterOutput"

#end

gk_FMWaterBell_front_to_back init -3
gk_FMWaterBell_left_to_right init .6
gk_FMWaterBell_bottom_to_top init -3

//////////////////////////////////////////////////////////////////////////////
// Define the initial values of all global variables/control channels.
//////////////////////////////////////////////////////////////////////////////

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
gk_ReverbSC_feedback init 0.86
gk_MasterOutput_level init -12
gk_Spatialize_SpeakerRigRadius init 15
gk_LocalReverbByDistance_ReverbDecay init .6
gk_LocalReverbByDistance_Wet init 0.2
gk_SpatialReverb_ReverbDecay init .2
gi_instrument_position init 0
gk_BformatDecoder2_MasterLevel init 0
gi_FMWaterBell_attack init 0.002936276551436901
gi_FMWaterBell_release init 0.022698875468554768
gi_FMWaterBell_exponent init 0
gi_FMWaterBell_sustain init 5.385256143273636
gi_FMWaterBell_sustain_level init 0.08267388588088297
gk_FMWaterBell_crossfade init 0.1234039047697504
gk_FMWaterBell_index init 1.1401499375260309
gk_FMWaterBell_vibrato_depth init 0.28503171595683335
gk_FMWaterBell_vibrato_rate init 2.4993821566850647
gk_FMWaterBell_level init 25
gk_Phaser_ratio1 init 1.028045002445785
gk_Phaser_ratio2 init 0.010598402087069948
gk_Phaser_index1 init 0.9709766835154084
gk_Phaser_index2 init 0.7361813142018588
gk_Phaser_level init 4
gk_STKBowed_vibrato_level init 0
gk_STKBowed_bow_pressure init 110
gk_STKBowed_bow_position init 21.81769218869982
gk_STKBowed_vibrato_frequency init 50.2
gk_STKBowed_level init 17.737876003647344
gk_Droner_partial1 init 0.4664927441708788
gk_Droner_partial2 init 0.16386760150008153
gk_Droner_partial3 init 0.13777922713190935
gk_Droner_partial4 init 0.4664927441708788
gk_Droner_partial5 init 0.15343225175281267
gk_Droner_level init 27.433556171531066
gk_Sweeper_bright_min init 3.5147562367519973
gk_Sweeper_bright_max init 0
gk_Sweeper_rate_min init 0.48850481004402413
gk_Sweeper_rate_max init 3.452144138268384
gk_Sweeper_level init -2
gk_Buzzer_harmonics init 4
gk_Buzzer_level init 0
gk_Shiner_level init -16.91668025436165
gk_Blower_grainDensity init 132.3332789825534
gk_Blower_grainDuration init 0.2854231208217838
gk_Blower_grainAmplitudeRange init 174.0746779716289
gk_Blower_grainFrequencyRange init 62.82406652535464
gk_Blower_level init 4
gk_ZakianFlute_level init 12
gk_PianoOutPianoteq_level init -12
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
gk_MasterOutput_level init 0
gk_Spatialize_SpeakerRigRadius init 15
gk_LocalReverbByDistance_ReverbDecay init 0.6
gk_LocalReverbByDistance_Wet init 0.2
gk_SpatialReverb_ReverbDecay init 2.408609163541497
gi_instrument_position init 0.0888635251915865
gk_BformatDecoder2_MasterLevel init 0
gk_PianoOutPianoteq_level init -10
gi_FMWaterBell_attack init 0.002936276551436901
gi_FMWaterBell_release init 0.022698875468554768
gi_FMWaterBell_exponent init 0
gi_FMWaterBell_sustain init 5.385256143273636
gi_FMWaterBell_sustain_level init 0.08267388588088297
gk_FMWaterBell_crossfade init 0.1234039047697504
gk_FMWaterBell_index init 1.1401499375260309
gk_FMWaterBell_vibrato_depth init 0.28503171595683335
gk_FMWaterBell_vibrato_rate init 2.4993821566850647
gk_FMWaterBell_level init 27
gk_Phaser_ratio1 init 1.028045002445785
gk_Phaser_ratio2 init 0.010598402087069948
gk_Phaser_index1 init 0.9709766835154084
gk_Phaser_index2 init 0.7361813142018588
gk_Phaser_level init 4
gk_STKBowed_vibrato_level init 0
gk_STKBowed_bow_pressure init 110
gk_STKBowed_bow_position init 21.81769218869982
gk_STKBowed_vibrato_frequency init 50.2
gk_STKBowed_level init 17.737876003647344
gk_Droner_partial1 init 0.4664927441708788
gk_Droner_partial2 init 0.16386760150008153
gk_Droner_partial3 init 0.13777922713190935
gk_Droner_partial4 init 0.4664927441708788
gk_Droner_partial5 init 0.15343225175281267
gk_Droner_level init 27.433556171531066
gk_Sweeper_bright_min init 3.5147562367519973
gk_Sweeper_bright_max init 0
gk_Sweeper_rate_min init 0.48850481004402413
gk_Sweeper_rate_max init 3.452144138268384
gk_Sweeper_level init -2
gk_Buzzer_harmonics init 4
gk_Buzzer_level init 0
gk_Shiner_level init -16.91668025436165
gk_Blower_grainDensity init 132.3332789825534
gk_Blower_grainDuration init 0.2854231208217838
gk_Blower_grainAmplitudeRange init 174.0746779716289
gk_Blower_grainFrequencyRange init 62.82406652535464
gk_Blower_level init 4
gk_ZakianFlute_level init 12
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
gk_MasterOutput_level init 0
gk_Spatialize_SpeakerRigRadius init 15
gk_LocalReverbByDistance_ReverbDecay init 0.21829564644342847
gk_LocalReverbByDistance_Wet init 0.4344495242802422
gk_SpatialReverb_ReverbDecay init 2.6867663412825897
gi_instrument_position init 0.005
gk_BformatDecoder2_MasterLevel init 7.152271510358744
gk_PianoOutPianoteq_level init -0.741888146094901
gi_FMWaterBell_attack init 0.002936276551436901
gi_FMWaterBell_release init 0.022698875468554768
gi_FMWaterBell_exponent init 0
gi_FMWaterBell_sustain init 5.385256143273636
gi_FMWaterBell_sustain_level init 0.08267388588088297
gk_FMWaterBell_crossfade init 0.1234039047697504
gk_FMWaterBell_index init 1.1401499375260309
gk_FMWaterBell_vibrato_depth init 0.28503171595683335
gk_FMWaterBell_vibrato_rate init 2.4993821566850647
gk_FMWaterBell_level init 35.621318835207376
gk_Phaser_ratio1 init 1.028045002445785
gk_Phaser_ratio2 init 0.010598402087069948
gk_Phaser_index1 init 0.9709766835154084
gk_Phaser_index2 init 0.7361813142018588
gk_Phaser_level init 4
gk_STKBowed_vibrato_level init 0
gk_STKBowed_bow_pressure init 110
gk_STKBowed_bow_position init 21.81769218869982
gk_STKBowed_vibrato_frequency init 50.2
gk_STKBowed_level init 17.737876003647344
gk_Droner_partial1 init 0.4664927441708788 
gk_Droner_partial2 init 0.16386760150008153
gk_Droner_partial3 init 0.13777922713190935
gk_Droner_partial4 init 0.4664927441708788
gk_Droner_partial5 init 0.15343225175281267
gk_Droner_level init 27.433556171531066
gk_Sweeper_bright_min init 3.5147562367519973
gk_Sweeper_bright_max init 0
gk_Sweeper_rate_min init 0.48850481004402413
gk_Sweeper_rate_max init 3.452144138268384
gk_Sweeper_level init -2
gk_Buzzer_harmonics init 4
gk_Buzzer_level init 0
gk_Shiner_level init -16.91668025436165
gk_Blower_grainDensity init 132.3332789825534
gk_Blower_grainDuration init 0.2854231208217838
gk_Blower_grainAmplitudeRange init 174.0746779716289
gk_Blower_grainFrequencyRange init 62.82406652535464
gk_Blower_level init -5.959563019729337
gk_ZakianFlute_level init 12
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
gk_MasterOutput_level init 0
gk_Spatialize_SpeakerRigRadius init 15
gk_LocalReverbByDistance_ReverbDecay init 0.14975905103175585
gk_LocalReverbByDistance_Wet init 0.4344495242802422
gk_SpatialReverb_ReverbDecay init 2.7711190740969562
gi_instrument_position init 0.0035
gk_BformatDecoder2_MasterLevel init 0.3016468286319949
gk_PianoOutPianoteq_level init -15;-9
gi_FMWaterBell_attack init 0.002936276551436901
gi_FMWaterBell_release init 0.022698875468554768
gi_FMWaterBell_exponent init 0
gi_FMWaterBell_sustain init 5.385256143273636
gi_FMWaterBell_sustain_level init 0.08267388588088297
gk_FMWaterBell_crossfade init 0.1234039047697504
gk_FMWaterBell_index init 1.1401499375260309
gk_FMWaterBell_vibrato_depth init 0.28503171595683335
gk_FMWaterBell_vibrato_rate init 2.4993821566850647
gk_FMWaterBell_level init 14;;26
gk_Phaser_ratio1 init 1.028045002445785
gk_Phaser_ratio2 init 0.010598402087069948
gk_Phaser_index1 init 0.9709766835154084
gk_Phaser_index2 init 0.7361813142018588
gk_Phaser_level init 4
gk_STKBowed_vibrato_level init 0
gk_STKBowed_bow_pressure init 110
gk_STKBowed_bow_position init 21.81769218869982
gk_STKBowed_vibrato_frequency init 50.2
gk_STKBowed_level init 17.737876003647344
gk_Droner_partial1 init 0.4664927441708788
gk_Droner_partial2 init 0.16386760150008153
gk_Droner_partial3 init 0.13777922713190935
gk_Droner_partial4 init 0.4664927441708788
gk_Droner_partial5 init 0.15343225175281267
gk_Droner_level init 27.433556171531066
gk_Sweeper_bright_min init 3.5147562367519973
gk_Sweeper_bright_max init 0
gk_Sweeper_rate_min init 0.48850481004402413
gk_Sweeper_rate_max init 3.452144138268384
gk_Sweeper_level init -2
gk_Buzzer_harmonics init 4
gk_Buzzer_level init 0
gk_Shiner_level init -16.91668025436165
gk_Blower_grainDensity init 132.3332789825534
gk_Blower_grainDuration init 0.2854231208217838
gk_Blower_grainAmplitudeRange init 174.0746779716289
gk_Blower_grainFrequencyRange init 62.82406652535464
gk_Blower_level init -5.959563019729337
gk_ZakianFlute_level init 12
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
gk_MasterOutput_level init 0
gk_Spatialize_SpeakerRigRadius init 15
gk_LocalReverbByDistance_ReverbDecay init 0.14975905103175585
gk_LocalReverbByDistance_Wet init 0.4344495242802422
gk_SpatialReverb_ReverbDecay init 2.7711190740969562
gi_instrument_position init 0.0035
gk_BformatDecoder2_MasterLevel init 5.570657770089383
gk_PianoOutPianoteq_level init -15;;9
gi_FMWaterBell_attack init 0.002936276551436901
gi_FMWaterBell_release init 0.022698875468554768
gi_FMWaterBell_exponent init 0
gi_FMWaterBell_sustain init 5.385256143273636
gi_FMWaterBell_sustain_level init 0.08267388588088297
gk_FMWaterBell_crossfade init 0.1234039047697504
gk_FMWaterBell_index init 1.1401499375260309
gk_FMWaterBell_vibrato_depth init 0.28503171595683335
gk_FMWaterBell_vibrato_rate init 2.4993821566850647
gk_FMWaterBell_level init 26
gk_Phaser_ratio1 init 0.48519296511388443
gk_Phaser_ratio2 init 1.9877260183697845
gk_Phaser_index1 init 0.9020140862473743
gk_Phaser_index2 init 0.9810947732608427
gk_Phaser_level init 4
gk_STKBowed_vibrato_level init 0
gk_STKBowed_bow_pressure init 110
gk_STKBowed_bow_position init 21.81769218869982
gk_STKBowed_vibrato_frequency init 50.2
gk_STKBowed_level init 17.737876003647344
gk_Droner_partial1 init 0.4664927441708788
gk_Droner_partial2 init 0.16386760150008153
gk_Droner_partial3 init 0.13777922713190935
gk_Droner_partial4 init 0.4664927441708788
gk_Droner_partial5 init 0.15343225175281267
gk_Droner_level init 27.433556171531066
gk_Sweeper_bright_min init 0.13509617364800858
gk_Sweeper_bright_max init 2.1595617611928004
gk_Sweeper_rate_min init 0.11400799044441698
gk_Sweeper_rate_max init 2.792207257300548
gk_Sweeper_level init -2
gk_Buzzer_harmonics init 4
gk_Buzzer_level init 0
gk_Shiner_level init -16.91668025436165
gk_Blower_grainDensity init 132.3332789825534
gk_Blower_grainDuration init 0.2854231208217838
gk_Blower_grainAmplitudeRange init 174.0746779716289
gk_Blower_grainFrequencyRange init 62.82406652535464
gk_Blower_level init -5.959563019729337
gk_ZakianFlute_level init 12
gk_ReverbSC_feedback init 0.82
gk_ReverbSC_wet init 0.5
gi_ReverbSC_delay_modulation init 0.0075
gk_ReverbSC_frequency_cutoff init 15000
gk_MasterOutputLevel init 0.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
gk_MasterOutput_level init 10
gk_Spatialize_SpeakerRigRadius init 0
gk_LocalReverbByDistance_ReverbDecay init 0
gk_LocalReverbByDistance_Wet init 0
gk_SpatialReverb_ReverbDecay init 0
gi_instrument_position init 0
gk_BformatDecoder2_MasterLevel init 0
gk_ReverbSC_feedback init 0.82
gk_ReverbSC_wet init 0.5
gi_ReverbSC_delay_modulation init 0.0075
gk_ReverbSC_frequency_cutoff init 15000
gk_PianoOutPianoteq_level init 25
gi_FMWaterBell_attack init 0.002936276551436901
gi_FMWaterBell_release init 0.022698875468554768
gi_FMWaterBell_exponent init 0
gi_FMWaterBell_sustain init 5.385256143273636
gi_FMWaterBell_sustain_level init 0.08267388588088297
gk_FMWaterBell_crossfade init 0.1234039047697504
gk_FMWaterBell_index init 1.1401499375260309
gk_FMWaterBell_vibrato_depth init 0.28503171595683335
gk_FMWaterBell_vibrato_rate init 2.4993821566850647
gk_FMWaterBell_level init 17.75;;18.5;;17;;26
gk_Phaser_ratio1 init 0.48519296511388443
gk_Phaser_ratio2 init 1.9877260183697845
gk_Phaser_index1 init 0.9020140862473743
gk_Phaser_index2 init 0.9810947732608427
gk_Phaser_level init 4
gk_STKBowed_vibrato_level init 0
gk_STKBowed_bow_pressure init 110
gk_STKBowed_bow_position init 21.81769218869982
gk_STKBowed_vibrato_frequency init 50.2
gk_STKBowed_level init 17.737876003647344
gk_Droner_partial1 init 0.4664927441708788
gk_Droner_partial2 init 0.16386760150008153
gk_Droner_partial3 init 0.13777922713190935
gk_Droner_partial4 init 0.4664927441708788
gk_Droner_partial5 init 0.15343225175281267
gk_Droner_level init 14
gk_Sweeper_bright_min init 0.13509617364800858
gk_Sweeper_bright_max init 2.1595617611928004
gk_Sweeper_rate_min init 0.11400799044441698
gk_Sweeper_rate_max init 2.792207257300548
gk_Sweeper_level init -2
gk_Buzzer_harmonics init 4
gk_Buzzer_level init 0
gk_Shiner_level init -16.91668025436165
gk_Blower_grainDensity init 132.3332789825534
gk_Blower_grainDuration init 0.2854231208217838
gk_Blower_grainAmplitudeRange init 174.0746779716289
gk_Blower_grainFrequencyRange init 62.82406652535464
gk_Blower_level init -5.959563019729337
gk_ZakianFlute_level init 12
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

gS_html init {{<!DOCTYPE html>
<html>
<head>
    <title>Red Leaves version 8.4.3.5</title>
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
    <script src="TrackballControls.js"></script>
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
            gk_Spatialize_SpeakerRigRadius: 15.,
            
            gk_LocalReverbByDistance_ReverbDecay: .6,
            gk_LocalReverbByDistance_Wet: 0.2,
            gk_SpatialReverb_ReverbDecay: .2,
            gi_instrument_position: 0.01,
            gk_BformatDecoder2_MasterLevel: -12.,
            
            gk_ReverbSC_feedback: 0.875,
            gk_ReverbSC_wet: 0.5,
            gi_ReverbSC_delay_modulation: 0.0075,
            gk_ReverbSC_frequency_cutoff: 15000,
            gk_MasterOutput_level: 0.,           
            gk_PianoOutPianoteq_level: -44.,
            gi_FMWaterBell_attack: 0.002936276551436901,
            gi_FMWaterBell_release: 0.022698875468554768,
            gi_FMWaterBell_exponent: 8.72147623362715,
            gi_FMWaterBell_sustain: 5.385256143273636,
            gi_FMWaterBell_sustain_level: 0.08267388588088297,
            gk_FMWaterBell_crossfade: 0.1234039047697504,
            gk_FMWaterBell_index: 1.1401499375260309,
            gk_FMWaterBell_vibrato_depth: 0.28503171595683335,
            gk_FMWaterBell_vibrato_rate: 2.4993821566850647,
            gk_FMWaterBell_level: 26.,
            gk_Phaser_ratio1: 1.0388005601779389,
            gk_Phaser_ratio2: 3.0422604827415767,
            gk_Phaser_index1: 0.5066315182469726,
            gk_Phaser_index2: 0.5066315182469726,
            gk_Phaser_level: 8.25438668753604,
            gk_STKBowed_vibrato_level: 2.8,
            gk_STKBowed_bow_pressure: 110,
            gk_STKBowed_bow_position: 20,
            gk_STKBowed_vibrato_frequency: 50.2,
            gk_STKBowed_level: 0,
            gk_Droner_partial1: 0.11032374600527997,
            gk_Droner_partial2: 0.4927052938724468,
            gk_Droner_partial3: 0.11921634014172572,
            gk_Droner_partial4: 0.06586077532305128,
            gk_Droner_partial5: 0.6616645824649159,
            gk_Droner_level: 29.76521954032458,
            gk_Sweeper_bright_min: 0.4258927115604109,
            gk_Sweeper_bright_max: 3.635884339731444,
            gk_Sweeper_rate_min: 1.1354964943746944,
            gk_Sweeper_rate_max: 3.222566443828469,
            gk_Sweeper_level: 7.606391651720202,
            gk_Buzzer_harmonics: 11.958151412801714,
            gk_Buzzer_level: 23.61650089678787,
            gk_Shiner_level: 22.3642589271156,
            gk_Blower_grainDensity: 79.99177885109444,
            gk_Blower_grainDuration: 0.2,
            gk_Blower_grainAmplitudeRange: 87.88408180043162,
            gk_Blower_grainFrequencyRange: 30.596081700708627,
            gk_Blower_level: 7.754769280939186,
            gk_ZakianFlute_level: 25.125628140703512,
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
            var Master = gui.addFolder('Master');
            var Ambisonic = Master.addFolder('Ambisonic')
            add_slider(Ambisonic, 'gk_LocalReverbByDistance_ReverbDecay', 0, 1.);
            add_slider(Ambisonic, 'gk_LocalReverbByDistance_Wet', 0, 1.);
            add_slider(Ambisonic, 'gk_SpatialReverb_ReverbDecay', 0, 4.);
            add_slider(Ambisonic, 'gk_Spatialize_SpeakerRigRadius', 2, 100.);
            add_slider(Ambisonic, 'gi_instrument_position', 0, .5);
            add_slider(Ambisonic, 'gk_BformatDecoder2_MasterLevel', -50, 50);
            var Stereo = Master.addFolder('Stereo')
            add_slider(Stereo, 'gk_ReverbSC_feedback', 0, 1.);
            add_slider(Stereo, 'gk_ReverbSC_wet', 0, 1.);
            add_slider(Stereo, 'gi_ReverbSC_delay_modulation', 0, 4.);
            add_slider(Stereo, 'gk_ReverbSC_frequency_cutoff', 0., 20000.);
            add_slider(Stereo, 'gk_MasterOutput_level', -50., 50.);
          
            var Pianoteq = gui.addFolder('Pianoteq');
            add_slider(Pianoteq, 'gk_PianoOutPianoteq_level', -50, 50);
            var FMWaterBell = gui.addFolder('FMWaterBell');
            add_slider(FMWaterBell, 'gi_FMWaterBell_attack', 0, .1);
            add_slider(FMWaterBell, 'gi_FMWaterBell_release', 0, .1);
            add_slider(FMWaterBell, 'gi_FMWaterBell_exponent', -30, 30);
            add_slider(FMWaterBell, 'gi_FMWaterBell_sustain', 0, 20);
            add_slider(FMWaterBell, 'gi_FMWaterBell_sustain_level', 0, 1.);
            add_slider(FMWaterBell, 'gk_FMWaterBell_crossfade', 0, 1.);
            add_slider(FMWaterBell, 'gk_FMWaterBell_index', 0, 15.);
            add_slider(FMWaterBell, 'gk_FMWaterBell_vibrato_depth', 0, 10.);
            add_slider(FMWaterBell, 'gk_FMWaterBell_vibrato_rate', 0, 10.);
            add_slider(FMWaterBell, 'gk_FMWaterBell_level',-50, 50);
            var Phaser = gui.addFolder('Phaser');
            add_slider(Phaser, 'gk_Phaser_ratio1', 0, 5);
            add_slider(Phaser, 'gk_Phaser_ratio2', 0, 5);
            add_slider(Phaser, 'gk_Phaser_index1', 0, 15);
            add_slider(Phaser, 'gk_Phaser_index2', 0, 15);
            add_slider(Phaser, 'gk_Phaser_level', -50, 50);
            var Droner = gui.addFolder('Droner');
            add_slider(Droner, 'gk_Droner_partial1', 0, 1);
            add_slider(Droner, 'gk_Droner_partial2', 0, 1);
            add_slider(Droner, 'gk_Droner_partial3', 0, 1);
            add_slider(Droner, 'gk_Droner_partial4', 0, 1);
            add_slider(Droner, 'gk_Droner_partial5', 0, 1);
            add_slider(Droner, 'gk_Droner_level', -50, 50);
            var Sweeper = gui.addFolder('Sweeper');
            add_slider(Sweeper, 'gk_Sweeper_bright_min', 0, 4);
            add_slider(Sweeper, 'gk_Sweeper_bright_max', 0, 4);
            add_slider(Sweeper, 'gk_Sweeper_rate_min', 0, 4);
            add_slider(Sweeper, 'gk_Sweeper_rate_max', 0, 4);
            add_slider(Sweeper, 'gk_Sweeper_level', -50, 50);
            var Buzzer = gui.addFolder('Buzzer');
            add_slider(Buzzer, 'gk_Buzzer_harmonics', 0, 20);
            add_slider(Buzzer, 'gk_Buzzer_level', -50, 50);
            var Shiner = gui.addFolder('Shiner');
            add_slider(Shiner, 'gk_Shiner_level', -50, 50);
            var Blower = gui.addFolder('Blower');
            add_slider(Blower, 'gk_Blower_grainDensity', 0, 400);
            add_slider(Blower, 'gk_Blower_grainDuration', 0, .5);
            add_slider(Blower, 'gk_Blower_grainAmplitudeRange', 0, 400);
            add_slider(Blower, 'gk_Blower_grainFrequencyRange', 0, 100);
            add_slider(Blower, 'gk_Blower_level', -50, 50);
            var STKBowed = gui.addFolder('STKBowed');
            add_slider(STKBowed, 'gk_STKBowed_vibrato_level', 0, 127);
            add_slider(STKBowed, 'gk_STKBowed_bow_pressure', 0, 127);
            add_slider(STKBowed, 'gk_STKBowed_bow_position', 0, 127);
            add_slider(STKBowed, 'gk_STKBowed_vibrato_frequency', 0, 127);
            add_slider(STKBowed, 'gk_STKBowed_level', -50, 50);
            var Flute = gui.addFolder('Zakian Flute');
            add_slider(Flute, 'gk_ZakianFlute_level', -50, 50);
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
                    csound.Message(`Initialized gui: ${key} = ${value_}\n`);
                 }
            };
            var saved_gui_parameters = gui.getSaveObject();
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
                var e_char = String.fromCharCode(e.keyCode || e.charCode);
                if (e.ctrlKey === true) {
                    if (e_char === 'H') {
                        var console = document.getElementById("console");
                        if (console.style.display === "none") {
                            console.style.display = "block";
                        } else {
                            console.style.display = "none";
                        }
                        gui.closed = true;
                        gui.closed = false;
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
gi_webserver webserver_create "/home/mkg/michael.gogins.studio/music/2022-NYCEMF/", 8080, 0
endif
if strcmp(gS_os, "macOS") == 0 then
gi_webserver webserver_create "/Users/michaelgogins/michael.gogins.studio/music/bandcamp/", 8080, 0
endif
// The following event source has to be created before we actually send it a 
// score to display.
webserver_send gi_webserver, "score_display", ""
webserver_open_html gi_webserver, gS_html

//////////////////////////////////////////////////////////////////////////////
// The following C++ code defines and executes a score generator that 
// implements the "multiple copy reducing machine" algorithm for computing an 
// iterated function system (IFS).
//
// Only a limited number of iterations are computed. On the final iteration, 
// each point in the attractor of the IFS is translated to a single note of 
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

// void* __dso_handle = (void *)&__dso_handle;

static bool diagnostics_enabled = false;

extern "C" { 

struct Cursor
{
    csound::Event note;
    csound::Chord chord;
};

auto generator = [] (const Cursor &cursor, int depth, csound::Score &score)
{
    Cursor result = cursor;
    return result;
};

//////////////////////////////////////////////////////////////////////////////
// Computes a deterministic, finite recurrent iterated function system by
// recursively applying a set of generators (transformations) to a pen
// that represent the position of a "pen" on a "score." The entries in
// the transitions matrix represent open or closed paths of recurrence through
// the tree of calls. Because the pen is passed by value, it is in effect
// copied at each call of a generator and at each layer of recursion. The
// generators may or may not append a local copy of the pen to the score,
// thus "writing" a "note" or other event on the "score."
//////////////////////////////////////////////////////////////////////////////
void recurrent(std::vector< std::function<Cursor(const Cursor &,int, csound::Score &)> > &generators,
        Eigen::MatrixXd &transitions,
        int depth,
        int transformationIndex,
        const Cursor pen,
        csound::Score &score) {
    depth = depth - 1;
    if (depth == 0) {
        score.append(pen.note);    
        return;
    }
    for (int transitionIndex = 0, transitionN = transitions.rows(); transitionIndex < transitionN; ++transitionIndex) {
        if (transitions(transformationIndex, transitionIndex)) {
            auto newCursor = generators[transitionIndex](pen, depth, score);
            recurrent(generators, transitions, depth, transitionIndex, newCursor, score);
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
    csound::ScoreModel model;
    std::map<double, csound::Chord> chordsForTimes;
    csound::Chord modality;
    Cursor pen;
    modality.fromString("0 4 7 11 14");
    pen.chord = modality;
    ///pen.note = csound::Event{1,35/1.25,144,1,1,1,0,0,0,0,1};
    pen.note = csound::Event{1,40,144,1,1,1,0,0,0,0,1};
    int base_level = 1;
    std::vector<std::function<Cursor(const Cursor &, int, csound::Score &)>> generators;
    auto g1 = [&chordsForTimes, &modality, &base_level](const Cursor &pen_, int depth, csound::Score &score) {
        Cursor pen = pen_;
        if ((depth + base_level) == 2) {
            pen.chord = pen.chord.T(5);
            chordsForTimes[pen.note.getTime()] = pen.chord;
        }
        pen.note[csound::Event::TIME] = (pen.note[csound::Event::TIME] * .5) + (0 - 5);
        pen.note[csound::Event::KEY] = (pen.note[csound::Event::KEY] * .75);
        return pen;
    };
    generators.push_back(g1);
    auto g2 = [&chordsForTimes, &modality, &base_level](const Cursor &pen_, int depth, csound::Score &score) {
        Cursor pen = pen_;
        if ((depth + base_level) == 2) {
            pen.chord = pen.chord.K();
            chordsForTimes[pen.note.getTime()] = pen.chord;
        }
        if ((depth + base_level) == 1) {
            pen.chord = pen.chord.Q(3, modality);
        }
        pen.note[csound::Event::TIME] = (pen.note[csound::Event::TIME] * .5) + (1000 + 1);
        pen.note[csound::Event::KEY] = (pen.note[csound::Event::KEY] * .76) - .25;
        pen.note[csound::Event::VELOCITY] =  std::cos(pen.note[csound::Event::TIME]);                    
        return pen;
    };
    generators.push_back(g2);
    auto g3 = [&chordsForTimes, &modality, &base_level](const Cursor &pen_, int depth, csound::Score &score) {
        Cursor pen = pen_;
        pen.note[csound::Event::TIME] = (pen.note[csound::Event::TIME] * .5) + (0 + 3);
        pen.note[csound::Event::KEY] = (pen.note[csound::Event::KEY] * .715) + 1.05;
        pen.note[csound::Event::InstruUMENT] = std::cos(pen.note[csound::Event::TIME]);
        pen.note[csound::Event::VELOCITY] =  std::cos(pen.note[csound::Event::TIME]);
        return pen;
    };
    generators.push_back(g3);
    auto g4 = [&chordsForTimes, &modality, &base_level](const Cursor &pen_, int depth, csound::Score &score) {
        Cursor pen = pen_;
        pen.note[csound::Event::TIME] = (pen.note[csound::Event::TIME] * .5) + (1000 - 5);
        pen.note[csound::Event::KEY] = (pen.note[csound::Event::KEY] * .77) + 1.;
        if ((depth + base_level) == 3) {
            pen.chord = pen.chord.T(3);
        }
       pen.note[csound::Event::InstruUMENT] = std::sin(pen.note[csound::Event::TIME]);
        pen.note[csound::Event::VELOCITY] =  std::cos(pen.note[csound::Event::TIME]);
        return pen;
    };
    generators.push_back(g4);
    // Generate the score.
    Eigen::MatrixXd transitions(4,4);
    transitions <<  1, 1, 1, 1,
                    1, 1, 1, 1,
                    1, 0, 1, 1,
                    1, 1, 0, 1;
    csound::Score score;
    //////////////////////////////////////////////////////////////////////////////
    // Before iterating, ensure that the score does start with a chord.
    //////////////////////////////////////////////////////////////////////////////
    chordsForTimes[-100.] = pen.chord;
    recurrent(generators, transitions, 7, 0, pen, score);
    ///recurrent(generators, transitions, 8, 0, pen, score);
    std::cout << "Generated duration:     " << score.getDuration() << std::endl;
    //////////////////////////////////////////////////////////////////////////////
    // We apply the chords that were generated along WITH the notes, TO the notes.
    // This creates an algorithmically generated chord progression.
    //////////////////////////////////////////////////////////////////////////////
    score.sort();
    score.rescale(csound::Event::KEY, true, 22.0, true,  74.0);
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
    score.rescale(csound::Event::TIME,          true,  0.0, false,  0.0);
    score.rescale(csound::Event::INSTRUUMENT,    true,  1.0, true,   9.999);
    score.rescale(csound::Event::VELOCITY,      true, 40.0, true,  20.0);
    score.rescale(csound::Event::PAN,           true,  0.0, true,   0.0);
    std::cout << "Move to origin duration:" << score.getDuration() << std::endl;
    score.setDuration(380.0 * 2.);
    std::cout << "set duration:           " << score.getDuration() << std::endl;
    score.tieOverlappingNotes(true);
    score.findScale();
    score.setDuration(360.0 * 2.);
    std::mt19937 mersenneTwister;
    std::uniform_real_distribution<> randomvariable(.05,.95);
    for (int i = 0, n = score.size(); i < n; ++i) {
        score[i].setPan(randomvariable(mersenneTwister));
        score[i].setDepth(randomvariable(mersenneTwister));
        score[i].setPhase(randomvariable(mersenneTwister));
    }
    score.rescale(csound::Event::TIME,          true,  2.0, false,  0.0);
    std::cout << "score:" << std::endl << score.getCsoundScore() << std::endl;
    std::cout << "Final duration:         " << score.getDuration() << std::endl;
    //////////////////////////////////////////////////////////////////////////
    // Using the EVTBLK struct for each note is more efficient than using a 
    // string for each note, or for the entire score.
    //////////////////////////////////////////////////////////////////////////
    EVTBLK evtblk;
    std::memset(&evtblk, 0, sizeof(EVTBLK));
    for (const auto &note : score) {
        evtblk.strarg = nullptr;
        evtblk.scnt = 0;
        evtblk.opcod = 'i';
        evtblk.pcnt = 9;
        // Add 4 to p1 only for SPATIALIZE_GOGINS.
        //evtblk.p[1] = std::floor(note.getInstrument() + 4);
        evtblk.p[1] = std::floor(note.getInstrument());
        evtblk.p[2] = note.getTime();
        evtblk.p[3] = note.getDuration();
        evtblk.p[4] = note.getKey();
        if (evtblk.p[1] == 1) {
            evtblk.p[4] += 12;
        }
        evtblk.p[5] = note.getVelocity();
        evtblk.p[6] = note.getDepth();
        evtblk.p[7] = note.getPan();
        evtblk.p[8] = note.getHeight();
        //std::fprintf(stderr, "%c %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f\\n", evtblk.opcod, evtblk.p[1], evtblk.p[2], evtblk.p[3], evtblk.p[4], evtblk.p[5], evtblk.p[6], evtblk.p[7], evtblk.p[8]);
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
// Note that dynamic link libraries must be passed as complete filepaths.
//////////////////////////////////////////////////////////////////////////////

if strcmp(gS_os, "Linux") == 0 then
i_result cxx_compile "score_generator", S_score_generator_code, "g++ -v -g -O2 -std=c++17 -shared -fPIC -DUSE_DOUBLE -I. -I/usr/local/include/csound -I/home/mkg/csound/interfaces -I/usr/include/eigen3 -I/home/mkg/csound-extended/CsoundAC -lCsoundAC -lpthread -lm", "libcsound_webserver.so libCsoundAC.so"
endif
if strcmp(gS_os, "macOS") == 0 then
prints "Compiling..."
i_result cxx_compile "score_generator", S_score_generator_code, "g++ -v -g -O2 -std=c++17 -shared -fPIC -DUSE_DOUBLE -I. -I/usr/local/include/csound -I/Users/michaelgogins/csound/interfaces -I/usr/include/eigen3 -I/System/Volumes/Data/opt/homebrew/include/eigen3 -I/Library/Frameworks/CsoundLib64.framework/Versions/6.0/Headers -I/opt/homebrew/Cellar/boost/1.88.0/include -I/Users/michaelgogins/csound-ac/CsoundAC -L/Users/michaelgogins/csound-ac/build-macos -Wl,-rpath,/opt/homebrew/lib -L/opt/hombrew/lib -lCsoundAC -lpthread -lm", "/Users/michaelgogins/csound-webserver-opcodes/build/libcsound_webserver.dylib"
endif

instr Exit
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
cxx_raise "SIGTERM"
endin

</CsInstruments>
<CsScore>
; f 0 does not work here, we actually need to schedule an instrument that 
; turns off Csound.
; a 0 1 270
i "Exit" [730]
;f 0 [6 * 60 + 5]
</CsScore>
</CsoundSynthesizer>
