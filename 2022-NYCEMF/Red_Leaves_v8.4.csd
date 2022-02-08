<CsoundSyntheizer>
<CsLicense>

R E D   L E A V E S   V E R S I O N   8 . 4

Michael Gogins, 2021

This piece is another in my "Leaves" series of pieces of electroacoustic 
concert music, algorithmically composed, available on electronic distribution. 
This is the first piece in the series that I have attempted to spatialize. The 
complete source code for this composition is available at 
https://github.com/gogins/michael.gogins.studio/blob/master/2022-NYCEMF/Red_Leaves_v8.4.csd. 

This piece is algorithmically composed using a deterministic iterated 
function system (IFS), implemented using my CsoundAC library for algorithmic 
composition. The C++ code for the score generator is embedded in the Csound 
.csd file and compiled and run using my Clang opcodes, which embed the 
Clang/LLVM just-in-time C++ compiler into the Csound runtime. The IFS uses 
operators upon chord transposition and upon the K and Q operators of the 
generalized contextual group of Fiore and Satyendra to generate chord 
progresssions, and uses operators upon instrument channel, time, pitch, and 
loudness to generate the actual notes. The piece uses a Web page to display a 
three-dimensional piano roll view of the score, with interactive controls to 
tweak the sounds and balance of the Csound instruments. The HTML5 source code 
for this page is embedded in the Csound .csd file and run using my WebKit 
opcodes, which embed the WebKit HTML browser and JavaScript runtime into the 
Csound runtime. The Csound instruments in this piece borrow and adapt 
instruments by Perry Cook, Ian McCurdy, Hans Mikelson, Steven Yi, and Lee 
Zakian, as well as myself. Spatialization is implemented using the IEM suite  
of VST 2 plugins from IEM Graz, embedded in the Csound 
runtime using the vst4cs opcodes for Csound by Andres Cabrera and myself. The 
plugin Csound opcodes used in this piece are available on GitHub at 
https://github.com/gogins, except for the vst4cs opcodes which can be 
downloaded from https://michaelgogins.tumblr.com/csound_extended. 

//////////////////////////////////////////////////////////////////////////////
// Tutorial comments like this are provided throughout the piece. 
//////////////////////////////////////////////////////////////////////////////

This piece has the following external dependencies, given for an Ubuntu Linux 
installation. 

 1. Csound 6.16 or later (https://github.com/csound/csound/actions/ for 
    GitHub builds, https://github.com/csound/csound/releases for official releases).
 2. The Csound opcodes for Faust 
    (https://github.com/csound/plugins/tree/develop/faustcsound).
 3. The Faust just-in-time compiler library 
    (https://github.com/grame-cncm/faust/releases).
 4. The Csound opcodes for the Clang/LLVM just-in-time C++ compiler 
    (https://github.com/gogins/clang-opcodes/releases).
 5. The Clang/LLVM infrastucture, version 13 (https://apt.llvm.org/, 
    "Install Stable Branch").
 6. The CsoundAC library (https://github.com/gogins/csound-extended/releases).
 7. The Csound opcodes for the WebKit browser 
    (https://github.com/gogins/webkit-opcodes/releases).
 8. The WebKit2Gtk+ library (https://webkitgtk.org/, system packages 
    `sudo apt-get install libwebkit2gtk-4.0-dev`).
 9. The libjson-rpc-cpp library (https://github.com/cinemast/libjson-rpc-cpp, 
    system packages `sudo apt-get install libjsonrpccpp-dev libjsonrpccpp-tools`).
10. The Csound vst4cs opcodes for hosting VST2 plugins 
    (https://href.li/?https://drive.google.com/file/d/1mYHyjoD7RUrPpST3ISa9CsxIg5wTspXc/view?usp=sharing).
11. The Pianoteq physically modelled piano synth plugin from Modartt 
    (https://www.modartt.com/pianoteq), this is a truly excellent 
    commercial product.

This is indeed a lot of stuff, but it makes for an_extremely_ powerful 
computer music system.

Even if you don't get everything here running, you may still find useful 
information on how to use some of these features in your own pieces.

</CsLicense>
<CsOptions>
-+msg_color=0 -m3 -d -odac 
</CsOptions>
<CsInstruments>

//////////////////////////////////////////////////////////////////////////////
// Change to sr=96000 with ksmps=1 for final rendering to soundfile.
//////////////////////////////////////////////////////////////////////////////
sr = 48000
ksmps = 128
nchnls = 2
0dbfs = 10
//////////////////////////////////////////////////////////////////////////////
// This random seed ensures that the same random stream  is used for each 
// rendering. Note that rand, randh, randi, rnd(x) and birnd(x) are not 
// affected by seed.
//////////////////////////////////////////////////////////////////////////////
seed 88818145

gi_size init 20

// Define just one of these.
;#define SPATIALIZE_STEREO #1#
#define SPATIALIZE_IEM #2#
;#define SPATIALIZE_AALTO #3#
;#define SPATIALIZE_GOGINS #4#

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
    } 9MT32V)@W4

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
printks S_template, .5, nstrstr(p1), int(p1), k_time, i_maximum_radius, k_x, k_y, k_z, k_azimuth, k_elevation, k_radius, k_azimuth * 180 / gi_pi, k_elevation * 180 / gi_pi, k_radius, k_azimuth_vst, k_elevation_vst, k_radius_vst
xout k_azimuth_vst, k_elevation_vst, k_radius_vst
endop

// Are the SPARTA and IEM Ambisonic conventions the same? Both seem to use 
// SN3D, but IEM uses azimuth, elevation, radius while SPARTA only seems to 
// use azimuth, elevation -- except for ambiRoomSim!

// N of the 64 channels are assigned and used in order, and unused channels are left as 0.

prints "====================================================\n"
prints "IEM Plugin Suite:\n"
prints "----------------------------------------------------\n"
prints "Send N instruments to one channel of one of these...\n"
gi_iem_multi_encoder vstinit "/usr/lib/x86\_64-linux-gnu/iem-plugin-suite/vst/MultiEncoder.so"
vstinfo gi_iem_multi_encoder
prints "````````````````````````````````````````````````````\n"
prints "...or to N of these.\n"
gi_iem_stereo_encoder vstinit "/usr/lib/x86\_64-linux-gnu/iem-plugin-suite/vst/StereoEncoder.so"
vstinfo gi_iem_stereo_encoder
prints "````````````````````````````````````````````````````\n"
prints "Then to the \"buss.\"\n"
gi_iem_omni_compressor vstinit "/usr/lib/x86\_64-linux-gnu/iem-plugin-suite/vst/OmniCompressor.so"
vstinfo gi_iem_omni_compressor
prints "````````````````````````````````````````````````````\n"
prints "Then to the room encoder (which does Doppler effects):\n"
gi_iem_room_encoder vstinit "/usr/lib/x86\_64-linux-gnu/iem-plugin-suite/vst/RoomEncoder.so"
vstinfo gi_iem_room_encoder
prints "````````````````````````````````````````````````````\n"
prints "Then to the FDN reverb:\n"
gi_iem_fdn_reverb vstinit "/usr/lib/x86\_64-linux-gnu/iem-plugin-suite/vst/FdnReverb.so"
vstinfo gi_iem_fdn_reverb
prints "````````````````````````````````````````````````````\n"
prints "Then to one of these outputs:\n"
gi_iem_simple_decoder vstinit "/usr/lib/x86\_64-linux-gnu/iem-plugin-suite/vst/SimpleDecoder.so"
vstinfo gi_iem_simple_decoder
gi_iem_allra_decoder vstinit "/usr/lib/x86\_64-linux-gnu/iem-plugin-suite/vst/AllRADecoder.so"
vstinfo gi_iem_allra_decoder
gi_iem_binaural_decoder vstinit "/usr/lib/x86\_64-linux-gnu/iem-plugin-suite/vst/BinauralDecoder.so"
vstinfo gi_iem_binaural_decoder
prints "====================================================\n"

prints "====================================================\n"
prints "SPARTA Suite:\n"
prints "----------------------------------------------------\n"
prints "Send N instruments to one channel of one of these...\n"
prints "````````````````````````````````````````````````````\n"
gi_sparta_ambi_enc vstinit "/home/mkg/.vst/libsparta_ambiENC.so"
vstinfo gi_sparta_ambi_enc
prints "...or to N of these (need 2 of these at 2nd order):\n"
prints "````````````````````````````````````````````````````\n"
gi_sparta_ambi_room_sim vstinit "/home/mkg/.vst/libsparta_ambiRoomSim.so"
vstinfo gi_sparta_ambi_room_sim
prints "Not sure if this makes sense or can be controlled with parameters.\n"
prints "````````````````````````````````````````````````````\n"
gi_compass_spatedit vstinit "/home/mkg/.vst/libcompass_spatedit.so"
vstinfo gi_compass_spatedit
prints "Then to this (N channels or binaural):\n"
prints "````````````````````````````````````````````````````\n"
gi_sparta_ambi_dec vstinit "/home/mkg/.vst/libsparta_ambiDEC.so"
vstinfo gi_sparta_ambi_dec
prints "Or this:\n"
prints "````````````````````````````````````````````````````\n"
gi_sparta_ambi_bin vstinit "/home/mkg/.vst/libsparta_ambiBIN.so"
vstinfo gi_sparta_ambi_bin
prints "Or this:\n"
prints "````````````````````````````````````````````````````\n"
gi_compass_decoder vstinit "/home/mkg/.vst/libcompass_decoder.so"
vstinfo gi_compass_decoder
prints "Or this (probably best for binaural):\n"
prints "````````````````````````````````````````````````````\n"
gi_compass_binaural vstinit "/home/mkg/.vst/libcompass_binaural.so"
vstinfo gi_compass_binaural
prints "====================================================\n"


#ifdef SPATIALIZE_GOGINS
#include "Spatialize3D.inc"

gi_Spatialize3D_room_table ftgen 1, 0, 64, -2,                                               \                                       
/*  depth1  depth2  max delay  ir length  idist  seed                               */ \
    3,      0,      -1,        0,         0,     123,                                  \
/*  used   distance  random  reflection  eq hz   eq level  eq q  eq mode            */ \
    1,     20,       0.05,   0.87,       4000.0, 0.6,      0.7,  2,      /* ceiling */ \
    1,     20,       0.05,   0.87,       3500.0, 0.5,      0.7,  2,      /* floor   */ \
    1,     20,       0.05,   0.87,       5000.0, 0.8,      0.7,  2,      /* front   */ \
    1,     20,       0.05,   0.87,       5000.0, 0.8,      0.7,  2,      /* back    */ \
    1,     20,       0.05,   0.87,       5000.0, 0.8,      0.7,  2,      /* right   */ \
    1,     20,       0.05,   0.87,       5000.0, 0.8,      0.7,  2       /* left    */

#end
#ifdef SPATIALIZE_STEREO

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
// These are all the Csound instruments and effects used in this piece.
//////////////////////////////////////////////////////////////////////////////

opcode instrument_position, kk, iii
i_onset, i_radius, i_rate xin
i_rate = (i_rate /3) + (i_rate / 66)
k_time times
// Depth.
k_x = i_radius * cos(i_onset + ((k_time - i_onset) * i_rate))
// Pan.
k_y = i_radius * sin(i_onset + ((k_time - i_onset) * i_rate))
xout k_x, k_y
endop

gi_Pianoteq vstinit "/home/mkg/Pianoteq\ 7/x86-64bit/Pianoteq\ 7.so", 1
#include "PianoNotePianoteq.inc"
#include "FMWaterBell.inc"
#include "Phaser.inc"
#include "Droner.inc"
#include "Sweeper.inc"
#include "Buzzer.inc"
#include "Shiner.inc"
#include "Blower.inc"
#include "ZakianFlute.inc"
#include "STKBowed.inc"

#include "PianoOutPianoteq.inc"
alwayson "PianoOutPianoteq"

#ifdef SPATIALIZE_STEREO
#include "ReverbSC.inc"
alwayson "ReverbSC"
#include "MasterOutput.inc"
alwayson "MasterOutput"
#end

#ifdef SPATIALIZE_IEM

// TODO: One room encoder for each instrument?
// Possibly, use reverb?
// Only need binaural decoder for now.

instr SpatializeIEM 
a_iem_encoder_in[] init 64
a_iem_encoder_in inletv "iem_in"
k_1 rms a_iem_encoder_in[0]
k_2 rms a_iem_encoder_in[1]
k_3 rms a_iem_encoder_in[2]
printks "SpatializeIEM: a_iem_encoder_in: %9.4f %9.4f %9.4f\n", .5, k_1, k_2, k_3
a_iem_encoder_out[] init 64
vstparamset gi_iem_multi_encoder, 0, 10
a_iem_encoder_out vstaudio gi_iem_multi_encoder, a_iem_encoder_in
a_iem_reverb_out[] init 64
a_iem_reverb_out vstaudio gi_iem_fdn_reverb, a_iem_encoder_out
a_iem_decoder_out[] init 64
a_iem_decoder_out vstaudio gi_iem_binaural_decoder, a_iem_reverb_out
k_left rms a_iem_decoder_out[0]
k_right rms a_iem_decoder_out[1]
printks "%-24.24s i %9.4f t %9.4f d %9.4f l %9.4f r %9.4f #%3d\n", 1, nstrstr(p1), p1, p2, p3, k_left, k_right, p7, active(p1)
out a_iem_decoder_out
endin
alwayson "SpatializeIEM"
alwayson "PianoOutPianoteq"

connect "Blower", "iem_out", "SpatializeIEM", "iem_in"
connect "STKBowed", "iem_out", "SpatializeIEM", "iem_in"
connect "Buzzer", "iem_out", "SpatializeIEM", "iem_in"
connect "Droner", "iem_out", "SpatializeIEM", "iem_in"
connect "FMWaterBell", "iem_out", "SpatializeIEM", "iem_in"
connect "Phaser", "iem_out", "SpatializeIEM", "iem_in"
connect "PianoOutPianoteq", "iem_out", "SpatializeIEM", "iem_in"
connect "Sweeper", "iem_out", "SpatializeIEM", "iem_in"
connect "Shiner", "iem_out", "SpatializeIEM", "iem_in"
connect "ZakianFlute", "iem_out", "SpatializeIEM", "iem_in"
#end

#ifdef SPATIALIZE_AALTO
instr SpatializeAALTO
a_aalto_encoder_in[] init 64
a_aalto_encoder_in inletv "aalto_in"
a_aalto_encoder_out[] init 64
// Ambisonic order (3 in {1,...,7}).
vstparamset gi_sparta_ambi_room_sim, 0, (3. / 7.001)
// Channel order (1 in {1, 2}).
vstparamset gi_sparta_ambi_room_sim, 1, (1. / 2.001)
// Normalization type (2 in {1, 2, 3}/
vstparamset gi_sparta_ambi_room_sim, 2, (2. / 3.001)
// Number of sources (3).
vstparamset gi_sparta_ambi_room_sim, 3, (3. / 16.001)
// Number of receivers (1).
vstparamset gi_sparta_ambi_room_sim, 4, (1. / 16.001)
// Receiver coordinates (center of room).
vstparamset gi_sparta_ambi_room_sim, 53, .5
vstparamset gi_sparta_ambi_room_sim, 54, .5
vstparamset gi_sparta_ambi_room_sim, 55, .5

a_aalto_encoder_out vstaudio gi_sparta_ambi_room_sim, a_aalto_encoder_in
k_left rms a_aalto_encoder_out[0]
k_right rms a_aalto_encoder_out[1]
printks "%-24.24s i %9.4f t %9.4f d %9.4f a %9.4f e %9.4f #%3d\n", 1, nstrstr(p1), p1, p2, p3, k_left, k_right, p7, active(p1)
a_aalto_decoder_out[] init 64
a_iem_reverb_out[] init 64
a_iem_reverb_out vstaudio gi_iem_fdn_reverb, a_aalto_encoder_out
; // Sadly, the source code for the "COMPASS" plugins, which sound markedly 
; // better, could not be found; therefore, VST normalizations could not be 
; // determined; using the SPARTA ambiBIN decoder instead.
; // Ambisonic order.
; vstparamset gi_compass_binaural, 0, (3. / 7.001)
; // Channel order.
; vstparamset gi_compass_binaural, 1, (1. / 2.001)
; // Covariance averaging.
; vstparamset gi_compass_binaural, 2, .88
; // Synth av.
; vstparamset gi_compass_binaural, 3, .88
; // Balance (diff-dir).
; vstparamset gi_compass_binaural, 4, .5
; // Balance (linear-par).
; vstparamset gi_compass_binaural, 5, 1.
; // Enable rotation.
; vstparamset gi_compass_binaural, 6, 0.
; a_aalto_decoder_out vstaudio gi_compass_binaural, a_aalto_encoder_out
// Ambisonic order (3 in {1,...,7}).
vstparamset gi_sparta_ambi_bin, 0, (3. / 7.001)
// Channel order (1 in {1, 2}).
vstparamset gi_sparta_ambi_bin, 1, (1. / 2.001)
// Normalization type (2 in {1, 2, 3}/
vstparamset gi_sparta_ambi_bin, 2, (2. / 3.001)
// Decode method (5 in {1, 2, 3, 4, 5}
vstparamset gi_sparta_ambi_bin, 3, 1.
// Apply diff match
vstparamset gi_sparta_ambi_bin, 4, 0.
// Apply maxre weights
vstparamset gi_sparta_ambi_bin, 5, 1.
// Apply enable rotation
vstparamset gi_sparta_ambi_bin, 6, 0.
a_aalto_decoder_out vstaudio gi_sparta_ambi_bin, a_iem_reverb_out
out a_aalto_decoder_out
k_left rms a_aalto_decoder_out[0]
k_right rms a_aalto_decoder_out[1]
printks "%-24.24s i %9.4f t %9.4f d %9.4f l %9.4f r %9.4f #%3d\n", 1, nstrstr(p1), p1, p2, p3, k_left, k_right, p7, active(p1)
endin
alwayson "SpatializeAALTO"

connect "Blower", "aalto_out", "SpatializeAALTO", "aalto_in"
connect "STKBowed", "aalto_out", "SpatializeAALTO", "aalto_in"
connect "Buzzer", "aalto_out", "SpatializeAALTO", "aalto_in"
connect "Droner", "aalto_out", "SpatializeAALTO", "aalto_in"
connect "FMWaterBell", "aalto_out", "SpatializeAALTO", "aalto_in"
connect "Phaser", "aalto_out", "SpatializeAALTO", "aalto_in"
connect "PianoOutPianoteq", "aalto_out", "SpatializeAALTO", "aalto_in"
connect "Sweeper", "aalto_out", "SpatializeAALTO", "aalto_in"
connect "Shiner", "aalto_out", "SpatializeAALTO", "aalto_in"
connect "ZakianFlute", "aalto_out", "SpatializeAALTO", "aalto_in"
#end

gk_PianoOutPianoteq_front_to_back init -3
gk_PianoOutPianoteq_left_to_right init .5
gk_PianoOutPianoteq_bottom_to_top init 3

gk_FMWaterBell_front_to_back init -3
gk_FMWaterBell_left_to_right init 1
gk_FMWaterBell_bottom_to_top init -3


//////////////////////////////////////////////////////////////////////////////
// These define the initial values of all the global variables/control 
// channels that can be controlled from the Web page.
//////////////////////////////////////////////////////////////////////////////

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
gk_ReverbSC_feedback init 0.86
gk_MasterOutput_level init 46.
gi_FMWaterBell_attack init 0.002936276551436901
gi_FMWaterBell_release init 0.022698875468554768
gi_FMWaterBell_exponent init 0
gi_FMWaterBell_sustain init 5.385256143273636
gi_FMWaterBell_sustain_level init 0.08267388588088297
gk_FMWaterBell_crossfade init 0.1234039047697504
gk_FMWaterBell_index init 1.1401499375260309
gk_FMWaterBell_vibrato_depth init 0.28503171595683335
gk_FMWaterBell_vibrato_rate init 2.4993821566850647
gk_FMWaterBell_level init 22.
gk_Phaser_ratio1 init 1.0388005601779389
gk_Phaser_ratio2 init 3.0422604827415767
gk_Phaser_index1 init 0.5066315182469726
gk_Phaser_index2 init 0.5066315182469726
gk_Phaser_level init 8.25438668753604
gk_STKBowed_vibrato_level init 0
gk_STKBowed_bow_pressure init 110
gk_STKBowed_bow_position init 20
gk_STKBowed_vibrato_frequency init 50.2
gk_STKBowed_level init 0
gk_Droner_partial1 init 0.11032374600527997
gk_Droner_partial2 init 0.4927052938724468
gk_Droner_partial3 init 0.11921634014172572
gk_Droner_partial4 init 0.06586077532305128
gk_Droner_partial5 init 0.6616645824649159
gk_Droner_level init 18.563508886352523
gk_Sweeper_britel init 0.4258927115604109
gk_Sweeper_briteh init 3.635884339731444
gk_Sweeper_britels init 1.1354964943746944
gk_Sweeper_britehs init 3.222566443828469
gk_Sweeper_level init 7.606391651720202
gk_Buzzer_harmonics init 11.958151412801714
gk_Buzzer_level init 23.61650089678787
gk_Shiner_level init 22.3642589271156
gk_Blower_grainDensity init 132.3332789825534
gk_Blower_grainDuration init 0.2854231208217838
gk_Blower_grainAmplitudeRange init 174.0746779716289
gk_Blower_grainFrequencyRange init 62.82406652535464
gk_Blower_level init 6.562856676993313
gk_ZakianFlute_level init 25.125628140703512
gk_PianoOutPianoteq_level init -44.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
gk_ReverbSC_feedback init 0.86
gk_MasterOutput_level init 46
gi_FMWaterBell_attack init 0.002936276551436901
gi_FMWaterBell_release init 0.022698875468554768
gi_FMWaterBell_exponent init 0
gi_FMWaterBell_sustain init 5.385256143273636
gi_FMWaterBell_sustain_level init 0.08267388588088297
gk_FMWaterBell_crossfade init 0.1234039047697504
gk_FMWaterBell_index init 1.1401499375260309
gk_FMWaterBell_vibrato_depth init 0.28503171595683335
gk_FMWaterBell_vibrato_rate init 2.4993821566850647
gk_FMWaterBell_level init 22
gk_Phaser_ratio1 init 1.0388005601779389
gk_Phaser_ratio2 init 3
gk_Phaser_index1 init 0.5
gk_Phaser_index2 init 1
gk_Phaser_level init 6
gk_STKBowed_vibrato_level init 0
gk_STKBowed_bow_pressure init 110
gk_STKBowed_bow_position init 20
gk_STKBowed_vibrato_frequency init 50.2
gk_STKBowed_level init 0
gk_Droner_partial1 init 0.11032374600527997
gk_Droner_partial2 init 0.4927052938724468
gk_Droner_partial3 init 0.11921634014172572
gk_Droner_partial4 init 0.06586077532305128
gk_Droner_partial5 init 0.6616645824649159
gk_Droner_level init 18.563508886352523
gk_Sweeper_britel init 0.4258927115604109
gk_Sweeper_briteh init 3.635884339731444
gk_Sweeper_britels init 1.1354964943746944
gk_Sweeper_britehs init 3.222566443828469
gk_Sweeper_level init 7.606391651720202
gk_Buzzer_harmonics init 11.958151412801714
gk_Buzzer_level init 23.61650089678787
gk_Shiner_level init 22.3642589271156
gk_Blower_grainDensity init 132.3332789825534
gk_Blower_grainDuration init 0.2854231208217838
gk_Blower_grainAmplitudeRange init 174.0746779716289
gk_Blower_grainFrequencyRange init 62.82406652535464
gk_Blower_level init 6.562856676993313
gk_ZakianFlute_level init 25.125628140703512
gk_PianoOutPianoteq_level init -38
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
gk_ReverbSC_feedback init 0.86
gk_MasterOutput_level init 46
gi_FMWaterBell_attack init 0.002936276551436901
gi_FMWaterBell_release init 0.022698875468554768
gi_FMWaterBell_exponent init 0
gi_FMWaterBell_sustain init 5.385256143273636
gi_FMWaterBell_sustain_level init 0.08267388588088297
gk_FMWaterBell_crossfade init 0.1234039047697504
gk_FMWaterBell_index init 1.1401499375260309
gk_FMWaterBell_vibrato_depth init 0.28503171595683335
gk_FMWaterBell_vibrato_rate init 2.4993821566850647
gk_FMWaterBell_level init 21
gk_Phaser_ratio1 init 1.0388005601779389
gk_Phaser_ratio2 init 3
gk_Phaser_index1 init 0.5
gk_Phaser_index2 init 1
gk_Phaser_level init 6
gk_STKBowed_vibrato_level init 0
gk_STKBowed_bow_pressure init 110
gk_STKBowed_bow_position init 20
gk_STKBowed_vibrato_frequency init 50.2
gk_STKBowed_level init 0
gk_Droner_partial1 init 0.19517365074188814
gk_Droner_partial2 init 0.2577857492255014
gk_Droner_partial3 init 0.11169085276373716
gk_Droner_partial4 init 0.15343225175281267
gk_Droner_partial5 init 0.4456220446763411
gk_Droner_level init 27.433556171531066
gk_Sweeper_britel init 0.4258927115604109
gk_Sweeper_briteh init 3.635884339731444
gk_Sweeper_britels init 1.1354964943746944
gk_Sweeper_britehs init 3.222566443828469
gk_Sweeper_level init 7.606391651720202
gk_Buzzer_harmonics init 6.61666394912767
gk_Buzzer_level init 0
gk_Shiner_level init -16.91668025436165
gk_Blower_grainDensity init 132.3332789825534
gk_Blower_grainDuration init 0.2854231208217838
gk_Blower_grainAmplitudeRange init 174.0746779716289
gk_Blower_grainFrequencyRange init 62.82406652535464
gk_Blower_level init 6.562856676993313
;gk_ZakianFlute_level init 25.125628140703512
gk_ZakianFlute_level init 23.
;gk_PianoOutPianoteq_level init -30
gk_PianoOutPianoteq_level init 37
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

gi_Spatialize3D_speaker_rig init 31

//////////////////////////////////////////////////////////////////////////////
// This instrument defines a WebKit browser embedded in Csound. The following 
// HTML5 code is pretty much the standard sort of thing for Web pages.
//
// However, the <csound.js> script brings a proxy for the instance of Csound 
// that is performing into the JavaScript context of the Web page, so the 
// event handlers of the sliders on the page can call Csound to set control 
// channel values.
//////////////////////////////////////////////////////////////////////////////
instr Browser
gS_html init {{<!DOCTYPE html>
<html>
<head>
    <title>Red Leaves version 1</title>
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
    <script src="csound.js"></script>    
</head>
<body style="background-color:black;box-sizing:border-box;padding:10px;:fullscreen">
    <canvas id="canvas" style="block;height:100vh;width:100vw">
    </canvas>
    <script>
        //////////////////////////////////////////////////////////////////////
        // This is the JSON-RPC proxy for the instance of Csound that is 
        // performing this piece.
        //////////////////////////////////////////////////////////////////////
        csound = new Csound("http://localhost:8383");
        //////////////////////////////////////////////////////////////////////
        // This hooks up JavaScript code for displaying a 3-dimensional piano 
        // roll display of the algorithmically generated score.
        //////////////////////////////////////////////////////////////////////
        var canvas = document.getElementById("canvas");
        var piano_roll = new PianoRoll.PianoRoll3D(canvas);
        let score_time = 0;
        let video_frame = 0;
        //////////////////////////////////////////////////////////////////////
        // The piano roll display is animated (a) to show a red ball that 
        // follows the score, and (b) to enable the use of the mouse or 
        // trackball to rotate, and zoom around in, the piano roll display.
        //////////////////////////////////////////////////////////////////////
        function animate() {
            //////////////////////////////////////////////////////////////////
            // By rendering the visual display only on every nth video frame, 
            // more time is given to Csound's audio rendering.
            //////////////////////////////////////////////////////////////////
            if (video_frame % 3 == 0) {
                //////////////////////////////////////////////////////////////
                // This is the pattern for obtaining return values for all of 
                // the Csound API's asynchronous JSON-RPC methods: by 
                // callback.
                //////////////////////////////////////////////////////////////
                csound.GetScoreTime(function(id, result) {
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
        
        //////////////////////////////////////////////////////////////////////
        // This is a stub that will be replaced with the actual code after all
        // the dat.gui controls have been created.
        //////////////////////////////////////////////////////////////////////
        var save_controls = function() {
        }

        var recenter = function() {
            piano_roll.draw3D(canvas);
        }
        
        //////////////////////////////////////////////////////////////////////
        // These are the controllers and commands that are initialized in the 
        // dat.gui controls. The controllers must have the same names and 
        // types as the Csound orchestra's control channels/global variables, 
        // but the initial values actually come from the CSound channels.
        //////////////////////////////////////////////////////////////////////
        var parameters = {
            gk_ReverbSC_feedback: 0.94060466265755,
            gk_MasterOutput_level: 23.199086906897115,
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
            gk_Sweeper_britel: 0.43034846362962353,
            gk_Sweeper_briteh: 3.635884339731444,
            gk_Sweeper_britels: 1.801136831699481,
            gk_Sweeper_britehs: 3.572617184282066,
            gk_Sweeper_level: 20.486036741082465,
            gk_Buzzer_harmonics: 11.958151412801714,
            gk_Buzzer_level: 23.61650089678787,
            gk_Shiner_level: 22.3642589271156,
            gk_Blower_grainDensity: 79.99177885109444,
            gk_Blower_grainDuration: 0.2,
            gk_Blower_grainAmplitudeRange: 87.88408180043162,
            gk_Blower_grainFrequencyRange: 30.596081700708627,
            gk_Blower_level: 7.754769280939186,
            gk_ZakianFlute_level: 25.125628140703512,
            gk_PianoOutPianoteq_level: -44,
            save_controls: save_controls,
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
        // storage, hence no "gui.remember()."
        //////////////////////////////////////////////////////////////////////
        window.onload = function() {
            gui = new dat.GUI({load: parameters, width: 500});
            gui.add(parameters, 'save_controls').name('Save control values [Ctrl-S]');
            gui.add(parameters, 'recenter').name('Re-center piano roll [Ctrl-C]');
            var Master = gui.addFolder('Master');
            add_slider(Master, 'gk_ReverbSC_feedback', 0, 1);
            add_slider(Master, 'gk_MasterOutput_level', -50, 50);
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
            var STKBowed = gui.addFolder('STKBowed');
            add_slider(STKBowed, 'gk_STKBowed_vibrato_level', 0, 127);
            add_slider(STKBowed, 'gk_STKBowed_bow_pressure', 0, 127);
            add_slider(STKBowed, 'gk_STKBowed_bow_position', 0, 127);
            add_slider(STKBowed, 'gk_STKBowed_vibrato_frequency', 0, 127);
            add_slider(STKBowed, 'gk_STKBowed_level', -50, 50);
            var Droner = gui.addFolder('Droner');
            add_slider(Droner, 'gk_Droner_partial1', 0, 1);
            add_slider(Droner, 'gk_Droner_partial2', 0, 1);
            add_slider(Droner, 'gk_Droner_partial3', 0, 1);
            add_slider(Droner, 'gk_Droner_partial4', 0, 1);
            add_slider(Droner, 'gk_Droner_partial5', 0, 1);
            add_slider(Droner, 'gk_Droner_level', -50, 50);
            var Sweeper = gui.addFolder('Sweeper');
            add_slider(Sweeper, 'gk_Sweeper_britel', 0, 4);
            add_slider(Sweeper, 'gk_Sweeper_briteh', 0, 4);
            add_slider(Sweeper, 'gk_Sweeper_britels', 0, 4);
            add_slider(Sweeper, 'gk_Sweeper_britehs', 0, 4);
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
            var Flute = gui.addFolder('Zakian Flute');
            add_slider(Flute, 'gk_ZakianFlute_level', -50, 50);
            var Pianoteq = gui.addFolder('Pianoteq');
            add_slider(Pianoteq, 'gk_PianoOutPianoteq_level', -50, 50);
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
                    console.log(`parameter ${key} = ${value} (${typeof value})`);
                    csound.GetControlChannel(key, function(id, value_) {
                        parameters[key] = value_;
                        console.log(`Initialized gui: ${key} = ${value_}`);
                    });
                }
            };
            console.log("Updated widgets with Csound control values.");
            //////////////////////////////////////////////////////////////////////
            // When the user clicks on the "Save control values" command, the 
            // current state of the control parameters is printed to the terminal
            // in the form of Csound orchestra code. These can be copied from the 
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
                        ///console.log(`parameter: ${key} = ${value}`);
                        let line = `${key} init ${value}\n`;
                        text = text + line;
                    }
                };
                text = text + delimiter;
                ///navigator.clipboard.writeText(text);
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

gi_browser webkit_create 8383, 0
//////////////////////////////////////////////////////////////////////////////
// The following lines find the current working directory from Csound, 
// and then use that to construct the base URI for the Web page.
//////////////////////////////////////////////////////////////////////////////
S_pwd pwd
S_base_uri sprintf "file://%s/", S_pwd
prints S_base_uri
webkit_open_html gi_browser, "Red Leaves version 8", gS_html, S_base_uri, 12000, 10000, 0
endin
alwayson "Browser"

//////////////////////////////////////////////////////////////////////////////
// The following C++ code defines and executes a score generator that 
// implements the "multiple copy reducing machine" algorithm for computing an 
// iterated function system (IFS).
//
// Only a limited number of iterations are computed. On the final iteration, 
// each point in the attractor of the IFS is translated to a single note of 
// music.
//
// This code uses only the Eigen 3 header file-only library and the C++ 
// standard library.
//////////////////////////////////////////////////////////////////////////////

S_score_generator_code init {{

#include <Eigen/Dense>
#include <csound.h>
#include <csound/csdl.h>
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
// LLVM startup code. Therefore, we define it here.
//////////////////////////////////////////////////////////////////////////////
void* __dso_handle = (void *)&__dso_handle;

static bool diagnostics_enabled = false;

extern "C" { 

//////////////////////////////////////////////////////////////////////////////
// This function is _defined_ in the WebKit opcodes shared library, but we 
// must _declare_ it here so that our C++ code can call it.
//////////////////////////////////////////////////////////////////////////////
void webkit_run_javascript(CSOUND *csound, int browser_handle, std::string javascript_code);

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

//////////////////////////////////////////////////////////////////////////////
// This is the entry point for this C++ module. It will be called by Csound 
// immediately once the module has been compiled and linked.
//////////////////////////////////////////////////////////////////////////////
extern "C" int score_generator(CSOUND *csound) {
    int result = OK;
    csound::ScoreModel model;
    std::map<double, csound::Chord> chordsForTimes;
    csound::Chord modality;
    Cursor pen;
    modality.fromString("0 4 7 11 14");
    pen.chord = modality;
    pen.note = {1,35,144,1,1,1,0,0,0,0,1};
    int base_level = 1;
    std::vector<std::function<Cursor(const Cursor &, int, csound::Score &)>> generators;
    auto g1 = [&chordsForTimes, &modality, &base_level](const Cursor &pen_, int depth, csound::Score &score) {
        Cursor pen = pen_;
        if ((depth + base_level) == 4) {
            pen.chord = pen.chord.T(4);
            chordsForTimes[pen.note.getTime()] = pen.chord;
        }
        pen.note[csound::Event::TIME] = (pen.note[csound::Event::TIME] * .5) + (0 - 5);
        pen.note[csound::Event::KEY] = (pen.note[csound::Event::KEY] * .75);
        return pen;
    };
    generators.push_back(g1);
    auto g2 = [&chordsForTimes, &modality, &base_level](const Cursor &pen_, int depth, csound::Score &score) {
        Cursor pen = pen_;
        if ((depth + base_level) == 3) {
            pen.chord = pen.chord.K();
            chordsForTimes[pen.note.getTime()] = pen.chord;
        }
        if ((depth + base_level) == 6) {
            ///pen.chord = pen.chord.Q(-1, modality);
            ///pen.chord = pen.chord.T(-1);
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
        pen.note[csound::Event::INSTRUMENT] = std::cos(pen.note[csound::Event::TIME]);
        pen.note[csound::Event::VELOCITY] =  std::cos(pen.note[csound::Event::TIME]);
        return pen;
    };
    generators.push_back(g3);
    auto g4 = [&chordsForTimes, &modality, &base_level](const Cursor &pen_, int depth, csound::Score &score) {
        Cursor pen = pen_;
        pen.note[csound::Event::TIME] = (pen.note[csound::Event::TIME] * .5) + (1000 - 5);
        pen.note[csound::Event::KEY] = (pen.note[csound::Event::KEY] * .77) + 1.;
        pen.note[csound::Event::INSTRUMENT] = std::sin(pen.note[csound::Event::TIME]);
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
    std::cout << "Generated duration:     " << score.getDuration() << std::endl;
    //////////////////////////////////////////////////////////////////////////////
    // We apply the chords that were generated along WITH the notes, TO the notes.
    // This creates an algorithmically generated chord progression.
    //////////////////////////////////////////////////////////////////////////////
    score.sort();
    score.rescale(csound::Event::KEY, true, 24.0, true,  72.0);
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
        //~ std::fprintf(stderr, "Before:\\n");
        //~ for (int i = 0, n = segment.size(); i < n; ++i) {
            //~ std::fprintf(stderr, "  %s\\n", segment[i]->toString().c_str());
        //~ }  
        }
        csound::apply(score, chord, startTime, endTime, true);
        //~ std::fprintf(stderr, "After:\\n");
        //~ for (int i = 0, n = segment.size(); i < n; ++i) {
            //~ std::fprintf(stderr, "  %s\\n", segment[i]->toString().c_str());
        //~ }  
        endTime = startTime;
    }
    std::cout << "Conformed notes:        " << size << std::endl;
    score.rescale(csound::Event::TIME,          true,  0.0, false,  0.0);
    score.rescale(csound::Event::INSTRUMENT,    true,  1.0, true,   9.999);
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
    score.rescale(csound::Event::TIME,          true,  1.0, false,  0.0);
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
    // Here, the PianoRoll3D instance is called to render our JSON score on 
    // that canvas as an animated, three-dimensional piano roll. Note the use 
    // of a JavaScript template string (delimited by `) to hold the multi-line 
    // JSON text with its embedded quotation marks and such.
    //////////////////////////////////////////////////////////////////////////
    std::string javascript_code = "piano_roll.fromJson(`" + json_score + "`);";
    //std::fprintf(stderr, "javascript_code: %s\\n", javascript_code.c_str());
    webkit_run_javascript(csound, 0, javascript_code);
    return result;
};

};

}}

//////////////////////////////////////////////////////////////////////////////
// This compiles the above C++ module and then calls its entry point function.
// Note that dynamic link libraries must be passed as complete filepaths.
//////////////////////////////////////////////////////////////////////////////
i_result clang_compile "score_generator", S_score_generator_code, "-g -O2 -std=c++17 -I/home/mkg/clang-opcodes -I/home/mkg/csound-extended/CsoundAC -I/usr/local/include/csound -I/user/local/eigen3 -stdlib=libstdc++", "/usr/lib/libCsoundAC.so.6.0 /usr/lib/gcc/x86_64-linux-gnu/9/libstdc++.so /usr/lib/gcc/x86_64-linux-gnu/9/libgcc_s.so /home/mkg/webkit-opcodes/webkit_opcodes.so /usr/lib/x86_64-linux-gnu/libm.so /usr/lib/x86_64-linux-gnu/libpthread.so"

instr Exit
prints "exitnow i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
exitnow
endin

</CsInstruments>
<CsScore>
; f 0 does not work here, we actually need to schedule an instrument that 
; turns off Csound.
; a 0 1 270
i "Exit" [12 * 60 + 5]
;f 0 [6 * 60 + 5]
</CsScore>
</CsoundSynthesizer>
