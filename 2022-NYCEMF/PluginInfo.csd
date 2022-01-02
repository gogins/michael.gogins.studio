<CsoundSynthesizer>
<CsLicense>
Copyright (C) 2013 by Michael Gogins.
All rights reserved.
</CsLicense>
<CsOptions>
-+msg_color=0 -odac -m3 -d 
</CsOptions>
<CsInstruments>
sr = 48000
ksmps = 128
nchnls = 2
0dbfs = 10

// Define just one of these.
;#define SPATIALIZE_STEREO #1#
;#define SPATIALIZE_IEM #2#
#define SPATIALIZE_AALTO #3#
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
opcode iem_cartesian_to_spherical, kkk, ikkk
i_maximum_radius, k_x, k_y, k_z xin
; Calculate the distance from the origin (the listener).
k_radius = sqrt(k_x^2 + k_y^2 + k_z^2)
; Normalize the coordinates to unit vectors from the origin.
k_x = k_x / k_radius
k_y = k_y / k_radius
k_z = k_z / k_radius
k_azimuth taninv2 k_y, k_x
k_elevation taninv2 k_z, sqrt(k_x^2 + k_y^2)
xout k_azimuth, k_elevation, k_radius
endop

// Converts IEM spherical coordinates to VST parameter ranges.
opcode iem_normalize_spherical_coordinates, kkk, ikkk
i_maximum_radius, k_azimuth, k_elevation, k_radius xin
k_normalized_azimuth = (k_azimuth / (2 * gi_pi)) + .5
k_normalized_elevation = (k_elevation / (2 * gi_pi)) + .5
k_normalized_radius = i_maximum_radius / k_radius
xout k_normalized_azimuth, k_normalized_elevation, k_normalized_radius
endop

// Converts IEM Cartesian coordinates to IEM 
// spherical coordinates that are normalized to VST parameter ranges.
opcode iem_cartesian_to_spherical_vst,kkk,ikkk
i_maximum_radius, k_x, k_y, k_z xin
k_azimuth, k_elevation, k_radius iem_cartesian_to_spherical i_maximum_radius, k_x, k_y, k_z
k_azimuth_vst, k_elevation_vst, k_radius_vst iem_normalize_spherical_coordinates i_maximum_radius, k_azimuth, k_elevation, k_radius
printks "i: %3d azimuth: %9.4f elevation: %9.4f radius: %9.4f\n", 1, p1, k_azimuth_vst, k_elevation_vst, k_radius_vst
xout k_azimuth_vst, k_elevation_vst, k_radius_vst
endop

// Are the SPARTA and IEM Ambisonic conventions the same? Both seem to use SN3D
// but IEM uses azimuth, elevation, radius but SPARTA only seems to use 
// azimuth, elevation.

// Presumably, N of the 64 channels are assigned and used in order, 
// and unused channels are left as 0.

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
#include "Spatialize.inc"
gk_BformatDecoder_SpeakerRig init 5

alwayson "SpatialReverb"
alwayson "BformatDecoder"
#else 
instr 1
endin
instr 2 
endin
instr 3 
endin
instr 4
endin
#end

gi_size init 1

// Need 3 instruments of distinct sound to exercise the spatializers.

// An electric piano will sit in the middle.

gk_Rhodes_level chnexport "gk_Rhodes_level", 3 ;  0
gk_Rhodes_level init 20

gi_Rhodes_sine ftgen 0, 0, 65537, 10, 1
gi_Rhodes_cosine ftgen 0, 0, 65537, 11, 1
gi_Rhodes_blank ftgen 0, 0, 65537, 10, 0 ; Blank wavetable for some Cook FM opcodes.

instr Rhodes
; Authors: Perry Cook, John ffitch, Michael Gogins
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
k_time times
k_space_front_to_back = sin(k_time / p1)
k_space_left_to_right = cos(k_time / p1)
k_space_bottom_to_top = 1
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_overall_amps = 82
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Rhodes_level)
iindex = 4
icrossfade = 3
ivibedepth = 0.2
iviberate = 6
ifn1 = gi_Rhodes_sine
ifn2 = gi_Rhodes_cosine
ifn3 = gi_Rhodes_sine
ifn4 = gi_Rhodes_blank
ivibefn = gi_Rhodes_sine
a_signal fmrhode i_amplitude, i_frequency, iindex, icrossfade, ivibedepth, iviberate, ifn1, ifn2, ifn3, ifn4, ivibefn
i_attack = .002
i_sustain = p3
i_release = 0.01
xtratim i_attack + i_release
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_signal * a_declicking
k_rms rms a_signal
printks "Rhodes rms:      %9.4f\n", .5, k_rms

prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
#ifdef SPATIALIZE_GOGINS 
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatializer a_signal, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#end
#ifdef SPATIALIZE_STEREO
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
#end
#ifdef SPATIALIZE_IEM
a_iem_out[] init 64
k_azimuth, k_elevation, k_radius iem_cartesian_to_spherical_vst gi_size, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
i_index init floor(p1) - 5
i_azimuth =     7 + i_index * 5 + 0
i_elevation =   7 + i_index * 5 + 1
i_gain =        7 + i_index * 5 + 2
prints "i_index: %3d i_azimuth: %3d i_elevation: %3d i_gain: %3d\n", i_index, i_azimuth, i_elevation, i_gain
vstparamset gi_iem_multi_encoder, i_azimuth, k_azimuth
vstparamset gi_iem_multi_encoder, i_elevation, k_elevation
vstparamset gi_iem_multi_encoder, i_gain, k_gain
a_iem_out[i_index] = a_signal
outletv "iem_out", a_iem_out
#end
#ifdef SPATIALIZE_AALTO
a_aalto_out[] init 64
a_aalto_out[0] = a_signal
vstparamset gi_sparta_ambi_room_sim, 5, k_space_front_to_back
vstparamset gi_sparta_ambi_room_sim, 6, k_space_left_to_right
vstparamset gi_sparta_ambi_room_sim, 7, k_space_front_to_back
outletv "aalto_out", a_aalto_out
#end
endin

gk_STKBeeThree_level chnexport "gk_STKBeeThree_level", 3 ;  0

gk_STKBeeThree_level init 0

instr STKBeeThree
; Authors: Michael Gogins
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
k_time times
k_space_front_to_back = sin(k_time / p1)
k_space_left_to_right = cos(k_time / p1)
k_space_bottom_to_top = 0
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_overall_amps = 64
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_STKBeeThree_level)
asignal STKBeeThree i_frequency, 1.0, 1, 1.5, 2, 4.8, 4, 2.1
; ares phaser1 asig, kfreq, kord, kfeedback [, iskip]
a_signal phaser1 asignal, 8000, 16, .2, .9
i_attack = .002
i_sustain = p3
i_release = 0.01
xtratim i_attack + i_release
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_signal * i_amplitude * a_declicking * k_gain * .75
k_rms rms a_signal
printks "STKBeeThree rms: %9.4f\n", .5, k_rms

prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
#ifdef SPATIALIZE_GOGINS 
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatializer a_signal, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#end
#ifdef SPATIALIZE_STEREO
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
#end
#ifdef SPATIALIZE_IEM
a_iem_out[] init 64
k_azimuth, k_elevation, k_radius iem_cartesian_to_spherical_vst gi_size, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
i_index init floor(p1) - 5
i_azimuth =     7 + i_index * 5 + 0
i_elevation =   7 + i_index * 5 + 1
i_gain =        7 + i_index * 5 + 2
prints "i_index: %3d i_azimuth: %3d i_elevation: %3d i_gain: %3d\n", i_index, i_azimuth, i_elevation, i_gain
vstparamset gi_iem_multi_encoder, i_azimuth, k_azimuth
vstparamset gi_iem_multi_encoder, i_elevation, k_elevation
vstparamset gi_iem_multi_encoder, i_gain, k_gain
a_iem_out[i_index] = a_signal
outletv "iem_out", a_iem_out
#end
#ifdef SPATIALIZE_AALTO
a_aalto_out[] init 64
a_aalto_out[0] = a_signal
vstparamset gi_sparta_ambi_room_sim, 8, k_space_front_to_back
vstparamset gi_sparta_ambi_room_sim, 9, k_space_left_to_right
vstparamset gi_sparta_ambi_room_sim, 10, k_space_front_to_back
outletv "aalto_out", a_aalto_out
#end
endin

gk_Guitar_midi_dynamic_range chnexport "gk_Guitar_midi_dynamic_range", 3 ; 127
gk_Guitar_midi_dynamic_range init 127
gk_Guitar_level chnexport "gk_Guitar_level", 3
gk_Guitar_level init 10

instr Guitar
; Michael Gogins
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_Guitar_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
k_time times
k_space_front_to_back = sin(k_time / p1)
k_space_left_to_right = cos(k_time / p1)
k_space_bottom_to_top = -1
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 80.275
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Guitar_level)
acomp pluck i_amplitude, 440.0, 440.0, 0, 1, .1
i_frequency2 = i_frequency / 2.0
kHz = k(i_frequency)
iattack = 0.004
isustain = p3
irelease = 0.05
xtratim iattack + irelease
asigcomp pluck 1.0, 440, 440, 0, 1
asig pluck 1.0, i_frequency, i_frequency, 0, 1
af1 reson asig, 110, 80
af2 reson asig, 220, 100
af3 reson asig, 440, 80
aout balance 0.6 * af1 + af2 + 0.6 * af3 + 0.4 * asig, asigcomp
aexp expseg 1.0, iattack, 2.0, isustain, 1.0, irelease, 1.0
aenv = aexp - 1.0
a_signal = aout * aenv
a_declicking linsegr 0, iattack, 1, isustain, 1, irelease, 0
a_signal = a_signal * i_amplitude * a_declicking * k_gain
k_rms rms a_signal
printks "Guitar rms:      %9.4f\n", .5, k_rms

prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
#ifdef SPATIALIZE_GOGINS 
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatializer a_signal, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#end
#ifdef SPATIALIZE_STEREO
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
#end
#ifdef SPATIALIZE_IEM
a_iem_out[] init 64
k_azimuth, k_elevation, k_radius iem_cartesian_to_spherical_vst gi_size, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
i_index init floor(p1) - 5
i_azimuth =     7 + i_index * 5 + 0
i_elevation =   7 + i_index * 5 + 1
i_gain =        7 + i_index * 5 + 2
prints "i_index: %3d i_azimuth: %3d i_elevation: %3d i_gain: %3d\n", i_index, i_azimuth, i_elevation, i_gain
vstparamset gi_iem_multi_encoder, i_azimuth, k_azimuth
vstparamset gi_iem_multi_encoder, i_elevation, k_elevation
vstparamset gi_iem_multi_encoder, i_gain, k_gain
a_iem_out[i_index] = a_signal
outletv "iem_out", a_iem_out
#end
#ifdef SPATIALIZE_AALTO
a_aalto_out[] init 64
a_aalto_out[0] = a_signal
vstparamset gi_sparta_ambi_room_sim, 11, k_space_front_to_back
vstparamset gi_sparta_ambi_room_sim, 12, k_space_left_to_right
vstparamset gi_sparta_ambi_room_sim, 13, k_space_front_to_back
outletv "aalto_out", a_aalto_out
#end
endin

#ifdef SPATIALIZE_STEREO
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
printks "a_iem_encoder_in: %9.4f %9.4f %9.4f\n", .5, k_1, k_2, k_3
a_iem_encoder_out[] init 64
vstparamset gi_iem_multi_encoder, 0, 3
a_iem_encoder_out vstaudio gi_iem_multi_encoder, a_iem_encoder_in
a_iem_compressor_out[] init 64
;a_iem_compressor_out vstaudio gi_iem_omni_compressor, a_iem_encoder_out
;a_iem_room_out[] init 64;
;_iem_room_out vstaudio gi_iem_room_encoder, a_iem_compressor_out
;a_iem_compressor_out vstaudio gi_iem_omni_compressor, a_iem_encoder_out
a_iem_reverb_out[] init 64
a_iem_reverb_out vstaudio gi_iem_fdn_reverb, a_iem_encoder_out
;a_iem_decoder_out vstaudio gi_iem_allra_decoder, a_iem_encoder_out
;a_iem_decoder_out vstaudio gi_iem_simple_decoder, a_iem_encoder_out
a_iem_decoder_out[] init 64
a_iem_decoder_out vstaudio gi_iem_binaural_decoder, a_iem_reverb_out
k_left rms a_iem_decoder_out[0]
k_right rms a_iem_decoder_out[1]
printks "%-24.24s i %9.4f t %9.4f d %9.4f l %9.4f r %9.4f #%3d\n", 1, nstrstr(p1), p1, p2, p3, k_left, k_right, p7, active(p1)
out a_iem_decoder_out
endin
alwayson "SpatializeIEM"

connect "STKBeeThree", "iem_out", "SpatializeIEM", "iem_in"
connect "Rhodes", "iem_out", "SpatializeIEM", "iem_in"
connect "Guitar", "iem_out", "SpatializeIEM", "iem_in"
#end

#ifdef SPATIALIZE_AALTO
instr SpatializeAALTO
a_aalto_encoder_in[] init 64
a_aalto_encoder_in inletv "aalto_in"
a_aalto_encoder_out[] init 64
vstparamset gi_sparta_ambi_room_sim, 0, 3
vstparamset gi_sparta_ambi_room_sim, 3, 3
vstparamset gi_sparta_ambi_room_sim, 3, 1
a_aalto_encoder_out vstaudio gi_sparta_ambi_room_sim, a_aalto_encoder_in
a_aalto_decoder_out[] init 64
vstparamset gi_compass_binaural, 0, 3
a_aalto_decoder_out vstaudio gi_compass_binaural, a_aalto_encoder_out
out a_aalto_decoder_out
k_left rms a_aalto_decoder_out[0]
k_right rms a_aalto_decoder_out[1]
printks "%-24.24s i %9.4f t %9.4f d %9.4f l %9.4f r %9.4f #%3d\n", 1, nstrstr(p1), p1, p2, p3, k_left, k_right, p7, active(p1)
endin
alwayson "SpatializeAALTO"

connect "STKBeeThree", "aalto_out", "SpatializeAALTO", "aalto_in"
connect "Rhodes", "aalto_out", "SpatializeAALTO", "aalto_in"
connect "Guitar", "aalto_out", "SpatializeAALTO", "aalto_in"
#end


#ifdef SPATIALIZE_STEREO
connect "STKBeeThree", "outright", "MasterOutput", "inright"
connect "STKBeeThree", "outleft", "MasterOutput", "inleft"
connect "Rhodes", "outright", "MasterOutput", "inright"
connect "Rhodes", "outleft", "MasterOutput", "inleft"
connect "Guitar", "outright", "MasterOutput", "inright"
connect "Guitar", "outleft", "MasterOutput", "inleft"
#end

S_score_generator_code init {{

#include <eigen3/Eigen/Dense>
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
// This is the entry point for this C++ module. It will be called by Csound 
// immediately once the module has been compiled and linked.
//////////////////////////////////////////////////////////////////////////////
extern "C" int score_generator(CSOUND *csound) {
    int result = OK;
    EVTBLK evtblk;
    std::memset(&evtblk, 0, sizeof(EVTBLK));
    for (int j = 0; j < 120; ++j) {
        for (int i = 1; i < 4; ++i) {
            evtblk.strarg = nullptr;
            evtblk.scnt = 0;
            evtblk.opcod = 'i';
            evtblk.pcnt = 9;
            evtblk.p[1] = 4 + i;
            evtblk.p[2] = j;
            evtblk.p[3] = 1;
            evtblk.p[4] = 60 + (i * 4);
            evtblk.p[5] = 60;
            evtblk.p[6] = 0;
            evtblk.p[7] = 0;
            evtblk.p[8] = 0;
            std::fprintf(stderr, "%c %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f\\n", evtblk.opcod, evtblk.p[1], evtblk.p[2], evtblk.p[3], evtblk.p[4], evtblk.p[5], evtblk.p[6], evtblk.p[7], evtblk.p[8]);
            int result = csound->insert_score_event(csound, &evtblk, 0.);
        }
    }
    return result;
};

};

}}

//////////////////////////////////////////////////////////////////////////////
// This compiles the above C++ module and then calls its entry point function.
// Note that dynamic link libraries must be passed as complete filepaths.
//////////////////////////////////////////////////////////////////////////////
i_result clang_compile "score_generator", S_score_generator_code, "-g -O2 -std=c++17 -I/home/mkg/clang-opcodes -I/home/mkg/csound-extended/CsoundAC -I/usr/local/include/csound -stdlib=libstdc++", "/usr/lib/libCsoundAC.so.6.0 /usr/lib/gcc/x86_64-linux-gnu/9/libstdc++.so /usr/lib/gcc/x86_64-linux-gnu/9/libgcc_s.so /home/mkg/webkit-opcodes/webkit_opcodes.so /usr/lib/x86_64-linux-gnu/libm.so /usr/lib/x86_64-linux-gnu/libpthread.so"



</CsInstruments>
<CsScore>

f 0 120


</CsScore>
</CsoundSynthesizer>






