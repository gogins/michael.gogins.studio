<CsoundSynthesizer>
<CsLicense>
Copyright (C) 2013 by Michael Gogins.
All rights reserved.
</CsLicense>
<CsOptions>
-+msg_color=0 -odac -m195 -d 
</CsOptions>
<CsInstruments>
sr = 48000
ksmps = 128
nchnls = 2
0dbfs = 10

// Define just one of these.
;#define SPATIALIZE_STEREO #1#
#define SPATIALIZE_IEM #2#
;#define SPATIALIZE_SPARTA #3#
;#define SPATIALIZE_GOGINS #4#

opcode cartesian_to_polar, kkk, kkk
kx, ky, kz xin
; Calculate the distance from the origin (the listener).
kdistance = sqrt(kx^2 + ky^2 + kz^2)
; Normalize the coordinates to unit vectors from the origin.
kx = kx / kdistance
ky = ky / kdistance
kz = kz / kdistance
; Limit distance to prevent too close a sound becoming too loud.
kdistance = (kdistance < 0.3 ? 0.3 : kdistance)
; Calculate the elevation.
kelevation = cosinv(sqrt(1 - kz^2))
; If z is negative, make elevation negative also.
kelevation = (kz < 0 ? -kelevation : kelevation)
; Calculate the angle.
kangle = sininv(ky / cos(kelevation))
; Distinguish between positive x and negative x.
kangle = (kx >= 0 ? kangle : 3.14159265 - kangle)
; Distinguish between positive and negative y and x.
kangle = (ky <= 0 && kx >= 0 ? 6.28318531 + kangle : kangle)
kangle = kangle * 57.295779513
kelevation = kelevation * 57.295779513
xout kangle, kelevation, kdistance
endop

// Presumably, N of the 64 channels are assigned and used in order, 
// and unused channels are left as 0.

prints "====================================================\n"
prints "IEM Plugin Suite:\n"
prints "----------------------------------------------------\n"
prints "Send N instruments to one channel of one of these...\n"
gi_iem_multi_encoder vstinit "/usr/lib/x86\_64-linux-gnu/iem-plugin-suite/vst/MultiEncoder.so", 1
prints "````````````````````````````````````````````````````\n"
prints "...or to N of these.\n"
gi_iem_stereo_encoder vstinit "/usr/lib/x86\_64-linux-gnu/iem-plugin-suite/vst/StereoEncoder.so", 1
prints "````````````````````````````````````````````````````\n"
prints "Then to the \"buss.\"\n"
gi_iem_omni_compressor vstinit "/usr/lib/x86\_64-linux-gnu/iem-plugin-suite/vst/OmniCompressor.so", 1
prints "````````````````````````````````````````````````````\n"
prints "Then to the room encoder (which does Doppler effects):\n"
gi_iem_room_encoder vstinit "/usr/lib/x86\_64-linux-gnu/iem-plugin-suite/vst/RoomEncoder.so", 1
prints "````````````````````````````````````````````````````\n"
prints "Then to one of these outputs:\n"
gi_iem_simple_decoder vstinit "/usr/lib/x86\_64-linux-gnu/iem-plugin-suite/vst/SimpleDecoder.so", 1
gi_iem_allra_decoder vstinit "/usr/lib/x86\_64-linux-gnu/iem-plugin-suite/vst/AllRADecoder.so", 1
gi_iem_binaural_decoder vstinit "/usr/lib/x86\_64-linux-gnu/iem-plugin-suite/vst/BinauralDecoder.so", 1
prints "====================================================\n"

prints "====================================================\n"
prints "SPARTA Suite:\n"
prints "----------------------------------------------------\n"
prints "Send N instruments to one channel of one of these...\n"
prints "````````````````````````````````````````````````````\n"
gi_sparta_ambi_enc vstinit "/home/mkg/.vst/libsparta_ambiENC.so", 1
prints "...or to N of these (need 2 of these at 2nd order):\n"
prints "````````````````````````````````````````````````````\n"
gi_sparta_ambi_room_sim vstinit "/home/mkg/.vst/libsparta_ambiRoomSim.so", 1
prints "Not sure if this makes sense or can be controlled with parameters.\n"
prints "````````````````````````````````````````````````````\n"
gi_compass_spatedit vstinit "/home/mkg/.vst/libcompass_spatedit.so", 1
prints "Then to this (N channels or binaural):\n"
prints "````````````````````````````````````````````````````\n"
gi_sparta_ambi_dec vstinit "/home/mkg/.vst/libsparta_ambiDEC.so", 1
prints "Or this:\n"
prints "````````````````````````````````````````````````````\n"
gi_sparta_ambi_bin vstinit "/home/mkg/.vst/libsparta_ambiBIN.so", 1
prints "Or this:\n"
prints "````````````````````````````````````````````````````\n"
gi_compass_decoder vstinit "/home/mkg/.vst/libcompass_decoder.so", 1
prints "Or this (probably best for binaural):\n"
prints "````````````````````````````````````````````````````\n"
gi_compass_binaural vstinit "/home/mkg/.vst/libcompass_binaural.so", 1
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
k_space_bottom_to_top = taninv(k_time / p1)
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
a_signal = a_signal * a_declicking * k_gain

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
k_azimuth, k_elevation, k_gain cartesian_to_polar k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
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
k_space_bottom_to_top = taninv(k_time / p1)
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
k_azimuth, k_elevation, k_gain cartesian_to_polar k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
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
k_space_bottom_to_top = taninv(k_time / p1)
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
k_azimuth, k_elevation, k_gain cartesian_to_polar k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
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
endin

#ifdef SPATIALIZE_STEREO
#include "MasterOutput.inc"
alwayson "MasterOutput"
#end

#ifdef SPATIALIZE_IEM

// TODO: One room encoder for each instrument.
// Possibly, use reverb.
// Only need binaural decoder for now.

instr SpatializeIEM 
a_iem_encoder_in[] init 64
a_iem_encoder_in inletv "iem_in"
a_iem_encoder_out[] init 64
vstparamset gi_iem_multi_encoder, 0, 3
a_iem_encoder_out vstaudio gi_iem_multi_encoder, a_iem_encoder_in
a_iem_compressor_out[] init 64
;a_iem_compressor_out vstaudio gi_iem_omni_compressor, a_iem_encoder_out
;a_iem_room_out[] init 64;
;_iem_room_out vstaudio gi_iem_room_encoder, a_iem_compressor_out
a_iem_decoder_out[] init 64
;a_iem_decoder_out vstaudio gi_iem_allra_decoder, a_iem_encoder_out
;a_iem_decoder_out vstaudio gi_iem_simple_decoder, a_iem_room_out
a_iem_decoder_out vstaudio gi_iem_binaural_decoder, a_iem_encoder_out
out a_iem_decoder_out
endin
alwayson "SpatializeIEM"

connect "STKBeeThree", "iem_out", "SpatializeIEM", "iem_in"
connect "STKBeeThree", "iem_out", "SpatializeIEM", "iem_in"
connect "Rhodes", "iem_out", "SpatializeIEM", "iem_in"
connect "Rhodes", "iem_out", "SpatializeIEM", "iem_in"
connect "Guitar", "iem_out", "SpatializeIEM", "iem_in"
connect "Guitar", "iem_out", "SpatializeIEM", "iem_in"
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






