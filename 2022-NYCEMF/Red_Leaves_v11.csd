<CsoundSyntheizer>
<CsLicense>

R E D   L E A V E S   V E R S I O N   11use find to search for text in files

Michael Gogins, 2021

This piece is another in the "Leaves" series of pieces of electroacoustic 
concert music, algorithmically composed, that have been performed in 
international festivals and are available on electronic distribution.

This piece demonstrates the use of the Faust opcodes, the Clang opcodes, the 
CsoundAC C++ library for algorithmic composition, the vst4cs opcodes, the 
signal flow graph opcodes, and the WebKit opcodes -- all in one .csd file.

TODO:

-- Spatialize, with piano not moving and other sounds rotating slowly.
   (a) First, get existing coordinates to work in binaural mode.
   (b) Then, implement the movements.
   (c) Have the pad sounds move at different speeds so that they are 
       heard merging and separationg.
   (d) For this, sounds that begin together enter at the same angle on 
       the circle, but different instruments move at just enough different 
       speeds to pull apart halfway to the the next piano cue.

-- Try more involving chord changes. Still pretty static, not sure how to 
   change. Still, it works.

-- More involving balances among the pads. _Pretty good now._ Needs less 
   of one of the high frequency pads, not sure which one.

-- Check that FMWaterBell level is tracking slider. _Yes, it is._

-- Check that Pianoteq patch is good and reverb is off. _Yes for both._

-- Transpose just the piano part up a 6th or octave. _An octave works._

-- There is sound that continues for minutes at least after the end of the 
   score. But there is no note with an off time nearly that long. Therefore, 
   one of the always on instruments is producing sound. I will try explicitly
   turning them all off. _Well, it turns out to be PianoOutPianoteq, possibly 
   the "soundboard" or something keeps on reverberating._ 
   
-- There is still a software problem with the PianoRoll3D containing what 
   appears to be an anomalously long and low note. I do not need to fix this 
   now to get a good piece.
   
-- There is still a _musical_ problem with long notes that impede the harmony, 
   which needs to be kind of static or at least slowly changing, but not like 
   this. Or there may be another cause. _Oops, bad assumption! Score pen must 
   create a chord change on every application of every transformation, 
   different addresses will result in different resulting changes._
   
-- Getting too close to treacle? 

-- Interest flags at about 8 minutes 15 seconds. Shorten?

This is indeed a lot of stuff, but it makes for an_extremely_ powerful 
computer music system.

Even if you don't get everything here running, you may still find useful 
information on how to use some of these features in your own pieces.

</CsLicense>
<CsOptions>
-+msg_color=0 -m3 -d -odac0
</CsOptions>
<CsInstruments>

//////////////////////////////////////////////////////////////////////////////
// Change to sr=96000 with ksmps=1 for final rendering to soundfile.
//////////////////////////////////////////////////////////////////////////////
sr = 48000
ksmps = 128
nchnls = 2
0dbfs = 1000
//////////////////////////////////////////////////////////////////////////////
// This random seed ensures that the same random stream  is used for each 
// rendering. Note that rand, randh, randi, rnd(x) and birnd(x) are not 
// affected by seed.
//////////////////////////////////////////////////////////////////////////////
seed 88818145

;#define USE_SPATIALIZATION ##

#ifdef USE_SPATIALIZATION
#include "Spatialize1.inc"
gk_BformatDecoder_SpeakerRig init 1
gk_Spatialize_SpeakerRigRadius init 5.0
gk_SpatialReverb_ReverbDecay init 0.96
gk_SpatialReverb_CutoffHz init sr
gk_SpatialReverb_RandomDelayModulation init 4.0
gk_LocalReverbByDistance_Wet init 0.5
; This is a fraction of the speaker rig radius.
gk_LocalReverbByDistance_FrontWall init 0.9
gk_LocalReverbByDistance_ReverbDecay init 0.6
gk_LocalReverbByDistance_CutoffHz init 20000
gk_LocalReverbByDistance_RandomDelayModulation init 1.0
gk_Spatialize_Verbose init 0

connect "Blower", "outbformat", "BformatDecoder", "inbformat"
connect "Blower", "out", "SpatialReverb", "in"
connect "STKBowed", "outbformat", "BformatDecoder", "inbformat"
connect "STKBowed", "out", "SpatialReverb", "in"
connect "Buzzer", "outbformat", "BformatDecoder", "inbformat"
connect "Buzzer", "out", "SpatialReverb", "in"
connect "Droner", "outbformat", "BformatDecoder", "inbformat"
connect "Droner", "out", "SpatialReverb", "in"
connect "FMWaterBell", "outbformat", "BformatDecoder", "inbformat"
connect "FMWaterBell", "out", "SpatialReverb", "in"
connect "Phaser", "outbformat", "BformatDecoder", "inbformat"
connect "Phaser", "out", "SpatialReverb", "in"
connect "PianoOutPianoteq", "outbformat", "BformatDecoder", "inbformat"
connect "PianoOutPianoteq", "out", "SpatialReverb", "in"
connect "Sweeper", "outbformat", "BformatDecoder", "inbformat"
connect "Sweeper", "out", "SpatialReverb", "in"
connect "Shiner", "outbformat", "BformatDecoder", "inbformat"
connect "Shiner", "out", "SpatialReverb", "in"
connect "ZakianFlute", "outbformat", "BformatDecoder", "inbformat"
connect "ZakianFlute", "out", "SpatialReverb", "in"
connect "SpatialReverb", "outbformat", "BformatDecoder", "inbformat"
#elseif
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
connect "PianoOutPianoteq", "outleft", "MasterOutput", "inleft"
connect "PianoOutPianoteq", "outright", "MasterOutput", "inright"
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
#ifndef USE_SPATIALIZATION
instr dummy1
endin

instr dummy2
endin

instr dummy3
endin

instr dummy4
endin
#end

gi_Pianoteq vst3init "/Library/Audio/Plug-Ins/VST3/Pianoteq 7.vst3", "Pianoteq 7", 1
#include "patches/PianoNotePianoteqVst3.inc"
#include "patches/FMWaterBell.inc"
#include "patches/Phaser.inc"
#include "patches/Droner.inc"
#include "patches/Sweeper.inc"
#include "patches/Buzzer.inc"
#include "patches/Shiner.inc"
#include "patches/Blower.inc"
#include "patches/ZakianFlute.inc"
#include "patches/STKBowed.inc"

;#include "patches/PianoOutPianoteqVst3.inc"
alwayson "PianoOutPianoteqVst3"

#ifdef USE_SPATIALIZATION
alwayson "SpatialReverb"
alwayson "SpatialReverb2"
alwayson "BformatDecoder"
alwayson "BformatDecoder2"
#else
#include "patches/ReverbSC.inc"
alwayson "ReverbSC"
#include "patches/MasterOutput.inc"
alwayson "MasterOutput"
#end

//////////////////////////////////////////////////////////////////////////////
// These define the initial values of all the global variables/control 
// channels that can be controlled from the Web page.
//////////////////////////////////////////////////////////////////////////////

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
gk_FMWaterBell_level init 32
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
gk_Sweeper_britel init 0.14258927115604109
gk_Sweeper_briteh init 3.635884339731444
gk_Sweeper_britels init 1.1354964943746944
gk_Sweeper_britehs init .322566443828469
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
gk_PianoOutPianoteq_level init -42
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
gk_FMWaterBell_level init 26
gk_Phaser_ratio1 init 1
gk_Phaser_ratio2 init 5
gk_Phaser_index1 init 5
gk_Phaser_index2 init .05
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
gk_Sweeper_britel init .01
gk_Sweeper_briteh init 5
gk_Sweeper_britels init .5
gk_Sweeper_britehs init .5
gk_Sweeper_level init 7.606391651720202
gk_Buzzer_harmonics init 5
gk_Buzzer_level init 18
gk_Shiner_level init 5
gk_Blower_grainDensity init 132.3332789825534
gk_Blower_grainDuration init 0.2854231208217838
gk_Blower_grainAmplitudeRange init 174.0746779716289
gk_Blower_grainFrequencyRange init 62.82406652535464
gk_Blower_level init 2
gk_ZakianFlute_level init 25.125628140703512
gk_PianoOutPianoteq_level init -42
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
gk_FMWaterBell_level init 26
gk_Phaser_ratio1 init 1
gk_Phaser_ratio2 init 5
gk_Phaser_index1 init 5
gk_Phaser_index2 init 0.05
gk_Phaser_level init 6
gk_STKBowed_vibrato_level init 0
gk_STKBowed_bow_pressure init 110
gk_STKBowed_bow_position init 20
gk_STKBowed_vibrato_frequency init 50.2
gk_STKBowed_level init 0
gk_Droner_partial1 init 0.32561552258274906
gk_Droner_partial2 init 0.1743029512473504
gk_Droner_partial3 init 0.4769280939181477
gk_Droner_partial4 init 0.205609000489157
gk_Droner_partial5 init 0.18473830099461927
gk_Droner_level init -35.17854231208218
gk_Sweeper_britel init 0.36328061307679765
gk_Sweeper_briteh init 1.5320397847709115
gk_Sweeper_britels init 0.17544431762595794
gk_Sweeper_britehs init 0.6763411054948638
gk_Sweeper_level init -14.829610304907874
gk_Buzzer_harmonics init 5
gk_Buzzer_level init 10
gk_Shiner_level init 5
gk_Blower_grainDensity init 132.3332789825534
gk_Blower_grainDuration init 0.2854231208217838
gk_Blower_grainAmplitudeRange init 174.0746779716289
gk_Blower_grainFrequencyRange init 62.82406652535464
gk_Blower_level init 2
gk_ZakianFlute_level init 25.125628140703512
gk_PianoOutPianoteq_level init -44.04858959726072
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
#include <csdl.h>
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
    ///pen.note = {1,35,144,1,1,1,0,0,0,0,1};
    pen.note = csound::Event{1,32,144,1,1,1,0,0,0,0,1};
    int depth = 7;
    std::vector<std::function<Cursor(const Cursor &, int, csound::Score &)>> generators;
    auto g1 = [&chordsForTimes, &modality](const Cursor &pen_, int depth, csound::Score &score) {
        Cursor pen = pen_;
        if (depth >= 3) {
            pen.chord = pen.chord.T(1);
            ///pen.chord = pen.chord.T(-1.);
            chordsForTimes[pen.note.getTime()] = pen.chord;
        }
        pen.note[csound::Event::TIME] = (pen.note[csound::Event::TIME] * .5) + (0 - 5);
        pen.note[csound::Event::KEY] = (pen.note[csound::Event::KEY] * .75);
        return pen;
    };
    generators.push_back(g1);
    auto g2 = [&chordsForTimes, &modality](const Cursor &pen_, int depth, csound::Score &score) {
        Cursor pen = pen_;
        if (depth >= 4) {
            ///pen.chord = pen.chord.T(4);
            pen.chord = pen.chord.T(5);
            chordsForTimes[pen.note.getTime()] = pen.chord;
        }
        //~ if ((depth + base_level) == 3) {
            //~ pen.chord = pen.chord.K();
            //~ chordsForTimes[pen.note.getTime()] = pen.chord;
        //~ }
        //~ if ((depth + base_level) == 6) {
             //~ pen.chord = pen.chord.Q(3, modality);
        //~ }
        pen.note[csound::Event::TIME] = (pen.note[csound::Event::TIME] * .5) + (1000 + 2);
        pen.note[csound::Event::KEY] = (pen.note[csound::Event::KEY] * .75) - .25  ;
        pen.note[csound::Event::VELOCITY] =  std::cos(pen.note[csound::Event::TIME]);                    
        return pen;
    };
    generators.push_back(g2);
    auto g3 = [&chordsForTimes, &modality](const Cursor &pen_, int depth, csound::Score &score) {
        Cursor pen = pen_;
        if (depth >= 4) {
            ///pen.chord = pen.chord.T(4);
            pen.chord = pen.chord.Q(4, modality);
            chordsForTimes[pen.note.getTime()] = pen.chord;
        }
        pen.note[csound::Event::TIME] = (pen.note[csound::Event::TIME] * .5) + (0 + 3);
        pen.note[csound::Event::KEY] = (pen.note[csound::Event::KEY] * .715) + 1.05;
        pen.note[csound::Event::INSTRUMENT] = std::cos(pen.note[csound::Event::TIME]);
        pen.note[csound::Event::VELOCITY] =  std::cos(pen.note[csound::Event::TIME]);
        return pen;
    };
    generators.push_back(g3);
    auto g4 = [&chordsForTimes, &modality](const Cursor &pen_, int depth, csound::Score &score) {
        Cursor pen = pen_;
        if (depth >= 6) {
            ///pen.chord = pen.chord.T(4);
            pen.chord = pen.chord.K();
            chordsForTimes[pen.note.getTime()] = pen.chord;
        }
        pen.note[csound::Event::TIME] = (pen.note[csound::Event::TIME] * .5) + (1000 - 5);
        pen.note[csound::Event::KEY] = (pen.note[csound::Event::KEY] * .75) + 1.;
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
    std::cout << "Sorted duration:        " << score.getDuration() << std::endl;
    score.rescale(csound::Event::KEY, true, 24.0, true,  72.0);
    score.temper(12.);
    std::cout << "Generated notes:        " << score.size() << std::endl;
    double endTime = score.back().getTime();
    std::cout << "Chord segments:         " << chordsForTimes.size() << std::endl;
    int size = 0;
    int segment_count = 0;
    for (auto it = chordsForTimes.rbegin(); it != chordsForTimes.rend(); ++it, ++segment_count) {
        auto startTime = it->first;
        auto chord = it->second;
        auto segment = csound::slice(score, startTime, endTime);
        size += segment.size();
        std::fprintf(stderr, "From %9.4f to %9.4f apply %s to %d notes.\\n", startTime, endTime, chord.eOP().name().c_str(), segment.size());
        //~ std::fprintf(stderr, "Before:\\n");
        //~ for (int i = 0, n = segment.size(); i < n; ++i) {
            //~ std::fprintf(stderr, "  %s\\n", segment[i]->toString().c_str());
        //~ }  
        csound::apply(score, chord, startTime, endTime, true);
        //~ std::fprintf(stderr, "After:\\n");
        //~ for (int i = 0, n = segment.size(); i < n; ++i) {
            //~ std::fprintf(stderr, "  %s\\n", segment[i]->toString().c_str());
        //~ }  
        endTime = startTime;
    }
    std::cout << "Conformed duration:     " << score.getDuration() << std::endl;
    std::cout << "Conformed notes:        " << size << std::endl;
    score.rescale(csound::Event::TIME,          true,  0.0, false,  0.0);
    score.rescale(csound::Event::INSTRUMENT,    true,  1.0, true,   9.999);
    score.rescale(csound::Event::VELOCITY,      true, 40.0, true,  20.0);
    score.rescale(csound::Event::PAN,           true,  0.0, true,   0.0);
    std::cout << "Move to origin duration:" << score.getDuration() << std::endl;
    score.setDuration(380.0 * 2.);
    std::cout << "set duration:           " << score.getDuration() << std::endl;
    score.tieOverlappingNotes(true);
    std::cout << "Tied duration:          " << score.getDuration() << std::endl;
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
        // Offset for spatialization instruments that actually need to be 
        // defined first.
        evtblk.p[1] += 4;
        evtblk.p[5] = note.getVelocity();
        evtblk.p[6] = note.getDepth();
        evtblk.p[7] = note.getPan();
        evtblk.p[8] = note.getHeight();
        ///std::fprintf(stderr, "%c %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f off: %9.4f\\n", evtblk.opcod, evtblk.p[1], evtblk.p[2], evtblk.p[3], evtblk.p[4], evtblk.p[5], evtblk.p[6], evtblk.p[7], evtblk.p[8], note.getOffTime());
        int result = csound->insert_score_event(csound, &evtblk, 0.);
    }
    return result;
};

};

}}

//////////////////////////////////////////////////////////////////////////////
// This compiles the above C++ module and then calls its entry point function.
// Note that dynamic link libraries must be passed as complete filepaths.
//////////////////////////////////////////////////////////////////////////////
i_result cxx_compile "score_generator", S_score_generator_code, "-g -v -O2 -fPIC -shared -std=c++14 -stdlib=libc++ -I/usr/local/include/csound -I/Library/Frameworks/CsoundLib64.framework/Versions/6.0/Headers -I/opt/homebrew/Cellar/eigen/3.4.0_1/include/eigen3 -I/opt/homebrew/Cellar/eigen/3.4.0_1/include -I/opt/homebrew/Cellar/boost/1.78.0_1/include /usr/local/lib/libCsoundAC.dylib -lpthread"

instr Exit
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
;exitnow
endin

instr Turnoffs
;turnoff2 15, 0, 0 
endin

</CsInstruments>
<CsScore>
; f 0 does not work here, we actually need to schedule an instrument that 
; turns off Csound.
i "Exit" [12 * 60 + 85] 1
i "Turnoffs" [12 * 60 + 4] 1

;f 0 [6 * 60 + 5]
</CsScore>
</CsoundSynthesizer>
