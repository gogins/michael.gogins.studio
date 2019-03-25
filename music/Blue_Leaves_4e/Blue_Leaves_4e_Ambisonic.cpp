#include <Composition.hpp>
#include <eigen3/Eigen/Dense>
#include <functional>
#include <memory>
#include <MusicModel.hpp>
#include <random>
#include <ScoreNode.hpp>
#include <VoiceleadingNode.hpp>
#include <vector>

/* Build with this command:

g++ Yellow_Leaves_4.cpp -o Yellow_Leaves_4.exe -mtune=native -std=c++0x -O2 -g -DUSE_DOUBLE -IC:/utah/opt/Csound/include -IC:/utah/opt/boost_1_47_0 -IC:/utah/opt -LC:/utah/opt/Csound/bin -lCsoundAC -lcsnd -l:csound64.dll.5.2 -lmusicxml2 -lpthread

 */

/**
 * Generators are lambdas with the same signature as the following example.
 * The pen is the current position of a "pen" in the score. The pen is
 * "written" by appending it to the score. Additional events can be computed
 * based on the parameters of the function, and also appended to the score.
 * The function can, and usually does, move the pen to a new position before
 * returning it.
 */
auto generator = [&](const csound::Event &pen, int depth, csound::Score &score, csound::VoiceleadingNode &voiceleader)
{
    csound::Event result = pen;
    return result;
};

/**
 * Computes a deterministic, finite recurrent iterated function system by
 * recursively applying a set of generators (transformations) to a pen
 * which represents the position of a "pen" on a "score." The entries in
 * the transitions matrix represent open or closed paths of recurrence through
 * the tree of calls. Because the pen is passed by value, it is in effect
 * copied at each call of a generator and at each layer of recursion. The
 * generators may or may not append a local copy of the pen to the score,
 * thus "writing" a "note" or other event on the "score."
 */
void recurrent(std::vector< std::function<csound::Event(const csound::Event &,int, csound::Score &, csound::VoiceleadingNode &)> > &generators,
        Eigen::MatrixXd &transitions,
        int depth,
        int transformationIndex,
        const csound::Event pen,
        csound::Score &score,
        csound::VoiceleadingNode &voiceleader)
{
    depth = depth - 1;
    if (depth == 0) {
        return;
    }
    // std::printf("recurrent(depth: %3d  index: %3d  %s)\n", depth, transformationIndex, pen.toString().c_str());
    // Index is that of the current transformation. The column vector at that index determines which
    // transformations may be applied.
    for (int transitionIndex = 0, transitionN = transitions.rows(); transitionIndex < transitionN; ++transitionIndex) {
        if (transitions(transformationIndex, transitionIndex)) {
            auto newCursor = generators[transitionIndex](pen, depth, score, voiceleader);
            recurrent(generators, transitions, depth, transitionIndex, newCursor, score, voiceleader);
        }
    }
}

/**
 * All composition and synthesis code is defined in the main function.
 * There is no need for any of this code to be in a separate file.
 */
int main(int argc, const char **argv)
{
    csound::MusicModel model;
    // These fields determine output filenames and ID 3 tags.
    model.setTitle("Blue Leaves 4e Ambisonic");
    model.setFilename("Blue_Leaves_4e Ambisonic");
    model.setAlbum("Silence");
    model.setArtist("Michael Gogins");
    model.setAuthor("Michael Gogins");
    model.setCopyright("\"(C) 2012 by Michael Gogins\"");

    std::vector<std::function<csound::Event(const csound::Event &, int, csound::Score &, csound::VoiceleadingNode &)>> generators;

    auto g1 = [](const csound::Event &pen_, int depth, csound::Score &score, csound::VoiceleadingNode &voiceleader) {
        csound::Event pen = pen_;
        if (depth <= 1) {
            return pen;
        }
        pen[csound::Event::TIME] = pen[csound::Event::TIME] * .7 + .25;
        pen[csound::Event::KEY] = pen[csound::Event::KEY] + 3.105;
        pen[csound::Event::INSTRUMENT] = 4.0 * 1.0 + double(depth % 4);
        pen[csound::Event::VELOCITY] =  1.0;
        pen[csound::Event::PAN] = .875;
        score.append(pen);     
        if (depth > 2) {
            voiceleader.C(pen[csound::Event::TIME], "A7b9");
        }
        return pen;
    };
    generators.push_back(g1);

    auto g2 = [](const csound::Event &pen_, int depth, csound::Score &score, csound::VoiceleadingNode &voiceleader) {
        csound::Event pen = pen_;
        if (depth <= 2) {
            return pen;
        }
        pen[csound::Event::TIME] = pen[csound::Event::TIME] * .7464 - .203487;
        pen[csound::Event::KEY] = pen[csound::Event::KEY] *.99 + 3.0;
        pen[csound::Event::INSTRUMENT] = 4.0 * 2.0 + double(depth % 4);      
        pen[csound::Event::VELOCITY] =  2.0;                        
        pen[csound::Event::PAN] = .675;
        score.append(pen);
        return pen;
    };
    generators.push_back(g2);

    auto g3 = [](const csound::Event &pen_, int depth, csound::Score &score, csound::VoiceleadingNode &voiceleader) {
        csound::Event pen = pen_;
        if (depth <= 1) {
            return pen;
        }
        pen[csound::Event::TIME] = pen[csound::Event::TIME] * .65 - .23;
        pen[csound::Event::KEY] = pen[csound::Event::KEY] - 3.07;
        pen[csound::Event::INSTRUMENT] = 4.0 * 3.0 + double(depth % 4);
        pen[csound::Event::VELOCITY] =  1.0;
        pen[csound::Event::PAN] = -.675;
        score.append(pen);
        return pen;
    };
    generators.push_back(g3);

    auto g4 = [](const csound::Event &pen_, int depth, csound::Score &score, csound::VoiceleadingNode &voiceleader) {
        csound::Event pen = pen_;
        if (depth <= 0) {
            return pen;
        }
        pen[csound::Event::TIME] = pen[csound::Event::TIME] * .79 + .2;
        pen[csound::Event::KEY] = pen[csound::Event::KEY] - 4.25;
        pen[csound::Event::INSTRUMENT] = 4.0 * 4.0 + double(depth % 4);
        pen[csound::Event::VELOCITY] =  2.0;
        pen[csound::Event::PAN] = -.875;
        if (depth > 2) {
            voiceleader.C(pen[csound::Event::TIME], "Dm9");
        }
        return pen;
    };
    generators.push_back(g4);

    // Generate the score.
    csound::Event pen = {1,1,144,0,1,1,0,0,0,0,1};
    pen[csound::Event::DURATION] = 0.0125;
    Eigen::MatrixXd transitions(4,4);
    transitions <<  1, 1, 0, 1,
                    1, 1, 0, 1,
                    0, 1, 1, 0,
                    1, 0, 1, 1;
    csound::ScoreNode scoreNode;
    csound::VoiceleadingNode voiceleadingNode;
    voiceleadingNode.setModality({0., 2., 5., 7., 11.});
    voiceleadingNode.addChild(&scoreNode);
    model.addChild(&voiceleadingNode);
    csound::Score &score = scoreNode.getScore();
    recurrent(generators, transitions, 9, 0, pen, score, voiceleadingNode);
    std::cout << "Generated duration:     " << score.getDuration() << std::endl;
    score.rescale(csound::Event::TIME,          true,  0.0, false,  0.0);
    score.rescale(csound::Event::INSTRUMENT,    true,  1.0, false, 11.99999);
    score.rescale(csound::Event::KEY,           true, 24.0, true,  72.0);
    score.rescale(csound::Event::VELOCITY,      true, 40.0, true,   0.0);
    score.rescale(csound::Event::DURATION,      true,  2.0, true,   4.0);
    score.rescale(csound::Event::PAN,           true,  0.0, true,   0.0);
    std::cout << "Move to origin duration:" << score.getDuration() << std::endl;
    score.setDuration(240.0);
    std::cout << "set duration:           " << score.getDuration() << std::endl;
    score.rescale(csound::Event::DURATION,      true,  2.375, true,   4.0);
    score.temper(12.);
    score.tieOverlappingNotes(true);
    score.findScale();
    score.setDuration(360.0);
    std::mt19937 mersenneTwister;
    std::uniform_real_distribution<> randomvariable(-0.9, +0.9);
    // I must get rid of or at least vary the repeated high notes in the last section.
    for (int i = 0, n = score.size(); i < n; ++i) {
         score[i].setPan(randomvariable(mersenneTwister));
    }
    //voiceleadingNode.transform(score);
    std::cout << "Final duration:         " << score.getDuration() << std::endl;

    // Csound code is embedded as c++0x multi-line string constants.

    model.setCsoundOrchestra(R"(

sr                              =                       96000
ksmps                           =                       100
nchnls                          =                       2
iampdbfs                        init                    32768
                                prints                  "Default amplitude at 0 dBFS:  %9.4f\n", iampdbfs
idbafs                          init                    dbamp(iampdbfs)
                                prints                  "dbA at 0 dBFS:                 %9.4f\n", idbafs
iheadroom                       init                    6
                                prints                  "Headroom (dB):                 %9.4f\n", iheadroom
idbaheadroom                    init                    idbafs - iheadroom
                                prints                  "dbA at headroom:               %9.4f\n", idbaheadroom
iampheadroom                    init                    ampdb(idbaheadroom)
                                prints                  "Amplitude at headroom:        %9.4f\n", iampheadroom
                                prints                  "Balance so the overall amps at the end of performance -6 dbfs.\n"
                                
gShelp init {{
S P A T I A L I Z E

Copyright (c) 2014 by Michael Gogins.
This software is licensed under the terms of the
GNU Lesser Public License.

Many thanks to Jan Jacob Hofmann for his advanced Csound
spatialization system, which I have adapted here.

This software is a system of Ambisonic and/or HRTF-based
audio spatialization for Csound. I have simplified Hofmann's
excellent code by avoiding all Csound macros, and instead
using user-defined opcodes along with the signal flow graph
opcodes that send and receive signals using audio-rate array
variables.

If a single source fans out to one or more sinks, the sinks are opcodes.
If multiple sources fan in to a single sink, the sources are instr outlets
and the sink is an instr inlet.

The available apeaker rigs are:

 0 Ambisonic stereo with minimal distance cues
 (possibly the best choice for efficiency in live performance).
 1 Ambisonic binaural with HRTF decoding (periphonic, a suitable choice
   for checking in stereophonic studio work).
 2 Ambisonic quadraphonic.
 3 Ambisonic pentaphonic or 5.0 (might work for 5.1, omit .1)
 4 Ambisonic octaphonic.
 5 Ambisonic cubic (periphonic, probably the best choice for concerts).

Please note the coordinate conventions. The listener is always the origin.
In this system, spatial locations are passed to the spatialize opcode in
Cartesian coordinates. These are the same as those used in physics:

-- x runs from ahead to behind.
-- y runs from left to right. This is the same as stereo pan.
-- z runs from below to above.

Spherical coordinates are more or less standard for Ambisonics, and are
similar to but not identical with those used in physics. The only differences
are that Ambisonics uses elevation but physics uses inclination, and
Ambisonics uses degrees but physics uses radians.

-- r is distance.
-- azimuth or theta is anti-clockwise angle from 0 in front of the listener.
-- elevation or phi is angle above or below the x-y plane. (Note that
   inclination, also called polar angle, would be angle away from the
   zenith pole.)

USAGE

In the orchestra header:

gk_BformatDecoder_SpeakerRig init 5

alwayson SpatialReverb
alwayson Bformat3Decoder

connect "SpatializingInstrument", "out", "SpatialReverb", "in"
connect "SpatializingInstrument", "outbformat", "Bformat3Decoder", "inbformat"
connect "SpatialReverb", "outbformat", "Bformat3Decoder", "inbformat"

instr SpatializingInstrument
absignal[] init 16
asignal STKBlowBotl cpsmidinn(p4), 1, 4, kv1, 11, 10, 1, 50, 128
absignal, asend Spatializer asignal, 87, 15, 15
; Send a signal with only distance cues to the global spatial reverb instr.
outleta "out", asend
; Send the spatialized signal to the global Ambisonc decoder instr.
outletv "outbformat", absignal
endin

TODO

-- Upgrade demonstration piece with jspline spatial trajectories
   and algorithmically generated percussive notes.

-- Plug in and test early reflections code.

-- Test with rigs 2 through 5.

-- See if babo can be adapted for spatial reverb.

-- Upgrade panning with arbitrary measured speaker positions.

-- Try spat3d for global reverb and early reflections (not doing,
   as not periphonic).

}}

prints gShelp

; Global parameters that can be re-defined in the orchestra header.

gk_Spatialize_Verbose init 0
gk_BformatDecoder_SpeakerRig init 5
gk_Spatialize_SpeakerRigRadius init 5.0

opcode CartesianToPolar, kkk, kkk
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
xout kangle, kelevation, kdistance
endop

opcode CartesianToPolarDegrees, kkk, kkk
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

; Zero distance blows this up.
gk_DopplerByDistance_Mach init 340.29
opcode DopplerByDistance, a, ak
asignal, kdistance xin
; ashifted doppler asource, ksourceposition, kmicposition [, isoundspeed, ifiltercutoff]
adopplered doppler asignal, kdistance * gk_Spatialize_SpeakerRigRadius, 0, i(gk_DopplerByDistance_Mach), 6
xout adopplered
endop

; Zero distance blows this up.
opcode LowpassByDistance, a, ak
asignal, kdistance xin
afiltered butterlp asignal, 22000 * sqrt(1.0 / kdistance)
abalanced balance afiltered, asignal
xout abalanced
endop

opcode ReverbSC, a, akkkk
asignal, kReverbFeedback, kDelayModulation, kReverbWet, itone xin
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
ak1 randi .001, 3.1, .06
ak2 randi .0011, 3.5, .9
ak3 randi .0017, 1.11, .7
ak4 randi .0006, 3.973, .3
ak5 randi .001, 2.341, .63
ak6 randi .0011, 1.897, .7
ak7 randi .0017, 0.891, .9
ak8 randi .0006, 3.221, .44
; apj is used to calculate "resultant junction pressure" for
; the scattering junction of 8 lossless waveguides
; of equal characteristic impedance. If you wish to
; add more delay lines, simply add them to the following
; equation, and replace the .25 by 2/N, where N is the
; number of delay lines.
apj = .25 * (afilt1 + afilt2 + afilt3 + afilt4 + afilt5 + afilt6 + afilt7 + afilt8)
adum1 delayr 1
adel1 deltapi idel1 + ak1 * kDelayModulation
 delayw asignal + apj - afilt1
adum2 delayr 1
adel2 deltapi idel2 + ak2 * kDelayModulation
 delayw asignal + apj - afilt2
adum3 delayr 1
adel3 deltapi idel3 + ak3 * kDelayModulation
 delayw asignal + apj - afilt3
adum4 delayr 1
adel4 deltapi idel4 + ak4 * kDelayModulation
 delayw asignal + apj - afilt4
adum5 delayr 1
adel5 deltapi idel5 + ak5 * kDelayModulation
 delayw asignal + apj - afilt5
adum6 delayr 1
adel6 deltapi idel6 + ak6 * kDelayModulation
 delayw asignal + apj - afilt6
adum7 delayr 1
adel7 deltapi idel7 + ak7 * kDelayModulation
 delayw asignal + apj - afilt7
adum8 delayr 1
adel8 deltapi idel8 + ak8 * kDelayModulation
 delayw asignal + apj - afilt8
; 1st order lowpass filters in feedback
; loops of delay lines.
afilt1 tone adel1 * kReverbFeedback, itone
afilt2 tone adel2 * kReverbFeedback, itone
afilt3 tone adel3 * kReverbFeedback, itone
afilt4 tone adel4 * kReverbFeedback, itone
afilt5 tone adel5 * kReverbFeedback, itone
afilt6 tone adel6 * kReverbFeedback, itone
afilt7 tone adel7 * kReverbFeedback, itone
afilt8 tone adel8 * kReverbFeedback, itone
; The outputs of the delay lines are summed
; and sent to the stereo outputs. This could
; easily be modified for a 4 or 8-channel
; sound system.
aout1 = (afilt1 + afilt3 + afilt5 + afilt7)
aout2 = (afilt2 + afilt4 + afilt6 + afilt8)
aoutput = aout1 + aout2
aoutwet = aoutput * kReverbWet
aoutdry = aoutput * (1 - kReverbWet)
aoutput = aoutwet + aoutdry
xout aoutput
endop

gk_LocalReverbByDistance_Wet init 0.2
; This is a fraction of the speaker rig radius.
gk_LocalReverbByDistance_FrontWall init 0.9
gk_LocalReverbByDistance_ReverbDecay init 0.6
gk_LocalReverbByDistance_CutoffHz init sr
gk_LocalReverbByDistance_RandomDelayModulation init 1.0
opcode LocalReverbByDistance, aa, ak
; Re-implemented from Dodge and Jerse.
asignal, kdistanceMeters xin
kdry = 1.0 - gk_LocalReverbByDistance_Wet
knormalizedDistance = kdistanceMeters / gk_Spatialize_SpeakerRigRadius
klocalAttenuation = 1.0 / knormalizedDistance
kglobalAttenuation = 1.0 / sqrt(knormalizedDistance)
alocalReverbSend = asignal * klocalAttenuation
aglobalReverbSend = asignal * kglobalAttenuation
iechoMeters = 2.0 * i(gk_Spatialize_SpeakerRigRadius) * i(gk_LocalReverbByDistance_FrontWall)
isecondsPerMeter = 1.0 / i(gk_DopplerByDistance_Mach)
imillisecondsPerMeter = isecondsPerMeter * 1000.0
iglobalReverbSendDelayMilliseconds = i(gk_Spatialize_SpeakerRigRadius) * i(gk_LocalReverbByDistance_FrontWall) * imillisecondsPerMeter
; print iglobalReverbSendDelayMilliseconds
aglobalReverbSendDelayed delay aglobalReverbSend, iglobalReverbSendDelayMilliseconds
alocalReverbLeft, alocalReverbRight reverbsc	alocalReverbSend, alocalReverbSend, gk_LocalReverbByDistance_ReverbDecay, gk_LocalReverbByDistance_CutoffHz, sr, i(gk_LocalReverbByDistance_RandomDelayModulation), 1
alocalReverbWet = ((alocalReverbLeft + alocalReverbRight) / 2.0) * gk_LocalReverbByDistance_Wet
alocalReverbDry = kdry * asignal
alocalReverberatedSignal = alocalReverbDry + alocalReverbWet
if gk_Spatialize_Verbose != 0 then goto gk_Spatialize_Verbose_0
goto gk_Spatialize_Verbose_not_0
gk_Spatialize_Verbose_0:
printks "LocalReverbByDistance asignal: %9.4f klocalAttentuaton: %9.4f iglobalReverbSendDelayMilliseconds: %9.4f kglobalAttenuation: %9.4f alocalReverberatedSignal: %9.4f aglobalReverbSend: %9.4f\n", 0.5, asignal, klocalAttenuation, iglobalReverbSendDelayMilliseconds, kglobalAttenuation, alocalReverberatedSignal, aglobalReverbSendDelayed
gk_Spatialize_Verbose_not_0:
xout alocalReverberatedSignal, aglobalReverbSendDelayed
endop

; Switch on/off diffuse reflections [0=off, 1=on] and adjust gain of reflection (e.g.: 1= normal gain, 2= twice as loud, 0.5 = half as loud).
gk_EarlyReflection_diffref init 0 * 0
; Switch on/off specular reflections [0=off, 1=on] and adjust gain of reflection (e.g.: 1= normal gain, 2= twice as loud, 0.5 = half as loud).
gk_EarlyReflection_specref init 0 * 0
; Position of FrontWall at the x-axis [units] for calculation of early reflections.
gk_EarlyReflection_FrontWall init 1.6
; Position of SideWall at the y-axis [units] for calculation of early reflections.
gk_EarlyReflection_SideWall init 1.3
; Height of the Ceiling measured from the zero-point of the coordinate-system [units] for calculation of early reflections.
gk_EarlyReflection_Ceiling init 1.1
; Damping factor of the reflecting surface (gain of reverb).
gk_EarlyReflection_damp init 0.1
; Absorbtion characteristics of the wall in respect to high frequencies: frequencies above ihighdamp get filtered out.
gk_EarlyReflection_highdamp init 15000
; Low frequency boost for sources near a wall.
gk_EarlyReflection_lfboost init 1
opcode DiffuseEarlyReflection, a[], akkki
asignal, kx, ky, kz, iwhichwall xin

if iwhichwall == 1 then goto iwhichwall_1
 goto iwhichwall_1_else
 iwhichwall_1:
 krefx = gk_EarlyReflection_FrontWall
 goto iwhichwall_1_endif
iwhichwall_1_else:
 krefx = kx
iwhichwall_1_endif:

if iwhichwall == 2 then goto iwhichwall_2
 goto iwhichwall_2_else
 iwhichwall_2:
 krefx = -gk_EarlyReflection_FrontWall
 goto iwhichwall_2_endif
iwhichwall_2_else:
 krefx = kx
iwhich_wall_2_endif:

if iwhichwall == 3 then goto iwhichwall_3
 goto iwhichwall_3_else
 iwhichwall_3:
 kfefy = gk_EarlyReflection_SideWall
 goto iwhichwall_3_endif
iwhichwall_3_else:
 krefy = ky
iwhichwall_3_endif:

if iwhichwall == 4 then goto iwhichwall_4
 goto iwhichwall_4_else:
 krefy = -gk_EarlyReflection_SideWall
 goto iwhichwall_4_endif
iwhichwall_4_else:
 krefy = ky
iwhichwall_4_endif:

if iwhichwall == 5 then
 krefz = gk_EarlyReflection_Ceiling
else
 krefz = kz
endif

if iwhichwall == 6 then
 krefz = -gk_EarlyReflection_Ceiling
else
 krefz = kz
endif

kA, kE, kD CartesianToPolar kx, ky, kz
kAref, kEref, kDref CartesianToPolar krefx, krefy, krefz
kDdiff = sqrt((kx - krefx)^2 + (ky-krefy)^2 + (kz - krefz)^2)
kDist = kDdiff + kDref
; Convert to meters.
kDmtr = kDist * gk_Spatialize_SpeakerRigRadius
; Delay time in milliseconds.
kdel = kDmtr / 0.345
; Reflection behind the source
kAdist = abs(kA - kAref)
kEdist = abs(kE - kEref)
; Reduce the angle kAdist to values between 0 and 2 pi.
kAdist = (kAdist > 6.2831 ? kAdist - 6.2831 : kAdist)
kmute = kAdist + kEdist
; Mute the reflection if it gets behind the source.
kmute = (kmute < 1.5707 ? sin(kmute) : 1)
; Attenuate the early reflection by distance.
k_gain = 1 / kDist * gk_EarlyReflection_damp * kmute
; Cut high frequencies by distance.
kDdiff = ( kDdiff < 0.05 ? 0.05 : kDdiff)
klfboost = (sqrt(1 / kDdiff ) + kD) * gk_EarlyReflection_lfboost
afilt tone asignal, (sqrt(1 / kDist)) * sr
; Boost low frequencies near the wall.
afilt pareq afilt, 174, klfboost, 0.7071, 1
afilt balance afilt, asignal
adel interp kdel
asigref vdelay3 afilt, adel, 200 ; milliseconds
asigref = asigref * k_gain
; Convert radians to degrees.
kAref = kAref * 57.2958
kEref = kEref * 57.2958
absignal[] init 16
absignal bformenc1 asigref, kAref, kEref
xout absignal
endop

opcode DiffuseEarlyReflections, a[], akkk
asignal, kx, ky, kz xin
; Filter high frequencies to represent absorbtion by the wall.
asignal tone asignal, gk_EarlyReflection_highdamp
absignal[] init 16
abreflectedSignal[] init 16
; Bounce the signal off each of the walls.
abreflectedSignal DiffuseEarlyReflection asignal, kx, ky, kz, 1
absignal[ 0] = absignal[ 0] + abreflectedSignal[ 0]
absignal[ 1] = absignal[ 1] + abreflectedSignal[ 1]
absignal[ 2] = absignal[ 2] + abreflectedSignal[ 2]
absignal[ 3] = absignal[ 3] + abreflectedSignal[ 3]
absignal[ 4] = absignal[ 4] + abreflectedSignal[ 4]
absignal[ 5] = absignal[ 5] + abreflectedSignal[ 5]
absignal[ 6] = absignal[ 6] + abreflectedSignal[ 6]
absignal[ 7] = absignal[ 7] + abreflectedSignal[ 7]
absignal[ 8] = absignal[ 8] + abreflectedSignal[ 8]
absignal[ 9] = absignal[ 9] + abreflectedSignal[ 9]
absignal[10] = absignal[10] + abreflectedSignal[10]
absignal[11] = absignal[11] + abreflectedSignal[11]
absignal[12] = absignal[12] + abreflectedSignal[12]
absignal[13] = absignal[13] + abreflectedSignal[13]
absignal[14] = absignal[14] + abreflectedSignal[14]
absignal[15] = absignal[15] + abreflectedSignal[15]
abreflectedSignal DiffuseEarlyReflection asignal, kx, ky, kz, 2
absignal[ 0] = absignal[ 0] + abreflectedSignal[ 0]
absignal[ 1] = absignal[ 1] + abreflectedSignal[ 1]
absignal[ 2] = absignal[ 2] + abreflectedSignal[ 2]
absignal[ 3] = absignal[ 3] + abreflectedSignal[ 3]
absignal[ 4] = absignal[ 4] + abreflectedSignal[ 4]
absignal[ 5] = absignal[ 5] + abreflectedSignal[ 5]
absignal[ 6] = absignal[ 6] + abreflectedSignal[ 6]
absignal[ 7] = absignal[ 7] + abreflectedSignal[ 7]
absignal[ 8] = absignal[ 8] + abreflectedSignal[ 8]
absignal[ 9] = absignal[ 9] + abreflectedSignal[ 9]
absignal[10] = absignal[10] + abreflectedSignal[10]
absignal[11] = absignal[11] + abreflectedSignal[11]
absignal[12] = absignal[12] + abreflectedSignal[12]
absignal[13] = absignal[13] + abreflectedSignal[13]
absignal[14] = absignal[14] + abreflectedSignal[14]
absignal[15] = absignal[15] + abreflectedSignal[15]
abreflectedSignal DiffuseEarlyReflection asignal, kx, ky, kz, 3
absignal[ 0] = absignal[ 0] + abreflectedSignal[ 0]
absignal[ 1] = absignal[ 1] + abreflectedSignal[ 1]
absignal[ 2] = absignal[ 2] + abreflectedSignal[ 2]
absignal[ 3] = absignal[ 3] + abreflectedSignal[ 3]
absignal[ 4] = absignal[ 4] + abreflectedSignal[ 4]
absignal[ 5] = absignal[ 5] + abreflectedSignal[ 5]
absignal[ 6] = absignal[ 6] + abreflectedSignal[ 6]
absignal[ 7] = absignal[ 7] + abreflectedSignal[ 7]
absignal[ 8] = absignal[ 8] + abreflectedSignal[ 8]
absignal[ 9] = absignal[ 9] + abreflectedSignal[ 9]
absignal[10] = absignal[10] + abreflectedSignal[10]
absignal[11] = absignal[11] + abreflectedSignal[11]
absignal[12] = absignal[12] + abreflectedSignal[12]
absignal[13] = absignal[13] + abreflectedSignal[13]
absignal[14] = absignal[14] + abreflectedSignal[14]
absignal[15] = absignal[15] + abreflectedSignal[15]
abreflectedSignal DiffuseEarlyReflection asignal, kx, ky, kz, 4
absignal[ 0] = absignal[ 0] + abreflectedSignal[ 0]
absignal[ 1] = absignal[ 1] + abreflectedSignal[ 1]
absignal[ 2] = absignal[ 2] + abreflectedSignal[ 2]
absignal[ 3] = absignal[ 3] + abreflectedSignal[ 3]
absignal[ 4] = absignal[ 4] + abreflectedSignal[ 4]
absignal[ 5] = absignal[ 5] + abreflectedSignal[ 5]
absignal[ 6] = absignal[ 6] + abreflectedSignal[ 6]
absignal[ 7] = absignal[ 7] + abreflectedSignal[ 7]
absignal[ 8] = absignal[ 8] + abreflectedSignal[ 8]
absignal[ 9] = absignal[ 9] + abreflectedSignal[ 9]
absignal[10] = absignal[10] + abreflectedSignal[10]
absignal[11] = absignal[11] + abreflectedSignal[11]
absignal[12] = absignal[12] + abreflectedSignal[12]
absignal[13] = absignal[13] + abreflectedSignal[13]
absignal[14] = absignal[14] + abreflectedSignal[14]
absignal[15] = absignal[15] + abreflectedSignal[15]
abreflectedSignal DiffuseEarlyReflection asignal, kx, ky, kz, 5
absignal[ 0] = absignal[ 0] + abreflectedSignal[ 0]
absignal[ 1] = absignal[ 1] + abreflectedSignal[ 1]
absignal[ 2] = absignal[ 2] + abreflectedSignal[ 2]
absignal[ 3] = absignal[ 3] + abreflectedSignal[ 3]
absignal[ 4] = absignal[ 4] + abreflectedSignal[ 4]
absignal[ 5] = absignal[ 5] + abreflectedSignal[ 5]
absignal[ 6] = absignal[ 6] + abreflectedSignal[ 6]
absignal[ 7] = absignal[ 7] + abreflectedSignal[ 7]
absignal[ 8] = absignal[ 8] + abreflectedSignal[ 8]
absignal[ 9] = absignal[ 9] + abreflectedSignal[ 9]
absignal[10] = absignal[10] + abreflectedSignal[10]
absignal[11] = absignal[11] + abreflectedSignal[11]
absignal[12] = absignal[12] + abreflectedSignal[12]
absignal[13] = absignal[13] + abreflectedSignal[13]
absignal[14] = absignal[14] + abreflectedSignal[14]
absignal[15] = absignal[15] + abreflectedSignal[15]
abreflectedSignal DiffuseEarlyReflection asignal, kx, ky, kz, 6
absignal[ 0] = absignal[ 0] + abreflectedSignal[ 0]
absignal[ 1] = absignal[ 1] + abreflectedSignal[ 1]
absignal[ 2] = absignal[ 2] + abreflectedSignal[ 2]
absignal[ 3] = absignal[ 3] + abreflectedSignal[ 3]
absignal[ 4] = absignal[ 4] + abreflectedSignal[ 4]
absignal[ 5] = absignal[ 5] + abreflectedSignal[ 5]
absignal[ 6] = absignal[ 6] + abreflectedSignal[ 6]
absignal[ 7] = absignal[ 7] + abreflectedSignal[ 7]
absignal[ 8] = absignal[ 8] + abreflectedSignal[ 8]
absignal[ 9] = absignal[ 9] + abreflectedSignal[ 9]
absignal[10] = absignal[10] + abreflectedSignal[10]
absignal[11] = absignal[11] + abreflectedSignal[11]
absignal[12] = absignal[12] + abreflectedSignal[12]
absignal[13] = absignal[13] + abreflectedSignal[13]
absignal[14] = absignal[14] + abreflectedSignal[14]
absignal[15] = absignal[15] + abreflectedSignal[15]
xout absignal
endop

opcode SpecularEarlyReflection, a[], akkkik
asignal, kx, ky, kz, iwhichwall, kmydiff xin
ksum = abs(iwhichwall) + abs(kmydiff)
krefx = (kx / ksum) * abs(iwhichwall)
krefy = (ky / ksum) * abs(iwhichwall)
krefz = (kz / ksum) * abs(iwhichwall)
krefx = (iwhichwall = 1 ? gk_EarlyReflection_FrontWall : (kx / ksum) * abs(iwhichwall))
krefx = (iwhichwall = 2 ? -gk_EarlyReflection_FrontWall : (kx / ksum) * abs(iwhichwall))
krefy = (ky / ksum) * abs(iwhichwall)
krefz = (kz / ksum) * abs(iwhichwall)
if iwhichwall == 1 then
 krefx = gk_EarlyReflection_FrontWall
else
 krefx = kx
endif
if iwhichwall == 2 then
 krefx = -gk_EarlyReflection_FrontWall
else
 krefx = kx
endif
if iwhichwall == 3 then
 kfefy = gk_EarlyReflection_SideWall
else
 krefy = ky
endif
if iwhichwall == 4 then
 krefy = -gk_EarlyReflection_SideWall
else
 krefy = ky
endif
if iwhichwall == 5 then
 krefz = gk_EarlyReflection_Ceiling
else
 krefz = kz
endif
if iwhichwall == 6 then
 krefz = -gk_EarlyReflection_Ceiling
else
 krefz = kz
endif
; Prevent zero to prevent division by zero error (NaN) later.
krefx = (krefx < 0.001 && krefx > -0.001 ? 0.001 : krefx)
krefy = (krefy < 0.001 && krefy > -0.001 ? 0.001 : krefy)
krefz = (krefz < 0.001 && krefz > -0.001 ? 0.001 : krefz)
kA, kE, kD CartesianToPolar kx, ky, kz
kAref, kEref, kDref CartesianToPolar krefx, krefy, krefz
kDdiff = sqrt((kx - krefx)^2 + (ky-krefy)^2 + (kz - krefz)^2)
kDist = kDdiff + kDref
; Convert to meters.
kDmtr = kDist * gk_Spatialize_SpeakerRigRadius
; Delay time in milliseconds.
kdel = kDmtr / 0.345
; Reflection behind the source
kAdist = abs(kA - kAref)
kEdist = abs(kE - kEref)
; Reduce the angle kAdist to values between 0 and 2 pi.
kAdist = (kAdist > 6.2831 ? kAdist - 6.2831 : kAdist)
kmute = kAdist + kEdist
; Mute the reflection if it gets behind the source.
kmute = (kmute < 1.5707 ? sin(kmute) : 1)
; Attenuate the early reflection by distance.
k_gain = 1 / kDist * gk_EarlyReflection_damp * kmute
; Cut high frequencies by distance.
kDdiff = ( kDdiff < 0.05 ? 0.05 : kDdiff)
klfboost = (sqrt(1 / kDdiff ) + kD) * gk_EarlyReflection_lfboost
afilt tone asignal, (sqrt(1 / kDist)) * sr
; Boost low frequencies near the wall.
afilt pareq afilt, 174, klfboost, 0.7071, 1
afilt balance afilt, asignal
adel interp kdel
asigref vdelay3 afilt, adel, 200
asigref = asigref * k_gain
; Convert radians to degrees.
kAref = kAref * 57.2958
kEref = kEref * 57.2958
absignal[] init 16
absignal bformenc1 asigref, kAref, kEref
xout absignal
endop

opcode SpecularEarlyReflections, a[], akkk
asignal, kx, ky, kz xin
kiwall1 = gk_EarlyReflection_FrontWall
kiwall2 = -gk_EarlyReflection_FrontWall
kiwall3 = gk_EarlyReflection_SideWall
kiwall4 = -gk_EarlyReflection_SideWall
kiwall5 = gk_EarlyReflection_Ceiling
kiwall6 = -gk_EarlyReflection_Ceiling
kdiff1 = kiwall1 - kx ; distance of reflection to the FrontWall
kdiff2 = kiwall2 - kx ; distance of reflection to the backwall
kdiff3 = kiwall3 - ky ; distance of reflection to the left SideWall
kdiff4 = kiwall4 - ky ; distance of reflection to the right SideWall
kdiff5 = kiwall5 - kz ; distance of reflection to the Ceiling
kdiff6 = kiwall6 - kz ; distance of reflection to the floor
; Filter high frequencies to represent absorption by the walls.
afilteredSignal tone asignal, gk_EarlyReflection_highdamp
absignal[] init 16
abreflectedSignal[] init 16
abreflectedSignal SpecularEarlyReflection afilteredSignal, kx, ky, kz, 1, kdiff1
absignal[ 0] = absignal[ 0] + abreflectedSignal[ 0]
absignal[ 1] = absignal[ 1] + abreflectedSignal[ 1]
absignal[ 2] = absignal[ 2] + abreflectedSignal[ 2]
absignal[ 3] = absignal[ 3] + abreflectedSignal[ 3]
absignal[ 4] = absignal[ 4] + abreflectedSignal[ 4]
absignal[ 5] = absignal[ 5] + abreflectedSignal[ 5]
absignal[ 6] = absignal[ 6] + abreflectedSignal[ 6]
absignal[ 7] = absignal[ 7] + abreflectedSignal[ 7]
absignal[ 8] = absignal[ 8] + abreflectedSignal[ 8]
absignal[ 9] = absignal[ 9] + abreflectedSignal[ 9]
absignal[10] = absignal[10] + abreflectedSignal[10]
absignal[11] = absignal[11] + abreflectedSignal[11]
absignal[12] = absignal[12] + abreflectedSignal[12]
absignal[13] = absignal[13] + abreflectedSignal[13]
absignal[14] = absignal[14] + abreflectedSignal[14]
absignal[15] = absignal[15] + abreflectedSignal[15]
abreflectedSignal SpecularEarlyReflection afilteredSignal, kx, ky, kz, 2, kdiff2
absignal[ 0] = absignal[ 0] + abreflectedSignal[ 0]
absignal[ 1] = absignal[ 1] + abreflectedSignal[ 1]
absignal[ 2] = absignal[ 2] + abreflectedSignal[ 2]
absignal[ 3] = absignal[ 3] + abreflectedSignal[ 3]
absignal[ 4] = absignal[ 4] + abreflectedSignal[ 4]
absignal[ 5] = absignal[ 5] + abreflectedSignal[ 5]
absignal[ 6] = absignal[ 6] + abreflectedSignal[ 6]
absignal[ 7] = absignal[ 7] + abreflectedSignal[ 7]
absignal[ 8] = absignal[ 8] + abreflectedSignal[ 8]
absignal[ 9] = absignal[ 9] + abreflectedSignal[ 9]
absignal[10] = absignal[10] + abreflectedSignal[10]
absignal[11] = absignal[11] + abreflectedSignal[11]
absignal[12] = absignal[12] + abreflectedSignal[12]
absignal[13] = absignal[13] + abreflectedSignal[13]
absignal[14] = absignal[14] + abreflectedSignal[14]
absignal[15] = absignal[15] + abreflectedSignal[15]
abreflectedSignal SpecularEarlyReflection afilteredSignal, kx, ky, kz, 3, kdiff3
absignal[ 0] = absignal[ 0] + abreflectedSignal[ 0]
absignal[ 1] = absignal[ 1] + abreflectedSignal[ 1]
absignal[ 2] = absignal[ 2] + abreflectedSignal[ 2]
absignal[ 3] = absignal[ 3] + abreflectedSignal[ 3]
absignal[ 4] = absignal[ 4] + abreflectedSignal[ 4]
absignal[ 5] = absignal[ 5] + abreflectedSignal[ 5]
absignal[ 6] = absignal[ 6] + abreflectedSignal[ 6]
absignal[ 7] = absignal[ 7] + abreflectedSignal[ 7]
absignal[ 8] = absignal[ 8] + abreflectedSignal[ 8]
absignal[ 9] = absignal[ 9] + abreflectedSignal[ 9]
absignal[10] = absignal[10] + abreflectedSignal[10]
absignal[11] = absignal[11] + abreflectedSignal[11]
absignal[12] = absignal[12] + abreflectedSignal[12]
absignal[13] = absignal[13] + abreflectedSignal[13]
absignal[14] = absignal[14] + abreflectedSignal[14]
absignal[15] = absignal[15] + abreflectedSignal[15]
abreflectedSignal SpecularEarlyReflection afilteredSignal, kx, ky, kz, 4, kdiff4
absignal[ 0] = absignal[ 0] + abreflectedSignal[ 0]
absignal[ 1] = absignal[ 1] + abreflectedSignal[ 1]
absignal[ 2] = absignal[ 2] + abreflectedSignal[ 2]
absignal[ 3] = absignal[ 3] + abreflectedSignal[ 3]
absignal[ 4] = absignal[ 4] + abreflectedSignal[ 4]
absignal[ 5] = absignal[ 5] + abreflectedSignal[ 5]
absignal[ 6] = absignal[ 6] + abreflectedSignal[ 6]
absignal[ 7] = absignal[ 7] + abreflectedSignal[ 7]
absignal[ 8] = absignal[ 8] + abreflectedSignal[ 8]
absignal[ 9] = absignal[ 9] + abreflectedSignal[ 9]
absignal[10] = absignal[10] + abreflectedSignal[10]
absignal[11] = absignal[11] + abreflectedSignal[11]
absignal[12] = absignal[12] + abreflectedSignal[12]
absignal[13] = absignal[13] + abreflectedSignal[13]
absignal[14] = absignal[14] + abreflectedSignal[14]
absignal[15] = absignal[15] + abreflectedSignal[15]
abreflectedSignal SpecularEarlyReflection afilteredSignal, kx, ky, kz, 5, kdiff5
absignal[ 0] = absignal[ 0] + abreflectedSignal[ 0]
absignal[ 1] = absignal[ 1] + abreflectedSignal[ 1]
absignal[ 2] = absignal[ 2] + abreflectedSignal[ 2]
absignal[ 3] = absignal[ 3] + abreflectedSignal[ 3]
absignal[ 4] = absignal[ 4] + abreflectedSignal[ 4]
absignal[ 5] = absignal[ 5] + abreflectedSignal[ 5]
absignal[ 6] = absignal[ 6] + abreflectedSignal[ 6]
absignal[ 7] = absignal[ 7] + abreflectedSignal[ 7]
absignal[ 8] = absignal[ 8] + abreflectedSignal[ 8]
absignal[ 9] = absignal[ 9] + abreflectedSignal[ 9]
absignal[10] = absignal[10] + abreflectedSignal[10]
absignal[11] = absignal[11] + abreflectedSignal[11]
absignal[12] = absignal[12] + abreflectedSignal[12]
absignal[13] = absignal[13] + abreflectedSignal[13]
absignal[14] = absignal[14] + abreflectedSignal[14]
absignal[15] = absignal[15] + abreflectedSignal[15]
abreflectedSignal SpecularEarlyReflection afilteredSignal, kx, ky, kz, 6, kdiff6
absignal[ 0] = absignal[ 0] + abreflectedSignal[ 0]
absignal[ 1] = absignal[ 1] + abreflectedSignal[ 1]
absignal[ 2] = absignal[ 2] + abreflectedSignal[ 2]
absignal[ 3] = absignal[ 3] + abreflectedSignal[ 3]
absignal[ 4] = absignal[ 4] + abreflectedSignal[ 4]
absignal[ 5] = absignal[ 5] + abreflectedSignal[ 5]
absignal[ 6] = absignal[ 6] + abreflectedSignal[ 6]
absignal[ 7] = absignal[ 7] + abreflectedSignal[ 7]
absignal[ 8] = absignal[ 8] + abreflectedSignal[ 8]
absignal[ 9] = absignal[ 9] + abreflectedSignal[ 9]
absignal[10] = absignal[10] + abreflectedSignal[10]
absignal[11] = absignal[11] + abreflectedSignal[11]
absignal[12] = absignal[12] + abreflectedSignal[12]
absignal[13] = absignal[13] + abreflectedSignal[13]
absignal[14] = absignal[14] + abreflectedSignal[14]
absignal[15] = absignal[15] + abreflectedSignal[15]
xout absignal
endop

opcode Spatialize, a[]a, akkk
asignal, kx, ky, kz xin
kazimuth, kelevation, kdistance CartesianToPolarDegrees kx, ky, kz
adoppleredSignal DopplerByDistance asignal, kdistance
afilteredSignal LowpassByDistance adoppleredSignal, kdistance
abspatializedSignal[] init 16
asend init 0
; Use only minimal distance cues for plain stereo.
if gk_BformatDecoder_SpeakerRig == 0 then
 abspatializedSignal bformenc1 afilteredSignal, kazimuth, kelevation
 asend = afilteredSignal
elseif gk_BformatDecoder_SpeakerRig == 1 then
 adistanceCuedSignal, aglobalReverbSend LocalReverbByDistance afilteredSignal, kdistance
 abspatializedSignal bformenc1 adistanceCuedSignal, kazimuth, kelevation
 asend = aglobalReverbSend
else
 adistanceCuedSignal, aglobalReverbSend LocalReverbByDistance afilteredSignal, kdistance
 abspatializedSignal bformenc1 adistanceCuedSignal, kazimuth, kelevation
 abdiffuseEarlyReflectionsSignal[] init 16
 abdiffuseEarlyReflectionsSignal DiffuseEarlyReflections adistanceCuedSignal, kx, ky, kz
 abspatializedSignal[ 0] = abspatializedSignal[ 0] + abdiffuseEarlyReflectionsSignal[ 0]
 abspatializedSignal[ 1] = abspatializedSignal[ 1] + abdiffuseEarlyReflectionsSignal[ 1]
 abspatializedSignal[ 2] = abspatializedSignal[ 2] + abdiffuseEarlyReflectionsSignal[ 2]
 abspatializedSignal[ 3] = abspatializedSignal[ 3] + abdiffuseEarlyReflectionsSignal[ 3]
 abspatializedSignal[ 4] = abspatializedSignal[ 4] + abdiffuseEarlyReflectionsSignal[ 4]
 abspatializedSignal[ 5] = abspatializedSignal[ 5] + abdiffuseEarlyReflectionsSignal[ 5]
 abspatializedSignal[ 6] = abspatializedSignal[ 6] + abdiffuseEarlyReflectionsSignal[ 6]
 abspatializedSignal[ 7] = abspatializedSignal[ 7] + abdiffuseEarlyReflectionsSignal[ 7]
 abspatializedSignal[ 8] = abspatializedSignal[ 8] + abdiffuseEarlyReflectionsSignal[ 8]
 abspatializedSignal[ 9] = abspatializedSignal[ 9] + abdiffuseEarlyReflectionsSignal[ 9]
 abspatializedSignal[10] = abspatializedSignal[10] + abdiffuseEarlyReflectionsSignal[10]
 abspatializedSignal[11] = abspatializedSignal[11] + abdiffuseEarlyReflectionsSignal[11]
 abspatializedSignal[12] = abspatializedSignal[12] + abdiffuseEarlyReflectionsSignal[12]
 abspatializedSignal[13] = abspatializedSignal[13] + abdiffuseEarlyReflectionsSignal[13]
 abspatializedSignal[14] = abspatializedSignal[14] + abdiffuseEarlyReflectionsSignal[14]
 abspatializedSignal[15] = abspatializedSignal[15] + abdiffuseEarlyReflectionsSignal[15]
 abspecularEarlyReflectionsSignal[] init 16
 abspecularEarlyReflectionsSignal SpecularEarlyReflections afilteredSignal, kx, ky, kz
 abspatializedSignal[ 0] = abspatializedSignal[ 0] + abspecularEarlyReflectionsSignal[ 0]
 abspatializedSignal[ 1] = abspatializedSignal[ 1] + abspecularEarlyReflectionsSignal[ 1]
 abspatializedSignal[ 2] = abspatializedSignal[ 2] + abspecularEarlyReflectionsSignal[ 2]
 abspatializedSignal[ 3] = abspatializedSignal[ 3] + abspecularEarlyReflectionsSignal[ 3]
 abspatializedSignal[ 4] = abspatializedSignal[ 4] + abspecularEarlyReflectionsSignal[ 4]
 abspatializedSignal[ 5] = abspatializedSignal[ 5] + abspecularEarlyReflectionsSignal[ 5]
 abspatializedSignal[ 6] = abspatializedSignal[ 6] + abspecularEarlyReflectionsSignal[ 6]
 abspatializedSignal[ 7] = abspatializedSignal[ 7] + abspecularEarlyReflectionsSignal[ 7]
 abspatializedSignal[ 8] = abspatializedSignal[ 8] + abspecularEarlyReflectionsSignal[ 8]
 abspatializedSignal[ 9] = abspatializedSignal[ 9] + abspecularEarlyReflectionsSignal[ 9]
 abspatializedSignal[10] = abspatializedSignal[10] + abspecularEarlyReflectionsSignal[10]
 abspatializedSignal[11] = abspatializedSignal[11] + abspecularEarlyReflectionsSignal[11]
 abspatializedSignal[12] = abspatializedSignal[12] + abspecularEarlyReflectionsSignal[12]
 abspatializedSignal[13] = abspatializedSignal[13] + abspecularEarlyReflectionsSignal[13]
 abspatializedSignal[14] = abspatializedSignal[14] + abspecularEarlyReflectionsSignal[14]
 abspatializedSignal[15] = abspatializedSignal[15] + abspecularEarlyReflectionsSignal[15]
 asend = aglobalReverbSend
endif
xout abspatializedSignal, asend
endop

gk_SpatialReverb_ReverbDecay init 0.6
gk_SpatialReverb_CutoffHz init sr
gk_SpatialReverb_RandomDelayModulation init 1.0

instr SpatialReverb2
; 12 delay line FDN reverb, with feedback matrix based upon
; physical modeling scattering junction of 12 lossless waveguides
; of equal characteristic impedance. Based on Julius O. Smith III,
; "A New Approach to Digital Reverberation using Closed Waveguide
; Networks," Proceedings of the International Computer Music
; Conference 1985, p. 47-53 (also available as a seperate
; publication from CCRMA), as well as some more recent papers by
; Smith and others.
;
; Coded by Sean Costello, October 1999/ modified by Jan Jacob Hofmann August 2005
; Note: arevsig is the global input to the reverb.

; Set ksmps to 1 because feedback lines are used.
setksmps 1
arevsig inleta "in"
itone init i(gk_SpatialReverb_CutoffHz)
afilt1 init 0
afilt2 init 0
afilt3 init 0
afilt4 init 0
afilt5 init 0
afilt6 init 0
afilt7 init 0
afilt8 init 0
afilt9 init 0
afilt10 init 0
afilt11 init 0
afilt12 init 0
; Delay times chosen to be prime numbers.
; Works with sr=44100/ sr=48000 ONLY. If you wish to
; use a different delay time, find some new
; prime numbers that will give roughly the
; same delay times for the new sampling rate.
; Or adjust to taste.
if sr == 41000 then
idel1 = (1237.000/sr)
idel2 = (1381.000/sr)
idel3 = (1609.000/sr)
idel4 = (1777.000/sr)
idel5 = (1951.000/sr)
idel6 = (2063.000/sr)
idel7 = (1069.000/sr)
idel8 = ( 967.000/sr)
idel9 = (1889.000/sr)
idel10 = (1693.000/sr)
idel11 = (1453.000/sr)
idel12 = (1291.000/sr)
elseif sr == 48000 then
idel1 = (1361.000/sr)
idel2 = (1499.000/sr)
idel3 = (1753.000/sr)
idel4 = (1933.000/sr)
idel5 = (2129.000/sr)
idel6 = (2243.000/sr)
idel7 = (1163.000/sr)
idel8 = (1051.000/sr)
idel9 = (2053.000/sr)
idel10 = (1693.000/sr)
idel11 = (1847.000/sr)
idel12 = (1409.000/sr)
endif
; k1-k8 are used to add random pitch modulation to the
; delay lines. Helps eliminate metallic overtones
; in the reverb sound.
k1 randi .001, 3.1, .06
k2 randi .0011, 3.5, .9
k3 randi .0017, 1.11, .7
k4 randi .0006, 3.973, .3
k5 randi .001, 2.341, .63
k6 randi .0011, 1.897, .17
k7 randi .0017, 0.891, .19
k8 randi .0006, 3.221, .44
k9 randi .001, 1.891, .88
k10 randi .0011, 2.317, .95
k11 randi .0017, 1.091, .36
k12 randi .0006, 2.821, .41
; apj is used to calculate "resultant junction pressure" for
; the scattering junction of e.g. 8 lossless waveguides
; of equal characteristic impedance. If you wish to
; add more delay lines, simply add them to the following
; equation, and replace the .25 by 2/N, where N is the
; number of delay lines.
apj = .16666666667 * (afilt1 + afilt2 + afilt3 + afilt4 + afilt5 + afilt6 +afilt7 + afilt8 + afilt9 + afilt10 +afilt11 + afilt12)
adum1 delayr 1
adel1 deltapi idel1 + k1 * gk_SpatialReverb_RandomDelayModulation
 delayw arevsig + apj - afilt1
adum2 delayr 1
adel2 deltapi idel2 + k2 * gk_SpatialReverb_RandomDelayModulation
 delayw arevsig + apj - afilt2
adum3 delayr 1
adel3 deltapi idel3 + k3 * gk_SpatialReverb_RandomDelayModulation
 delayw arevsig + apj - afilt3
adum4 delayr 1
adel4 deltapi idel4 + k4 * gk_SpatialReverb_RandomDelayModulation
 delayw arevsig + apj - afilt4
adum5 delayr 1
adel5 deltapi idel5 + k5 * gk_SpatialReverb_RandomDelayModulation
 delayw arevsig + apj - afilt5
adum6 delayr 1
adel6 deltapi idel6 + k6 * gk_SpatialReverb_RandomDelayModulation
 delayw arevsig + apj - afilt6
adum7 delayr 1
adel7 deltapi idel7 + k7 * gk_SpatialReverb_RandomDelayModulation
 delayw arevsig + apj - afilt7
adum8 delayr 1
adel8 deltapi idel8 + k8 * gk_SpatialReverb_RandomDelayModulation
 delayw arevsig + apj - afilt8
adum9 delayr 1
adel9 deltapi idel9 + k9 * gk_SpatialReverb_RandomDelayModulation
 delayw arevsig + apj - afilt9
adum10 delayr 1
adel10 deltapi idel10 + k10 * gk_SpatialReverb_RandomDelayModulation
 delayw arevsig + apj - afilt10
adum11 delayr 1
adel11 deltapi idel11 + k11 * gk_SpatialReverb_RandomDelayModulation
 delayw arevsig + apj - afilt11
adum12 delayr 1
adel12 deltapi idel12 + k12 * gk_SpatialReverb_RandomDelayModulation
 delayw arevsig + apj - afilt12
; 1st order lowpass filters in feedback
; loops of delay lines.
afilt1 tone adel1 * gk_SpatialReverb_ReverbDecay, itone
afilt2 tone adel2 * gk_SpatialReverb_ReverbDecay, itone
afilt3 tone adel3 * gk_SpatialReverb_ReverbDecay, itone
afilt4 tone adel4 * gk_SpatialReverb_ReverbDecay, itone
afilt5 tone adel5 * gk_SpatialReverb_ReverbDecay, itone
afilt6 tone adel6 * gk_SpatialReverb_ReverbDecay, itone
afilt7 tone adel7 * gk_SpatialReverb_ReverbDecay, itone
afilt8 tone adel8 * gk_SpatialReverb_ReverbDecay, itone
afilt9 tone adel9 * gk_SpatialReverb_ReverbDecay, itone
afilt10 tone adel10 * gk_SpatialReverb_ReverbDecay, itone
afilt11 tone adel11 * gk_SpatialReverb_ReverbDecay, itone
afilt12 tone adel12 * gk_SpatialReverb_ReverbDecay, itone
; iA1-iA12 are the angles pointing to the 12 corners of an icosahedron.
; iE1-iE12 are the elevations pointing to the 12 corners of an icosahedron.
iA1 = 0 ; angle reverberant point 1
iA2 = 0.6238 ; angle reverberant point 2
iA3 = 1.2566 ; angle reverberant point 3
iA4 = 1.8849 ; angle reverberant point 4
iA5 = 2.5132 ; angle reverberant point 5
iA6 = 3.1415 ; angle reverberant point 6
iA7 = 3.7699 ; angle reverberant point 7
iA8 = 4.3982 ; angle reverberant point 8
iA9 = 5.0265 ; angle reverberant point 9
iA10 = 5.6548 ; angle reverberant point 10
iA11 = 0 ; angle reverberant point 11
iA12 = 0 ; angle reverberant point 12
iE1 = 0.463646 ; height reverberant point 1 ( 26.565?)
iE2 = -0.463646 ; height reverberant point 2 ( -26.565?)
iE3 = 0.463646 ; height reverberant point 3 ( 26.565?)
iE4 = -0.463646 ; height reverberant point 4 ( -26.565?)
iE5 = 0.463646 ; height reverberant point 5 ( 26.565?)
iE6 = -0.463646 ; height reverberant point 6 ( -26.565?)
iE7 = 0.463646 ; height reverberant point 7 ( 26.565?)
iE8 = -0.463646 ; height reverberant point 8 ( -26.565?)
iE9 = 0.463646 ; height reverberant point 9 ( 26.565?)
iE10 = -0.463646 ; height reverberant point 10 ( -26.565?)
iE11 = 1.570796 ; height reverberant point 11 ( 90?)
iE12 = -1.570796 ; height reverberant point 12 ( -90?)
aboutput[] init 16
abreverberated[] init 16
aboutput       bformenc1 afilt1, iA1, iE1
abreverberated bformenc1 afilt2, iA2, iE2
aboutput[ 0] = aboutput[ 0] + abreverberated[ 0]
aboutput[ 1] = aboutput[ 1] + abreverberated[ 1]
aboutput[ 2] = aboutput[ 2] + abreverberated[ 2]
aboutput[ 3] = aboutput[ 3] + abreverberated[ 3]
aboutput[ 4] = aboutput[ 4] + abreverberated[ 4]
aboutput[ 5] = aboutput[ 5] + abreverberated[ 5]
aboutput[ 6] = aboutput[ 6] + abreverberated[ 6]
aboutput[ 7] = aboutput[ 7] + abreverberated[ 7]
aboutput[ 8] = aboutput[ 8] + abreverberated[ 8]
aboutput[ 9] = aboutput[ 9] + abreverberated[ 9]
aboutput[10] = aboutput[10] + abreverberated[10]
aboutput[11] = aboutput[11] + abreverberated[11]
aboutput[12] = aboutput[12] + abreverberated[12]
aboutput[13] = aboutput[13] + abreverberated[13]
aboutput[14] = aboutput[14] + abreverberated[14]
aboutput[15] = aboutput[15] + abreverberated[15]
abreverberated bformenc1 afilt3, iA3, iE3
aboutput[ 0] = aboutput[ 0] + abreverberated[ 0]
aboutput[ 1] = aboutput[ 1] + abreverberated[ 1]
aboutput[ 2] = aboutput[ 2] + abreverberated[ 2]
aboutput[ 3] = aboutput[ 3] + abreverberated[ 3]
aboutput[ 4] = aboutput[ 4] + abreverberated[ 4]
aboutput[ 5] = aboutput[ 5] + abreverberated[ 5]
aboutput[ 6] = aboutput[ 6] + abreverberated[ 6]
aboutput[ 7] = aboutput[ 7] + abreverberated[ 7]
aboutput[ 8] = aboutput[ 8] + abreverberated[ 8]
aboutput[ 9] = aboutput[ 9] + abreverberated[ 9]
aboutput[10] = aboutput[10] + abreverberated[10]
aboutput[11] = aboutput[11] + abreverberated[11]
aboutput[12] = aboutput[12] + abreverberated[12]
aboutput[13] = aboutput[13] + abreverberated[13]
aboutput[14] = aboutput[14] + abreverberated[14]
aboutput[15] = aboutput[15] + abreverberated[15]
abreverberated bformenc1 afilt4, iA4, iE4
aboutput[ 0] = aboutput[ 0] + abreverberated[ 0]
aboutput[ 1] = aboutput[ 1] + abreverberated[ 1]
aboutput[ 2] = aboutput[ 2] + abreverberated[ 2]
aboutput[ 3] = aboutput[ 3] + abreverberated[ 3]
aboutput[ 4] = aboutput[ 4] + abreverberated[ 4]
aboutput[ 5] = aboutput[ 5] + abreverberated[ 5]
aboutput[ 6] = aboutput[ 6] + abreverberated[ 6]
aboutput[ 7] = aboutput[ 7] + abreverberated[ 7]
aboutput[ 8] = aboutput[ 8] + abreverberated[ 8]
aboutput[ 9] = aboutput[ 9] + abreverberated[ 9]
aboutput[10] = aboutput[10] + abreverberated[10]
aboutput[11] = aboutput[11] + abreverberated[11]
aboutput[12] = aboutput[12] + abreverberated[12]
aboutput[13] = aboutput[13] + abreverberated[13]
aboutput[14] = aboutput[14] + abreverberated[14]
aboutput[15] = aboutput[15] + abreverberated[15]
abreverberated bformenc1 afilt5, iA5, iE5
aboutput[ 0] = aboutput[ 0] + abreverberated[ 0]
aboutput[ 1] = aboutput[ 1] + abreverberated[ 1]
aboutput[ 2] = aboutput[ 2] + abreverberated[ 2]
aboutput[ 3] = aboutput[ 3] + abreverberated[ 3]
aboutput[ 4] = aboutput[ 4] + abreverberated[ 4]
aboutput[ 5] = aboutput[ 5] + abreverberated[ 5]
aboutput[ 6] = aboutput[ 6] + abreverberated[ 6]
aboutput[ 7] = aboutput[ 7] + abreverberated[ 7]
aboutput[ 8] = aboutput[ 8] + abreverberated[ 8]
aboutput[ 9] = aboutput[ 9] + abreverberated[ 9]
aboutput[10] = aboutput[10] + abreverberated[10]
aboutput[11] = aboutput[11] + abreverberated[11]
aboutput[12] = aboutput[12] + abreverberated[12]
aboutput[13] = aboutput[13] + abreverberated[13]
aboutput[14] = aboutput[14] + abreverberated[14]
aboutput[15] = aboutput[15] + abreverberated[15]
abreverberated bformenc1 afilt6, iA6, iE6
aboutput[ 0] = aboutput[ 0] + abreverberated[ 0]
aboutput[ 1] = aboutput[ 1] + abreverberated[ 1]
aboutput[ 2] = aboutput[ 2] + abreverberated[ 2]
aboutput[ 3] = aboutput[ 3] + abreverberated[ 3]
aboutput[ 4] = aboutput[ 4] + abreverberated[ 4]
aboutput[ 5] = aboutput[ 5] + abreverberated[ 5]
aboutput[ 6] = aboutput[ 6] + abreverberated[ 6]
aboutput[ 7] = aboutput[ 7] + abreverberated[ 7]
aboutput[ 8] = aboutput[ 8] + abreverberated[ 8]
aboutput[ 9] = aboutput[ 9] + abreverberated[ 9]
aboutput[10] = aboutput[10] + abreverberated[10]
aboutput[11] = aboutput[11] + abreverberated[11]
aboutput[12] = aboutput[12] + abreverberated[12]
aboutput[13] = aboutput[13] + abreverberated[13]
aboutput[14] = aboutput[14] + abreverberated[14]
aboutput[15] = aboutput[15] + abreverberated[15]
abreverberated bformenc1 afilt7, iA7, iE7
aboutput[ 0] = aboutput[ 0] + abreverberated[ 0]
aboutput[ 1] = aboutput[ 1] + abreverberated[ 1]
aboutput[ 2] = aboutput[ 2] + abreverberated[ 2]
aboutput[ 3] = aboutput[ 3] + abreverberated[ 3]
aboutput[ 4] = aboutput[ 4] + abreverberated[ 4]
aboutput[ 5] = aboutput[ 5] + abreverberated[ 5]
aboutput[ 6] = aboutput[ 6] + abreverberated[ 6]
aboutput[ 7] = aboutput[ 7] + abreverberated[ 7]
aboutput[ 8] = aboutput[ 8] + abreverberated[ 8]
aboutput[ 9] = aboutput[ 9] + abreverberated[ 9]
aboutput[10] = aboutput[10] + abreverberated[10]
aboutput[11] = aboutput[11] + abreverberated[11]
aboutput[12] = aboutput[12] + abreverberated[12]
aboutput[13] = aboutput[13] + abreverberated[13]
aboutput[14] = aboutput[14] + abreverberated[14]
aboutput[15] = aboutput[15] + abreverberated[15]
abreverberated bformenc1 afilt8, iA8, iE8
aboutput[ 0] = aboutput[ 0] + abreverberated[ 0]
aboutput[ 1] = aboutput[ 1] + abreverberated[ 1]
aboutput[ 2] = aboutput[ 2] + abreverberated[ 2]
aboutput[ 3] = aboutput[ 3] + abreverberated[ 3]
aboutput[ 4] = aboutput[ 4] + abreverberated[ 4]
aboutput[ 5] = aboutput[ 5] + abreverberated[ 5]
aboutput[ 6] = aboutput[ 6] + abreverberated[ 6]
aboutput[ 7] = aboutput[ 7] + abreverberated[ 7]
aboutput[ 8] = aboutput[ 8] + abreverberated[ 8]
aboutput[ 9] = aboutput[ 9] + abreverberated[ 9]
aboutput[10] = aboutput[10] + abreverberated[10]
aboutput[11] = aboutput[11] + abreverberated[11]
aboutput[12] = aboutput[12] + abreverberated[12]
aboutput[13] = aboutput[13] + abreverberated[13]
aboutput[14] = aboutput[14] + abreverberated[14]
aboutput[15] = aboutput[15] + abreverberated[15]
abreverberated bformenc1 afilt9, iA9, iE9
aboutput[ 0] = aboutput[ 0] + abreverberated[ 0]
aboutput[ 1] = aboutput[ 1] + abreverberated[ 1]
aboutput[ 2] = aboutput[ 2] + abreverberated[ 2]
aboutput[ 3] = aboutput[ 3] + abreverberated[ 3]
aboutput[ 4] = aboutput[ 4] + abreverberated[ 4]
aboutput[ 5] = aboutput[ 5] + abreverberated[ 5]
aboutput[ 6] = aboutput[ 6] + abreverberated[ 6]
aboutput[ 7] = aboutput[ 7] + abreverberated[ 7]
aboutput[ 8] = aboutput[ 8] + abreverberated[ 8]
aboutput[ 9] = aboutput[ 9] + abreverberated[ 9]
aboutput[10] = aboutput[10] + abreverberated[10]
aboutput[11] = aboutput[11] + abreverberated[11]
aboutput[12] = aboutput[12] + abreverberated[12]
aboutput[13] = aboutput[13] + abreverberated[13]
aboutput[14] = aboutput[14] + abreverberated[14]
aboutput[15] = aboutput[15] + abreverberated[15]
outletv "outbformat", aboutput
if gk_Spatialize_Verbose != 0 then
kelapsed timeinsts
printks "SpatialReverb i %5.2f t %5.2f [%5.2f] d %5.2f c %9.2f in %5.2f A[0] %5.2f\n", 1.0, p1, p2, kelapsed, p3, gk_SpatialReverb_CutoffHz, downsamp(arevsig), downsamp(aboutput[0])
endif
endin

gk_SpatialReverb_Gain init 0.2
instr SpatialReverb
ainput inleta "in"
asignal = ainput / 32.0 * gk_SpatialReverb_Gain
;kcutoff = 22000 / gk_SpatialReverb_CutoffHz
kcutoff = gk_SpatialReverb_CutoffHz
aleft1, aright1 reverbsc asignal, asignal, gk_SpatialReverb_ReverbDecay, kcutoff, sr
afilt1 = aleft1 + aright1
aleft2, aright2 reverbsc asignal, asignal, gk_SpatialReverb_ReverbDecay, kcutoff, sr
afilt2 = aleft2 + aright2
aleft3, aright3 reverbsc asignal, asignal, gk_SpatialReverb_ReverbDecay, kcutoff, sr
afilt3 = aleft3 + aright3
aleft4, aright4 reverbsc asignal, asignal, gk_SpatialReverb_ReverbDecay, kcutoff, sr
afilt4 = aleft4 + aright4
aleft5, aright5 reverbsc asignal, asignal, gk_SpatialReverb_ReverbDecay, kcutoff, sr
afilt5 = aleft5 + aright5
aleft6, aright6 reverbsc asignal, asignal, gk_SpatialReverb_ReverbDecay, kcutoff, sr
afilt6 = aleft6 + aright6
aleft7, aright7 reverbsc asignal, asignal, gk_SpatialReverb_ReverbDecay, kcutoff, sr
afilt7 = aleft7 + aright7
aleft8, aright8 reverbsc asignal, asignal, gk_SpatialReverb_ReverbDecay, kcutoff, sr
afilt8 = aleft8 + aright8
aleft9, aright9 reverbsc asignal, asignal, gk_SpatialReverb_ReverbDecay, kcutoff, sr
afilt9 = aleft9 + aright9
aleft10, aright10 reverbsc asignal, asignal, gk_SpatialReverb_ReverbDecay, kcutoff, sr
afilt10 = aleft10 + aright10
aleft11, aright11 reverbsc asignal, asignal, gk_SpatialReverb_ReverbDecay, kcutoff, sr
afilt11 = aleft11 + aright11
aleft12, aright12 reverbsc asignal, asignal, gk_SpatialReverb_ReverbDecay, kcutoff, sr
afilt12 = aleft12 + aright12
afiltc1 tone afilt1, gk_SpatialReverb_CutoffHz
afiltc2 tone afilt2, gk_SpatialReverb_CutoffHz
afiltc3 tone afilt3, gk_SpatialReverb_CutoffHz
afiltc4 tone afilt4, gk_SpatialReverb_CutoffHz
afiltc5 tone afilt5, gk_SpatialReverb_CutoffHz
afiltc6 tone afilt6, gk_SpatialReverb_CutoffHz
afiltc7 tone afilt7, gk_SpatialReverb_CutoffHz
afiltc8 tone afilt8, gk_SpatialReverb_CutoffHz
afiltc9 tone afilt9, gk_SpatialReverb_CutoffHz
afiltc10 tone afilt10, gk_SpatialReverb_CutoffHz
afiltc11 tone afilt11, gk_SpatialReverb_CutoffHz
afiltc12 tone afilt12, gk_SpatialReverb_CutoffHz
; iA1-iA12 are the angles pointing to the 12 corners of an icosahedron.
; iE1-iE12 are the elevations pointing to the 12 corners of an icosahedron.
iA1 = 0 ; angle reverberant point 1
iA2 = 0.6238 ; angle reverberant point 2
iA3 = 1.2566 ; angle reverberant point 3
iA4 = 1.8849 ; angle reverberant point 4
iA5 = 2.5132 ; angle reverberant point 5
iA6 = 3.1415 ; angle reverberant point 6
iA7 = 3.7699 ; angle reverberant point 7
iA8 = 4.3982 ; angle reverberant point 8
iA9 = 5.0265 ; angle reverberant point 9
iA10 = 5.6548 ; angle reverberant point 10
iA11 = 0 ; angle reverberant point 11
iA12 = 0 ; angle reverberant point 12
iE1 = 0.463646 ; height reverberant point 1 ( 26.565?)
iE2 = -0.463646 ; height reverberant point 2 ( -26.565?)
iE3 = 0.463646 ; height reverberant point 3 ( 26.565?)
iE4 = -0.463646 ; height reverberant point 4 ( -26.565?)
iE5 = 0.463646 ; height reverberant point 5 ( 26.565?)
iE6 = -0.463646 ; height reverberant point 6 ( -26.565?)
iE7 = 0.463646 ; height reverberant point 7 ( 26.565?)
iE8 = -0.463646 ; height reverberant point 8 ( -26.565?)
iE9 = 0.463646 ; height reverberant point 9 ( 26.565?)
iE10 = -0.463646 ; height reverberant point 10 ( -26.565?)
iE11 = 1.570796 ; height reverberant point 11 ( 90?)
iE12 = -1.570796 ; height reverberant point 12 ( -90?)
aboutput[] init 16
abreverberated1[] init 16
abreverberated2[] init 16
abreverberated3[] init 16
abreverberated4[] init 16
abreverberated5[] init 16
abreverberated6[] init 16
abreverberated7[] init 16
abreverberated8[] init 16
abreverberated9[] init 16
abreverberated10[] init 16
abreverberated11[] init 16
abreverberated12[] init 16
aboutput[ 0] = 0
aboutput[ 1] = 0
aboutput[ 2] = 0
aboutput[ 3] = 0
aboutput[ 4] = 0
aboutput[ 5] = 0
aboutput[ 6] = 0
aboutput[ 7] = 0
aboutput[ 8] = 0
aboutput[ 9] = 0
aboutput[10] = 0
aboutput[11] = 0
aboutput[12] = 0
aboutput[13] = 0
aboutput[14] = 0
aboutput[15] = 0
abreverberated1 bformenc1 afiltc1, iA1, iE1
aboutput[ 0] = aboutput[ 0] + abreverberated1[ 0]
aboutput[ 1] = aboutput[ 1] + abreverberated1[ 1]
aboutput[ 2] = aboutput[ 2] + abreverberated1[ 2]
aboutput[ 3] = aboutput[ 3] + abreverberated1[ 3]
aboutput[ 4] = aboutput[ 4] + abreverberated1[ 4]
aboutput[ 5] = aboutput[ 5] + abreverberated1[ 5]
aboutput[ 6] = aboutput[ 6] + abreverberated1[ 6]
aboutput[ 7] = aboutput[ 7] + abreverberated1[ 7]
aboutput[ 8] = aboutput[ 8] + abreverberated1[ 8]
aboutput[ 9] = aboutput[ 9] + abreverberated1[ 9]
aboutput[10] = aboutput[10] + abreverberated1[10]
aboutput[11] = aboutput[11] + abreverberated1[11]
aboutput[12] = aboutput[12] + abreverberated1[12]
aboutput[13] = aboutput[13] + abreverberated1[13]
aboutput[14] = aboutput[14] + abreverberated1[14]
aboutput[15] = aboutput[15] + abreverberated1[15]
abreverberated2 bformenc1 afiltc2, iA2, iE2
aboutput[ 0] = aboutput[ 0] + abreverberated2[ 0]
aboutput[ 1] = aboutput[ 1] + abreverberated2[ 1]
aboutput[ 2] = aboutput[ 2] + abreverberated2[ 2]
aboutput[ 3] = aboutput[ 3] + abreverberated2[ 3]
aboutput[ 4] = aboutput[ 4] + abreverberated2[ 4]
aboutput[ 5] = aboutput[ 5] + abreverberated2[ 5]
aboutput[ 6] = aboutput[ 6] + abreverberated2[ 6]
aboutput[ 7] = aboutput[ 7] + abreverberated2[ 7]
aboutput[ 8] = aboutput[ 8] + abreverberated2[ 8]
aboutput[ 9] = aboutput[ 9] + abreverberated2[ 9]
aboutput[10] = aboutput[10] + abreverberated2[10]
aboutput[11] = aboutput[11] + abreverberated2[11]
aboutput[12] = aboutput[12] + abreverberated2[12]
aboutput[13] = aboutput[13] + abreverberated2[13]
aboutput[14] = aboutput[14] + abreverberated2[14]
aboutput[15] = aboutput[15] + abreverberated2[15]
abreverberated2 bformenc1 afiltc3, iA3, iE3
aboutput[ 0] = aboutput[ 0] + abreverberated3[ 0]
aboutput[ 1] = aboutput[ 1] + abreverberated3[ 1]
aboutput[ 2] = aboutput[ 2] + abreverberated3[ 2]
aboutput[ 3] = aboutput[ 3] + abreverberated3[ 3]
aboutput[ 4] = aboutput[ 4] + abreverberated3[ 4]
aboutput[ 5] = aboutput[ 5] + abreverberated3[ 5]
aboutput[ 6] = aboutput[ 6] + abreverberated3[ 6]
aboutput[ 7] = aboutput[ 7] + abreverberated3[ 7]
aboutput[ 8] = aboutput[ 8] + abreverberated3[ 8]
aboutput[ 9] = aboutput[ 9] + abreverberated3[ 9]
aboutput[10] = aboutput[10] + abreverberated3[10]
aboutput[11] = aboutput[11] + abreverberated3[11]
aboutput[12] = aboutput[12] + abreverberated3[12]
aboutput[13] = aboutput[13] + abreverberated3[13]
aboutput[14] = aboutput[14] + abreverberated3[14]
aboutput[15] = aboutput[15] + abreverberated3[15]
abreverberated4 bformenc1 afiltc4, iA4, iE4
aboutput[ 0] = aboutput[ 0] + abreverberated4[ 0]
aboutput[ 1] = aboutput[ 1] + abreverberated4[ 1]
aboutput[ 2] = aboutput[ 2] + abreverberated4[ 2]
aboutput[ 3] = aboutput[ 3] + abreverberated4[ 3]
aboutput[ 4] = aboutput[ 4] + abreverberated4[ 4]
aboutput[ 5] = aboutput[ 5] + abreverberated4[ 5]
aboutput[ 6] = aboutput[ 6] + abreverberated4[ 6]
aboutput[ 7] = aboutput[ 7] + abreverberated4[ 7]
aboutput[ 8] = aboutput[ 8] + abreverberated4[ 8]
aboutput[ 9] = aboutput[ 9] + abreverberated4[ 9]
aboutput[10] = aboutput[10] + abreverberated4[10]
aboutput[11] = aboutput[11] + abreverberated4[11]
aboutput[12] = aboutput[12] + abreverberated4[12]
aboutput[13] = aboutput[13] + abreverberated4[13]
aboutput[14] = aboutput[14] + abreverberated4[14]
aboutput[15] = aboutput[15] + abreverberated4[15]
abreverberated5 bformenc1 afiltc5, iA5, iE5
aboutput[ 0] = aboutput[ 0] + abreverberated5[ 0]
aboutput[ 1] = aboutput[ 1] + abreverberated5[ 1]
aboutput[ 2] = aboutput[ 2] + abreverberated5[ 2]
aboutput[ 3] = aboutput[ 3] + abreverberated5[ 3]
aboutput[ 4] = aboutput[ 4] + abreverberated5[ 4]
aboutput[ 5] = aboutput[ 5] + abreverberated5[ 5]
aboutput[ 6] = aboutput[ 6] + abreverberated5[ 6]
aboutput[ 7] = aboutput[ 7] + abreverberated5[ 7]
aboutput[ 8] = aboutput[ 8] + abreverberated5[ 8]
aboutput[ 9] = aboutput[ 9] + abreverberated5[ 9]
aboutput[10] = aboutput[10] + abreverberated5[10]
aboutput[11] = aboutput[11] + abreverberated5[11]
aboutput[12] = aboutput[12] + abreverberated5[12]
aboutput[13] = aboutput[13] + abreverberated5[13]
aboutput[14] = aboutput[14] + abreverberated5[14]
aboutput[15] = aboutput[15] + abreverberated5[15]
abreverberated6 bformenc1 afiltc6, iA6, iE6
aboutput[ 0] = aboutput[ 0] + abreverberated6[ 0]
aboutput[ 1] = aboutput[ 1] + abreverberated6[ 1]
aboutput[ 2] = aboutput[ 2] + abreverberated6[ 2]
aboutput[ 3] = aboutput[ 3] + abreverberated6[ 3]
aboutput[ 4] = aboutput[ 4] + abreverberated6[ 4]
aboutput[ 5] = aboutput[ 5] + abreverberated6[ 5]
aboutput[ 6] = aboutput[ 6] + abreverberated6[ 6]
aboutput[ 7] = aboutput[ 7] + abreverberated6[ 7]
aboutput[ 8] = aboutput[ 8] + abreverberated6[ 8]
aboutput[ 9] = aboutput[ 9] + abreverberated6[ 9]
aboutput[10] = aboutput[10] + abreverberated6[10]
aboutput[11] = aboutput[11] + abreverberated6[11]
aboutput[12] = aboutput[12] + abreverberated6[12]
aboutput[13] = aboutput[13] + abreverberated6[13]
aboutput[14] = aboutput[14] + abreverberated6[14]
aboutput[15] = aboutput[15] + abreverberated6[15]
abreverberated7 bformenc1 afiltc7, iA7, iE7
aboutput[ 0] = aboutput[ 0] + abreverberated7[ 0]
aboutput[ 1] = aboutput[ 1] + abreverberated7[ 1]
aboutput[ 2] = aboutput[ 2] + abreverberated7[ 2]
aboutput[ 3] = aboutput[ 3] + abreverberated7[ 3]
aboutput[ 4] = aboutput[ 4] + abreverberated7[ 4]
aboutput[ 5] = aboutput[ 5] + abreverberated7[ 5]
aboutput[ 6] = aboutput[ 6] + abreverberated7[ 6]
aboutput[ 7] = aboutput[ 7] + abreverberated7[ 7]
aboutput[ 8] = aboutput[ 8] + abreverberated7[ 8]
aboutput[ 9] = aboutput[ 9] + abreverberated7[ 9]
aboutput[10] = aboutput[10] + abreverberated7[10]
aboutput[11] = aboutput[11] + abreverberated7[11]
aboutput[12] = aboutput[12] + abreverberated7[12]
aboutput[13] = aboutput[13] + abreverberated7[13]
aboutput[14] = aboutput[14] + abreverberated7[14]
aboutput[15] = aboutput[15] + abreverberated7[15]
abreverberated8 bformenc1 afiltc8, iA8, iE8
aboutput[ 0] = aboutput[ 0] + abreverberated8[ 0]
aboutput[ 1] = aboutput[ 1] + abreverberated8[ 1]
aboutput[ 2] = aboutput[ 2] + abreverberated8[ 2]
aboutput[ 3] = aboutput[ 3] + abreverberated8[ 3]
aboutput[ 4] = aboutput[ 4] + abreverberated8[ 4]
aboutput[ 5] = aboutput[ 5] + abreverberated8[ 5]
aboutput[ 6] = aboutput[ 6] + abreverberated8[ 6]
aboutput[ 7] = aboutput[ 7] + abreverberated8[ 7]
aboutput[ 8] = aboutput[ 8] + abreverberated8[ 8]
aboutput[ 9] = aboutput[ 9] + abreverberated8[ 9]
aboutput[10] = aboutput[10] + abreverberated8[10]
aboutput[11] = aboutput[11] + abreverberated8[11]
aboutput[12] = aboutput[12] + abreverberated8[12]
aboutput[13] = aboutput[13] + abreverberated8[13]
aboutput[14] = aboutput[14] + abreverberated8[14]
aboutput[15] = aboutput[15] + abreverberated8[15]
abreverberated9 bformenc1 afiltc9, iA9, iE9
aboutput[ 0] = aboutput[ 0] + abreverberated9[ 0]
aboutput[ 1] = aboutput[ 1] + abreverberated9[ 1]
aboutput[ 2] = aboutput[ 2] + abreverberated9[ 2]
aboutput[ 3] = aboutput[ 3] + abreverberated9[ 3]
aboutput[ 4] = aboutput[ 4] + abreverberated9[ 4]
aboutput[ 5] = aboutput[ 5] + abreverberated9[ 5]
aboutput[ 6] = aboutput[ 6] + abreverberated9[ 6]
aboutput[ 7] = aboutput[ 7] + abreverberated9[ 7]
aboutput[ 8] = aboutput[ 8] + abreverberated9[ 8]
aboutput[ 9] = aboutput[ 9] + abreverberated9[ 9]
aboutput[10] = aboutput[10] + abreverberated9[10]
aboutput[11] = aboutput[11] + abreverberated9[11]
aboutput[12] = aboutput[12] + abreverberated9[12]
aboutput[13] = aboutput[13] + abreverberated9[13]
aboutput[14] = aboutput[14] + abreverberated9[14]
aboutput[15] = aboutput[15] + abreverberated9[15]
abreverberated10 bformenc1 afiltc10, iA10, iE10
aboutput[ 0] = aboutput[ 0] + abreverberated10[ 0]
aboutput[ 1] = aboutput[ 1] + abreverberated10[ 1]
aboutput[ 2] = aboutput[ 2] + abreverberated10[ 2]
aboutput[ 3] = aboutput[ 3] + abreverberated10[ 3]
aboutput[ 4] = aboutput[ 4] + abreverberated10[ 4]
aboutput[ 5] = aboutput[ 5] + abreverberated10[ 5]
aboutput[ 6] = aboutput[ 6] + abreverberated10[ 6]
aboutput[ 7] = aboutput[ 7] + abreverberated10[ 7]
aboutput[ 8] = aboutput[ 8] + abreverberated10[ 8]
aboutput[ 9] = aboutput[ 9] + abreverberated10[ 9]
aboutput[10] = aboutput[10] + abreverberated10[10]
aboutput[11] = aboutput[11] + abreverberated10[11]
aboutput[12] = aboutput[12] + abreverberated10[12]
aboutput[13] = aboutput[13] + abreverberated10[13]
aboutput[14] = aboutput[14] + abreverberated10[14]
aboutput[15] = aboutput[15] + abreverberated10[15]
abreverberated11 bformenc1 afiltc11, iA11, iE11
aboutput[ 0] = aboutput[ 0] + abreverberated11[ 0]
aboutput[ 1] = aboutput[ 1] + abreverberated11[ 1]
aboutput[ 2] = aboutput[ 2] + abreverberated11[ 2]
aboutput[ 3] = aboutput[ 3] + abreverberated11[ 3]
aboutput[ 4] = aboutput[ 4] + abreverberated11[ 4]
aboutput[ 5] = aboutput[ 5] + abreverberated11[ 5]
aboutput[ 6] = aboutput[ 6] + abreverberated11[ 6]
aboutput[ 7] = aboutput[ 7] + abreverberated11[ 7]
aboutput[ 8] = aboutput[ 8] + abreverberated11[ 8]
aboutput[ 9] = aboutput[ 9] + abreverberated11[ 9]
aboutput[10] = aboutput[10] + abreverberated11[10]
aboutput[11] = aboutput[11] + abreverberated11[11]
aboutput[12] = aboutput[12] + abreverberated11[12]
aboutput[13] = aboutput[13] + abreverberated11[13]
aboutput[14] = aboutput[14] + abreverberated11[14]
aboutput[15] = aboutput[15] + abreverberated11[15]
abreverberated12 bformenc1 afiltc12, iA12, iE12
aboutput[ 0] = aboutput[ 0] + abreverberated12[ 0]
aboutput[ 1] = aboutput[ 1] + abreverberated12[ 1]
aboutput[ 2] = aboutput[ 2] + abreverberated12[ 2]
aboutput[ 3] = aboutput[ 3] + abreverberated12[ 3]
aboutput[ 4] = aboutput[ 4] + abreverberated12[ 4]
aboutput[ 5] = aboutput[ 5] + abreverberated12[ 5]
aboutput[ 6] = aboutput[ 6] + abreverberated12[ 6]
aboutput[ 7] = aboutput[ 7] + abreverberated12[ 7]
aboutput[ 8] = aboutput[ 8] + abreverberated12[ 8]
aboutput[ 9] = aboutput[ 9] + abreverberated12[ 9]
aboutput[10] = aboutput[10] + abreverberated12[10]
aboutput[11] = aboutput[11] + abreverberated12[11]
aboutput[12] = aboutput[12] + abreverberated12[12]
aboutput[13] = aboutput[13] + abreverberated12[13]
aboutput[14] = aboutput[14] + abreverberated12[14]
aboutput[15] = aboutput[15] + abreverberated12[15]
outletv "outbformat", aboutput
if gk_Spatialize_Verbose != 0 then
aoutput sum aboutput[0], aboutput[1], aboutput[2], aboutput[3], aboutput[4], aboutput[5], aboutput[6], aboutput[7], aboutput[8], aboutput[9], aboutput[10], aboutput[11], aboutput[12], aboutput[13], aboutput[14], aboutput[15]
kelapsed timeinsts
printks "SpatialReverb i %5.2f t %5.2f [%5.2f] d %5.2f c %9.2f in %5.2f A %5.2f\n", 1.0, p1, p2, kelapsed, p3, kcutoff, asignal, aoutput
endif
endin

gk_Bformat2DecodeBinaural_krelcoef init 1
opcode Bformat2DecodeBinaural, a[], a[]i
absignal[], isr xin
krelcoef = gk_Bformat2DecodeBinaural_krelcoef
aw = absignal[ 0]
ax = absignal[ 1]
ay = absignal[ 2]
az = absignal[ 3]
ar = absignal[ 4]
as = absignal[ 5]
at = absignal[ 6]
au = absignal[ 7]
av = absignal[ 8]
; Decode 2nd order ambisonic as if it is a 12 channel dodecahedron loudspeaker rig.
; This configuration produces an 'in-phase' response and each speaker will have only a single maximum given a moving virtual sound source.
; w x y z r s t u v
; Speaker 1 <0.0000,0.0000,1.0000> centre top
achnl_co_1 sum aw * 0.1179, az * 0.1316, ar * 0.0491
; Speaker 2 <0.0000,0.0000,-1.0000> centre bottom
achnl_co_2 sum aw * 0.1179, az * -0.1316, ar * 0.0491
; Speaker 3 <0.7236,0.5257,0.4472>
achnl_co_3 sum aw * 0.1179, ax * 0.0952, ay * 0.0692, az * 0.0589, ar * -0.0098, as * 0.0238, at * 0.0173, au * 0.0091, av * 0.0280
; Speaker 4 <-0.7236,-0.5257,-0.4472>
achnl_co_4 sum aw * 0.1179, ax * -0.0952, ay * -0.0692, az * -0.0589, ar * -0.0098, as * 0.0238, at * 0.0173, au * 0.0091, av * 0.0280
; Speaker 5 <0.7236,-0.5257,0.4472>
achnl_co_5 sum aw * 0.1179, ax * 0.0952, ay * -0.0692, az * 0.0589, ar * -0.0098, as * 0.0238, at * -0.0173, au * 0.0091, av * -0.0280
; Speaker 6 <-0.7236,0.5257,-0.4472>
achnl_co_6 sum aw * 0.1179, ax * -0.0952, ay * 0.0692, az * -0.0589, ar * -0.0098, as * 0.0238, at * -0.0173, au * 0.0091, av * -0.0280
; Speaker 7 <-0.2764,0.8507,0.4472>
achnl_co_7 sum aw * 0.1179, ax * -0.0364, ay * 0.1119, az * 0.0589, ar * -0.0098, as * -0.0091, at * 0.0280, au * -0.0238, av * -0.0173
; Speaker 8 <0.2764,-0.8507,-0.4472>
achnl_co_8 sum aw * 0.1179, ax * 0.0364, ay * -0.1119, az * -0.0589, ar * -0.0098, as * -0.0091, at * 0.0280, au * -0.0238, av * -0.0173
; Speaker 9 <-0.2764,-0.8507,0.4472>
achnl_co_9 sum aw * 0.1179, ax * -0.0364, ay * -0.1119, az * 0.0589, ar * -0.0098, as * -0.0091, at * -0.0280, au * -0.0238, av * 0.0173
; Speaker 10 <0.2764,0.8507,-0.4472>
achnl_co_10 sum aw * 0.1179, ax * 0.0364, ay * 0.1119, az * -0.0589, ar * -0.0098, as * -0.0091, at * -0.0280, au * -0.0238, av * 0.0173
; Speaker 11 <-0.8944,0.0000,0.4472>
achnl_co_11 sum aw * 0.1179, ax * -0.1177, az * 0.0589, ar * -0.0098, as * -0.0295, au * 0.0295
; Speaker 12 <0.8944,0.0000,-0.4472>
achnl_co_12 sum aw * 0.1179, ax * 0.1177, az * -0.0589, ar * -0.0098, as * -0.0295, au * 0.0295
; Rig Decode Matrix to Reproduce Spherical Harmonics (Second Order)
; This rig configuration produces a strict idealised response that satisfies the Ambisonic matching equations. Generally this type of configuration produces a relatively small stable listening area.
; w x y z r s t u v
; Speaker 1 <0.0000,0.0000,1.0000> centre top
achnl_sh_1 sum aw * 0.1179, az * 0.2500, ar * 0.4167
; Speaker 2 <0.0000,0.0000,-1.0000> centre bottom
achnl_sh_2 sum aw * 0.1179, az * -0.2500, ar * 0.4167
; Speaker 3 <0.7236,0.5257,0.4472>
achnl_sh_3 sum aw * 0.1179, ax * 0.1809, ay * 0.1314, az * 0.1118, ar * -0.0833, as * 0.2023, at * 0.1469, au * 0.0773, av * 0.2378
; Speaker 4 <-0.7236,-0.5257,-0.4472>
achnl_sh_4 sum aw * 0.1179, ax * -0.1809, ay * -0.1314, az * -0.1118, ar * -0.0833, as * 0.2023, at * 0.1469, au * 0.0773, av * 0.2378
; Speaker 5 <0.7236,-0.5257,0.4472>
achnl_sh_5 sum aw * 0.1179, ax * 0.1809, ay * -0.1314, az * 0.1118, ar * -0.0833, as * 0.2023, at * -0.1469, au * 0.0773, av * -0.2378
; Speaker 6 <-0.7236,0.5257,-0.4472>
achnl_sh_6 sum aw * 0.1179, ax * -0.1809, ay * 0.1314, az * -0.1118, ar * -0.0833, as * 0.2023, at * -0.1469, au * 0.0773, av * -0.2378
; Speaker 7 <-0.2764,0.8507,0.4472>
achnl_sh_7 sum aw * 0.1179, ax * -0.0691, ay * 0.2127, az * 0.1118, ar * -0.0833, as * -0.0773, at * 0.2378, au * -0.2023, av * -0.1469
; Speaker 8 <0.2764,-0.8507,-0.4472>
achnl_sh_8 sum aw * 0.1179, ax * 0.0691, ay * -0.2127, az * -0.1118, ar * -0.0833, as * -0.0773, at * 0.2378, au * -0.2023, av * -0.1469
; Speaker 9 <-0.2764,-0.8507,0.4472>
achnl_sh_9 sum aw * 0.1179, ax * -0.0691, ay * -0.2127, az * 0.1118, ar * -0.0833, as * -0.0773, at * -0.2378, au * -0.2023, av * 0.1469
; Speaker 10 <0.2764,0.8507,-0.4472>
achnl_sh_10 sum aw * 0.1179, ax * 0.0691, ay * 0.2127, az * -0.1118, ar * -0.0833, as * -0.0773, at * -0.2378, au * -0.2023, av * 0.1469
; Speaker 11 <-0.8944,0.0000,0.4472>
achnl_sh_11 sum aw * 0.1179, ax * -0.2236, az * 0.1118, ar * -0.0833, as * -0.2500, au * 0.2500
; Speaker 12 <0.8944,0.0000,-0.4472>
achnl_sh_12 sum aw * 0.1179, ax * 0.2236, az * -0.1118, ar * -0.0833, as * -0.2500, au * 0.2500
achnl_1 = achnl_co_1 * krelcoef + achnl_sh_1 * (1 - krelcoef) ; ratio between controlled opposites and spherical harmonic coefficients
achnl_2 = achnl_co_2 * krelcoef + achnl_sh_2 * (1 - krelcoef) ; ratio between controlled opposites and spherical harmonic coefficients
achnl_3 = achnl_co_3 * krelcoef + achnl_sh_3 * (1 - krelcoef) ; ratio between controlled opposites and spherical harmonic coefficients
achnl_4 = achnl_co_4 * krelcoef + achnl_sh_4 * (1 - krelcoef) ; ratio between controlled opposites and spherical harmonic coefficients
achnl_5 = achnl_co_5 * krelcoef + achnl_sh_5 * (1 - krelcoef) ; ratio between controlled opposites and spherical harmonic coefficients
achnl_6 = achnl_co_6 * krelcoef + achnl_sh_6 * (1 - krelcoef) ; ratio between controlled opposites and spherical harmonic coefficients
achnl_7 = achnl_co_7 * krelcoef + achnl_sh_7 * (1 - krelcoef) ; ratio between controlled opposites and spherical harmonic coefficients
achnl_8 = achnl_co_8 * krelcoef + achnl_sh_8 * (1 - krelcoef) ; ratio between controlled opposites and spherical harmonic coefficients
achnl_9 = achnl_co_9 * krelcoef + achnl_sh_9 * (1 - krelcoef) ; ratio between controlled opposites and spherical harmonic coefficients
achnl_10 = achnl_co_10 * krelcoef + achnl_sh_10 * (1 - krelcoef) ; ratio between controlled opposites and spherical harmonic coefficients
achnl_11 = achnl_co_11 * krelcoef + achnl_sh_11 * (1 - krelcoef) ; ratio between controlled opposites and spherical harmonic coefficients
achnl_12 = achnl_co_12 * krelcoef + achnl_sh_12 * (1 - krelcoef) ; ratio between controlled opposites and spherical harmonic coefficients
if isr == 44100 then
aleft_1, aright1 hrtfstat achnl_1, 1, 89.99, "hrtf-44100-left.dat", "hrtf-44100-right.dat", 8.8, 44100
aleft_2, aright2 hrtfstat achnl_2, -1, -89.99, "hrtf-44100-left.dat", "hrtf-44100-right.dat", 8.8, 44100
aleft_3, aright_3 hrtfstat achnl_3, -36, 26.565, "hrtf-44100-left.dat", "hrtf-44100-right.dat", 8.8, 44100
aleft_4, aright_4 hrtfstat achnl_4, 144, -26.565, "hrtf-44100-left.dat", "hrtf-44100-right.dat", 8.8, 44100
aleft_5, aright_5 hrtfstat achnl_5, 36, 26.565, "hrtf-44100-left.dat", "hrtf-44100-right.dat", 8.8, 44100
aleft_6, aright_6 hrtfstat achnl_6, -144, -26.565, "hrtf-44100-left.dat", "hrtf-44100-right.dat", 8.8, 44100
aleft_7, aright_7 hrtfstat achnl_7, -108, 26.565, "hrtf-44100-left.dat", "hrtf-44100-right.dat", 8.8, 44100
aleft_8, aright_8 hrtfstat achnl_8, 72, -26.565, "hrtf-44100-left.dat", "hrtf-44100-right.dat", 8.8, 44100
aleft_9, aright_9 hrtfstat achnl_9, 108, 26.565, "hrtf-44100-left.dat", "hrtf-44100-right.dat", 8.8, 44100
aleft_10, aright_10 hrtfstat achnl_10, -72, -26.565, "hrtf-44100-left.dat", "hrtf-44100-right.dat", 8.8, 44100
aleft_11, aright_11 hrtfstat achnl_11, 180, 26.565, "hrtf-44100-left.dat", "hrtf-44100-right.dat", 8.8, 44100
aleft_12, aright_12 hrtfstat achnl_12, 0, -26.565, "hrtf-44100-left.dat", "hrtf-44100-right.dat", 8.8, 44100
else
if isr == 48000 then
aleft_1, aright_1 hrtfstat achnl_1, 1, 89.99, "hrtf-48000-left.dat", "hrtf-48000-right.dat", 8.8, 48000
aleft_2, aright_2 hrtfstat achnl_2, -1, -89.99, "hrtf-48000-left.dat", "hrtf-48000-right.dat", 8.8, 48000
aleft_3, aright_3 hrtfstat achnl_3, -36, 26.565, "hrtf-48000-left.dat", "hrtf-48000-right.dat", 8.8, 48000
aleft_4, aright_4 hrtfstat achnl_4, 144, -26.565, "hrtf-48000-left.dat", "hrtf-48000-right.dat", 8.8, 48000
aleft_5, aright_5 hrtfstat achnl_5, 36, 26.565, "hrtf-48000-left.dat", "hrtf-48000-right.dat", 8.8, 48000
aleft_6, aright_6 hrtfstat achnl_6, -144, -26.565, "hrtf-48000-left.dat", "hrtf-48000-right.dat", 8.8, 48000
aleft_7, aright_7 hrtfstat achnl_7, -108, 26.565, "hrtf-48000-left.dat", "hrtf-48000-right.dat", 8.8, 48000
aleft_8, aright_8 hrtfstat achnl_8, 72, -26.565, "hrtf-48000-left.dat", "hrtf-48000-right.dat", 8.8, 48000
aleft_9, aright_9 hrtfstat achnl_9, 108, 26.565, "hrtf-48000-left.dat", "hrtf-48000-right.dat", 8.8, 48000
aleft_10, aright_10 hrtfstat achnl_10, -72, -26.565, "hrtf-48000-left.dat", "hrtf-48000-right.dat", 8.8, 48000
aleft_11, aright_11 hrtfstat achnl_11, 180, 26.565, "hrtf-48000-left.dat", "hrtf-48000-right.dat", 8.8, 48000
aleft_12, aright_12 hrtfstat achnl_12, 0, -26.565, "hrtf-48000-left.dat", "hrtf-48000-right.dat", 8.8, 48000
else
if isr == 96000 then
aleft_1, aright_1 hrtfstat achnl_1, 1, 89.99, "hrtf-96000-left.dat", "hrtf-96000-right.dat", 8.8, 96000
aleft_2, aright_2 hrtfstat achnl_2, -1, -89.99, "hrtf-96000-left.dat", "hrtf-96000-right.dat", 8.8, 96000
aleft_3, aright_3 hrtfstat achnl_3, -36, 26.565, "hrtf-96000-left.dat", "hrtf-96000-right.dat", 8.8, 96000
aleft_4, aright_4 hrtfstat achnl_4, 144, -26.565, "hrtf-96000-left.dat", "hrtf-96000-right.dat", 8.8, 96000
aleft_5, aright_5 hrtfstat achnl_5, 36, 26.565, "hrtf-96000-left.dat", "hrtf-96000-right.dat", 8.8, 96000
aleft_6, aright_6 hrtfstat achnl_6, -144, -26.565, "hrtf-96000-left.dat", "hrtf-96000-right.dat", 8.8, 96000
aleft_7, aright_7 hrtfstat achnl_7, -108, 26.565, "hrtf-96000-left.dat", "hrtf-96000-right.dat", 8.8, 96000
aleft_8, aright_8 hrtfstat achnl_8, 72, -26.565, "hrtf-96000-left.dat", "hrtf-96000-right.dat", 8.8, 96000
aleft_9, aright_9 hrtfstat achnl_9, 108, 26.565, "hrtf-96000-left.dat", "hrtf-96000-right.dat", 8.8, 96000
aleft_10, aright_10 hrtfstat achnl_10, -72, -26.565, "hrtf-96000-left.dat", "hrtf-96000-right.dat", 8.8, 96000
aleft_11, aright_11 hrtfstat achnl_11, 180, 26.565, "hrtf-96000-left.dat", "hrtf-96000-right.dat", 8.8, 96000
aleft_12, aright_12 hrtfstat achnl_12, 0, -26.565, "hrtf-96000-left.dat", "hrtf-96000-right.dat", 8.8, 96000
else
printks "Sampling rate = %f is not handled by Bformat2DecodeBinaural.", 1, sr
exitnow
endif
endif
endif
prints "Loaded HRTF data.\n"
; Inter-aural time delay (seconds)
aleft_1 delay aleft_1, 0.00028986
aleft_2 delay aleft_2, 0.00028986
aleft_3 delay aleft_3, 0.00020974
aleft_4 delay aleft_4, 0.00036997
aleft_5 delay aleft_5, 0.00036997
aleft_6 delay aleft_6, 0.00020974
aleft_7 delay aleft_7, 0.00008011
aleft_8 delay aleft_8, 0.00049960
aleft_9 delay aleft_9, 0.00049960
aleft_10 delay aleft_10, 0.00008011
aleft_11 delay aleft_11, 0.00028986
aleft_12 delay aleft_12, 0.00028986
aright_1 delay aright_1, 0.00028986
aright_2 delay aright_2, 0.00028986
aright_3 delay aright_3, 0.00036997
aright_4 delay aright_4, 0.00020974
aright_5 delay aright_5, 0.00020974
aright_6 delay aright_6, 0.00036997
aright_7 delay aright_7, 0.000499597
aright_8 delay aright_8, 0.00008011
aright_9 delay aright_9, 0.00008011
aright_10 delay aright_10, 0.000499597
aright_11 delay aright_11, 0.00028986
aright_12 delay aright_12, 0.00028986
asignal[] init 2
aleft sum aright_1, aright_2, aright_3, aright_4, aright_5, aright_6, aright_7, aright_8, aright_9, aright_10, aright_11, aright_12
aright sum aleft_1, aleft_2, aleft_3, aleft_4, aleft_5, aleft_6, aleft_7, aleft_8, aleft_9, aleft_10, aleft_11, aleft_12
asignal[0] dcblock aleft
asignal[1] dcblock aright
xout asignal
endop

gk_BformatDecoder_MasterLevel init 0.0
instr BformatDecoder
irig init i(gk_BformatDecoder_SpeakerRig)
kmasterlevel = pow(10, gk_BformatDecoder_MasterLevel / 20)
asignal[] init nchnls
absignal[] init 16
absignal[] inletv "inbformat"
if irig == 0 then
 asignal bformdec1 1, absignal
elseif irig == 1 then
 asignal Bformat2DecodeBinaural, absignal, sr
 asignal[0] = asignal[0] * kmasterlevel
 asignal[1] = asignal[1] * kmasterlevel
elseif irig == 2 then
 asignal bformdec1 2, absignal
 asignal[0] = asignal[0] * kmasterlevel
 asignal[1] = asignal[1] * kmasterlevel
 asignal[2] = asignal[2] * kmasterlevel
 asignal[3] = asignal[3] * kmasterlevel
elseif irig == 3 then
 asignal bformdec1 3, absignal
 asignal[0] = asignal[0] * kmasterlevel
 asignal[1] = asignal[1] * kmasterlevel
 asignal[2] = asignal[2] * kmasterlevel
 asignal[3] = asignal[3] * kmasterlevel
 asignal[4] = asignal[4] * kmasterlevel
else
 asignal bformdec1 irig, absignal
 asignal[0] = asignal[0] * kmasterlevel
 asignal[1] = asignal[1] * kmasterlevel
 asignal[2] = asignal[2] * kmasterlevel
 asignal[3] = asignal[3] * kmasterlevel
 asignal[4] = asignal[4] * kmasterlevel
 asignal[5] = asignal[5] * kmasterlevel
 asignal[6] = asignal[6] * kmasterlevel
 asignal[7] = asignal[7] * kmasterlevel
endif
out asignal
endin
                                

giFlatQ                         init                    sqrt(0.5)
giseed				            init                    0.5

gkHarpsichordGain               chnexport               "gkHarpsichordGain",            1
gkHarpsichordGain               init                    1
gkHarpsichordPan                chnexport               "gkHarpsichordPan",             1
gkHarpsichordPan                init                    0.5

gkChebyshevDroneCoefficient1    chnexport               "gkChebyshevDroneCoefficient1", 1
gkChebyshevDroneCoefficient1    init                    0.5
gkChebyshevDroneCoefficient2    chnexport               "gkChebyshevDroneCoefficient2", 1
gkChebyshevDroneCoefficient3    chnexport               "gkChebyshevDroneCoefficient3", 1
gkChebyshevDroneCoefficient4    chnexport               "gkChebyshevDroneCoefficient4", 1
gkChebyshevDroneCoefficient5    chnexport               "gkChebyshevDroneCoefficient5", 1
gkChebyshevDroneCoefficient6    chnexport               "gkChebyshevDroneCoefficient6", 1
gkChebyshevDroneCoefficient7    chnexport               "gkChebyshevDroneCoefficient7", 1
gkChebyshevDroneCoefficient8    chnexport               "gkChebyshevDroneCoefficient8", 1
gkChebyshevDroneCoefficient9    chnexport               "gkChebyshevDroneCoefficient9", 1
gkChebyshevDroneCoefficient10   chnexport               "gkChebyshevDroneCoefficient10", 1
gkChebyshevDroneCoefficient10   init                    0.05

gkReverberationEnabled          chnexport               "gkReverberationEnabled", 1
gkReverberationEnabled          init                    1
gkReverberationDelay            chnexport               "gkReverberationDelay", 1
gkReverberationDelay            init                    0.325
gkReverberationWet          	chnexport               "gkReverberationWet", 1
gkReverberationWet          	init                    0.15

gkMasterLevel                   chnexport               "gkMasterLevel", 1
gkMasterLevel                   init                    1.5

                                connect                 "BanchoffKleinBottle",  "outleft", 	"Reverberation",        "inleft"
                                connect                 "BanchoffKleinBottle",  "outright", "Reverberation",        "inright"
                                connect                 "BandedWG",             "outleft", 	"Reverberation",        "inleft"
                                connect                 "BandedWG",             "outright", "Reverberation",        "inright"
                                connect                 "BassModel",            "outleft", 	"Reverberation",        "inleft"
                                connect                 "BassModel",            "outright", "Reverberation",        "inright"
                                connect                 "ChebyshevDrone",       "outleft", 	"Reverberation",        "inleft"
                                connect                 "ChebyshevDrone",       "outright", "Reverberation",        "inright"
                                connect                 "ChebyshevMelody",      "outleft", 	"Reverberation",        "inleft"
                                connect                 "ChebyshevMelody",      "outright", "Reverberation",        "inright"
                                connect                 "DelayedPluckedString", "outleft", 	"Reverberation",        "inleft"
                                connect                 "DelayedPluckedString", "outright", "Reverberation",        "inright"
                                connect                 "EnhancedFMBell",       "outleft", 	"Reverberation",        "inleft"
                                connect                 "EnhancedFMBell",       "outright", "Reverberation",        "inright"
                                connect                 "FenderRhodesModel",    "outleft", 	"Reverberation",        "inleft"
                                connect                 "FenderRhodesModel",    "outright", "Reverberation",        "inright"
                                connect                 "FilteredSines",        "outleft", 	"Reverberation",        "inleft"
                                connect                 "FilteredSines",        "outright", "Reverberation",        "inright"
                                connect                 "Flute",                "outleft", 	"Reverberation",        "inleft"
                                connect                 "Flute",                "outright", "Reverberation",        "inright"
                                connect                 "FMModulatedChorusing", "outleft", 	"Reverberation",        "inleft"
                                connect                 "FMModulatedChorusing", "outright", "Reverberation",        "inright"
                                connect                 "FMModerateIndex",      "outleft", 	"Reverberation",        "inleft"
                                connect                 "FMModerateIndex",      "outright", "Reverberation",        "inright"
                                connect                 "FMModerateIndex2",     "outleft", 	"Reverberation",        "inleft"
                                connect                 "FMModerateIndex2",     "outright", "Reverberation",        "inright"
                                connect                 "FMWaterBell",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "FMWaterBell",          "outright", "Reverberation",        "inright"
                                connect                 "Granular",             "outleft", 	"Reverberation",        "inleft"
                                connect                 "Granular",             "outright", "Reverberation",        "inright"
                                connect                 "Guitar",               "outleft", 	"Reverberation",        "inleft"
                                connect                 "Guitar",               "outright", "Reverberation",        "inright"
                                connect                 "Guitar2",              "outleft", 	"Reverberation",        "inleft"
                                connect                 "Guitar2",              "outright", "Reverberation",        "inright"
                                connect                 "Harpsichord",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "Harpsichord",          "outright", "Reverberation",        "inright"
                                connect                 "HeavyMetalModel",      "outleft", 	"Reverberation",        "inleft"
                                connect                 "HeavyMetalModel",      "outright", "Reverberation",        "inright"
                                connect                 "Hypocycloid",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "Hypocycloid",          "outright", "Reverberation",        "inright"
                                connect                 "KungModulatedFM",      "outleft", 	"Reverberation",        "inleft"
                                connect                 "KungModulatedFM",      "outright", "Reverberation",        "inright"
                                connect                 "ModerateFM",          	"outleft", 	"Reverberation",        "inleft"
                                connect                 "ModerateFM",          	"outright", "Reverberation",        "inright"
                                connect                 "ModulatedFM",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "ModulatedFM",          "outright", "Reverberation",        "inright"
                                connect                 "Melody",               "outleft", 	"Reverberation",        "inleft"
                                connect                 "Melody",               "outright", "Reverberation",        "inright"
                                connect                 "PlainPluckedString",   "outleft", 	"Reverberation",        "inleft"
                                connect                 "PlainPluckedString",   "outright", "Reverberation",        "inright"
                                connect                 "PRCBeeThree",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "PRCBeeThree",          "outright", "Reverberation",        "inright"
                                connect                 "PRCBeeThreeDelayed",   "outleft", 	"Reverberation",        "inleft"
                                connect                 "PRCBeeThreeDelayed",   "outright", "Reverberation",        "inright"
                                connect                 "PRCBowed",             "outleft", 	"Reverberation",        "inleft"
                                connect                 "PRCBowed",             "outright", "Reverberation",        "inright"
                                connect                 "Reverberation",        "outleft", 	"MasterOutput",           "inleft"
                                connect                 "Reverberation",        "outright", "MasterOutput",           "inright"
                                connect                 "STKBandedWG",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKBandedWG",          "outright", "Reverberation",        "inright"
                                connect                 "STKBeeThree",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKBeeThree",          "outright", "Reverberation",        "inright"
                                connect                 "STKBlowBotl",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKBlowBotl",          "outright", "Reverberation",        "inright"
                                connect                 "STKBlowHole",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKBlowHole",          "outright", "Reverberation",        "inright"
                                connect                 "STKBowed",             "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKBowed",             "outright", "Reverberation",        "inright"
                                connect                 "STKClarinet",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKClarinet",          "outright", "Reverberation",        "inright"
                                connect                 "STKDrummer",           "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKDrummer",           "outright", "Reverberation",        "inright"
                                connect                 "STKFlute",             "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKFlute",             "outright", "Reverberation",        "inright"
                                connect                 "STKFMVoices",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKFMVoices",          "outright", "Reverberation",        "inright"
                                connect                 "STKHvyMetl",           "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKHvyMetl",           "outright", "Reverberation",        "inright"
                                connect                 "STKMandolin",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKMandolin",          "outright", "Reverberation",        "inright"
                                connect                 "STKModalBar",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKModalBar",          "outright", "Reverberation",        "inright"
                                connect                 "STKMoog",              "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKMoog",              "outright", "Reverberation",        "inright"
                                connect                 "STKPercFlut",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKPercFlut",          "outright", "Reverberation",        "inright"
                                connect                 "STKPlucked",           "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKPlucked",           "outright", "Reverberation",        "inright"
                                connect                 "STKResonate",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKResonate",          "outright", "Reverberation",        "inright"
                                connect                 "STKRhodey",            "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKRhodey",            "outright", "Reverberation",        "inright"
                                connect                 "STKSaxofony",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKSaxofony",          "outright", "Reverberation",        "inright"
                                connect                 "STKShakers",           "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKShakers",           "outright", "Reverberation",        "inright"
                                connect                 "STKSimple",            "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKSimple",            "outright", "Reverberation",        "inright"
                                connect                 "STKSitar",             "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKSitar",             "outright", "Reverberation",        "inright"
                                connect                 "STKTubeBell",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKTubeBell",          "outright", "Reverberation",        "inright"
                                connect                 "STKVoicForm",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKVoicForm",          "outright", "Reverberation",        "inright"
                                connect                 "STKWhistle",           "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKWhistle",           "outright", "Reverberation",        "inright"
                                connect                 "STKWurley",            "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKWurley",            "outright", "Reverberation",        "inright"
                                connect                 "StringPad",            "outleft", 	"Reverberation",        "inleft"
                                connect                 "StringPad",            "outright", "Reverberation",        "inright"
                                connect                 "ToneWheelOrgan",       "outleft", 	"Reverberation",        "inleft"
                                connect                 "ToneWheelOrgan",       "outright", "Reverberation",        "inright"
                                connect                 "TubularBellModel",     "outleft", 	"Reverberation",        "inleft"
                                connect                 "TubularBellModel",     "outright", "Reverberation",        "inright"
                                connect                 "WaveguideGuitar",      "outleft", 	"Reverberation",        "inleft"
                                connect                 "WaveguideGuitar",      "outright", "Reverberation",        "inright"
                                connect                 "Xing",                 "outleft", 	"Reverberation",        "inleft"
                                connect                 "Xing",                 "outright", "Reverberation",        "inright"
                                connect                 "ZakianFlute",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "ZakianFlute",          "outright", "Reverberation",        "inright"
                                
                                alwayson                "Reverberation"
                                alwayson                "MasterOutput"

                                instr                   BanchoffKleinBottle
                                //////////////////////////////////////////////
                                // Original by Hans Mikelson.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) 
                                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                ;   p1  p2     p3   p4    p5     p6   p7
                                ;       Start  Dur  Amp   Frqc   U    V  
                                ; i 4   32     6    6000  6.00   3    2
                                ; i 4   36     4    .     5.11   5.6  0.4
                                ; i 4   +      4    .     6.05   2    8.5
                                ; i 4   .      2    .     6.02   4    5
                                ; i 4   .      2    .     6.02   5    0.5
iHz                             =                       ifrequency
ifqc                            init                    iHz
ip4                             init                    iamplitude
iu                              init                    5 ; p6
iv                              init                    0.5 ; p7
irt2                            init                    sqrt(2)
aampenv                         linseg                  0, 0.02, ip4,  p3 - 0.04, ip4, 0.02, 0
isine                  	        ftgenonce               0, 0, 65536, 10, 1
icosine                  	    ftgenonce               0, 0, 65536, 11, 1
                                ; Cosines
acosu                           oscili                  1, iu * ifqc, icosine
acosu2                          oscili                  1, iu * ifqc / 2, icosine
acosv                           oscili                  1, iv * ifqc, icosine
                                ; Sines
asinu                           oscili                  1, iu * ifqc, isine
asinu2                          oscili                  1, iu * ifqc / 2, isine
asinv                           oscili                  1, iv * ifqc, isine
                                ; Compute X and Y
ax                              =                       acosu * (acosu2 * (irt2 + acosv) + asinu2 * asinv * acosv)
ay                              =                       asinu * (acosu2 * (irt2 + acosv) + asinu2 * asinv * acosv)
                                ; Low frequency rotation in spherical coordinates z, phi, theta.
klfsinth                        oscili                  1, 4, isine
klfsinph                        oscili                  1, 1, isine
klfcosth                        oscili                  1, 4, icosine
klfcosph                        oscili                  1, 1, icosine
aox                             =                       -ax * klfsinth + ay * klfcosth
aoy                             =                       -ax * klfsinth * klfcosph - ay * klfsinth * klfcosph + klfsinph
aoutleft                        =                       aampenv * aox
aoutright                       =                       aampenv * aoy
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin                                

                                instr                   BandedWG
                                //////////////////////////////////////////////
                                // By Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 512
iattack                         =                       0.005
isustain                        =                       p3
irelease                        =                       0.06
p3                              =                       isustain + iattack + irelease
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
asignal                         STKBandedWG             ifrequency,1
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin
                        
                                instr                   BassModel
                                //////////////////////////////////////////////
                                // Original by Hans Mikelson.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) / 35
                                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                ; p1  p2     p3   p4    p5     p6
                                ;     Start  Dur  Amp   Pitch  PluckDur
                                ; i2  128    4    1400  6.00   0.25
                                ; i2  +      2    1200  6.01   0.25
                                ; i2  .      4    1000  6.05   0.5
                                ; i2  .      2     500  6.04   1
                                ; i2  .      4    1000  6.03   0.5
                                ; i2  .      16   1000  6.00   0.5
iHz                             =                       ifrequency
ifqc                            =                       iHz
ip4                             =                       iamplitude
ip6                             =                       0.5
ipluck                          =                       1 / ifqc * ip6
kcount                          init                    0
adline                          init                    0
ablock2                         init                    0
ablock3                         init                    0
afiltr                          init                    0
afeedbk                         init                    0
koutenv                         linseg                  0, .01, 1, p3 - .11 , 1, .1 , 0 ; Output envelope
kfltenv                         linseg                  0, 1.5, 1, 1.5, 0 
                                ; This envelope loads the string with a triangle wave.
kenvstr                         linseg                  0, ipluck / 4, -ip4 / 2, ipluck / 2, ip4 / 2, ipluck / 4, 0, p3 - ipluck, 0
aenvstr                         =                       kenvstr
ainput                          tone                    aenvstr, 200
                                ; DC Blocker
ablock2                         =                       afeedbk - ablock3 + .99 * ablock2
ablock3                         =                       afeedbk
ablock                          =                       ablock2
                                ; Delay line with filtered feedback
adline                          delay                   ablock + ainput, 1 / ifqc - 15 / sr
afiltr                          tone                    adline, 400
                                ; Resonance of the body 
abody1                          reson                   afiltr, 110, 40
abody1                          =                       abody1 / 5000
abody2                          reson                   afiltr, 70, 20
abody2                          =                       abody2 / 50000
afeedbk                         =                       afiltr
aout                            =                       afeedbk
asignal                         =                       50 * koutenv * (aout + kfltenv * (abody1 + abody2))
iattack                         =                       0.005
isustain                        =                       p3
irelease                        =                       0.06
p3                              =                       isustain + iattack + irelease
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin                                

                                instr                   ChebyshevDrone
                                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                ; By Michael Gogins.
                                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ihertz                          =                       cpsmidinn(i_midikey)
iamp                            =                       ampdb(i_midivelocity) * 6
idampingattack                  =                       .01
idampingrelease                 =                       .02
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
iattack                         init                    p3 / 4.0
idecay                          init                    p3 / 4.0
isustain                        init                    p3 / 2.0
aenvelope                       transeg                 0.0, iattack / 2.0, 2.5, iamp / 2.0, iattack / 2.0, -2.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 2.5, iamp / 2.0, idecay / 2.0, -2.5, 0.
isinetable                      ftgenonce               0, 0, 65536, 10, 1, 0, .02
asignal                         poscil3                 1, ihertz, isinetable
asignal                         chebyshevpoly           asignal, 0, gkChebyshevDroneCoefficient1, gkChebyshevDroneCoefficient2, gkChebyshevDroneCoefficient3, gkChebyshevDroneCoefficient4, gkChebyshevDroneCoefficient5, gkChebyshevDroneCoefficient6, gkChebyshevDroneCoefficient7, gkChebyshevDroneCoefficient8, gkChebyshevDroneCoefficient9, gkChebyshevDroneCoefficient10
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
asignal                         =                       asignal * aenvelope
aoutleft, aoutright             pan2                    asignal * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   ChebyshevMelody
                                ///////////////////////////////////////////////////////
                                // Original by Jon Nelson.
                                // Adapted by Michael Gogins.
                                ///////////////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
iHz                             =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 7.
iattack                         =                       .01
isustain                        =                       p3
irelease                        =                       .01
p3                              =                       iattack + isustain + irelease
adeclick                        linsegr                 0, iattack, 1, isustain, 1, irelease, 0
kHz                             =                       k(iHz)
idB                             =                       i_midivelocity
i1                              =                       iHz
k100                            randi                   1,0.05
isine                           ftgenonce               0, 0, 65536, 10, 1
k101                            poscil                  1, 5 + k100, isine
k102                            linseg                  0, .5, 1, p3, 1
k100                            =                       i1 + (k101 * k102)
; Envelope for driving oscillator.
ip3                             init                    3.0
; k1                            linenr                  0.5, ip3 * .3, ip3 * 2, 0.01
k1                              linseg                  0, ip3 * .3, .5, ip3 * 2, 0.01, isustain, 0.01, irelease, 0
; k2                            line                    1, p3, .5
k2                              linseg                  1.0, ip3, .5, isustain, .5, irelease, 0
k1                              =                       k2 * k1
; Amplitude envelope.
k10                             expseg                  0.0001, iattack, 1.0, isustain, 0.8, irelease, .0001
k10                             =                       (k10 - .0001)
; Power to partials.
k20                             linseg                  1.485, iattack, 1.5, (isustain + irelease), 1.485
; a1-3 are for cheby with p6=1-4
icook3                          ftgenonce               0, 0, 65536,    10,     1, .4, 0.2, 0.1, 0.1, .05
a1                              poscil                  k1, k100 - .25, icook3
; Tables a1 to fn13, others normalize,
ip6                             ftgenonce               0, 0, 65536,    -7,    -1, 150, 0.1, 110, 0, 252, 0
a2                              tablei                  a1, ip6, 1, .5
a3                              balance                 a2, a1
; Try other waveforms as well.
a4                              foscili                 1, k100 + .04, 1, 2.000, k20, isine
a5                              poscil                  1, k100, isine
a6                              =                       ((a3 * .1) + (a4 * .1) + (a5 * .8)) * k10
a7                              comb                    a6, .5, 1 / i1
a8                              =                       (a6 * .9) + (a7 * .1)
asignal        		            balance         	    a8, a1
asignal                         =                       asignal * iamplitude
aoutleft, aoutright		        pan2			        asignal * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   DelayedPluckedString
                                //////////////////////////////////////////////////////
                                // By Michael Gogins.
                                //////////////////////////////////////////////////////
                                ;pset                    0, 0, 1000
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
iattack                         =                       0.02
isustain                        =                       p3
irelease                        =                       0.15
p3                              =                       isustain + iattack + irelease
adeclick                        linsegr                  0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
ikeyin                          =                       i_midikey
ihertz                          =                       cpsmidinn(ikeyin)
; Detuning of strings by 4 cents each way.
idetune                         =                       4.0 / 1200.0
ihertzleft                      =                       cpsmidinn(ikeyin + idetune)
ihertzright                     =                       cpsmidinn(ikeyin - idetune)
iamplitude                      =                       ampdb(i_midivelocity)
isine                          ftgenonce                   0, 0, 65536,    10,     1
icosine                        ftgenonce                   0, 0, 65536,    11,     1 
igenleft                        =                       isine
igenright                       =                       icosine
kvibrato                        oscili                  1.0 / 120.0, 7.0, icosine
kexponential                    expseg                  1.0, p3 + iattack, 0.0001, irelease, 0.0001
aenvelope                       =                       (kexponential - 0.0001) * adeclick
ag                              pluck                   iamplitude, cpsmidinn(ikeyin + kvibrato), 200, igenleft, 1
agleft                          pluck                   iamplitude, ihertzleft, 200, igenleft, 1
agright                         pluck                   iamplitude, ihertzright, 200, igenright, 1
imsleft                         =                       0.2 * 1000
imsright                        =                       0.21 * 1000
adelayleft                      vdelay                  ag * aenvelope, imsleft, imsleft + 100
adelayright                     vdelay                  ag * aenvelope, imsright, imsright + 100
asignal                         =                       adeclick * (agleft + adelayleft + agright + adelayright)
                                ; Highpass filter to exclude speaker cone excursions.
asignal1                        butterhp                asignal, 32.0
asignal2                        balance                 asignal1, asignal
aoutleft, aoutright             pan2                    asignal2 * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   EnhancedFMBell
                                //////////////////////////////////////////////////////
                                // Original by John ffitch.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
iattack                         =                       0.005
isustain                        =                       p3
irelease                        =                       0.25
i_duraton                       =                       15; isustain + iattack + irelease
p3                              =                       i_duration
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
ifrequency                      =                       cpsmidinn(i_midikey)
; Normalize so iamplitude for p5 of 80 == ampdb(80).
iamplitude                      =                       ampdb(i_midivelocity) 
idur                            =                       50
iamp                            =                       iamplitude
iffitch1                        ftgenonce               0, 0, 65536,     10,     1
iffitch2                        ftgenonce               0, 0, 8193,     5,      1, 1024, 0.01
iffitch3                        ftgenonce               0, 0, 8193,     5,      1, 1024, 0.001
ifenv                           =                       iffitch2                       ; BELL SETTINGS:
ifdyn                           =                       iffitch3                       ; AMP AND INDEX ENV ARE EXPONENTIAL
ifq1                            =                       cpsmidinn(i_midikey) ;* 5              ; DECREASING, N1:N2 IS 5:7, imax=10
if1                             =                       iffitch1                               ; DURATION = 15 sec
ifq2                            =                       cpsmidinn(i_midikey) * 5/7
if2                             =                       iffitch1
imax                            =                       10
aenv                            oscili                  iamp, 1 / idur, ifenv           ; ENVELOPE
adyn                            oscili                  ifq2 * imax, 1 / idur, ifdyn    ; DYNAMIC
anoise                          rand                    50
amod                            oscili                  adyn + anoise, ifq2, if2        ; MODULATOR
acar                            oscili                  aenv, ifq1 + amod, if1          ; CARRIER
                                timout                  0.5, idur, noisend
knenv                           linsegr                 iamp, 0.2, iamp, 0.3, 0
anoise3                         rand                    knenv
anoise4                         butterbp                anoise3, iamp, 100
anoise5                         balance                 anoise4, anoise3
noisend:
arvb                            nreverb                 acar, 2, 0.1
aenvelope                       transeg                 1, idur, -3, 0
asignal                         =                       aenvelope * (acar + arvb) ;+ anoise5
aoutleft, aoutright             pan2                    asignal * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   FenderRhodesModel
                                //////////////////////////////////////////////////////
                                // Original by Perry Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
iattack                         =                       0.01
isustain                        =                       p3
irelease                        =                       0.125
p3                              =                       isustain + iattack + irelease
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
iindex                          =                       4
icrossfade                      =                       3
ivibedepth                      =                       0.2
iviberate                       =                       6
isine                           ftgenonce               0, 0, 65536,    10,     1
icosine                         ftgenonce               0, 0, 65536,    11,     1 
icookblank                      ftgenonce               0, 0, 65536,     10,     0 ; Blank wavetable for some Cook FM opcodes.
ifn1                            =                       isine
ifn2                            =                       icosine
ifn3                            =                       isine
ifn4                            =                       icookblank
ivibefn                         =                       isine
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 6
asignal                         fmrhode                 iamplitude, ifrequency, iindex, icrossfade, ivibedepth, iviberate, ifn1, ifn2, ifn3, ifn4, ivibefn
aoutleft, aoutright		        pan2			        asignal * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   FilteredSines
                                //////////////////////////////////////////////////////
                                // Original by Michael Bergeman.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////////////
                                ; Original pfields
                                ; p1 p2 p3 p4 p5 p6 p7 p8 p9
                                ; ins st dur db func at dec freq1 freq2
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
iattack                         =                       0.03
isustain                        =                       p3
irelease                        =                       0.52
p3                              =                       p3 + iattack + irelease
i_duration                      =                       p3
adeclick                        linsegr                  0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
ip4                             =                       i_midivelocity
idb                             =                       ampdb(i_midivelocity) * 4
ibergeman                       ftgenonce               0, 0, 65536,     10,     0.28, 1, 0.74, 0.66, 0.78, 0.48, 0.05, 0.33, 0.12, 0.08, 0.01, 0.54, 0.19, 0.08, 0.05, 0.16, 0.01, 0.11, 0.3, 0.02, 0.2 ; Bergeman f1
ip5                             =                       ibergeman
ip3                             =                       i_duration
ip6                             =                       i_duration * 0.25
ip7                             =                       i_duration * 0.75
ip8                             =                       cpsmidinn(i_midikey - 0.01)
ip9                             =                       cpsmidinn(i_midikey + 0.01)
isc                             =                       idb * 0.333
k1                              line                    40, p3, 800
k2                              line                    440, p3, 220
k3                              linen                   isc, ip6, p3, ip7
k4                              line                    800, ip3, 40
k5                              line                    220, ip3, 440
k6                              linen                   isc, ip6, ip3, ip7
k7                              linen                   1, ip6, ip3, ip7
a5                              oscili                  k3, ip8, ip5
a6                              oscili                  k3, ip8 * 0.999, ip5
a7                              oscili                  k3, ip8 * 1.001, ip5
a1                              =                       a5 + a6 + a7
a8                              oscili                  k6, ip9, ip5
a9                              oscili                  k6, ip9 * 0.999, ip5
a10                             oscili                  k6, ip9 * 1.001, ip5
a11                             =                       a8 + a9 + a10
a2                              butterbp                a1, k1, 40
a3                              butterbp                a2, k5, k2 * 0.8
a4                              balance                 a3, a1
a12                             butterbp                a11, k4, 40
a13                             butterbp                a12, k2, k5 * 0.8
a14                             balance                 a13, a11
a15                             reverb2                 a4, 5, 0.3
a16                             reverb2                 a4, 4, 0.2
                                ; Constant-power pan.
ipi                             =                       4.0 * taninv(1.0)
iradians                        =                       i_pan * ipi / 2.0
itheta                          =                       iradians / 2.0
; Translate angle in [-1, 1] to left and right gain factors.
irightgain                      =                       sqrt(2.0) / 2.0 * (cos(itheta) + sin(itheta))
ileftgain                       =                       sqrt(2.0) / 2.0 * (cos(itheta) - sin(itheta))
a17                             =                       (a15 + a4) * ileftgain * k7
a18                             =                       (a16 + a4) * irightgain * k7
aoutleft                        =                       a17 * adeclick
aoutright                       =                       a18 * adeclick
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   Flute
                                //////////////////////////////////////////////////////
                                // Original by James Kelley.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////////////
                                ;pset                    0, 0, 1000
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ikellyflute                     ftgenonce               0, 0, 65536,     10,     1, 0.25, 0.1 ; Kelley flute.
; Do some phasing.
icpsp1                          =                       cpsmidinn(i_midikey - 0.0002)
icpsp2                          =                       cpsmidinn(i_midikey + 0.0002)
ip6                             =                       ampdb(i_midivelocity)
iattack                         =                       0.04
isustain                        =                       p3
irelease                        =                       0.15
p3                              =                       isustain + iattack + irelease
adeclick                        linsegr                  0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
ip4                             =                       0
                                if                      (ip4 == int(ip4 / 2) * 2) goto initslurs
                                ihold
initslurs:
iatttm                          =                       0.09
idectm                          =                       0.1
isustm                          =                       p3 - iatttm - idectm
idec                            =                       ip6
ireinit                         =                       -1
                                if                      (ip4 > 1) goto checkafterslur
ilast                           =                       0
checkafterslur:
                                if                      (ip4 == 1 || ip4 == 3) goto doneslurs
idec                            =                       0
ireinit                         =                       0
; KONTROL
doneslurs:
                                if                      (isustm <= 0)   goto simpleenv
kamp                            linsegr                  ilast, iatttm, ip6, isustm, ip6, idectm, idec, 0, idec
                                goto                    doneenv
simpleenv:
kamp                            linsegr                  ilast, p3 / 2,ip6, p3 / 2, idec, 0, idec
doneenv:
ilast                           =                       ip6
; Some vibrato.
kvrandamp                       rand                    0.1
kvamp                           =                       (8 + p4) *.06 + kvrandamp
kvrandfreq                      rand                    1
kvfreq                          =                       5.5 + kvrandfreq
isine                           ftgenonce               0, 0, 65536,    10,     1
kvbra                           oscili                  kvamp, kvfreq, isine, ireinit
kfreq1                          =                       icpsp1 + kvbra
kfreq2                          =                       icpsp2 + kvbra
; Noise for burst at beginning of note.
knseenv                         expon                   ip6 / 4, 0.2, 1
; AUDIO
anoise1                         rand                    knseenv
anoise                          tone                    anoise1, 200
a1                              oscili                  kamp, kfreq1, ikellyflute, ireinit
a2                              oscili                  kamp, kfreq2, ikellyflute, ireinit
a3                              =                       a1 + a2 + anoise
aoutleft, aoutright             pan2                    a3 * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   FMModerateIndex
                                //////////////////////////////////////////////////////
                                // By Michael Gogins.
                                //////////////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 3
icarrier                        =                       1
iratio                          =                       1.25
ifmamplitude                    =                       8
index                           =                       5.4
iattack                         =                       0.01
isustain                        =                       p3
irelease                        =                       0.05
p3                              =                       isustain + iattack + irelease
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
ifrequencyb                     =                       ifrequency * 1.003
icarrierb                       =                       icarrier * 1.004
kindenv                         transeg                 0, iattack, -4, 1,  isustain, -2, 0.125, irelease, -4, 0
kindex                          =                       kindenv * index * ifmamplitude
isine                           ftgenonce               0, 0, 65536,    10,     1
aouta                           foscili                 1, ifrequency, icarrier, iratio, index, isine
aoutb                           foscili                 1, ifrequencyb, icarrierb, iratio, index, isine
asignal                         =                       (aouta + aoutb) * kindenv
aoutleft, aoutright		        pan2			        asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   FMModerateIndex2
                                //////////////////////////////////////////////////////
                                // By Michael Gogins.
                                //////////////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 3
icarrier                        =                       1
iratio                          =                       1
ifmamplitude                    =                       6
index                           =                       2.5
iattack                         =                       0.02
isustain                        =                       p3
irelease                        =                       0.05
p3                              =                       isustain + iattack + irelease
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
ifrequencyb                     =                       ifrequency * 1.003
icarrierb                       =                       icarrier * 1.004
kindenv                         expseg                  0.000001, iattack, 1.0, isustain, 0.0125, irelease, 0.000001
kindex                          =                       kindenv * index * ifmamplitude - 0.000001
isine                           ftgenonce               0, 0, 65536,    10,     1
aouta                           foscili                 1, ifrequency, icarrier, iratio, index, isine
aoutb                           foscili                 1, ifrequencyb, icarrierb, iratio, index, isine
asignal                         =                       (aouta + aoutb) * kindenv
aoutleft, aoutright		        pan2			        asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   FMModulatedChorusing
                                //////////////////////////////////////////////
                                // Original by Thomas Kung.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
iattack                         =                       0.333333
irelease                        =                       0.1
isustain                        =                       p3
p3                              =                       isustain + iattack + irelease
adeclick                        linsegr                  0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
iamplitude                      =                       ampdb(i_midikey) / 1200
ip6                             =                       0.3
ip7                             =                       2.2
                                ; shift it.
ishift                          =                       4.0 / 12000
                                ; convert parameter 5 to cps.
ipch                            =                       cpsmidinn(i_midikey)
                                ; convert parameter 5 to oct.
ioct                            =                       i_midikey
kadsr                           linen                   1.0, iattack, irelease, 0.01
kmodi                           linseg                  0, iattack, 5, isustain, 2, irelease, 0
                                ; r moves from ip6 to ip7 in p3 secs.
kmodr                           linseg                  ip6, p3, ip7
a1                              =                       kmodi * (kmodr - 1 / kmodr) / 2
                                ; a1*2 is argument normalized from 0-1.
a1ndx                           =                       abs(a1 * 2 / 20)
a2                              =                       kmodi * (kmodr + 1 / kmodr) / 2
                                ; Look up table is in f43, normalized index.
iln                             ftgenonce               0, 0, 65536,     -12,    20.0 ; Unscaled ln(I(x)) from 0 to 20.0.
a3                              tablei                  a1ndx, iln, 1
icosine                         ftgenonce                   0, 0, 65536,    11,     1 ; Cosine wave. Get that noise down on the most widely used table!
ao1                             oscili                  a1, ipch, icosine
a4                              =                       exp(-0.5 * a3 + ao1)
                                ; Cosine
ao2                             oscili                  a2 * ipch, ipch, icosine
isine                          ftgenonce                   2, 0, 65536,    10,     1
                                ; Final output left
aoutl                           oscili                  1 * kadsr * a4, ao2 + cpsmidinn(ioct + ishift), isine
                                ; Final output right
aoutr                           oscili                  1 * kadsr * a4, ao2 + cpsmidinn(ioct - ishift), isine
asignal                         =                       aoutl + aoutr
asignal                         =                       asignal * iamplitude
aoutleft, aoutright		        pan2			        asignal * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   FMWaterBell
                                //////////////////////////////////////////////
                                // Original by Steven Yi.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ipch                            =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 2.0
ipch2                           =                       ipch
kpchline 	                    line                    ipch, i_duration, ipch2
iamp 	                        =                       2
ienvType	                    =                       2
kenv 	                        init 	                0
                                if ienvType == 0 kgoto env0  ; adsr
                                if ienvType == 1 kgoto env1  ; pyramid
                                if ienvType == 2 kgoto env2  ; ramp
env0:
kenv	                        adsr	                .3, .2, .9, .5
                                kgoto                   endEnvelope
env1:
kenv 	                        linseg	                0, i_duration * .5, 1, i_duration * .5, 0
                                kgoto                   endEnvelope
env2:
kenv	                        linseg 	                0, i_duration - .1, 1, .1, 0	
kgoto                           endEnvelope
endEnvelope:
kc1                             =                       5
kc2                             =                       5
kvdepth                         =                       0.005
kvrate                          =                       6
icosine                  	    ftgenonce               0, 0, 65536, 11, 1
ifn1                            =                       icosine
ifn2                            =                       icosine
ifn3                            =                       icosine
ifn4                            =                       icosine
ivfn                            =                       icosine
asignal                         fmbell	                iamp, kpchline, kc1, kc2, kvdepth, kvrate, ifn1, ifn2, ifn3, ifn4, ivfn
iattack                         =                       0.003
isustain                        =                       p3
irelease                        =                       0.06
p3                              =                       isustain + iattack + irelease
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
aoutleft, aoutright             pan2                    iamplitude * asignal * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   Granular
                                //////////////////////////////////////////////
                                // Original by Hans Mikelson.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) / 175
                                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                ; f1  0 65536 1 "hahaha.aif" 0 4 0
                                ; f2  0 1024  7 0 224 1 800 0
                                ; f3  0 8192  7 1 8192 -1
                                ; f4  0 1024  7 0 512 1 512 0
                                ; f5  0 1024 10 1 .3 .1 0 .2 .02 0 .1 .04
                                ; f6  0 1024 10 1 0 .5 0 .33 0 .25 0 .2 0 .167
                                ; a0 14 50
                                ; p1   p2     p3    p4    p5    p6     p7      p8      p9    p10
                                ;      Start  Dur  Amp    Freq  GrTab  WinTab  FqcRng  Dens  Fade
                                ; i1   0.0    6.5  700    9.00  5      4       .210    200   1.8
                                ; i1   3.2    3.5  800    7.08  .      4       .042    100   0.8
                                ; i1   5.1    5.2  600    7.10  .      4       .032    100   0.9
                                ; i1   7.2    6.6  900    8.03  .      4       .021    150   1.6
                                ; i1  21.3    4.5  1000   9.00  .      4       .031    150   1.2
                                ; i1  26.5   13.5  1100   6.09  .      4       .121    150   1.5
                                ; i1  30.7    9.3  900    8.05  .      4       .014    150   2.5
                                ; i1  34.2    8.8  700   10.02  .      4       .14     150   1.6
igrtab                          ftgenonce               0, 0, 65536,    10,     1, .3, .1, 0, .2, .02, 0, .1, .04
iwintab                         ftgenonce               0, 0, 65536,    10,     1, 0, .5, 0, .33, 0, .25, 0, .2, 0, .167
iHz                             =                       ifrequency
ip4                             =                       iamplitude
ip5                             =                       iHz
ip6                             =                       igrtab
ip7                             =                       iwintab
ip8                             =                       0.033
ip9                             =                       150
ip10                            =                       1.6
idur                            =                       p3
iamp                            =                       iamplitude ; p4
ifqc                            =                       iHz ; cpspch(p5)
igrtab                          =                       ip6
iwintab                         =                       ip7
ifrng                           =                       ip8
idens                           =                       ip9
ifade                           =                       ip10
igdur                           =                       0.2
kamp                            linseg                  0, ifade, 1, idur - 2 * ifade, 1, ifade, 0
;                                                       Amp   Fqc    Dense  AmpOff PitchOff      GrDur  GrTable   WinTable  MaxGrDur
aoutl                           grain                   ip4,  ifqc,  idens, 100,   ifqc * ifrng, igdur, igrtab,   iwintab,  5
aoutr                           grain                   ip4,  ifqc,  idens, 100,   ifqc * ifrng, igdur, igrtab,   iwintab,  5
aoutleft                        =                       aoutl * kamp * iamplitude
aoutright                       =                       aoutr * kamp * iamplitude
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   Guitar
                                //////////////////////////////////////////////
                                // By Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) / 8.0
iattack                         =                       0.01
isustain                        =                       p3
irelease                        =                       0.05
p3                              =                       isustain + iattack + irelease
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
ifrequency                      =                       cpsmidinn(p4)
iamplitude                      =                       ampdb(p5) * 20
kamp                            linsegr                 0.0, iattack, iamplitude, isustain, iamplitude, irelease, 0.0
asigcomp                        pluck                   1, 440, 440, 0, 1
asig                            pluck                   1, ifrequency, ifrequency, 0, 1
af1                             reson                   asig, 110, 80
af2                             reson                   asig, 220, 100
af3                             reson                   asig, 440, 80
aout                            balance                 0.6 * af1+ af2 + 0.6 * af3 + 0.4 * asig, asigcomp
kexp                            expseg                  1.0, iattack, 2.0, isustain, 1.0, irelease, 1.0
kenv                            =                       kexp - 1.0
asignal                         =                       aout * kenv * kamp
aoutleft, aoutright             pan2                    asignal * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   Guitar2
                                //////////////////////////////////////////////////////
                                // By Michael Gogins.
                                //////////////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 12
iattack                         =                       0.01
isustain                        =                       p3
irelease                        =                       0.05
p3                              =                       isustain + iattack + irelease
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
kamp                            linsegr                  0.0, iattack, 1, isustain, 1, irelease, 0.0
asigcomp                        pluck                   kamp, 440, 440, 0, 1
asig                            pluck                   kamp, ifrequency, ifrequency, 0, 1
af1                             reson                   asig, 110, 80
af2                             reson                   asig, 220, 100
af3                             reson                   asig, 440, 80
aout                            balance                 0.6 * af1+ af2 + 0.6 * af3 + 0.4 * asig, asigcomp
kexp                            expseg                  1.0, iattack, 2.0, isustain, 1.0, irelease, 1.0
kenv                            =                       kexp - 1.0
asignal                         =                       aout * kenv
asignal                         dcblock                 asignal
aoutleft, aoutright		        pan2			        asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr 			        Harpsichord
                                //////////////////////////////////////////////
                                // Original by James Kelley.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
insno           		        =                       p1
itime           		        =                       p2
iduration       		        =                       p3
ikey            		        =                       p4
ivelocity                       =                       p5
iphase                          =                       p6
ipan                            =                       p7
idepth                          =                       p8
iheight                         =                       p9
ipcs                            =                       p10
ihomogeneity                    =                       p11
gkHarpsichordGain               =                       .25
gkHarpsichordPan                =                       .5
iattack                         =                       .005
isustain                        =                       p3
irelease                        =                       .3
p3                              =                       iattack + isustain + irelease
iHz                             =                       cpsmidinn(ikey)
kHz                             =                       k(iHz)
iamplitude                      =                       ampdb(ivelocity) * 36
aenvelope               	    transeg                 1.0, 20.0, -10.0, 0.05
apluck                  	    pluck                   1, kHz, iHz, 0, 1
iharptable              	    ftgenonce               0, 0, 65536,  7, -1, 1024, 1, 1024, -1
aharp                   	    poscil                  1, kHz, iharptable
aharp2                  	    balance                 apluck, aharp
asignal			                =                       (apluck + aharp2) * iamplitude * aenvelope * gkHarpsichordGain
adeclick                        linsegr                 0, iattack, 1, isustain, 1, irelease, 0
aoutleft, aoutright             pan2                    asignal * adeclick, ipan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   HeavyMetalModel
                                //////////////////////////////////////////////
                                // Original by Perry Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 1000
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
iattack                         =                       0.01
idecay                          =                       2.0
isustain                        =                       i_duration
irelease                        =                       0.125
p3                              =                       iattack + iattack + idecay + irelease
adeclick                        linsegr                 0.0, iattack, 1.0, idecay + isustain, 1.0, irelease, 0.0
iindex                          =                       1
icrossfade                      =                       3
ivibedepth                      =                       0.02
iviberate                       =                       4.8
isine                           ftgenonce               0, 0, 65536,    10,     1
icosine                         ftgenonce               0, 0, 65536,    11,     1 ; Cosine wave. Get that noise down on the most widely used table!
iexponentialrise                ftgenonce               0, 0, 65536,     5,      0.001, 513, 1 ; Exponential rise.
ithirteen                       ftgenonce               0, 0, 65536,     9,      1, 0.3, 0
ifn1                            =                       isine
ifn2                            =                       iexponentialrise
ifn3                            =                       ithirteen
ifn4                            =                       isine
ivibefn                         =                       icosine
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 48.0
adecay                          transeg                 0.0, iattack, 4, 1.0, idecay + isustain, -4, 0.1, irelease, -4, 0.0
asignal                         fmmetal                 0.1, ifrequency, iindex, icrossfade, ivibedepth, iviberate, ifn1, ifn2, ifn3, ifn4, ivibefn
asignal                         =                       asignal * iamplitude
aoutleft, aoutright             pan2                    asignal * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   Hypocycloid
                                //////////////////////////////////////////////
                                // Original by Hans Mikelson.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ; This set of parametric equations defines the path traced by
                                ; a point on a circle of radius B rotating inside a circle of
                                ; radius A.
                                ;   p1  p2     p3   p4    p5     p6   p7   p8
                                ;       Start  Dur  Amp   Frqc   A    B    Hole
                                ; i 3   16     6    8000  8.00  10    2    1
                                ; i 3   20     4    .     7.11   5.6  0.4  0.8
                                ; i 3   +      4    .     8.05   2    8.5  0.7
                                ; i 3   .      2    .     8.02   4    5    0.6
                                ; i 3   .      2    .     8.02   5    0.5  1.2
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 4
iHz                             =                       ifrequency
ifqc                            init                    iHz        
ip4                             init                    iamplitude
ifqci                           init                    iHz
ia                              =                       0.6 ; p6
ib                              =                       0.4 ; p7
ihole                           =                       0.8 ; p8
iscale                          =                       (ia < ib ? 1 / ib : 1 / ia)
kampenv                         linseg                  0, .1, ip4 * iscale, p3 - .2, ip4 * iscale, .1, 0
kptchenv                        linseg                  ifqci, .2 * p3, ifqc, .8 * p3, ifqc
kvibenv                         linseg                  0, .5, 0, .2, 1, .2, 1
isine                  	        ftgenonce               0, 0, 65536, 10, 1
icosine                  	    ftgenonce               0, 0, 65536, 11, 1
kvibr                           oscili                  20, 8, icosine
kfqc                            =                       kptchenv+kvibr*kvibenv
                                ; Sine and Cosine
acos1                           oscili                  ia - ib, kfqc, icosine
acos2                           oscili                  ib * ihole, (ia - ib) / ib * kfqc, icosine
ax                              =                       acos1 + acos2
asin1                           oscili                  ia-ib, kfqc, isine
asin2                           oscili                  ib, (ia - ib) / ib * kfqc, isine
ay                              =                       asin1 - asin2
aoutleft                        =                       kampenv * ax
aoutright                       =                       kampenv * ay
                                ; Constant-power pan.
ipi                             =                       4.0 * taninv(1.0)
iradians                        =                       i_pan * ipi / 2.0
itheta                          =                       iradians / 2.0
                                ; Translate angle in [-1, 1] to left and right gain factors.
irightgain                      =                       sqrt(2.0) / 2.0 * (cos(itheta) + sin(itheta))
ileftgain                       =                       sqrt(2.0) / 2.0 * (cos(itheta) - sin(itheta))
                                outleta                 "outleft",  aoutleft * ileftgain
                                outleta                 "outright", aoutright * irightgain
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr			        ModerateFM
                                //////////////////////////////////////////////
                                // By Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 1000
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
iattack			                =			            0.002
isustain		                =			            p3
idecay				            =			            8
irelease		                =			            0.05
iHz                             =                       cpsmidinn(i_midikey)
idB                             =                       i_midivelocity
iamplitude                      =                       ampdb(idB) * 4.0
icarrier                	    =                       1
imodulator              	    =                       0.5
ifmamplitude            	    =                       0.25
index                   	    =                       .5
ifrequencyb             	    =                       iHz * 1.003
icarrierb               	    =                       icarrier * 1.004
aindenv                 	    transeg                 0.0, iattack, -11.0, 1.0, idecay, -7.0, 0.025, isustain, 0.0, 0.025, irelease, -7.0, 0.0
aindex                  	    =                       aindenv * index * ifmamplitude
isinetable                      ftgenonce               0, 0, 65536, 10, 1, 0, .02
; ares                  	    foscili                 xamp, kcps, xcar, xmod, kndx, ifn [, iphs]
aouta                   	    foscili                 1.0, iHz, icarrier, imodulator, index / 4., isinetable
aoutb                   	    foscili                 1.0, ifrequencyb, icarrierb, imodulator, index, isinetable
; Plus amplitude correction.
asignal               		    =                       (aouta + aoutb) * aindenv
adeclick                        linsegr                 0, iattack, 1, isustain, 1, irelease, 0
asignal                         =                       asignal * iamplitude
aoutleft, aoutright             pan2                    asignal * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr 			        ModulatedFM
                                //////////////////////////////////////////////
                                // Original by Thomas Kung.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 1000
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
iattack                         =                       .25
isustain                        =                       p3
irelease                        =                       .33333333
p3                              =                       iattack + isustain + irelease
iHz                             =                       cpsmidinn(i_midikey)
kHz                             =                       k(iHz)
idB                             =                       i_midivelocity
iamplitude                      =                       ampdb(i_midivelocity)
adeclick                        linsegr                 0, iattack, 1, isustain, 1, irelease, 0
ip6                     	    =                       0.3
ip7                     	    =                       2.2
ishift      		    	    =           		    4.0 / 12000.0
kpch       		                =           		    kHz
koct        		    	    =           		    octcps(kHz)
aadsr                   	    linen                   1.0, iattack, irelease, 0.01
amodi                   	    linseg                  0, iattack, 5, p3, 2, irelease, 0
; r moves from ip6 to ip7 in p3 secs.
amodr                   	    linseg                  ip6, p3, ip7
a1                      	    =                       amodi * (amodr - 1 / amodr) / 2
; a1*2 is argument normalized from 0-1.
a1ndx                   	    =                       abs(a1 * 2 / 20)
a2                      	    =                       amodi * (amodr + 1 / amodr) / 2
; Unscaled ln(I(x)) from 0 to 20.0.
iln                    		    ftgenonce               0, 0, 65536, -12, 20.0
a3                      	    tablei                  a1ndx, iln, 1
icosine                  	    ftgenonce               0, 0, 65536, 11, 1
ao1                     	    poscil                  a1, kpch, icosine
a4                      	    =                       exp(-0.5 * a3 + ao1)
; Cosine
ao2                     	    poscil                  a2 * kpch, kpch, icosine
isine                  		    ftgenonce               0, 0, 65536, 10, 1
; Final output left
aleft                   	    poscil                  a4, ao2 + cpsoct(koct + ishift), isine
; Final output right
aright                  	    poscil                  a4, ao2 + cpsoct(koct - ishift), isine
asignal                         =                       (aleft + aright) * iamplitude
aoutleft, aoutright             pan2                    asignal * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   PlainPluckedString
                                //////////////////////////////////////////////////////
                                // By Michael Gogins.
                                //////////////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
iattack                         =                       0.002
isustain                        =                       p3
irelease                        =                       0.075
p3                              =                       isustain + iattack + irelease
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 6
aenvelope                       transeg                 0, iattack, -4, iamplitude,  isustain, -4, iamplitude / 10.0, irelease, -4, 0
asignal1                        pluck                   1, ifrequency, ifrequency * 1.002, 0, 1
asignal2                        pluck                   1, ifrequency * 1.003, ifrequency, 0, 1
asignal                         =                       (asignal1 + asignal2) * aenvelope
aoutleft, aoutright		        pan2			        asignal * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   PRCBeeThree
                                //////////////////////////////////////////////////////
                                // By Michael Gogins.
                                //////////////////////////////////////////////////////
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                      cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 6
iattack                         =                       0.005
isustain                        =                       p3
irelease                        =                       0.06
p3                              =                       isustain + iattack + irelease
asignal                         STKBeeThree             ifrequency, 1
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin
                                
                               instr                   PRCBeeThreeDelayed
                                //////////////////////////////////////////////////////
                                // By Michael Gogins.
                                //////////////////////////////////////////////////////
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 6
iattack                         =                       0.2
isustain                        =                       p3
irelease                        =                       0.3
p3                              =                       isustain + iattack + irelease
asignal                         STKBeeThree             ifrequency, 1, 2, 3, 1, 0, 11, 0
amodulator                      oscils                  0.00015, 0.2, 0.0
                                ; Read delayed signal, first delayr instance:
adump                           delayr                  4.0 
adly1                           deltapi                 0.03 + amodulator; associated with first delayr instance
                                ; Read delayed signal, second delayr instance:
adump                           delayr                  4.0 
adly2                           deltapi                 0.029 + amodulator      ; associated with second delayr instance
                                ; Do some cross-coupled manipulation:
afdbk1                          =                       0.7 * adly1 + 0.7 * adly2 + asignal
afdbk2                          =                       -0.7 * adly1 + 0.7 * adly2 + asignal 
                                ; Feed back signal, associated with first delayr instance:
                                delayw                  afdbk1 
                                ; Feed back signal, associated with second delayr instance:
                                delayw                  afdbk2
asignal2                        =                       adly1 + adly2
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
aoutleft, aoutright             pan2                    asignal2 * iamplitude * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   PRCBowed
                                //////////////////////////////////////////////////////
                                // By Michael Gogins.
                                //////////////////////////////////////////////////////
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 6
                                ; Controllers: 
                                ;   1  Vibrato Gain
                                ;   2  Bow Pressure
                                ;   4  Bow Position
                                ;  11  Vibrato Frequency
                                ; 128  Volume 
asignal 		                STKBowed 		        ifrequency, 1.0, 1, 1.8, 2, 120.0, 4, 50.0, 11, 20.0
iattack                         =                       0.005
isustain                        =                       p3
irelease                        =                       0.06
p3                              =                       isustain + iattack + irelease
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin
                                
                                instr                   STKBandedWG
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 256
asignal 		                STKBandedWG 		    ifrequency, 1.0
iattack                         =                       0.005
isustain                        =                       p3
irelease                        =                       0.06
p3                              =                       isustain + iattack + irelease
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin                                
                                
                                instr                   STKBeeThree
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 16
asignal 		                STKBeeThree 		    ifrequency, 1.0, 1, 1.5, 2, 4.8, 4, 2.1
aphased                         phaser1                 asignal, 4000, 16, .2, .9
idampingattack                  =                       .002
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    aphased * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin                                

                                instr                   STKBlowBotl
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 4
asignal 		                STKBlowBotl 		    ifrequency, 1.0
idampingattack                  =                       .002
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin                                

                                instr                   STKBlowHole
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 4
asignal 		                STKBlowHole 		    ifrequency, 1.0
idampingattack                  =                       .002
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin                                

                                instr                   STKBowed
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 8
                                ; Controllers: 
                                ;   1  Vibrato Gain
                                ;   2  Bow Pressure
                                ;   4  Bow Position
                                ;  11  Vibrato Frequency
                                ; 128  Volume 
asignal 		                STKBowed 		        ifrequency, 1.0, 1, 0.8, 2, 120.0, 4, 20.0, 11, 20.0
idampingattack                  =                       .002
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin                                

                                instr                   STKClarinet
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 4
asignal 		                STKClarinet 		    ifrequency, 1.0, 1, 1.5
idampingattack                  =                       .002
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin                                

                                instr                   STKDrummer
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 8
asignal 		                STKDrummer 		        ifrequency, 1.0
idampingattack                  =                       .002
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin                                

                                instr                   STKFlute
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 4
                                ; Control Change Numbers:
                                ;    * Jet Delay = 2
                                ;    * Noise Gain = 4
                                ;    * Vibrato Frequency = 11
                                ;    * Vibrato Gain = 1
                                ;    * Breath Pressure = 128
asignal 		                STKFlute 		        ifrequency, 1.0, 128, 100, 2, 70, 4, 10
idampingattack                  =                       .002
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin                                

                                instr                   STKFMVoices
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 4
                                ; Control Change Numbers:
                                ;    * Vowel = 2
                                ;    * Spectral Tilt = 4
                                ;    * LFO Speed = 11
                                ;    * LFO Depth = 1
                                ;    * ADSR 2 & 4 Target = 128
asignal 		                STKFMVoices 		    ifrequency, 1.0, 2, 1, 4, 3.0, 11, 5, 1, .8
idampingattack                  =                       .002
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin                                

                                instr                   STKHvyMetl
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 8
                                ; Control Change Numbers:
                                ;    * Total Modulator Index = 2
                                ;    * Modulator Crossfade = 4
                                ;    * LFO Speed = 11
                                ;    * LFO Depth = 1
                                ;    * ADSR 2 & 4 Target = 128
asignal 		                STKHevyMetl 		    ifrequency, 1.0, 2, 17.0, 4, 70, 128, 80
idampingattack                  =                       .0003
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin                                

                                instr                   STKMandolin
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 24
asignal 		                STKMandolin 		    ifrequency, 1.0
idampingattack                  =                       .0003
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin                                
                                
                                instr                   STKModalBar
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 24
                                ; Control Change Numbers:
                                ;    * Stick Hardness = 2
                                ;    * Stick Position = 4
                                ;    * Vibrato Gain = 1
                                ;    * Vibrato Frequency = 11
                                ;    * Direct Stick Mix = 8
                                ;    * Volume = 128
                                ;    * Modal Presets = 16
                                ;          o Marimba = 0
                                ;          o Vibraphone = 1
                                ;          o Agogo = 2
                                ;          o Wood1 = 3
                                ;          o Reso = 4
                                ;          o Wood2 = 5
                                ;          o Beats = 6
                                ;          o Two Fixed = 7
                                ;          o Clump = 8
asignal 		                STKModalBar 		    ifrequency, 1.0, 16, 1
idampingattack                  =                       .0003
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin                                
                                
                                 instr                   STKMoog
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 8
asignal 		                STKMoog 		        ifrequency, 1.0
idampingattack                  =                       .0003
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin                                
                                
                                instr                   STKPercFlut
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 8
asignal 		                STKPercFlut 		    ifrequency, 1.0
idampingattack                  =                       .0003
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin                                
                                
                                instr                   STKPlucked
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 16
asignal 		                STKPlucked 		        ifrequency, 1.0
idampingattack                  =                       .0003
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin                                
                                
                                instr                   STKResonate
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) 
                                ;Control Change Numbers:
                                ;    * Resonance Frequency (0-Nyquist) = 2
                                ;    * Pole Radii = 4
                                ;    * Notch Frequency (0-Nyquist) = 11
                                ;    * Zero Radii = 1
                                ;    * Envelope Gain = 128
asignal 		                STKResonate 		    ifrequency, 1.;, 2, 40, 4, .7, 11, 120, 1, .5
idampingattack                  =                       .0003
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin                                
                                
                                instr                   STKRhodey
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 8
asignal 		                STKRhodey 		        ifrequency, 1
idampingattack                  =                       .0003
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin                                
                                
                                instr                   STKSaxofony
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 4
                                ; Control Change Numbers:
                                ;    * Reed Stiffness = 2
                                ;    * Reed Aperture = 26
                                ;    * Noise Gain = 4
                                ;    * Blow Position = 11
                                ;    * Vibrato Frequency = 29
                                ;    * Vibrato Gain = 1
                                ;    * Breath Pressure = 128
asignal 		                STKSaxofony 		    ifrequency, 1.0, 2, 80, 11, 100;, 29, 5, 1, 12
idampingattack                  =                       .0003
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin                                

                                instr                   STKShakers
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 256
                                ;Control Change Numbers:
                                ;    * Shake Energy = 2
                                ;    * System Decay = 4
                                ;    * Number Of Objects = 11
                                ;    * Resonance Frequency = 1
                                ;    * Shake Energy = 128
                                ;    * Instrument Selection = 1071
                                ;          o Maraca = 0
                                ;          o Cabasa = 1
                                ;          o Sekere = 2
                                ;          o Guiro = 3
                                ;          o Water Drops = 4
                                ;          o Bamboo Chimes = 5
                                ;          o Tambourine = 6
                                ;          o Sleigh Bells = 7
                                ;          o Sticks = 8
                                ;          o Crunch = 9
                                ;          o Wrench = 10
                                ;          o Sand Paper = 11
                                ;          o Coke Can = 12
                                ;          o Next Mug = 13
                                ;          o Penny + Mug = 14
                                ;          o Nickle + Mug = 15
                                ;          o Dime + Mug = 16
                                ;          o Quarter + Mug = 17
                                ;          o Franc + Mug = 18
                                ;          o Peso + Mug = 19
                                ;          o Big Rocks = 20
                                ;          o Little Rocks = 21
                                ;          o Tuned Bamboo Chimes = 22
asignal 		                STKShakers 		        ifrequency, 1.0, 1071, 22, 11, 4;, 128, 100, 1, 30
idampingattack                  =                       .0003
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin                                

                                instr                   STKSimple
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 64
                                ; Control Change Numbers:
                                ;    * Filter Pole Position = 2
                                ;    * Noise/Pitched Cross-Fade = 4
                                ;    * Envelope Rate = 11
                                ;    * Gain = 128
asignal 		                STKSimple 		        ifrequency, 1.0, 2, 98, 4, 50, 11, 3
idampingattack                  =                       .0003
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin                                

                                instr                   STKSitar
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 8
asignal 		                STKSitar 		        ifrequency, 1.0
idampingattack                  =                       .0003
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin                                

                                instr                   STKTubeBell
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 8
asignal 		                STKTubeBell 		    ifrequency, 1.0
idampingattack                  =                       .0003
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin                                

                                instr                   STKVoicForm
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 16
asignal 		                STKVoicForm 		    ifrequency, 1.0
idampingattack                  =                       .0003
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin                                

                                instr                   STKWhistle
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 4
asignal 		                STKWhistle 		        ifrequency, 1.0
idampingattack                  =                       .0003
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin                                

                                instr                   STKWurley
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 16
asignal 		                STKWurley 		        ifrequency, 1.0
idampingattack                  =                       .0003
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin                                

                                instr                   StringPad
                                //////////////////////////////////////////////
                                // Original by Anthony Kozar.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ; String-pad borrowed from the piece "Dorian Gray",
                                ; http://akozar.spymac.net/music/ Modified to fit my needs
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ihz                             =                       cpsmidinn(i_midikey)
iamp                            =                       ampdb(i_midivelocity) * 3
idb                             =                       i_midivelocity
ipos                            =                       i_pan
                                ; Slow attack and release
akctrl                           linsegr                     0, i_duration * 0.5, iamp, i_duration *.5, 0
                                ; Slight chorus effect
iwave                           ftgenonce                   0, 0, 65536,    10,     1, 0.5, 0.33, 0.25,  0.0, 0.1,  0.1, 0.1
afund                           oscili                      akctrl, ihz,        iwave       ; audio oscillator
acel1                           oscili                      akctrl, ihz - 0.1,  iwave       ; audio oscillator - flat
acel2                           oscili                      akctrl, ihz + 0.1,  iwave       ; audio oscillator - sharp
asig                            =                           afund + acel1 + acel2
                                ; Cut-off high frequencies depending on midi-velocity
                                ; (larger velocity implies more brighter sound)
asignal                         butterlp                asig, (i_midivelocity - 60) * 40 + 900
iattack                         =                       0.005
isustain                        =                       p3
irelease                        =                       0.06
p3                              =                       isustain + iattack + irelease
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
aoutleft, aoutright             pan2                    asignal * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   ToneWheelOrgan
                                //////////////////////////////////////////////
                                // Original by Hans Mikelson.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ;pset                    0, 0, 1000
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) / 8.0
iattack                         =                       0.02
isustain                        =                       i_duration
irelease                        =                       0.1
i_duration                      =                       iattack + isustain + irelease
p3                              =                       i_duration
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
                                ; Rotor Tables
itonewheel1                     ftgenonce               0, 0, 65536,     10,     1, 0.02, 0.01
itonewheel2                     ftgenonce               0, 0, 65536,     10,     1, 0,    0.2, 0, 0.1, 0, 0.05, 0, 0.02
                                ; Rotating Speaker Filter Envelopes
itonewheel3                     ftgenonce               0, 0, 65536,     7,      0, 110, 0, 18, 1, 18, 0, 110, 0
itonewheel4                     ftgenonce               0, 0, 65536,     7,      0, 80, 0.2, 16, 1, 64, 1, 16, 0.2, 80, 0
                                ; Distortion Tables
itonewheel5                     ftgenonce               0, 0, 65536,     8,      -.8, 336, -.78,  800, -.7, 5920, 0.7,  800, 0.78, 336, 0.8
itonewheel6                     ftgenonce               0, 0, 65536,     8,       -.8, 336, -.76, 3000, -.7, 1520, 0.7, 3000, 0.76, 336, 0.8
icosine                  	    ftgenonce               0, 0, 65536, 11, 1
iphase                          =                       p2
ikey                            =                       12 * int(i_midikey - 6) + 100 * (i_midikey - 6)
ifqc                            =                       ifrequency
                                ; The lower tone wheels have increased odd harmonic content.
iwheel1                         =                       ((ikey - 12) > 12 ? itonewheel1 : itonewheel2)
iwheel2                         =                       ((ikey +  7) > 12 ? itonewheel1 : itonewheel2)
iwheel3                         =                        (ikey       > 12 ? itonewheel1 : itonewheel2)
iwheel4                         =                       icosine
;   insno Start Dur   Amp   Pitch  SubFund  Sub3rd  Fund  2nd  3rd  4th  5th  6th  8th
; i 1     0     6     200   8.04   8        8       8     8    3    2    1    0    4
asubfund                        oscili                  8, 0.5    * ifqc,  iwheel1, iphase / (ikey - 12)
asub3rd                         oscili                  8, 1.4983 * ifqc,  iwheel2, iphase / (ikey +  7)
afund                           oscili                  8,          ifqc,  iwheel3, iphase /  ikey
a2nd                            oscili                  8, 2      * ifqc,  iwheel4, iphase / (ikey + 12)
a3rd                            oscili                  3, 2.9966 * ifqc,  iwheel4, iphase / (ikey + 19)
a4th                            oscili                  2, 4      * ifqc,  iwheel4, iphase / (ikey + 24)
a5th                            oscili                  1, 5.0397 * ifqc,  iwheel4, iphase / (ikey + 28)
a6th                            oscili                  0, 5.9932 * ifqc,  iwheel4, iphase / (ikey + 31)
a8th                            oscili                  4, 8      * ifqc,  iwheel4, iphase / (ikey + 36)
asignal                         =                       iamplitude * (asubfund + asub3rd + afund + a2nd + a3rd + a4th + a5th + a6th + a8th)
aoutleft, aoutright             pan2                    asignal * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   TubularBellModel
                                //////////////////////////////////////////////////////
                                // Original by Perry Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 4
iattack                         =                       0.003
isustain                        =                       p3
irelease                        =                       0.125
p3                              =                       isustain + iattack + irelease
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
iindex                          =                       1
icrossfade                      =                       2
ivibedepth                      =                       0.2
iviberate                       =                       6
isine                           ftgenonce               0, 0, 65536,    10,     1
icosine                         ftgenonce               0, 0, 65536,    11,     1 ; Cosine wave. Get that noise down on the most widely used table!
icook3                          ftgenonce               0, 0, 65536,    10,     1, 0.4, 0.2, 0.1, 0.1, 0.05
ifn1                            =                       isine
ifn2                            =                       icook3
ifn3                            =                       isine
ifn4                            =                       isine
ivibefn                         =                       icosine
asignal                         fmbell                  1.0, ifrequency, iindex, icrossfade, ivibedepth, iviberate, ifn1, ifn2, ifn3, ifn4, ivibefn
aoutleft, aoutright		        pan2	                asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   WaveguideGuitar
                                //////////////////////////////////////////////////////
                                // Original by Jeff Livingston.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////////////
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) / 16
iHz                             =                       ifrequency
                                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                ; The model takes pluck position, and pickup position (in % of string length), and generates
                                ; a pluck excitation signal, representing the string displacement.  The pluck consists 
                                ; of a forward and backward traveling displacement wave, which are recirculated thru two 
                                ; separate delay lines, to simulate the one dimensional string waveguide, with 
                                ; fixed ends.
                                ;
                                ; Losses due to internal friction of the string, and with air, as well as
                                ; losses due to the mechanical impedance of the string terminations are simulated by 
                                ; low pass filtering the signal inside the feedback loops.
                                ; Delay line outputs at the bridge termination are summed and fed into an IIR filter
                                ; modeled to simulate the lowest two vibrational modes (resonances) of the guitar body.
                                ; The theory implies that force due to string displacement, which is equivalent to 
                                ; displacement velocity times bridge mechanical impedance, is the input to the guitar
                                ; body resonator model. Here we have modified the transfer fuction representing the bridge
                                ; mech impedance, to become the string displacement to bridge input force transfer function.
                                ; The output of the resulting filter represents the displacement of the guitar's top plate,
                                ; and sound hole, since thier respective displacement with be propotional to input force.
                                ; (based on a simplified model, viewing the top plate as a force driven spring).
                                ;
                                ; The effects of pluck hardness, and contact with frets during pluck release,
                                ; have been modeled by injecting noise into the initial pluck, proportional to initial 
                                ; string displacement.
                                ;
                                ; Note on pluck shape: Starting with a triangular displacment, I found a decent sounding
                                ; initial pluck shape after some trial and error.  This pluck shape, which is a linear
                                ; ramp, with steep fall off, doesn't necessarily agree with the pluck string models I've 
                                ; studied.  I found that initial pluck shape significantly affects the realism of the 
                                ; sound output, but I the treatment of this topic in musical acoustics literature seems
                                ; rather limited as far as I've encountered.  
                                ;
                                ; Original pfields
                                ; p1     p2   p3    p4    p5    p6      p7      p8       p9        p10         p11    p12   p13
                                ; in     st   dur   amp   pch   plklen  fbfac	pkupPos	 pluckPos  brightness  vibf   vibd  vibdel
                                ; i01.2	 0.5  0.75  5000  7.11	.85     0.9975	.0	    .25	       1	       0	  0	 0
ip4                             init                    iamplitude
ip6                             init                    0.85
ip7                             init                    0.9975
ip8                             init                    0
ip9                             init                    0.25
ip10                            init                    1.0
ip11                            init                    0.001
ip12                            init                    0.0
ip13                            init                    0.0
afwav                           init                    0
abkwav                          init                    0
abkdout                         init                    0
afwdout                         init                    0 
iEstr	                        init                    1.0 / cpspch(6.04)
ifqc                            init                    iHz ; cpspch(p5)
                                ; note:delay time=2x length of string (time to traverse it)
idlt                            init                    1.0 / ifqc		
ipluck                          =                       0.5 * idlt * ip6 * ifqc / cpspch(8.02)
ifbfac = ip7  			        ; feedback factor
                                ; (exponentialy scaled) additive noise to add hi freq content
ibrightness                     =                       ip10 * exp(ip6 * log(2)) / 2 
ivibRate                        =                       ip11	
ivibDepth                       pow                     2, ip12 / 12
                                ; vibrato depth, +,- ivibDepth semitones
ivibDepth                       =                       idlt - 1.0 / (ivibDepth * ifqc)	
                                ; vibrato start delay (secs)
ivibStDly                       =                       ip13 
                                ; termination impedance model
                                ; cutoff freq of LPF due to mech. impedance at the nut (2kHz-10kHz)
if0                             =                       10000 
                                ; damping parameter of nut impedance
iA0                             =                       ip7  
ialpha                          =                       cos(2 * 3.14159265 * if0 * 1 / sr)
                                ; FIR LPF model of nut impedance,  H(z)=a0+a1z^-1+a0z^-2
ia0                             =                       0.3 * iA0 / (2 * (1 - ialpha))
ia1                             =                       iA0 - 2 * ia0
                                ; NOTE each filter pass adds a sampling period delay,so subtract 1/sr from tap time to compensate
                                ; determine (in crude fashion) which string is being played
                                ; icurStr = (ifqc > cpspch(6.04) ? 2 : 1)
                                ; icurStr = (ifqc > cpspch(6.09) ? 3 : icurStr)
                                ; icurStr = (ifqc > cpspch(7.02) ? 4 : icurStr)
                                ; icurStr = (ifqc > cpspch(7.07) ? 5 : icurStr)
                                ; icurStr = (ifqc > cpspch(7.11) ? 6 : icurStr)
ipupos                          =                       ip8 * idlt / 2 ; pick up position (in % of low E string length)
ippos                           =                       ip9 * idlt / 2 ; pluck position (in % of low E string length)
isegF                           =                       1 / sr
isegF2                          =                       ipluck
iplkdelF                        =                       (ipluck / 2 > ippos ? 0 : ippos - ipluck / 2)
isegB                           =                       1 / sr
isegB2                          =                       ipluck
iplkdelB                        =                       (ipluck / 2 > idlt / 2 - ippos ? 0 : idlt / 2 - ippos - ipluck / 2)
                                ; EXCITATION SIGNAL GENERATION
                                ; the two excitation signals are fed into the fwd delay represent the 1st and 2nd 
                                ; reflections off of the left boundary, and two accelerations fed into the bkwd delay 
                                ; represent the the 1st and 2nd reflections off of the right boundary.
                                ; Likewise for the backward traveling acceleration waves, only they encouter the 
                                ; terminations in the opposite order.
ipw                             =                       1
ipamp                           =                       ip4 * ipluck ; 4 / ipluck
aenvstrf                        linseg                  0, isegF, -ipamp / 2, isegF2, 0
adel1	                        delayr                  (idlt > 0) ? idlt : 0.01
                                ; initial forward traveling wave (pluck to bridge)
aenvstrf1                       deltapi                 iplkdelF        
                                ; first forward traveling reflection (nut to bridge) 
aenvstrf2                       deltapi                 iplkdelB + idlt / 2 
                                delayw                  aenvstrf
                                ; inject noise for attack time string fret contact, and pre pluck vibrations against pick 
anoiz                           rand	                ibrightness
aenvstrf1                       =                       aenvstrf1 + anoiz*aenvstrf1
aenvstrf2                       =                       aenvstrf2 + anoiz*aenvstrf2
                                ; filter to account for losses along loop path
aenvstrf2	                    filter2                 aenvstrf2, 3, 0, ia0, ia1, ia0 
                                ; combine into one signal (flip refl wave's phase)
aenvstrf                        =                       aenvstrf1 - aenvstrf2
                                ; initial backward excitation wave  
aenvstrb                        linseg                  0, isegB, - ipamp / 2, isegB2, 0  
adel2	                        delayr                  (idlt > 0) ? idlt : 0.01
                                ; initial bdwd traveling wave (pluck to nut)
aenvstrb1                       deltapi                 iplkdelB        
                                ; first forward traveling reflection (nut to bridge) 
aenvstrb2                       deltapi                 idlt / 2 + iplkdelF 
                                delayw                  aenvstrb
                                ; initial bdwd traveling wave (pluck to nut)
;  aenvstrb1	delay	aenvstrb,  iplkdelB
                                ; first bkwd traveling reflection (bridge to nut)
;  aenvstrb2	delay	aenvstrb, idlt/2+iplkdelF
                                ; inject noise
aenvstrb1                       =                       aenvstrb1 + anoiz*aenvstrb1
aenvstrb2                       =                       aenvstrb2 + anoiz*aenvstrb2
                                ; filter to account for losses along loop path
aenvstrb2	                    filter2                 aenvstrb2, 3, 0, ia0, ia1, ia0
                                ; combine into one signal (flip refl wave's phase)
aenvstrb	                    =	                    aenvstrb1 - aenvstrb2
                                ; low pass to band limit initial accel signals to be < 1/2 the sampling freq
ainputf                         tone                    aenvstrf, sr * 0.9 / 2
ainputb                         tone                    aenvstrb, sr * 0.9 / 2
                                ; additional lowpass filtering for pluck shaping\
                                ; Note, it would be more efficient to combine stages into a single filter
ainputf                         tone                    ainputf, sr * 0.9 / 2
ainputb                         tone                    ainputb, sr * 0.9 / 2
                                ; Vibrato generator
icosine                         ftgenonce               0, 0, 65536,    11,     1.0                        
avib                            poscil                  ivibDepth, ivibRate, icosine
avibdl		                    delayr		            (((ivibStDly * 1.1)) > 0.0) ? (ivibStDly * 1.1) : 0.01
avibrato	                    deltapi	                ivibStDly
                                delayw		            avib
                                ; Dual Delay line, 
                                ; NOTE: delay length longer than needed by a bit so that the output at t=idlt will be interpolated properly        
                                ;forward traveling wave delay line
afd  		                    delayr                  (((idlt + ivibDepth) * 1.1) > 0.0) ? ((idlt + ivibDepth) * 1.1) : 0.01
                                ; output tap point for fwd traveling wave
afwav  	                        deltapi                 ipupos    	
                                ; output at end of fwd delay (left string boundary)
afwdout	                        deltapi                 idlt - 1 / sr + avibrato	
                                ; lpf/attn due to reflection impedance		
afwdout	                        filter2                 afwdout, 3, 0, ia0, ia1, ia0  
                                delayw                  ainputf + afwdout * ifbfac * ifbfac
                                ; backward trav wave delay line
abkwd  	                        delayr                  (((idlt + ivibDepth) * 1.1) > 0) ? ((idlt + ivibDepth) * 1.1) : 0.01
                                ; output tap point for bkwd traveling wave
abkwav  	                    deltapi                 idlt / 2 - ipupos		
                                ; output at the left boundary
; abkterm	deltapi	idlt/2				
                                ; output at end of bkwd delay (right string boundary)
abkdout	                        deltapi                 idlt - 1 / sr + avibrato	
abkdout	                        filter2                 abkdout, 3, 0, ia0, ia1, ia0  	
                                delayw                  ainputb + abkdout * ifbfac * ifbfac
                                ; resonant body filter model, from Cuzzucoli and Lombardo
                                ; IIR filter derived via bilinear transform method
                                ; the theoretical resonances resulting from circuit model should be:
                                ; resonance due to the air volume + soundhole = 110Hz (strongest)
                                ; resonance due to the top plate = 220Hz
                                ; resonance due to the inclusion of the back plate = 400Hz (weakest)
aresbod                         filter2                 (afwdout + abkdout), 5, 4, 0.000000000005398681501844749, .00000000000001421085471520200, -.00000000001076383426834582, -00000000000001110223024625157, .000000000005392353230604385, -3.990098622573566, 5.974971737738533, -3.979630684599723, .9947612723736902
asignal                         =                       (1500 * (afwav + abkwav + aresbod * .000000000000000000003)) ; * adeclick
aoutleft, aoutright             pan2                    asignal * iamplitude, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin
                                
                                instr                   Xing
                                //////////////////////////////////////////////
                                // Original by Andrew Horner.
                                // Adapted by Michael Gogins.
                                // p4 pitch in octave.pch
                                // original pitch        = A6
                                // range                 = C6 - C7
                                // extended range        = F4 - C7
                                //////////////////////////////////////////////
insno           		        =                       p1
itime           		        =                       p2
iduration       		        =                       p3
ikey            		        =                       p4
ivelocity                       =                       p5
iphase                          =                       p6
ipan                            =                       p7
idepth                          =                       p8
iheight                         =                       p9
ipcs                            =                       p10
ihomogeneity                    =                       p11
kgain			    	=                       1.25
iHz                             =                       cpsmidinn(ikey)
kHz                             =                       k(iHz)
iattack                         =                       (440.0 / iHz) * 0.01
                                print                   iHz, iattack
isustain                        =                       p3
irelease                        =                       .3
p3                              =                       iattack + isustain + irelease
iduration                       =                       p3
iamplitude                      =                       ampdb(ivelocity) * 8.
isine                           ftgenonce               0, 0, 65536,    10,     1
kfreq                           =                       cpsmidinn(ikey)
iamp                            =                       1
inorm                           =                       32310
aamp1                           linseg                  0,.001,5200,.001,800,.001,3000,.0025,1100,.002,2800,.0015,1500,.001,2100,.011,1600,.03,1400,.95,700,1,320,1,180,1,90,1,40,1,20,1,12,1,6,1,3,1,0,1,0
adevamp1                        linseg                  0, .05, .3, iduration - .05, 0
adev1                           poscil                  adevamp1, 6.7, isine, .8
amp1                            =                       aamp1 * (1 + adev1)
aamp2                           linseg                  0,.0009,22000,.0005,7300,.0009,11000,.0004,5500,.0006,15000,.0004,5500,.0008,2200,.055,7300,.02,8500,.38,5000,.5,300,.5,73,.5,5.,5,0,1,1
adevamp2                        linseg                  0,.12,.5,iduration-.12,0
adev2                           poscil                  adevamp2, 10.5, isine, 0
amp2                            =                       aamp2 * (1 + adev2)
aamp3                           linseg                  0,.001,3000,.001,1000,.0017,12000,.0013,3700,.001,12500,.0018,3000,.0012,1200,.001,1400,.0017,6000,.0023,200,.001,3000,.001,1200,.0015,8000,.001,1800,.0015,6000,.08,1200,.2,200,.2,40,.2,10,.4,0,1,0
adevamp3                        linseg                  0, .02, .8, iduration - .02, 0
adev3                           poscil                  adevamp3, 70, isine ,0
amp3                            =                       aamp3 * (1 + adev3)
awt1                            poscil                  amp1, kfreq, isine
awt2                            poscil                  amp2, 2.7 * kfreq, isine
awt3                            poscil                  amp3, 4.95 * kfreq, isine
asig                            =                       awt1 + awt2 + awt3
arel                            linenr                  1,0, iduration, .06
; asignal                         =                       asig * arel * (iamp / inorm) * iamplitude * kgain
asignal                         =                       asig * (iamp / inorm) * iamplitude * kgain
adeclick                        linsegr                 0, iattack, 1, isustain, 1, irelease, 0
asignal                         =                       asignal
aoutleft, aoutright		        pan2			        asignal * adeclick, .875;ipan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr			        ZakianFlute
                                //////////////////////////////////////////////
                                // Original by Lee Zakian.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
if1                    		    ftgenonce               0, 0, 65536,    10,     1
iwtsin				            init			        if1
if2                    		    ftgenonce               0, 0, 16,       -2,     40, 40, 80, 160, 320, 640, 1280, 2560, 5120, 10240, 10240
if26                   		    ftgenonce               0, 0, 65536,    -10,    2000, 489, 74, 219, 125, 9, 33, 5, 5
if27                   		    ftgenonce               0, 0, 65536,    -10,    2729, 1926, 346, 662, 537, 110, 61, 29, 7
if28                   		    ftgenonce               0, 0, 65536,    -10,    2558, 2012, 390, 361, 534, 139, 53, 22, 10, 13, 10
if29                   		    ftgenonce               0, 0, 65536,    -10,    12318, 8844, 1841, 1636, 256, 150, 60, 46, 11
if30                   		    ftgenonce               0, 0, 65536,    -10,    1229, 16, 34, 57, 32
if31                   		    ftgenonce               0, 0, 65536,    -10,    163, 31, 1, 50, 31
if32                   		    ftgenonce               0, 0, 65536,    -10,    4128, 883, 354, 79, 59, 23
if33                   		    ftgenonce               0, 0, 65536,    -10,    1924, 930, 251, 50, 25, 14
if34                   		    ftgenonce               0, 0, 65536,    -10,    94, 6, 22, 8
if35                   		    ftgenonce               0, 0, 65536,    -10,    2661, 87, 33, 18
if36                   		    ftgenonce               0, 0, 65536,    -10,    174, 12
if37                   		    ftgenonce               0, 0, 65536,    -10,    314, 13
                                ;pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 4
iattack                         =                       .25
isustain                        =                       p3
irelease                        =                       .33333333
p3                              =                       iattack + isustain + irelease
iHz                             =                       ifrequency
kHz                             =                       k(iHz)
idB                             =                       i_midivelocity
adeclick77                        linsegr                 0, iattack, 1, isustain, 1, irelease, 0
ip3                     	    =                       (p3 < 3.0 ? p3 : 3.0)
; parameters
; p4    overall amplitude scaling factor
ip4                     	    init                    iamplitude
; p5    pitch in Hertz (normal pitch range: C4-C7)
ip5                     	    init                    iHz
; p6    percent vibrato depth, recommended values in range [-1., +1.]
ip6                     	    init                    1
;        0.0    -> no vibrato
;       +1.     -> 1% vibrato depth, where vibrato rate increases slightly
;       -1.     -> 1% vibrato depth, where vibrato rate decreases slightly
; p7    attack time in seconds
;       recommended value:  .12 for slurred notes, .06 for tongued notes
;                            (.03 for short notes)
ip7                     	    init                    .08
; p8    decay time in seconds
;       recommended value:  .1 (.05 for short notes)
ip8                     	    init                    .08
; p9    overall brightness / filter cutoff factor
;       1 -> least bright / minimum filter cutoff frequency (40 Hz)
;       9 -> brightest / maximum filter cutoff frequency (10,240Hz)
ip9                     	    init                    5
; initial variables
iampscale               	    =                       ip4                              ; overall amplitude scaling factor
ifreq                   	    =                       ip5                              ; pitch in Hertz
ivibdepth               	    =                       abs(ip6*ifreq/100.0)             ; vibrato depth relative to fundamental frequency
iattack                 	    =                       ip7 * (1.1 - .2*giseed)          ; attack time with up to +-10% random deviation
giseed                  	    =                       frac(giseed*105.947)             ; reset giseed
idecay                  	    =                       ip8 * (1.1 - .2*giseed)          ; decay time with up to +-10% random deviation
giseed                  	    =                       frac(giseed*105.947)
ifiltcut                	    tablei                  ip9, if2                          ; lowpass filter cutoff frequency
iattack                 	    =                       (iattack < 6/kr ? 6/kr : iattack)               ; minimal attack length
idecay                  	    =                       (idecay < 6/kr ? 6/kr : idecay)                 ; minimal decay length
isustain                	    =                       p3 - iattack - idecay
p3                      	    =                       (isustain < 5/kr ? iattack+idecay+5/kr : p3)    ; minimal sustain length
isustain                	    =                       (isustain < 5/kr ? 5/kr : isustain)
iatt                    	    =                       iattack/6
isus                    	    =                       isustain/4
idec                    	    =                       idecay/6
iphase                  	    =                       giseed                          ; use same phase for all wavetables
giseed                  	    =                       frac(giseed*105.947)
; vibrato block
; kvibdepth               	    linseg                  .1, .8*p3, 1, .2*p3, .7
kvibdepth               	    linseg                  .1, .8*ip3, 1, isustain, 1, .2*ip3, .7
kvibdepth               	    =                       kvibdepth* ivibdepth            ; vibrato depth
kvibdepthr              	    randi                   .1*kvibdepth, 5, giseed         ; up to 10% vibrato depth variation
giseed                  	    =                       frac(giseed*105.947)
kvibdepth               	    =                       kvibdepth + kvibdepthr
ivibr1                  	    =                       giseed                          ; vibrato rate
giseed                  	    =                       frac(giseed*105.947)
ivibr2                  	    =                       giseed
giseed                  	    =                       frac(giseed*105.947)

                                if                      ip6 < 0 goto            vibrato1
kvibrate                	    linseg                  2.5+ivibr1, p3, 4.5+ivibr2      ; if p6 positive vibrato gets faster
                                goto                    vibrato2
vibrato1:
ivibr3                  	    =                       giseed
giseed                  	    =                       frac(giseed*105.947)
kvibrate                	    linseg                  3.5+ivibr1, .1, 4.5+ivibr2, p3-.1, 2.5+ivibr3   ; if p6 negative vibrato gets slower
vibrato2:
kvibrater               	    randi                   .1*kvibrate, 5, giseed          ; up to 10% vibrato rate variation
giseed                  	    =                       frac(giseed*105.947)
kvibrate                	    =                       kvibrate + kvibrater
kvib                    	    oscili                  kvibdepth, kvibrate, iwtsin
ifdev1                  	    =                       -.03 * giseed                           ; frequency deviation
giseed                  	    =                       frac(giseed*105.947)
ifdev2                  	    =                       .003 * giseed
giseed                  	    =                       frac(giseed*105.947)
ifdev3                  	    =                       -.0015 * giseed
giseed                  	    =                       frac(giseed*105.947)
ifdev4                  	    =                       .012 * giseed
giseed                  	    =                       frac(giseed*105.947)
kfreqr                  	    linseg                  ifdev1, iattack, ifdev2, isustain, ifdev3, idecay, ifdev4
kfreq                   	    =                       kHz * (1 + kfreqr) + kvib
                                if                      ifreq <  427.28 goto    range1                          ; (cpspch(8.08) + cpspch(8.09))/2
                                if                      ifreq <  608.22 goto    range2                          ; (cpspch(9.02) + cpspch(9.03))/2
                                if                      ifreq <  1013.7 goto    range3                          ; (cpspch(9.11) + cpspch(10.00))/2
                                goto                    range4
; wavetable amplitude envelopes
range1:                 	    ; for low range tones
kamp1                   	    linseg                  0, iatt, 0.002, iatt, 0.045, iatt, 0.146, iatt,  \
                                                        0.272, iatt, 0.072, iatt, 0.043, isus, 0.230, isus, 0.000, isus, \
                                                        0.118, isus, 0.923, idec, 1.191, idec, 0.794, idec, 0.418, idec, \
                                                        0.172, idec, 0.053, idec, 0
kamp2                   	    linseg                  0, iatt, 0.009, iatt, 0.022, iatt, -0.049, iatt,  \
                                                        -0.120, iatt, 0.297, iatt, 1.890, isus, 1.543, isus, 0.000, isus, \
                                                        0.546, isus, 0.690, idec, -0.318, idec, -0.326, idec, -0.116, idec, \
                                                        -0.035, idec, -0.020, idec, 0
kamp3                   	    linseg                  0, iatt, 0.005, iatt, -0.026, iatt, 0.023, iatt,    \
                                                        0.133, iatt, 0.060, iatt, -1.245, isus, -0.760, isus, 1.000, isus,  \
                                                        0.360, isus, -0.526, idec, 0.165, idec, 0.184, idec, 0.060, idec,   \
                                                        0.010, idec, 0.013, idec, 0
iwt1                    	    =                       if26                                      ; wavetable numbers
iwt2                    	    =                       if27
iwt3                    	    =                       if28
inorm                   	    =                       3949
                                goto                    end
range2:                 	    ; for low mid-range tones
kamp1                   	    linseg                  0, iatt, 0.000, iatt, -0.005, iatt, 0.000, iatt, \
                                                        0.030, iatt, 0.198, iatt, 0.664, isus, 1.451, isus, 1.782, isus, \
                                                        1.316, isus, 0.817, idec, 0.284, idec, 0.171, idec, 0.082, idec, \
                                                        0.037, idec, 0.012, idec, 0
kamp2                   	    linseg                  0, iatt, 0.000, iatt, 0.320, iatt, 0.882, iatt,      \
                                                        1.863, iatt, 4.175, iatt, 4.355, isus, -5.329, isus, -8.303, isus,   \
                                                        -1.480, isus, -0.472, idec, 1.819, idec, -0.135, idec, -0.082, idec, \
                                                        -0.170, idec, -0.065, idec, 0
kamp3                   	    linseg                  0, iatt, 1.000, iatt, 0.520, iatt, -0.303, iatt,     \
                                                        0.059, iatt, -4.103, iatt, -6.784, isus, 7.006, isus, 11, isus,      \
                                                        12.495, isus, -0.562, idec, -4.946, idec, -0.587, idec, 0.440, idec, \
                                                        0.174, idec, -0.027, idec, 0
iwt1                    	    =                       if29
iwt2                    	    =                       if30
iwt3                    	    =                       if31
inorm                   	    =                       27668.2
                                goto                    end
range3:                 	    ; for high mid-range tones
kamp1                   	    linseg                  0, iatt, 0.005, iatt, 0.000, iatt, -0.082, iatt,      \
                                                        0.36, iatt, 0.581, iatt, 0.416, isus, 1.073, isus, 0.000, isus,       \
                                                        0.356, isus, .86, idec, 0.532, idec, 0.162, idec, 0.076, idec, 0.064, \
                                                        idec, 0.031, idec, 0
kamp2                   	    linseg                  0, iatt, -0.005, iatt, 0.000, iatt, 0.205, iatt,      \
                                                        -0.284, iatt, -0.208, iatt, 0.326, isus, -0.401, isus, 1.540, isus,   \
                                                        0.589, isus, -0.486, idec, -0.016, idec, 0.141, idec, 0.105, idec,    \
                                                        -0.003, idec, -0.023, idec, 0
kamp3                   	    linseg                  0, iatt, 0.722, iatt, 1.500, iatt, 3.697, iatt,       \
                                                        0.080, iatt, -2.327, iatt, -0.684, isus, -2.638, isus, 0.000, isus,   \
                                                        1.347, isus, 0.485, idec, -0.419, idec, -.700, idec, -0.278, idec,    \
                                                        0.167, idec, -0.059, idec, 0
iwt1                    	    =                       if32
iwt2                    	    =                       if33
iwt3                    	    =                       if34
inorm                   	    =                       3775
                                goto                    end
range4:                                                 ; for high range tones
kamp1                   	    linseg                  0, iatt, 0.000, iatt, 0.000, iatt, 0.211, iatt,         \
                                                        0.526, iatt, 0.989, iatt, 1.216, isus, 1.727, isus, 1.881, isus,        \
                                                        1.462, isus, 1.28, idec, 0.75, idec, 0.34, idec, 0.154, idec, 0.122,    \
                                                        idec, 0.028, idec, 0
kamp2                   	    linseg                  0, iatt, 0.500, iatt, 0.000, iatt, 0.181, iatt,         \
                                                        0.859, iatt, -0.205, iatt, -0.430, isus, -0.725, isus, -0.544, isus,    \
                                                        -0.436, isus, -0.109, idec, -0.03, idec, -0.022, idec, -0.046, idec,    \
                                                        -0.071, idec, -0.019, idec, 0
kamp3                   	    linseg                  0, iatt, 0.000, iatt, 1.000, iatt, 0.426, iatt,         \
                                                        0.222, iatt, 0.175, iatt, -0.153, isus, 0.355, isus, 0.175, isus,       \
                                                        0.16, isus, -0.246, idec, -0.045, idec, -0.072, idec, 0.057, idec,      \
                                                        -0.024, idec, 0.002, idec, 0
iwt1                    	    =                       if35
iwt2                    	    =                       if36
iwt3                    	    =                       if37
inorm                   	    =                       4909.05
                                goto                    end
end:
kampr1                  	    randi                   .02*kamp1, 10, giseed                   ; up to 2% wavetable amplitude variation
giseed                  	    =                       frac(giseed*105.947)
kamp1                   	    =                       kamp1 + kampr1
kampr2                  	    randi                   .02*kamp2, 10, giseed                   ; up to 2% wavetable amplitude variation
giseed                  	    =                       frac(giseed*105.947)
kamp2                   	    =                       kamp2 + kampr2
kampr3                  	    randi                   .02*kamp3, 10, giseed                   ; up to 2% wavetable amplitude variation
giseed                  	    =                       frac(giseed*105.947)
kamp3                   	    =                       kamp3 + kampr3
awt1                    	    poscil                  kamp1, kfreq, iwt1, iphase              ; wavetable lookup
awt2                    	    poscil                  kamp2, kfreq, iwt2, iphase
awt3                    	    poscil                  kamp3, kfreq, iwt3, iphase
asig                    	    =                       awt1 + awt2 + awt3
asig                    	    =                       asig*(iampscale/inorm)
kcut                    	    linseg                  0, iattack, ifiltcut, isustain, ifiltcut, idecay, 0     ; lowpass filter for brightness control
afilt                   	    tone                    asig, kcut
asignal                    	    balance                 afilt, asig
iattack                         =                       0.005
isustain                        =                       p3
irelease                        =                       0.06
p3                              =                       isustain + iattack + irelease
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
aoutleft, aoutright             pan2                    asignal * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                //////////////////////////////////////////////
                                // OUTPUT INSTRUMENTS MUST GO BELOW HERE
                                //////////////////////////////////////////////

                                instr                   Reverberation
                                //////////////////////////////////////////////
                                // By Michael Gogins.
                                //////////////////////////////////////////////
ainleft                         inleta                  "inleft"
ainright                        inleta                  "inright"
if (gkReverberationEnabled == 0) goto thenclause
goto elseclause
thenclause:
aoutleft                        =                       ainleft
aoutright                       =                       ainright
kdry				            =			            1.0 - gkReverberationWet
goto endifclause
elseclause:
awetleft, awetright             reverbsc                ainleft, ainright, gkReverberationDelay, 18000.0
aoutleft			            =			            ainleft *  kdry + awetleft  * gkReverberationWet
aoutright			            =			            ainright * kdry + awetright * gkReverberationWet
endifclause:
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                endin

                                instr                   MasterOutput
                                //////////////////////////////////////////////
                                // By Michael Gogins.
                                //////////////////////////////////////////////
ainleft                         inleta                  "inleft"
ainright                        inleta                  "inright"
aoutleft                        =                       gkMasterLevel * ainleft
aoutright                       =                       gkMasterLevel * ainright
                                outs                    aoutleft, aoutright
                                endin
                                
            )");
    model.arrange( 1,  9,-12.00); // Was 25.
    model.arrange( 2, 16,  7.00);
    model.arrange( 3, 14,  1.00); // Was 5.
    model.arrange( 4,  5,  1.00);
    model.arrange( 5, 57,  0.75);
    model.arrange( 6, 15,  5.00);
    model.arrange( 7, 16,  7.00);
    model.arrange( 8, 16,  7.00);
    model.arrange( 9, 13, -7.00);
    model.arrange(10, 11,  3.00);
    model.arrange(11, 14,  5.00); // Was 5.
    model.arrange(12,  4,  5.00);
    model.processArgv(argc, argv);
}

