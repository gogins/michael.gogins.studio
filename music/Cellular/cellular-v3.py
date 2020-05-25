'''
Cellular, for Computer Piano

Copyright (C) 2019 by Michael Gogins

Mozart's musical dice game of 1787 is taken apart and put back together along 
the lines of Terry Riley's "In C" using Python, re-harmonized using the 
CsoundAC.Scale class, and rendered with a built-in Csound orchestra that 
the Pianoteq synthesized piano.
'''
print(__doc__)
import CsoundAC
import os
import random
random.seed(29389)
import signal
import string
import sys
import traceback

print('Set "rendering" to:     "soundfile" or "audio".')
print
rendering = 'audio'


model = CsoundAC.MusicModel()
score = model.getScore()

scriptFilename = sys.argv[0]
print('Full Python script:     %s' % scriptFilename)
title, exte = os.path.splitext(os.path.basename(scriptFilename))
model.setTitle(title)
model.setArtist("Michael Gogins")
model.setAuthor("Michael Gogins")
model.setYear("2020")
model.generateAllNames()
soundfileName = model.getOutputSoundfileFilepath()
print('Soundfile name:         %s' % soundfileName)
dacName = 'dac:plughw:1,0'
print('Audio output name:      %s' % dacName)
print

print('Rendering option:       %s' % rendering)
commandsForRendering = {
    'soundfile':    'csound -d -r 48000 -k 128 -m195 -+msg_color=0 -RWZdfo %s' % (soundfileName),
    'audio':        'csound -d -r 48000 -k 128 -m195 -+msg_color=0 -o%s'       % (dacName),
}
csoundCommand = commandsForRendering[rendering]
print('Csound command line:    %s' % csoundCommand)
print

# This is Mozart's original minuet table for his musical dice game.
minuetTable = {}
minuetTable[ 2] = { 1: 96,  2: 22,  3:141,  4: 41,  5:105,  6:122,  7: 11,  8: 30,  9: 70, 10:121, 11: 26, 12:  9, 13:112, 14: 49, 15:109, 16: 14}
minuetTable[ 3] = { 1: 32,  2:  6,  3:128,  4: 63,  5:146,  6: 46,  7:134,  8: 81,  9:117, 10: 39, 11:126, 12: 56, 13:174, 14: 18, 15:116, 16: 83}
minuetTable[ 4] = { 1: 69,  2: 95,  3:158,  4: 13,  5:153,  6: 55,  7:110,  8: 24,  9: 66, 10:139, 11: 15, 12:132, 13: 73, 14: 58, 15:145, 16: 79}
minuetTable[ 5] = { 1: 40,  2: 17,  3:113,  4: 85,  5:161,  6:  2,  7:159,  8:100,  9: 90, 10:176, 11:  7, 12: 34, 13: 67, 14:160, 15: 52, 16:170}
minuetTable[ 6] = { 1:148,  2: 74,  3:163,  4: 45,  5: 80,  6: 97,  7: 36,  8:107,  9: 25, 10:143, 11: 64, 12:125, 13: 76, 14:136, 15:  1, 16: 93}
minuetTable[ 7] = { 1:104,  2:157,  3: 27,  4:167,  5:154,  6: 68,  7:118,  8: 91,  9:138, 10: 71, 11:150, 12: 29, 13:101, 14:162, 15: 23, 16:151}
minuetTable[ 8] = { 1:152,  2: 60,  3:171,  4: 53,  5: 99,  6:133,  7: 21,  8:127,  9: 16, 10:155, 11: 57, 12:175, 13: 43, 14:168, 15: 89, 16:172}
minuetTable[ 9] = { 1:119,  2: 84,  3:114,  4: 50,  5:140,  6: 86,  7:169,  8: 94,  9:120, 10: 88, 11: 48, 12:166, 13: 51, 14:115, 15: 72, 16:111}
minuetTable[10] = { 1: 98,  2:142,  3: 42,  4:156,  5: 75,  6:129,  7: 62,  8:123,  9: 65, 10: 77, 11: 19, 12: 82, 13:137, 14: 38, 15:149, 16:  8}
minuetTable[11] = { 1:  3,  2: 87,  3:165,  4: 61,  5:135,  6: 47,  7:147,  8: 33,  9:102, 10:  4, 11: 31, 12:164, 13:144, 14: 59, 15:173, 16: 78}
minuetTable[12] = { 1: 54,  2:130,  3: 10,  4:103,  5: 28,  6: 37,  7:106,  8:  5,  9: 35, 10: 20, 11:108, 12: 92, 13: 12, 14:124, 15: 44, 16:131}

def reverse_enumeration(L):
   for index in reversed(range(len(L))):
      yield index, L[index]
   
# These individually varied repetitions are the process used by Terry Riley's 
# "In C."      
repetitions = []
for i in range(16 * 4):
    repetitions.append(1 + int(random.random() * 6.0))     

# However, we ensure that each track lasts as long as the others.
for i in range(6):
    random.shuffle(repetitions)
    print("repetitions: {}".format(repetitions))

def readMeasure(number):
    scoreNode = CsoundAC.ScoreNode()
    scoreNode.thisown = 0
    filename = 'M' + str(number) + '.mid'
    score = scoreNode.getScore()
    score.load(filename)
    # Remove false notes.
    for i, event in reverse_enumeration(score):
        if event.getChannel() < 0:
            score.remove(i)
    return scoreNode

def buildTrack(voiceleadingNode, sequence, channel, bass, time_offset, pan):
    global repetitions
    tempo = 1.5
    scale = CsoundAC.Scale("Ab major")
    chord = scale.chord(1, 4)
    cumulativeTime = 1.0 + time_offset
    for i in range(1, 16):
        m = 0
        for j in range(2, 6):
            repeatCount = repetitions[m]
            m = m + 1
            scales = scale.modulations(chord)
            scale_count = len(scales)
            count = 0
            # When we pick a number of repetitions for a measure, we see if 
            # if the current chord can be a pivot chord, and if so, we choose 
            # one of the possible modulations to perform.
            if (scale_count > 1):
                random_index = random.randint(0, scale_count -1)
                for s in scales:
                    print("Possible modulation at: {:9.4f} {} {}".format(cumulativeTime, s.toString(), s.name()))
                    if count == random_index:
                        scale = s  
                        print("             Chose modulation to: {} {}".format(scale.toString(), scale.name()))
                    count = count + 1
                print(" ")
            for k in range(repeatCount):
                if channel == 2:
                    # Once the scale is chosen, we perform root progressions 
                    # within the scale; away from the tonic is multiples of -2
                    # scale degrees, back to the tonic is multiples of 1 scale 
                    # degree with a preference for 4 (as used by V to I). 
                    # These root progressions are weighted.
                    progression = random.choices([-2, -4, -6, -8, -10, -12, 3, 6], [10, 3, 2, 1, 1, 1, 8, 3], k=1)
                    steps = progression[0]
                    print("choice: {} steps: {}".format(progression, steps))
                    chord = scale.transpose_degrees(chord, steps)
                    print("{:9.4f} by {:4}: {}".format(cumulativeTime, steps, chord.eOP().name()))
                    voiceleadingNode.chord(chord, cumulativeTime)
                measure = readMeasure(minuetTable[j][i+1])
                duration = measure.getScore().getDuration() * tempo
                measure.getScore().setDuration(duration)
                rescale = CsoundAC.Rescale() 
                rescale.setRescale(CsoundAC.Event.TIME, bool(1), bool(0), cumulativeTime, 0)
                rescale.setRescale(CsoundAC.Event.INSTRUMENT, bool(1), bool(1), channel, 0)
                rescale.setRescale(CsoundAC.Event.KEY, bool(1), bool(1), float(bass), 48)
                rescale.setRescale(CsoundAC.Event.PAN, bool(1), bool(0), float(pan), 0)
                rescale.thisown = 0
                rescale.addChild(measure)
                sequence.addChild(rescale)
                cumulativeTime = cumulativeTime + duration

sequence = CsoundAC.Rescale()
voiceleadingNode = CsoundAC.VoiceleadingNode()
voiceleadingNode.addChild(sequence);
model.addChild(voiceleadingNode)
sequence.setRescale(CsoundAC.Event.VELOCITY,   bool(1), bool(1), 60, 12)

duration = 1.5
timeoffset = (duration * 6.0) / 4.0

buildTrack(voiceleadingNode, sequence,  2, 34, timeoffset * 0.0, 1/7)
buildTrack(voiceleadingNode, sequence, 56, 34, timeoffset * 1.0, 2/7)
buildTrack(voiceleadingNode, sequence, 27, 34, timeoffset * 2.0, 3/7)
buildTrack(voiceleadingNode, sequence, 29, 34, timeoffset * 4.0, 4/7)
buildTrack(voiceleadingNode, sequence, 17, 34, timeoffset * 5.0, 5/7)
buildTrack(voiceleadingNode, sequence,  1, 34, timeoffset * 3.0, 6/7)

orc = '''
; Change to 96000 with 1 ksmps for final rendering.
sr = 48000
ksmps = 128
nchnls = 2
0dbfs = 3

; Ensure the same random stream for each rendering.
; rand, randh, randi, rnd(x) and birnd(x) are not affected by seed.

seed 88818145

// These must be initialized here to be in scope for both 
// the note and the audio patches.

gi_Fluidsynth fluidEngine 0, 0
gi_FluidSteinway fluidLoad "Steinway_C.sf2", gi_Fluidsynth, 1
fluidProgramSelect gi_Fluidsynth, 0, gi_FluidSteinway, 0, 1

gi_Pianoteq vstinit "/home/mkg/Pianoteq\ 6/amd64/Pianoteq\ 6.so", 0
vstinfo gi_Pianoteq 

alwayson "PianoOutPianoteq"
alwayson "PianoOutFluidsynth"
;alwayson "MVerb"
alwayson "ReverbSC"
alwayson "MasterOutput"

// Watch out for 8, 1, 17, and 47.

gi_Pianoteq vstinit "/home/mkg/Pianoteq\ 6/amd64/Pianoteq\ 6.so", 0

#include "PianoNoteFluidsynth.inc"
#include "PianoNotePianoteq.inc"

#include "BandedWG.inc"
#include "BarModel.inc"
#include "BassModel.inc"
#include "Blower.inc"
#include "Bower.inc"
#include "Buzzer.inc"
#include "Cascone_ClickyFilterSweep.inc"
#include "Cascone_FMReverseEnv.inc"
#include "BanchoffKleinBottle.inc"
#include "Cascone_RissetCascadeHarmonics.inc"
#include "Cascone_SampleAndHold.inc"
#include "Cascone_Sine.inc"
#include "Cascone_ThreeBranch.inc"
#include "Cascone_Water.inc"
#include "ChebyshevMelody.inc"
#include "ChebyshevPoly.inc"
;;;#include "CostelloGong.inc"
#include "DelayedPlucked.inc"
#include "Droner.inc"
#include "FilteredSines.inc"
#include "FMBell.inc"
//#include "FM_Clang_Controls.inc"
//#include "FM_Clang.inc"
//#include "FM_Clang_Preset.inc"
#include "FMDroner.inc"
#include "FMModerate2.inc"
#include "FMModerate.inc"
#include "FMModulatedChorus.inc"
#include "FMWaterBell.inc"
#include "Guitar.inc"
#include "Harpsichord.inc"
#include "HeavyMetal.inc"
//#include "JackAudio.inc"
//#include "JackNote.inc"
#include "KarplusStrong3.inc"
//#include "LeftReverberator.inc"
#include "LivingstonGuitar.inc"
#include "Melody.inc"
//#include "MonoReverberator.inc"
#include "Night2.inc"
#include "Phaser.inc"
#include "PhysicalModel2.inc"
#include "Plucked.inc"
#include "PulseWidthModulation.inc"
#include "RampSaw.inc"
//#include "Reverb1.inc"
//#include "Reverb2.inc"
//#include "Reverberator.inc"
#include "Rhodes.inc"
//#include "RightReverberator.inc"
#include "Shiner.inc"
//#include "SolinaChorus.inc"
//#include "Soundfile.inc"
//#include "Spatialize1.inc"
//#include "Spatialize2.inc"
//#include "Spatialize.inc"
#include "STKBeeThree.inc"
#include "STKBowed.inc"
#include "STKPlucked.inc"
#include "StringPad.inc"
#include "Sweeper.inc"
//#include "Template.inc"
#include "TerrainMappedBass.inc"
#include "TerrainMappedLead.inc"
#include "TerrainMappedPulsar.inc"
#include "TerrainMappedSquarish.inc"
#include "ToneWheelOrgan.inc"
#include "TubularBell.inc"
//#include "WaveTerrain.inc"
#include "WGPluck.inc"
#include "Xing.inc"
#include "YiString.inc"
#include "ZakianFlute.inc"

#include "FluidAudio.inc"
#include "PianoOutFluidsynth.inc"
#include "PianoOutPianoteq.inc"
//#include "ParametricEQ.inc"
//#include "Compressor.inc"
#include "MVerb.inc"
#include "ReverbSC.inc"
#include "MasterOutput.inc"

connect "BanchoffKleinBottle", "outleft", "ReverbSC", "inleft"
connect "BanchoffKleinBottle", "outright", "ReverbSC", "inright"
connect "BandedWG", "outleft", "ReverbSC", "inleft"
connect "BandedWG", "outright", "ReverbSC", "inright"
;connect "BarModel", "outleft", "ReverbSC", "inleft"
;connect "BarModel", "outright", "ReverbSC", "inright"
connect "BassModel", "outleft", "ReverbSC", "inleft"
connect "BassModel", "outright", "ReverbSC", "inright"
connect "Blower", "outleft", "ReverbSC", "inleft"
connect "Blower", "outright", "ReverbSC", "inright"
connect "Bower", "outleft", "ReverbSC", "inleft"
connect "Bower", "outright", "ReverbSC", "inright"
connect "Buzzer", "outleft", "ReverbSC", "inleft"
connect "Buzzer", "outright", "ReverbSC", "inright"
connect "Cascone_ClickyFilterSweep", "outleft", "ReverbSC", "inleft"
connect "Cascone_ClickyFilterSweep", "outright", "ReverbSC", "inright"
connect "Cascone_FMReverseEnv", "outleft", "ReverbSC", "inleft"
connect "Cascone_FMReverseEnv", "outright", "ReverbSC", "inright"
connect "Cascone_RissetCascadeHarmonics", "outleft", "ReverbSC", "inleft"
connect "Cascone_RissetCascadeHarmonics", "outright", "ReverbSC", "inright"
connect "Cascone_SampleAndHold", "outleft", "ReverbSC", "inleft"
connect "Cascone_SampleAndHold", "outright", "ReverbSC", "inright"
connect "Cascone_Sine", "outleft", "ReverbSC", "inleft"
connect "Cascone_Sine", "outright", "ReverbSC", "inright"
connect "Cascone_ThreeBranch", "outleft", "ReverbSC", "inleft"
connect "Cascone_ThreeBranch", "outright", "ReverbSC", "inright"
connect "Cascone_Water", "outleft", "ReverbSC", "inleft"
connect "Cascone_Water", "outright", "ReverbSC", "inright"
connect "ChebyshevMelody", "outleft", "ReverbSC", "inleft"
connect "ChebyshevMelody", "outright", "ReverbSC", "inright"
connect "ChebyshevPoly", "outleft", "ReverbSC", "inleft"
connect "ChebyshevPoly", "outright", "ReverbSC", "inright"
;;connect "CostelloGong", "outleft", "ReverbSC", "inleft"
;;connect "CostelloGong", "outright", "ReverbSC", "inright"
connect "Droner", "outleft", "ReverbSC", "inleft"
connect "Droner", "outright", "ReverbSC", "inright"
connect "FilteredSines", "outleft", "ReverbSC", "inleft"
connect "FilteredSines", "outright", "ReverbSC", "inright"
connect "FMBell", "outleft", "ReverbSC", "inleft"
connect "FMBell", "outright", "ReverbSC", "inright"
connect "FMDroner", "outleft", "ReverbSC", "inleft"
connect "FMDroner", "outright", "ReverbSC", "inright"
connect "FMModerate", "outleft", "ReverbSC", "inleft"
connect "FMModerate", "outright", "ReverbSC", "inright"
connect "FMModerate2", "outleft", "ReverbSC", "inleft"
connect "FMModerate2", "outright", "ReverbSC", "inright"
connect "FMModulatedChorus", "outleft", "ReverbSC", "inleft"
connect "FMModulatedChorus", "outright", "ReverbSC", "inright"
connect "FMWaterBell", "outleft", "ReverbSC", "inleft"
connect "FMWaterBell", "outright", "ReverbSC", "inright"
connect "Guitar", "outleft", "ReverbSC", "inleft"
connect "Guitar", "outright", "ReverbSC", "inright"
connect "Harpsichord", "outleft", "ReverbSC", "inleft"
connect "Harpsichord", "outright", "ReverbSC", "inright"
connect "HeavyMetal", "outleft", "ReverbSC", "inleft"
connect "HeavyMetal", "outright", "ReverbSC", "inright"
connect "KarplusStrong3", "outleft", "ReverbSC", "inleft"
connect "KarplusStrong3", "outright", "ReverbSC", "inright"
connect "Melody", "outleft", "ReverbSC", "inleft"
connect "Melody", "outright", "ReverbSC", "inright"
connect "Night2", "outleft", "ReverbSC", "inleft"
connect "Night2", "outright", "ReverbSC", "inright"
connect "Phaser", "outleft", "ReverbSC", "inleft"
connect "Phaser", "outright", "ReverbSC", "inright"
connect "PhysicalModel2", "outleft", "ReverbSC", "inleft"
connect "PhysicalModel2", "outright", "ReverbSC", "inright"
connect "PianoNoteFluidsynth", "outleft", "ReverbSC", "inleft"
connect "PianoNoteFluidsynth", "outright", "ReverbSC", "inright"
connect "PianoNotePianoteq", "outleft", "ReverbSC", "inleft"
connect "PianoNotePianoteq", "outright", "ReverbSC", "inright"
connect "Plucked", "outleft", "ReverbSC", "inleft"
connect "Plucked", "outright", "ReverbSC", "inright"
connect "PulseWidthModulation", "outleft", "ReverbSC", "inleft"
connect "PulseWidthModulation", "outright", "ReverbSC", "inright"
connect "RampSaw", "outleft", "ReverbSC", "inleft"
connect "RampSaw", "outright", "ReverbSC", "inright"
connect "Rhodes", "outleft", "ReverbSC", "inleft"
connect "Rhodes", "outright", "ReverbSC", "inright"
connect "Shiner", "outleft", "ReverbSC", "inleft"
connect "Shiner", "outright", "ReverbSC", "inright"
connect "STKBeeThree", "outleft", "ReverbSC", "inleft"
connect "STKBeeThree", "outright", "ReverbSC", "inright"
connect "STKBowed", "outleft", "ReverbSC", "inleft"
connect "STKBowed", "outright", "ReverbSC", "inright"
connect "STKPlucked", "outleft", "ReverbSC", "inleft"
connect "STKPlucked", "outright", "ReverbSC", "inright"
connect "StringPad", "outleft", "ReverbSC", "inleft"
connect "StringPad", "outright", "ReverbSC", "inright"
connect "Sweeper", "outleft", "ReverbSC", "inleft"
connect "Sweeper", "outright", "ReverbSC", "inright"
connect "TerrainMappedBass", "outleft", "ReverbSC", "inleft"
connect "TerrainMappedBass", "outright", "ReverbSC", "inright"
connect "TerrainMappedLead", "outleft", "ReverbSC", "inleft"
connect "TerrainMappedLead", "outright", "ReverbSC", "inright"
connect "TerrainMappedPulsar", "outleft", "ReverbSC", "inleft"
connect "TerrainMappedPulsar", "outright", "ReverbSC", "inright"
connect "TerrainMappedSquarish", "outleft", "ReverbSC", "inleft"
connect "TerrainMappedSquarish", "outright", "ReverbSC", "inright"
connect "ToneWheelOrgan", "outleft", "ReverbSC", "inleft"
connect "ToneWheelOrgan", "outright", "ReverbSC", "inright"
connect "TubularBell", "outleft", "ReverbSC", "inleft"
connect "TubularBell", "outright", "ReverbSC", "inright"
connect "WaveTerrain", "outleft", "ReverbSC", "inleft"
connect "WaveTerrain", "outright", "ReverbSC", "inright"
connect "WGPluck", "outleft", "ReverbSC", "inleft"
connect "WGPluck", "outright", "ReverbSC", "inright"
connect "Xing", "outleft", "ReverbSC", "inleft"
connect "Xing", "outright", "ReverbSC", "inright"
connect "YiString", "outleft", "ReverbSC", "inleft"
connect "YiString", "outright", "ReverbSC", "inright"
connect "ZakianFlute", "outleft", "ReverbSC", "inleft"
connect "ZakianFlute", "outright", "ReverbSC", "inright"

connect "PianoOutFluidsynth", "outleft", "ReverbSC", "inleft"
connect "PianoOutFluidsynth", "outright", "ReverbSC", "inright"
connect "PianoOutPianoteq", "outleft", "ReverbSC", "inleft"
connect "PianoOutPianoteq", "outright", "ReverbSC", "inright"
connect "PianoOut", "outleft", "ReverbSC", "inleft"
connect "PianoOut", "outright", "ReverbSC", "inright"
;connect "ReverbSC", "outleft", "MVerb", "inleft"
;connect "ReverbSC", "outright", "MVerb", "inright"
connect "ReverbSC", "outleft", "MasterOutput", "inleft"
connect "ReverbSC", "outright", "MasterOutput", "inright"
connect "MVerb", "outleft", "MasterOutput", "inleft"
connect "MVerb", "outright", "MasterOutput", "inright"

gk_MVerb_FB init .5
gk_MVerb_wet init .375
gk_MVerb_random init 1
gk_MVerb_rslow init 1.1
gk_MVerb_rfast init 3.8
gk_MVerb_rmax init .0005
gk_MVerb_print init 1
gk_MVerb_DFact init .5

gk_ChebyshevMelody_level init 5
gk_ZakianFlute_level init 8
gk_PianoOutPianoteq_level init -3
gk_FMWaterBell_level init 10
gk_PianoOutFluidsynth_level init -1
gk_Harpsichord_level init 11
gk_Rhodes_level init 12
gk_Reverb_wet init 0.25
gk_Reverb_feedback init 0.85
gi_Reverb_delay_modulation init 0.0875
gk_Reverb_frequency_cutoff init 15000
'''

model.setCsoundOrchestra(orc)
model.setCsoundCommand(csoundCommand)
model.generate()
score.save(model.getMidifileFilepath())
model.performMaster()
if rendering == 'soundfile':
    model.translateMaster()
