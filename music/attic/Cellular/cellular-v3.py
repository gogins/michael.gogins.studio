'''
Cellular version 3

Copyright (C) 2019 by Michael Gogins

Mozart's musical dice game of 1787 is taken apart and put back together along 
the lines of Terry Riley's "In C" using Python, re-harmonized using the 
CsoundAC.Scale class, and rendered with a built-in Csound orchestra that 
uses the Pianoteq synthesized piano and other Csound instruments.
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

script_filename = sys.argv[0]
print('Full Python script:     %s' % script_filename)
title, exte = os.path.splitext(os.path.basename(script_filename))
model.setTitle(title)
model.setArtist("Michael Gogins")
model.setAuthor("Michael Gogins")
model.setYear("2020")
model.generateAllNames()
soundfile_name = model.getOutputSoundfileFilepath()
print('Soundfile name:         %s' % soundfile_name)
dac_name = 'dac:plughw:1,0'
print('Audio output name:      %s' % dac_name)
print

print('Rendering option:       %s' % rendering)
rendering_commands = {
    'soundfile':    'csound -d -r 48000 -k 128 -m195 -+msg_color=0 -RWZdfo %s' % (soundfile_name),
    'audio':        'csound -d -r 48000 -k 128 -m195 -+msg_color=0 -o%s'       % (dac_name),
}
csound_command = rendering_commands[rendering]
print('Csound command line:    %s' % csound_command)
print

# This is Mozart's original minuet table for his musical dice game. We do not 
# use his trio table. We preserve Mozart's idiosyncratic (by our standards) 
# indexing ranges. 

minuet_table = {}
minuet_table[ 2] = { 1: 96,  2: 22,  3:141,  4: 41,  5:105,  6:122,  7: 11,  8: 30,  9: 70, 10:121, 11: 26, 12:  9, 13:112, 14: 49, 15:109, 16: 14}
minuet_table[ 3] = { 1: 32,  2:  6,  3:128,  4: 63,  5:146,  6: 46,  7:134,  8: 81,  9:117, 10: 39, 11:126, 12: 56, 13:174, 14: 18, 15:116, 16: 83}
minuet_table[ 4] = { 1: 69,  2: 95,  3:158,  4: 13,  5:153,  6: 55,  7:110,  8: 24,  9: 66, 10:139, 11: 15, 12:132, 13: 73, 14: 58, 15:145, 16: 79}
minuet_table[ 5] = { 1: 40,  2: 17,  3:113,  4: 85,  5:161,  6:  2,  7:159,  8:100,  9: 90, 10:176, 11:  7, 12: 34, 13: 67, 14:160, 15: 52, 16:170}
minuet_table[ 6] = { 1:148,  2: 74,  3:163,  4: 45,  5: 80,  6: 97,  7: 36,  8:107,  9: 25, 10:143, 11: 64, 12:125, 13: 76, 14:136, 15:  1, 16: 93}
minuet_table[ 7] = { 1:104,  2:157,  3: 27,  4:167,  5:154,  6: 68,  7:118,  8: 91,  9:138, 10: 71, 11:150, 12: 29, 13:101, 14:162, 15: 23, 16:151}
minuet_table[ 8] = { 1:152,  2: 60,  3:171,  4: 53,  5: 99,  6:133,  7: 21,  8:127,  9: 16, 10:155, 11: 57, 12:175, 13: 43, 14:168, 15: 89, 16:172}
minuet_table[ 9] = { 1:119,  2: 84,  3:114,  4: 50,  5:140,  6: 86,  7:169,  8: 94,  9:120, 10: 88, 11: 48, 12:166, 13: 51, 14:115, 15: 72, 16:111}
minuet_table[10] = { 1: 98,  2:142,  3: 42,  4:156,  5: 75,  6:129,  7: 62,  8:123,  9: 65, 10: 77, 11: 19, 12: 82, 13:137, 14: 38, 15:149, 16:  8}
minuet_table[11] = { 1:  3,  2: 87,  3:165,  4: 61,  5:135,  6: 47,  7:147,  8: 33,  9:102, 10:  4, 11: 31, 12:164, 13:144, 14: 59, 15:173, 16: 78}
minuet_table[12] = { 1: 54,  2:130,  3: 10,  4:103,  5: 28,  6: 37,  7:106,  8:  5,  9: 35, 10: 20, 11:108, 12: 92, 13: 12, 14:124, 15: 44, 16:131}

def reverse_enumeration(L):
   for index in reversed(range(len(L))):
      yield index, L[index]
   
# These individually varied repetitions are the process used by Terry Riley's 
# "In C." Here, we randomly choose between 1 and 6 repetitions for each 
# measure of one complete circuit through a subset of the minuet table.
# NOTE: A "measure" is Mozart's measure to be played for N repetitions, a 
# "bar" is one measure played one time.

rows_to_play = 4
columns_to_play = 16
measures_to_play = rows_to_play * columns_to_play
repetitions_for_measures = []
for i in range(measures_to_play):
    repetitions_for_measures.append(1 + int(random.random() * 6.0))     
    
def read_measure(number):
    score_node = CsoundAC.ScoreNode()
    score_node.thisown = 0
    filename = 'M' + str(number) + '.mid'
    score_for_measure = score_node.getScore()
    score_for_measure.load(filename)
    # Remove false notes.
    for i, event in reverse_enumeration(score_for_measure):
        if event.getChannel() < 0:
            score_for_measure.remove(i)
    return score_node

tempo = 1.5

def build_track(voiceleading_node, sequence, instrument, bass, time_offset, pan):
    global repetitions_for_measures
    global tempo
    global off_time
    # We ensure that each track does a different sequence of repetitions, as 
    # in "In C."
    if len(repetitions_for_measures) > 0:
        random.shuffle(repetitions_for_measures)
    bars_total = sum(repetitions_for_measures)
    print("Instrument: {:3} measures: {} bars: {} repetitions_for_measures: {}".format(instrument, len(repetitions_for_measures), bars_total, repetitions_for_measures))    
    print()
    scale = CsoundAC.Scale("D major")
    chord = scale.chord(1, 3)
    bars_played = 0
    real_time = 1.0
    cumulative_time = real_time + time_offset
    bass = 48.
    bass_at_end = 34.
    bass_increment_per_bar = (bass_at_end - bass) / bars_total
    range_ = 12.
    range_at_end = 70.
    range_increment_per_bar = (range_at_end - range_) / bars_total
    piano = 60.
    piano_at_end = 50.
    piano_increment_per_bar = (piano_at_end - piano) / bars_total
    dynamic_range = 20.
    dynamic_range_at_end = 30.
    dynamic_range_increment_per_bar = (dynamic_range_at_end - dynamic_range) / bars_total
    # Mozart's minuet table has columns indexed [1,16] and rows indexed [2,12]. 
    repetitions_for_measure_index = 0
    # Preserve Mozart's indexing.
    for minuet_column in range(1, columns_to_play + 1):
        for minuet_row in range(2, rows_to_play + 2):
            repetitions_for_measure = repetitions_for_measures[repetitions_for_measure_index]
            repetitions_for_measure_index = repetitions_for_measure_index + 1
            scales = scale.modulations(chord)
            scale_count = len(scales)
            count = 0
            # When we pick a number of repetitions for a measure, we see if 
            # the current chord can be a pivot chord, and if so, we choose 
            # one of the possible modulations to perform.
            if (scale_count > 1):
                random_index = random.randint(0, scale_count -1)
                for s in scales:
                    print("Possible modulation at: {:9.4f} {} {}".format(cumulative_time, s.toString(), s.name()))
                    if count == random_index:
                        scale = s  
                        print("             Chose modulation to: {} {}".format(scale.toString(), scale.name()))
                    count = count + 1
                print(" ")
            for k in range(repetitions_for_measure):
                if time_offset == 0:
                    # Once the scale is chosen, we perform root progressions 
                    # within the scale; away from the tonic in multiples of -2
                    # scale degrees, back to the tonic in multiples of 1 scale 
                    # degree with a preference for 3 steps (as used by V to I). 
                    # These root progressions are weighted.
                    progression = random.choices([-2, -4, -6, -8, -10, -12, 3, 6], [10, 3, 2, 1, 1, 1, 8, 3], k=1)
                    steps = progression[0]
                    print("choice: {} steps: {}".format(progression, steps))
                    chord = scale.transpose_degrees(chord, steps)
                    print("{:9.4f} by {:4}: {}".format(cumulative_time, steps, chord.eOP().name()))
                    voiceleading_node.chord(chord, cumulative_time)
                measure = read_measure(minuet_table[minuet_row][minuet_column])
                score_for_measure = measure.getScore()
                duration = score_for_measure.getDuration() * tempo
                score_for_measure.setDuration(duration)
                rescale = CsoundAC.Rescale() 
                rescale.setRescale(CsoundAC.Event.TIME, bool(1), bool(0), cumulative_time, 0)
                rescale.setRescale(CsoundAC.Event.INSTRUMENT, bool(1), bool(1), instrument, 0)
                bass = bass + bass_increment_per_bar
                range_ = range_ + range_increment_per_bar
                rescale.setRescale(CsoundAC.Event.KEY, bool(1), bool(1), bass, range_)
                rescale.setRescale(CsoundAC.Event.PAN, bool(1), bool(0), float(pan), 0)
                piano = piano + piano_increment_per_bar
                dynamic_range = dynamic_range + dynamic_range_increment_per_bar
                rescale.setRescale(CsoundAC.Event.VELOCITY, bool(1), bool(1), piano, dynamic_range)
                rescale.thisown = 0
                rescale.addChild(measure)
                bars_played = bars_played + 1
                sequence.addChild(rescale)
                cumulative_time = cumulative_time + duration
                real_time = real_time + duration
    print("Bars played for instrument {}: {}".format(instrument, bars_played))
    print()

sequence = CsoundAC.Rescale()
voiceleading_node = CsoundAC.VoiceleadingNode()
voiceleading_node.addChild(sequence);
model.addChild(voiceleading_node)
sequence.setRescale(CsoundAC.Event.VELOCITY,   bool(1), bool(1), 60, 16)

timeoffset = (tempo * 6.0) / 4.0
build_track(voiceleading_node, sequence,  2, 34, timeoffset * 0.0, 1/7)
build_track(voiceleading_node, sequence, 56, 34, timeoffset * 1.0, 2/7)
build_track(voiceleading_node, sequence, 27, 34, timeoffset * 2.0, 3/7)
build_track(voiceleading_node, sequence, 29, 34, timeoffset * 4.0, 4/7)
build_track(voiceleading_node, sequence, 17, 34, timeoffset * 5.0, 5/7)
build_track(voiceleading_node, sequence,  1, 34, timeoffset * 3.0, 6/7)

orc = '''
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
connect "ReverbSC", "outleft", "MasterOutput", "inleft"
connect "ReverbSC", "outright", "MasterOutput", "inright"
connect "MVerb", "outleft", "MasterOutput", "inleft"
connect "MVerb", "outright", "MasterOutput", "inright"

gk_ChebyshevMelody_level init 5
gk_ZakianFlute_level init 8
gk_PianoOutPianoteq_level init -2
gk_FMWaterBell_level init 10
gk_PianoOutFluidsynth_level init 6
gk_Harpsichord_level init 11

gk_Reverb_wet init 0.25
gk_Reverb_feedback init 0.75
gi_Reverb_delay_modulation init 0.0875
gk_Reverb_frequency_cutoff init 18000
'''

model.setCsoundOrchestra(orc)
model.setCsoundCommand(csound_command)
model.generate()
# Fix ending. Instruments would drop out, we want them to sustain till the 
# end. Some of course will decay first.
score = model.getScore()
score_duration = score.getDuration() + 6.
sounding = set()
for i, event in reverse_enumeration(score):
    event.setOffTime(score_duration)
    instrument = str(event.getInstrument())
    sounding.add(instrument)
    if len(sounding) == 6:
        break
score.save(model.getMidifileFilepath())
model.performMaster()
if rendering == 'soundfile':
    model.translateMaster()
