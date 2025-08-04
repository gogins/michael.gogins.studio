'''
PORPHYRY

Version 7.4.2 

Copyright (C) 2021 by Michael Gogins

Mozart's musical dice game of 1787 is taken apart and put back together along 
the lines of Terry Riley's "In C" using Python, re-harmonized using the 
CsoundAC.Scale class, and rendered with a built-in Csound orchestra that 
integrates the Organteq physically modeled pipe organ with a waveguide reverb.

Comments in the code are provided in an attempt to clarify what is going on.

The source code for the chord and scale classes used here (inspired by the 
music theory of Dmitri Tymoczko), can be found at:

https://github.com/gogins/csound-extended/blob/develop/CsoundAC/ChordSpace.hpp

'''
print(__doc__)
import CsoundAC
import math
import os
import random
import signal
import string
import sys
import traceback

print('Set "rendering" to:     "soundfile" or "audio".')
print
rendering = "audio"

# Using the same random seed for each performance makes the performance 
# deterministic, not random.
random.seed(221)
random.seed(45850)
random.seed(222)
random.seed(11171111)
random.seed(11116911)
random.seed(11118111)
random.seed(11117112)
random.seed(41)
random.seed(41571)
random.seed(4171)
random.seed(4171317)
random.seed(11116911)
random.seed(13313115)
random.seed(11117111) # Champ so far.

scale = CsoundAC.Scale("F# major")
#scale = CsoundAC.Scale("G# major")
chord = scale.chord(1, 4)
#chord = scale.chord(4, 4)

bass_offset = 18
column_begin = 1
master_measure_duration = 1.8
offset_factor = 2
row_begin = 2
bass_offset = 13
columns_to_play = 15
rows_to_play = 2
measures_to_play = rows_to_play * columns_to_play
minimum_repetitions_per_measure =  3
maximum_repetitions_per_measure = 11 

target_duration = 8.2 * 60.
cutoff = (7. * 60) + 15.
cutoff = math.ceil(10. * cutoff) / 10.

model = CsoundAC.MusicModel()
score = model.getScore()

script_filename = sys.argv[0]
print('Full Python script:     %s' % script_filename)
title, exte = os.path.splitext(os.path.basename(script_filename))
model.setTitle(title)
model.setArtist("Michael Gogins")
model.setAuthor("Michael Gogins")
model.setYear("2022")
model.generateAllNames()
soundfile_name = model.getOutputSoundfileFilepath()
print('Soundfile name:         %s' % soundfile_name)
dac_name = 'dac'
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

# This is Mozart's original minuet table for his musical dice game; his trio 
# table is not used. Mozart's idiosyncratic (by our standards) indexing scheme 
# is preserved. Rather than pick measures by throwing dice according to 
# Mozart's original instructions, each voice in this piece plays each measure 
# in each column on each row in turn, but for a randomly chosen number of 
# repetitions in each voice, as in Terry Riley's "In C."

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
   
# These individually varied repetitions implement the process used by Terry 
# Riley's "In C:" In each voice, randomly choose between 1 and N repetitions 
# for each measure of one complete circuit through a selected subset of the 
# minuet table. The number of repetitions of each measure will often differ 
# between the voices, and this creates many differential canons between the 
# voices.
# 
# NOTE: A "measure" is Mozart's measure to be played for N repetitions, a 
# "bar" is one measure played one time.

repetitions_for_measures = []
for i in range(measures_to_play):
    repetitions_for_measures.append(random.randint(minimum_repetitions_per_measure, maximum_repetitions_per_measure))     
    
def time_scale(measure, duration, scale):
    score = measure.getScore()
    measure_duration = score.getDuration()
    measure_duration = measure_duration * scale
    score.setDuration(measure_duration)
    return duration * scale
    
def pitch_scale(measure, scale):
    score = measure.getScore()
    for i, event in enumeration(score):
        key = event.getKey()
        key = key * scale
        key = CsoundAC.Conversions_temper(key, 12.)
        event.setKey(key)
        event.temper(12.)
    
def read_measure(number):
    if number == 141:
        number = 14
    score_node = CsoundAC.ScoreNode()
    score_node.thisown = 0
    filename = 'M' + str(number) + '.mid'
    score_for_measure = score_node.getScore()
    score_for_measure.load(filename)
    # Remove false notes.
    for i, event in reverse_enumeration(score_for_measure):
        if event.getChannel() < 0:
            score_for_measure.remove(i)
    return score_node, filename

forte_measures = random.choices([1,0], [2/6, 4/6], k=measures_to_play)

def build_voice(voiceleading_node, sequence, instrument, bass_, time_offset, pan, level):
    global master_measure_duration
    global repetitions_for_measures
    global tempo
    global off_time
    global chord
    global scale
    global sequence_holder
    # Ensure that each voice plays a different sequence of repetitions, as in 
    # "In C". But note that shuffling, rather than resampling, ensures 
    # that each voice actually plays the same number of bars.
    random.shuffle(repetitions_for_measures)
    random.shuffle(forte_measures)
    bars_total = sum(repetitions_for_measures)
    print("Instrument: {:3} measures: {} bars: {} repetitions_for_measures: {}".format(instrument, len(repetitions_for_measures), bars_total, repetitions_for_measures))    
    print("Instrument: {:3} measures: {} bars: {} forte_mearues:            {}".format(instrument, len(repetitions_for_measures), bars_total, forte_measures))    
    bass = bass_
    modulation_count = 0
    bars_played = 0
    real_time = 1.8
    print()
    for total_iterations in range(2):
        cumulative_time = real_time + time_offset
        # Make both pitch range and dynamic range get bigger through time.
        bass_at_end = bass - 5
        bass_increment_per_bar = (bass_at_end - bass) / bars_total
        range_ = 48.
        range_at_end = 52.
        range_increment_per_bar = (range_at_end - range_) / bars_total
        piano = 60. + level
        piano_at_end = piano - 4.
        piano_increment_per_bar = (piano_at_end - piano) / bars_total
        dynamic_range = 20.
        dynamic_range_at_end = 30.
        dynamic_range_increment_per_bar = (dynamic_range_at_end - dynamic_range) / bars_total
        bass = random.choices([24, 30, 36], [8, 6, 4], k=1)[0]
        #~ range_ = random.choices([60, 45, 48, 30, 24], [12, 10, 8, 6, 5], k=1)[0]
        range_ = random.choices([48, 30, 24], [12, 10, 8], k=1)[0]
        duration = master_measure_duration
        timescale = 1.
        measure_count = 0
        # Mozart's minuet table has columns indexed [1,16] and rows indexed [2,12]. 
        repetitions_for_measure_index = 0
        # Preserve Mozart's indexing.
        for minuet_column in range(column_begin, columns_to_play + column_begin):
            for minuet_row in range(row_begin, rows_to_play + row_begin):
                measure_count = measure_count + 1
                repetitions_for_measure = repetitions_for_measures[repetitions_for_measure_index]
                forte = forte_measures[repetitions_for_measure]
                repetitions_for_measure_index = repetitions_for_measure_index + 1
                scales = scale.modulations_for_voices(chord, 4)
                scale_count = len(scales)
                count = 0
                # After picking a number of repetitions for a measure, check if the 
                # current chord can be a pivot chord, and if so, choose one of the 
                # possible modulations to perform. Do this in the first voice 
                # only, but it will be applied to all voices.
                if (scale_count > 1 and time_offset == 0):
                    modulation_count = modulation_count + 1
                    print("Modulation point:       {:4d}".format(modulation_count))
                    print("Time:                   {:9.4f}".format(cumulative_time))
                    print("Current key:            {} {}".format(scale.toString(), scale.name()))
                    print("Pivot chord:            {}                                        {}".format(chord.toString(), chord.eOP().name()))
                    # Every few modulations, use a different combination of stops.
                    combination = 0
                    if modulation_count == 5:
                        combination = 1.
                    if modulation_count == 10:
                        combination = 2.
                    if modulation_count == 14:
                        combination = 3.
                    if modulation_count == 19:
                        combination = 4.
                    if modulation_count == 21:
                        combination = 1.
                    if modulation_count == 26:
                        combination = 3.
                    if modulation_count == 31:
                        combination = 2.
                    if modulation_count == 36:
                        combination = 3.
                    if modulation_count == 41:
                        combination = 2.
                    for i in range(len(scales)):
                        s = scales[i]
                        print("Possible modulation to: {} {}".format(s.toString(), s.name()))
                    scale = random.choice(scales)
                    if combination != 0:
                        sequence_holder.getScore().append(cumulative_time, 1., 144., 5., combination, combination, 1.)
                        print("==> Combination:           {}".format(combination))
                    print("==> Modulated to:       {} {}".format(scale.toString(), scale.name()))
                    timescale = random.choices([1., 2., .5, 2./3., 4./3.], [12, 2, 2, 2, 2], k=1)[0]
                    print()
                bass = random.choices([24, 30, 36], [8, 6, 4], k=1)[0] + bass_offset
                # range_ = random.choices([48, 42, 36, 30, 24], [12, 10, 8, 6, 5], k=1)[0]
                range_ = random.choices([48, 36, 24], [12, 10, 8, ], k=1)[0]
                for k in range(repetitions_for_measure):
                    if (time_offset == 0) and (k % 3 == 0):
                        # Once the scale is chosen, perform root progressions 
                        # within the scale; away from the tonic in multiples of -2
                        # scale degrees, back to the tonic in multiples of 1 scale 
                        # degree with a preference for 3 steps (as used by V to I). 
                        # These root progressions are random but weighted.
                        progression = random.choices([-2, -4, -6, -8, -10, -12, 3, 6], [5, 3, 2, 1, 1, 1, 9, 3], k=1)
                        steps = progression[0]
                        chord = scale.transpose_degrees(chord, steps)
                        voiceleading_node.chord(chord, cumulative_time)
                    measure, filename = read_measure(minuet_table[minuet_row][minuet_column])
                    score_for_measure = measure.getScore()
                    score_duration = duration # math.ceil(10. * score_for_measure.getDurationFromZero()) / 10.
                    #~ print("Loaded '%s' at %9.4f (%9.4f):\n%s" % (filename, real_time, score_duration, score_for_measure.toString()))
                    #~ if len(score_for_measure) < 1:
                        #~ print("******* NO NOTES IN MEASURE!")
                    rescale = CsoundAC.Rescale() 
                    rescale.setRescale(CsoundAC.Event.TIME, True, False, cumulative_time, 0)
                    rescale.setRescale(CsoundAC.Event.INSTRUMENT, True, True, instrument, 0)
                    rescale.setRescale(CsoundAC.Event.KEY, True, True, bass, range_)
                    piano = piano + piano_increment_per_bar
                    dynamic_range = dynamic_range + dynamic_range_increment_per_bar
                    rescale.setRescale(CsoundAC.Event.VELOCITY, True, True, piano + (forte * 4), dynamic_range)
                    rescale.setRescale(CsoundAC.Event.PAN, True, True, pan, float(0))
                    rescale.thisown = 0
                    rescale.addChild(measure)
                    bars_played = bars_played + 1
                    sequence.addChild(rescale)
                    cumulative_time = cumulative_time + score_duration
                    real_time = real_time + score_duration
                    if real_time > target_duration:
                        break
                if real_time > target_duration:
                    break
            if real_time > target_duration:
                break
        if real_time > target_duration:
            break
    print("Bars played for instrument {}: {}".format(instrument, bars_played))
    print()
sequence_holder = CsoundAC.ScoreNode()
sequence = CsoundAC.Rescale()
sequence_holder.addChild(sequence)
sequence.setRescale(CsoundAC.Event.VELOCITY, True, True, 60., 12.)
sequence.setRescale(CsoundAC.Event.KEY, True, True, 30., 72.)
# The actual harmony is applied after the notes for all voices have been 
# generated.
voiceleading_node = CsoundAC.VoiceleadingNode()
voiceleading_node.addChild(sequence_holder);
model.addChild(voiceleading_node)

instruments_used = 0
total_instruments = 4
# Stagger starting times for each voice to create a canon at the very 
# beginning.
time_offset = (master_measure_duration * offset_factor)
print("time_offset:", time_offset)
# Make it possible to experimentally shrink or enlarge the arrangement.
instruments_used = instruments_used + 1
build_voice(voiceleading_node, sequence, 1, bass_offset +  0, time_offset * (instruments_used - 1), (instruments_used / (total_instruments + 1)),  9)
instruments_used = instruments_used + 1
build_voice(voiceleading_node, sequence, 2, bass_offset +  2, time_offset * (instruments_used - 1), (instruments_used / (total_instruments + 1)),  0)
instruments_used = instruments_used + 1
build_voice(voiceleading_node, sequence, 3, bass_offset +  4, time_offset * (instruments_used - 1), (instruments_used / (total_instruments + 1)),  6)
instruments_used = instruments_used + 1
build_voice(voiceleading_node, sequence, 4, bass_offset +  6, time_offset * (instruments_used - 1), (instruments_used / (total_instruments + 1)),  6)
instruments_used = instruments_used + 1
build_voice(voiceleading_node, sequence, 3, bass_offset +  4, time_offset * (instruments_used - 1), (instruments_used / (total_instruments + 1)), 12)
instruments_used = instruments_used + 1
build_voice(voiceleading_node, sequence, 4, bass_offset +  6, time_offset * (instruments_used - 1), (instruments_used / (total_instruments + 1)), 12)

# No #includes are used here, all Csound instruments are defined in this very file.
# The only non-standard external dependencies are the vst4cs opcodes, and the 
# Pianoteq physically modeled piano VST instrument plugin. These could easily be 
# replaced with a Fluidsynth sampled piano soundfont.

orc = '''
sr = 48000
ksmps = 128
nchnls = 2
0dbfs = 1  

; Ensure the same random stream for each rendering.
; rand, randh, randi, rnd(x) and birnd(x) are not affected by seed.

seed 29384 ;38493

gi_MverbVst vst3init "/Library/Audio/Plug-Ins/VST3/Mverb2020.vst3", "Mverb2020", 1
gi_Organteq vst3init "/Library/Audio/Plug-Ins/VST3/Organteq\ 2.vst3", "Organteq 2", 1
gi_Pianoteq vst3init "/Library/Audio/Plug-Ins/VST3/Pianoteq\ 8.vst3", "Pianoteq 8", 1

alwayson "OrganOutOrganteq"
alwayson "MverbVst"
alwayson "MasterOutput"

connect "OrganOutOrganteq", "outleft", "MverbVst", "inleft"
connect "OrganOutOrganteq", "outright", "MverbVst", "inright"
connect "MverbVst", "outleft", "MasterOutput", "inleft"
connect "MverbVst", "outright", "MasterOutput", "inright"

gk_OrganNoteOrganteq_midi_dynamic_range init 127
instr Pedale, Positif, Grand_Orgue, Recit
if p3 == -1 then
  p3 = 1000000
endif
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_OrganNoteOrganteq_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_instrument = p1 - 1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
i_homogeneity = p11
instances active p1
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
i_pitch_correction = 44100 / sr
; prints "Pitch factor:   %9.4f\\n", i_pitch_correction
id vst3note gi_Organteq, i_instrument, i_midi_key, i_midi_velocity, i_duration
endin

instr OrganteqCombination
prints "\\n****************************************************************************************\\n\\n"
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
prints "\\n****************************************************************************************\\n\\n"

; NOTE: In Organteq 2, the stop buttons are numbered starting with 109, not 
; with 33 as in Organteq 1.

i_combination = p5
if (i_combination == 0.) then

; Keyboard 1 -- Pedale

vst3paramset gi_Organteq, 109, 1 ;  1
vst3paramset gi_Organteq, 110, 0 ;  2 
vst3paramset gi_Organteq, 111, 0 ;  3
vst3paramset gi_Organteq, 112, 0 ;  4
vst3paramset gi_Organteq, 113, 0 ;  5
vst3paramset gi_Organteq, 114, 0 ;  6
vst3paramset gi_Organteq, 115, 1 ;  7
vst3paramset gi_Organteq, 116, 0 ;  8
vst3paramset gi_Organteq, 117, 0 ;  9
vst3paramset gi_Organteq, 118, 0 ; 10

; Keyboard 2 -- Positif

vst3paramset gi_Organteq, 119, 0 ;  1
vst3paramset gi_Organteq, 120, 0 ;  2
vst3paramset gi_Organteq, 121, 1 ;  3
vst3paramset gi_Organteq, 122, 0 ;  4
vst3paramset gi_Organteq, 123, 0 ;  5
vst3paramset gi_Organteq, 124, 1 ;  6
vst3paramset gi_Organteq, 125, 0 ;  7
vst3paramset gi_Organteq, 126, 0 ;  8
vst3paramset gi_Organteq, 127, 0 ;  9
vst3paramset gi_Organteq, 128, 0 ; 10

; Keyboard 3 -- Grand Orgue

vst3paramset gi_Organteq, 129, 0 ;  1
vst3paramset gi_Organteq, 130, 0 ;  2
vst3paramset gi_Organteq, 131, 0 ;  3
vst3paramset gi_Organteq, 132, 0 ;  4
vst3paramset gi_Organteq, 133, 1 ;  5
vst3paramset gi_Organteq, 134, 0 ;  6
vst3paramset gi_Organteq, 135, 0 ;  7
vst3paramset gi_Organteq, 136, 0 ;  8
vst3paramset gi_Organteq, 137, 0 ;  9
vst3paramset gi_Organteq, 138, 0 ; 10

; Keyboard 4 - Recit 

vst3paramset gi_Organteq, 139, 0 ;  1
vst3paramset gi_Organteq, 140, 0 ;  2
vst3paramset gi_Organteq, 141, 0 ;  3
vst3paramset gi_Organteq, 142, 0 ;  4
vst3paramset gi_Organteq, 143, 0 ;  5
vst3paramset gi_Organteq, 144, 0 ;  6
vst3paramset gi_Organteq, 145, 0 ;  7 
vst3paramset gi_Organteq, 146, 1 ;  8 
vst3paramset gi_Organteq, 147, 0 ;  9 
vst3paramset gi_Organteq, 148, 0 ; 10

endif
if (i_combination == 1.) then

; Keyboard 1 -- Pedale

vst3paramset gi_Organteq, 109, 1 ;  1
vst3paramset gi_Organteq, 110, 0 ;  2 
vst3paramset gi_Organteq, 111, 0 ;  3
vst3paramset gi_Organteq, 112, 0 ;  4
vst3paramset gi_Organteq, 113, 0 ;  5
vst3paramset gi_Organteq, 114, 0 ;  6
vst3paramset gi_Organteq, 115, 0 ;  7
vst3paramset gi_Organteq, 116, 0 ;  8
vst3paramset gi_Organteq, 117, 1 ;  9
vst3paramset gi_Organteq, 118, 1 ; 10

; Keyboard 2 -- Positif

vst3paramset gi_Organteq, 119, 0 ;  1
vst3paramset gi_Organteq, 120, 0 ;  2
vst3paramset gi_Organteq, 121, 1 ;  3
vst3paramset gi_Organteq, 122, 0 ;  4
vst3paramset gi_Organteq, 123, 0 ;  5
vst3paramset gi_Organteq, 124, 1 ;  6
vst3paramset gi_Organteq, 125, 0 ;  7
vst3paramset gi_Organteq, 126, 0 ;  8
vst3paramset gi_Organteq, 127, 0 ;  9
vst3paramset gi_Organteq, 128, 0 ; 10

; Keyboard 3 -- Grand Orgue

vst3paramset gi_Organteq, 129, 0 ;  1
vst3paramset gi_Organteq, 130, 0 ;  2
vst3paramset gi_Organteq, 131, 1 ;  3
vst3paramset gi_Organteq, 132, 1 ;  4
vst3paramset gi_Organteq, 133, 1 ;  5
vst3paramset gi_Organteq, 134, 0 ;  6
vst3paramset gi_Organteq, 135, 0 ;  7
vst3paramset gi_Organteq, 136, 0 ;  8
vst3paramset gi_Organteq, 137, 0 ;  9
vst3paramset gi_Organteq, 138, 0 ; 10

; Keyboard 4 - Recit 

vst3paramset gi_Organteq, 139, 0 ;  1
vst3paramset gi_Organteq, 140, 0 ;  2
vst3paramset gi_Organteq, 141, 0 ;  3
vst3paramset gi_Organteq, 142, 0 ;  4
vst3paramset gi_Organteq, 143, 0 ;  5
vst3paramset gi_Organteq, 144, 0 ;  6
vst3paramset gi_Organteq, 145, 0 ;  7 
vst3paramset gi_Organteq, 146, 0 ;  8 
vst3paramset gi_Organteq, 147, 1 ;  9 
vst3paramset gi_Organteq, 148, 0 ; 10

endif
if (i_combination == 2.) then

; Keyboard 1 -- Pedale

vst3paramset gi_Organteq, 109, 0 ;  1
vst3paramset gi_Organteq, 110, 0 ;  2 
vst3paramset gi_Organteq, 111, 0 ;  3
vst3paramset gi_Organteq, 112, 0 ;  4
vst3paramset gi_Organteq, 113, 1 ;  5
vst3paramset gi_Organteq, 114, 0 ;  6
vst3paramset gi_Organteq, 115, 1 ;  7
vst3paramset gi_Organteq, 116, 0 ;  8
vst3paramset gi_Organteq, 117, 1 ;  9
vst3paramset gi_Organteq, 118, 0 ; 10

; Keyboard 2 -- Positif

vst3paramset gi_Organteq, 119, 1 ;  1
vst3paramset gi_Organteq, 120, 0 ;  2
vst3paramset gi_Organteq, 121, 1 ;  3
vst3paramset gi_Organteq, 122, 1 ;  4
vst3paramset gi_Organteq, 123, 1 ;  5
vst3paramset gi_Organteq, 124, 0 ;  6
vst3paramset gi_Organteq, 125, 0 ;  7
vst3paramset gi_Organteq, 126, 0 ;  8
vst3paramset gi_Organteq, 127, 0 ;  9
vst3paramset gi_Organteq, 128, 0 ; 10

; Keyboard 3 -- Grand Orgue

vst3paramset gi_Organteq, 129, 1 ;  1
vst3paramset gi_Organteq, 130, 0 ;  2
vst3paramset gi_Organteq, 131, 0 ;  3
vst3paramset gi_Organteq, 132, 0 ;  4
vst3paramset gi_Organteq, 133, 1 ;  5
vst3paramset gi_Organteq, 134, 0 ;  6
vst3paramset gi_Organteq, 135, 1 ;  7
vst3paramset gi_Organteq, 136, 0 ;  8
vst3paramset gi_Organteq, 137, 0 ;  9
vst3paramset gi_Organteq, 138, 0 ; 10

; Keyboard 4 - Recit 

vst3paramset gi_Organteq, 139, 0 ;  1
vst3paramset gi_Organteq, 140, 1 ;  2
vst3paramset gi_Organteq, 141, 0 ;  3
vst3paramset gi_Organteq, 142, 0 ;  4
vst3paramset gi_Organteq, 143, 1 ;  5
vst3paramset gi_Organteq, 144, 0 ;  6
vst3paramset gi_Organteq, 145, 0 ;  7 
vst3paramset gi_Organteq, 146, 0 ;  8 
vst3paramset gi_Organteq, 147, 1 ;  9 
vst3paramset gi_Organteq, 148, 0 ; 10

endif
if (i_combination == 3.) then
; Keyboard 1 -- Pedale

vst3paramset gi_Organteq, 109, 1 ;  1
vst3paramset gi_Organteq, 110, 1 ;  2 
vst3paramset gi_Organteq, 111, 1 ;  3
vst3paramset gi_Organteq, 112, 1 ;  4
vst3paramset gi_Organteq, 113, 1 ;  5
vst3paramset gi_Organteq, 114, 1 ;  6
vst3paramset gi_Organteq, 115, 0 ;  7
vst3paramset gi_Organteq, 116, 1 ;  8
vst3paramset gi_Organteq, 117, 0 ;  9
vst3paramset gi_Organteq, 18, 0 ; 10

; Keyboard 2 -- Positif

vst3paramset gi_Organteq, 119, 1 ;  1
vst3paramset gi_Organteq, 120, 1 ;  2
vst3paramset gi_Organteq, 121, 1 ;  3
vst3paramset gi_Organteq, 122, 0 ;  4
vst3paramset gi_Organteq, 123, 1 ;  5
vst3paramset gi_Organteq, 124, 0 ;  6
vst3paramset gi_Organteq, 125, 0 ;  7
vst3paramset gi_Organteq, 126, 1 ;  8
vst3paramset gi_Organteq, 127, 0 ;  9
vst3paramset gi_Organteq, 128, 0 ; 10

; Keyboard 3 -- Grand Orgue

vst3paramset gi_Organteq, 129, 1 ;  1
vst3paramset gi_Organteq, 130, 1 ;  2
vst3paramset gi_Organteq, 131, 0 ;  3
vst3paramset gi_Organteq, 132, 0 ;  4
vst3paramset gi_Organteq, 133, 1 ;  5
vst3paramset gi_Organteq, 134, 1 ;  6
vst3paramset gi_Organteq, 135, 1 ;  7
vst3paramset gi_Organteq, 136, 1 ;  8
vst3paramset gi_Organteq, 137, 1 ;  9
vst3paramset gi_Organteq, 138, 0 ; 10

; Keyboard 4 - Recit 

vst3paramset gi_Organteq, 139, 1 ;  1
vst3paramset gi_Organteq, 140, 0 ;  2
vst3paramset gi_Organteq, 141, 0 ;  3
vst3paramset gi_Organteq, 142, 1 ;  4
vst3paramset gi_Organteq, 143, 1 ;  5
vst3paramset gi_Organteq, 144, 0 ;  6
vst3paramset gi_Organteq, 145, 0 ;  7 
vst3paramset gi_Organteq, 146, 0 ;  8 
vst3paramset gi_Organteq, 147, 1 ;  9 
vst3paramset gi_Organteq, 148, 1 ; 10

endif
if (i_combination == 4.) then
; Keyboard 1 -- Pedale

vst3paramset gi_Organteq, 109, 1 ;  1
vst3paramset gi_Organteq, 110, 1 ;  2 
vst3paramset gi_Organteq, 111, 0 ;  3
vst3paramset gi_Organteq, 112, 0 ;  4
vst3paramset gi_Organteq, 113, 0 ;  5
vst3paramset gi_Organteq, 114, 0 ;  6
vst3paramset gi_Organteq, 115, 1 ;  7
vst3paramset gi_Organteq, 116, 0 ;  8
vst3paramset gi_Organteq, 117, 0 ;  9
vst3paramset gi_Organteq, 118, 0 ; 10

; Keyboard 2 -- Positif

vst3paramset gi_Organteq, 119, 0 ;  1
vst3paramset gi_Organteq, 120, 0 ;  2
vst3paramset gi_Organteq, 121, 1 ;  3
vst3paramset gi_Organteq, 122, 0 ;  4
vst3paramset gi_Organteq, 123, 0 ;  5
vst3paramset gi_Organteq, 124, 0 ;  6
vst3paramset gi_Organteq, 125, 1 ;  7
vst3paramset gi_Organteq, 126, 0 ;  8
vst3paramset gi_Organteq, 127, 0 ;  9
vst3paramset gi_Organteq, 128, 0 ; 10

; Keyboard 3 -- Grand Orgue

vst3paramset gi_Organteq, 129, 0 ;  1
vst3paramset gi_Organteq, 130, 0 ;  2
vst3paramset gi_Organteq, 131, 0 ;  3
vst3paramset gi_Organteq, 132, 0 ;  4
vst3paramset gi_Organteq, 133, 1 ;  5
vst3paramset gi_Organteq, 134, 0 ;  6
vst3paramset gi_Organteq, 135, 1 ;  7
vst3paramset gi_Organteq, 136, 0 ;  8
vst3paramset gi_Organteq, 137, 0 ;  9
vst3paramset gi_Organteq, 138, 0 ; 10

; Keyboard 4 - Recit 

vst3paramset gi_Organteq, 139, 1 ;  1
vst3paramset gi_Organteq, 140, 0 ;  2
vst3paramset gi_Organteq, 141, 0 ;  3
vst3paramset gi_Organteq, 142, 0 ;  4
vst3paramset gi_Organteq, 143, 1 ;  5
vst3paramset gi_Organteq, 144, 0 ;  6
vst3paramset gi_Organteq, 145, 0 ;  7 
vst3paramset gi_Organteq, 146, 0 ;  8 
vst3paramset gi_Organteq, 147, 0 ;  9 
vst3paramset gi_Organteq, 148, 0 ; 10

endif
endin

gk_OrganOutOrganteq_level init 0
gi_OrganOutOrganteq_print init 1
gk_OrganOutOrganteq_front_to_back init 0
gk_OrganOutOrganteq_left_to_right init 0.5
gk_OrganOutOrganteq_bottom_to_top init 0
instr OrganOutOrganteq
; Internal reverb off.
vst3paramset gi_Organteq, 8, 0

; Uncomment a line to evaluate stops starting from that particular time.

//scoreline_i "a 0 .5 45"
//scoreline_i "a 0 .5 106.2"
//scoreline_i "a 0 .5 152"
//scoreline_i "a 0 .5 226"
//scoreline_i "a 0 .5 270"
//scoreline_i "a 0 .5 318.6"
//scoreline_i "a 0 .5 370.8"
//scoreline_i "a 0 .5 437.4"

k_gain = ampdb(gk_OrganOutOrganteq_level)
i_overall_amps = 100
i_normalization = ampdb(-i_overall_amps) * 2
i_amplitude = ampdb(80) * i_normalization
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
ainleft init 0
ainright init 0
a_out_left, a_out_right vst3audio gi_Organteq
printks "OrganOutPt     L %9.4f R %9.4f l %9.4f\\n", 0.5, a_out_left, a_out_right, gk_OrganOutOrganteq_level
outleta "outleft", a_out_left
outleta "outright", a_out_right
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
prints "Loading preset...\\n"
vst3presetload gi_Organteq, "/Users/michaelgogins/Library/Application Support/Modartt/Organteq/Presets/My Presets/Porphyry-1.fxp"
endin

gk_MverbVst_level init 0
gk_MverbVst_Mix init .5
gk_MverbVst_Pre_delay init 0.25
gk_MverbVst_Early_late_mix init 0.25
gk_MverbVst_Size init 0.5
gk_MverbVst_Density init 0.5
gk_MverbVst_Bandwith_Frequency init 0.5
gk_MverbVst_Decay init 0.55
gk_MverbVst_Damping_Frequency init 0.5
gk_MverbVst_Gain init 1
gi_MverbVst_Program init 4
instr MverbVst
k_gain = ampdb(gk_MverbVst_level)
ainleft inleta "inleft"
ainright inleta "inright"
aoutleft, aoutright vst3audio gi_MverbVst, ainleft, ainright
outleta "outleft", aoutleft
outleta "outright", aoutright
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
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
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

; It is important for levels to be evenly balanced _on average_! This 
; enables instruments to come forward and recede according to their 
; response to MIDI velocity.

gk_PianoOutPianoteq_level init  -1 
gk_ZakianFlute_level init       19
gk_FMWaterBell_level init       24;16
gk_ChebyshevMelody_level init   28;19
gk_Harpsichord_level init       11
gk_Rhodes_level init            31

gk_Reverb_wet init 0.25
gk_Reverb_feedback init 0.85
gi_Reverb_delay_modulation init 0.0875
gk_Reverb_frequency_cutoff init 14000
'''

model.setCsoundOrchestra(orc)
model.setCsoundCommand(csound_command)
model.generate()
score = model.getScore()
extend_from = cutoff - 1.8
extend_to = cutoff + (1.8 * 1.)
for i in range(len(score) - 1, 0, -1):
    if score[i].getOffTime() >= cutoff:
        score.remove(i)
# Extend all notes from 7'46" - 1.8" to 7'46" + 1.8".
for i in range(len(score) - 1, 0, -1):
    note = score[i]
    if note.getOffTime() >= extend_from:
        note.setOffTime(extend_to)
        score[i] = note
score.save(model.getMidifileFilepath())
model.setExtendSeconds(9.)
model.performMaster()
if rendering == 'soundfile':
    model.translateMaster()
