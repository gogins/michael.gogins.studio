'''
Copyright (C) 2005 by Michael Gogins.
All rights reserved.
'''
print(__doc__)
print
print('IMPORTING REQUIRED MODULES...')
print
import CsoundAC
import os
import random
random.seed(839)
import signal
import string
import sys
import traceback

print('CREATING FILENAMES...')
print
scriptFilename = sys.argv[0]
print('Full Python script:     %s' % scriptFilename)
title, exte = os.path.splitext(os.path.basename(scriptFilename))
print('Base Python script:     %s' % title)
directory = os.path.dirname(scriptFilename)
if len(directory):
    print('Working directory:      %s' % directory)
    os.chdir(directory)
print('Working directory:      %s' % directory)
orcFilename = title + '.orc'
midiFilename = title + '.mid'
print('MIDI filename:          %s' % midiFilename)
soundfileName = title + '.wav'
print('Soundfile name:         %s' % soundfileName)
dacName = 'dac:plughw:1,0'
print('Audio output name:      %s' % dacName)
print

print('SETTING RENDERING AND PLAYBACK OPTIONS...')
print
print('Set "rendering" to:     "soundfile" or "audio".')
print('Set "playback" to:      True (default) or False.')
print
rendering = 'audio'
playback = True
print('Rendering option:       %s' % rendering)
print('Play after rendering:   %s' % playback)
commandsForRendering = {
    'soundfile':    'csound -r 96000 -k 100 -m195 -+msg_color=0 -RWZdfo %s' % (soundfileName),
    'audio':        'csound -r 48000 -k 128 -m0   -+msg_color=0 -o%s' % (dacName),
}
csoundCommand = commandsForRendering[rendering]
print('Csound command line:    %s' % csoundCommand)
print

print('CREATING GLOBAL OBJECTS...')
print
model = CsoundAC.MusicModel()

print('RANDOM SEED...')
print
random.Random(29389)

print('CREATING MUSIC MODEL...')
print

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

random.seed()

duration = 1.25
repetitions = []
for i in xrange(16 * 4):
    repetitions.append(1 + int(random.random() * 6.0))     

for i in xrange(6):
    random.shuffle(repetitions)
    print repetitions
    
def reverse_enumeration(L):
   for index in reversed(xrange(len(L))):
      yield index, L[index]    

def readMeasure(number):
    scoreNode = CsoundAC.ScoreNode()
    score_ = scoreNode.getScore()
    scoreNode.thisown = 0
    filename = 'M' + str(number) + '.mid'
    print 'Reading "%s"' % (filename)
    score_.load(filename)
    # Remove false notes.
    for i, event in reverse_enumeration(score_):
        if event.getChannel() < 0:
            score_.remove(i)
    print(score_.getCsoundScore())
    return scoreNode

t2  = lambda chord : chord.T(2).eOP()
t5  = lambda chord : chord.T(5).eOP()
t7  = lambda chord : chord.T(7).eOP()
t9  = lambda chord : chord.T(9).eOP()
t10 = lambda chord : chord.T(10).eOP()
k0  = lambda chord : chord.K().eOP()
k2  = lambda chord : chord.K().eOP().T(2).eOP()
k5  = lambda chord : chord.K().eOP().T(5).eOP()
k7  = lambda chord : chord.K().eOP().T(7).eOP()
k10 = lambda chord : chord.K().eOP().T(10).eOP()

chord = CsoundAC.chordForName("Db9")
print("Chord:\n{}".format(chord.information()))
print("k0:\n{}".format(k0(chord).information()))

def buildTrack(voiceleadingNode, sequence, channel, gain, timeoffset, pan, add=0.0):
    global chord
    print 'Building track for channel %3d gain %3d...' % (channel, gain)
    cumulativeTime = timeoffset
    random.shuffle(repetitions)
    m = 0
    for i in xrange(0, 16):
        for j in xrange(2, 6):
            if True:
                transformation = random.choice([t2, t5, t7, t9, t10, k0, k2, k5, k7, k10]) #, weights=[1, 5, 2, 1, 1, 3, 3, 1, 1, 1])
                #transformation = random.choice([t2, t5, t7, t9, t10, k0])
                chord = transformation(chord)
                voiceleadingNode.chord(chord, cumulativeTime)
            repeatCount = repetitions[m]
            m = m + 1
            for k in xrange(repeatCount):
                measure = readMeasure(minuetTable[j][i+1])
                rescale = CsoundAC.Rescale()
                rescale.setRescale(CsoundAC.Event.TIME,         bool(1), bool(0), cumulativeTime, 0)
                rescale.setRescale(CsoundAC.Event.INSTRUMENT,   bool(1), bool(1), channel, 0)
                rescale.setRescale(CsoundAC.Event.VELOCITY,     bool(1), bool(0), gain, 0)
                rescale.setRescale(CsoundAC.Event.PAN,          bool(1), bool(1), pan, 0)
                rescale.setRescale(CsoundAC.Event.KEY,          bool(1), bool(0), add, 0)
                rescale.thisown = 0
                rescale.addChild(measure)
                print 'Repeat %4d of %4d at %8.3f with %3d notes of duration %7.3f...' %(k + 1, repeatCount, cumulativeTime, len(measure.getScore()), duration)
                sequence.addChild(rescale)
                cumulativeTime = cumulativeTime + duration

print 'Generating score...'
filename = 'track2'
csound.setFilename(filename)
model.setConformPitches(True)
voiceleadingNode = CsoundAC.VoiceleadingNode()
model.addChild(voiceleadingNode)
rescale = CsoundAC.Rescale()
rescale.thisown = 0
voiceleadingNode.addChild(rescale)
rescale.setRescale(CsoundAC.Event.KEY,        bool(1), bool(0), 36, 60)
rescale.setRescale(CsoundAC.Event.VELOCITY,   bool(1), bool(1), 45, 18)
#rescale.setRescale(CsoundAC.Event.PITCHES,    bool(1), bool(1), CsoundAC.Conversions_nameToM("Db major"), 0)
# rescale.setRescale(CsoundAC.Event.TIME,       bool(1), bool(1),  0, 700)
rescale.setRescale(CsoundAC.Event.DURATION,   bool(1), bool(1),  0.25, 1.5)

timeoffset = (duration * 6.0) / 4.0
#instr 1 ; FluidSynth Steinway 51
#instr 2 ; FluidSynth General MIDI 52
#instr 3 ; Harpsichord, James Kelley 7
#instr 4 ; Heavy metal model, Perry Cook 8 
#instr 5,6 ; Xing by Andrew Horner 9
#for instrument in [51,53,39,8,9]: #in xrange(55, 56):

buildTrack(voiceleadingNode, rescale, 1, 0, timeoffset * 0.0, 0)
buildTrack(voiceleadingNode, rescale, 1, 0, timeoffset * 0.0, 0)
buildTrack(voiceleadingNode, rescale, 2, 8, timeoffset * 2.0,-.75, 12.0)
buildTrack(voiceleadingNode, rescale, 2, 8, timeoffset * 2.0,.75, 12.0)
buildTrack(voiceleadingNode, rescale, 3, 16, timeoffset * 2.0,-.5, 24.0)

print('CREATING CSOUND ORCHESTRA...')
print
csoundOrchestra = \
'''

sr              =               48000
ksmps           =               128
nchnls          =               2
0dbfs           =               1

gi_pianoteq1     vstinit         "/home/mkg/Pianoteq\ 6/amd64/Pianoteq\ 6.so", 0
                vstinfo         gi_pianoteq1

                instr 1, 2
i_midichannel   init            0
                vstnote         gi_pianoteq1, i_midichannel, p4, p5, p3
                prints          "Pianoteq:    Channel: %3d Time: %9.4f Duration: %9.4f Key: %9.4f Velocity: %9.4f\\n", p1, p2, p3, p4, p5
                endin

                instr PianoOutput1
; Should be "D4 Daily Practice".
vstprogset gi_pianoteq1, 0
; Sustain off.
vstparamset gi_pianoteq1, 0, 0
; Reverb off.
vstparamset gi_pianoteq1, 72, 0
a_blankinput    init            0
a_left, a_right   vstaudio      gi_pianoteq1, a_blankinput, a_blankinput
a_signal        =               a_left + a_right
a_left, a_right  pan2            a_signal/3, .9
                outs            a_left, a_right
                prints          "PianoOutput: Channel: %3d Time: %9.4f Duration: %9.4f\\n", p1, p2, p3
                endin
                alwayson "PianoOutput1"

gi_pianoteq2     vstinit         "/home/mkg/Pianoteq\ 6/amd64/Pianoteq\ 6.so", 0
                vstinfo         gi_pianoteq2

                instr 3, 4
i_midichannel   init            0
                vstnote         gi_pianoteq2, i_midichannel, p4, p5, p3
                prints          "Pianoteq:    Channel: %3d Time: %9.4f Duration: %9.4f Key: %9.4f Velocity: %9.4f\\n", p1, p2, p3, p4, p5
                endin

                instr PianoOutput2
; Should be "D4 Daily Practice".
vstprogset gi_pianoteq2, 14
; Sustain off.
vstparamset gi_pianoteq2, 0, 0
; Reverb off.
vstparamset gi_pianoteq2, 72, 0
a_blankinput    init            0
a_left, a_right   vstaudio      gi_pianoteq2, a_blankinput, a_blankinput
a_signal        =               a_left + a_right
a_left, a_right  pan2            a_signal, .1
                outs            a_left, a_right
                prints          "PianoOutput: Channel: %3d Time: %9.4f Duration: %9.4f\\n", p1, p2, p3
                endin
                alwayson "PianoOutput2"

gi_pianoteq3     vstinit         "/home/mkg/Pianoteq\ 6/amd64/Pianoteq\ 6.so", 0
                vstinfo         gi_pianoteq3

                instr 5, 6
i_midichannel   init            0
                vstnote         gi_pianoteq3, i_midichannel, p4, p5, p3
                prints          "Pianoteq:    Channel: %3d Time: %9.4f Duration: %9.4f Key: %9.4f Velocity: %9.4f\\n", p1, p2, p3, p4, p5
                endin

                instr PianoOutput3
; Should be "D4 Daily Practice".
vstprogset gi_pianoteq3, 7
; Sustain off.
vstparamset gi_pianoteq3, 0, 0
; Reverb off.
vstparamset gi_pianoteq3, 72, 0
a_blankinput    init            0
a_left, a_right   vstaudio      gi_pianoteq3, a_blankinput, a_blankinput
a_signal        =               a_left + a_right
a_left, a_right  pan2            a_signal, .5
                outs            a_left, a_right
                prints          "PianoOutput: Channel: %3d Time: %9.4f Duration: %9.4f\\n", p1, p2, p3
                endin
                alwayson "PianoOutput3"
'''

print('CREATING CSOUND ARRANGEMENT...')
print
model.setCsoundOrchestra(csoundOrchestra)
model.setCsoundCommand(csoundCommand)

print('RENDERING...')
print
model.generate()
print(model.getScore().getCsoundScore())
#score.save(midiFilename)
model.perform()

print('FINISHED.')
print










































