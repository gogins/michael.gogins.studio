'''
Cellular, for Computer Piano

Copyright (C) 2019 by Michael Gogins

Mozart's musical dice game is taken apart and put back together along the 
lines of Terry Riley's "In C" using Python, and realized using Csound with 
the Pianoteq synthesized piano.
'''
print __doc__
print 'IMPORTING REQUIRED MODULES...'
print
import CsoundAC
import os
import random
random.seed(839)
import signal
import string
import sys
import traceback

print 'CREATING FILENAMES...'
print
scriptFilename = sys.argv[0]
print 'Full Python script:     %s' % scriptFilename
title, exte = os.path.splitext(os.path.basename(scriptFilename))
print 'Base Python script:     %s' % title
directory = os.path.dirname(scriptFilename)
if len(directory):
    print 'Working directory:      %s' % directory
    os.chdir(directory)
print 'Working directory:      %s' % directory
orcFilename = title + '.orc'
midiFilename = title + '.mid'
print 'MIDI filename:          %s' % midiFilename
soundfileName = title + '.wav'
print 'Soundfile name:         %s' % soundfileName
dacName = 'dac'
print 'Audio output name:      %s' % dacName
print

print 'SETTING RENDERING AND PLAYBACK OPTIONS...'
print
print 'Set "rendering" to:     "cd", "preview" (default), or "audio".'
print 'Set "playback" to:      True (default) or False.'
print
rendering = 'audio'
playback = True
print 'Rendering option:       %s' % rendering
print 'Play after rendering:   %s' % playback
commandsForRendering = {
    'soundfile':    'csound -r 96000 -k 100 -m195 -+msg_color=0 -RWZdfo %s' % (soundfileName),
    'audio':        'csound -r 48000 -k 100 -m0   -+msg_color=0 -RWZdfo %s' % (dacName),
}    
csoundCommand = commandsForRendering[rendering]
print 'Csound command line:    %s' % csoundCommand
print

print 'CREATING GLOBAL OBJECTS...'
print
model = CsoundAC.MusicModel()
score = model.getScore()

print 'CREATING MUSIC MODEL...'
print

minuetTable = {}
# Put some changes in here, perhaps a bitonal differential.
# There needs to be some association between a measure and a harmony. 
# This could be in one, or even several, master tracks, or in a separate track.
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
   for index in reversed(xrange(len(L))):
      yield index, L[index]
   
CM = CsoundAC.Conversions_nameToM("CM")
Em = CsoundAC.Conversions.nameToM("Em")
BbM = CsoundAC.Conversions.nameToM("BbM")
GM9 = CsoundAC.Conversions.nameToM("GM9")

def readMeasure(number, pitches):
    scoreNode = CsoundAC.ScoreNode()
    scoreNode.thisown = 0
    filename = 'M' + str(number) + '.mid'
    score = scoreNode.getScore()
    score.load(filename)
    # Remove false notes.
    for i, event in reverse_enumeration(score):
        event.setPitches(pitches)
        if event.getChannel() < 0:
            score.remove(i)
    #rint score.getCsoundScore()
    score.setDuration(random.choice([2, 3, 4, 6, 8]))
    print 'Read "%s" with duration %9.4f.' % (filename, score.getDuration())
    return scoreNode    

def buildTrack(sequence, channel, bass):
    print 'Building track for channel %3d bass %3d...' % (channel, bass)
    cumulativeTime = 0.0
    for i in xrange(1, 16):
        for j in xrange(3, 7):
            pitches = random.choice([CM, Em, CM, Em, BbM, GM9])
            repeatCount = 1 + int(random.random() * 12)
            for k in xrange(repeatCount):
                measure = readMeasure(minuetTable[j][i], pitches)
                duration = measure.getScore().getDuration()
                rescale = CsoundAC.Rescale()
                rescale.setRescale(CsoundAC.Event.TIME, bool(1), bool(0), cumulativeTime, 0)
                rescale.setRescale(CsoundAC.Event.INSTRUMENT, bool(1), bool(0), channel, 0)
                rescale.setRescale(CsoundAC.Event.KEY, bool(1), bool(0), float(bass), 48)
                rescale.thisown = 0
                rescale.addChild(measure)
                print 'Repeat %4d of %4d at %8.3f with %3d notes of duration %7.3f at bass %7.3f...' %(k + 1, repeatCount, cumulativeTime, len(measure.getScore()), duration, bass)
                sequence.addChild(rescale)
                cumulativeTime = cumulativeTime + duration

sequence = CsoundAC.Rescale()
model.setAuthor("Michael Gogins")
model.setArtist("Michael Gogins")
model.setTitle("Cellular, for Computer Piano")
model.addChild(sequence)
model.setConformPitches(bool(1))
sequence.setRescale(CsoundAC.Event.VELOCITY,   bool(1), bool(1), 60, 24)
sequence.setRescale(CsoundAC.Event.INSTRUMENT, bool(1), bool(1),  1, 5)
#sequence.setRescale(CsoundAC.Event.TIME,       bool(1), bool(1),  1, 1700)
buildTrack(sequence, 1, 24)
buildTrack(sequence, 2, 36)
buildTrack(sequence, 3, 36)
buildTrack(sequence, 4, 48)
buildTrack(sequence, 5, 48)

print 'CREATING CSOUND ORCHESTRA...'
print
csoundOrchestra = \
'''

sr              =               48000
ksmps           =               64
nchnls          =               2 
0dbfs           =               1

gi_pianoteq     vstinit         "/home/mkg/pianoteq_linux_v630/Pianoteq\ 6/amd64/Pianoteq\ 6.so", 0
                vstinfo         gi_pianoteq

                instr 1, 2, 3, 4, 5, 6 
i_midichannel   init            0
                vstnote         gi_pianoteq, i_midichannel, p4, p5, p3
                prints          "Pianoteq:    Channel: %3d Time: %9.4f Duration: %9.4f Key: %9.4f Velocity: %9.4f\\n", p1, p2, p3, p4, p5
                endin

                instr PianoOutput 
a_blankinput    init            0
a_left, a_right   vstaudio      gi_pianoteq, a_blankinput, a_blankinput
                outs            a_left, a_right
                prints          "PianoOutput: Channel: %3d Time: %9.4f Duration: %9.4f\\n", p1, p2, p3
                endin
                alwayson "PianoOutput"
'''

print 'CREATING CSOUND ARRANGEMENT...'
print
model.setCsoundOrchestra(csoundOrchestra)
model.setCsoundCommand(csoundCommand)

print 'RENDERING...'
print
model.render()
print score.getCsoundScore()
score.save(midiFilename)

print 'FINISHED.'
print
