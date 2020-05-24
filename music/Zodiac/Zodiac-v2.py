'''
ZODIAC VERSION 2

This piece is a re-composition of "Atlas Eclipticalis" by John Cage.
The basic idea of Cage's piece is to place star charts over staff paper
and transcribe the stars as notes of music. Many aspects
of Cage's piece are determined by the performers, and the piece may also
be played at the same time as other pieces by Cage. I do not observe
these possibilities.

In my piece, there are 13 sections, one for each of the CONSTELLATIONS
of the Zodiac (NOT the SIGNS of the Zodiac), because my piece is defined
by the astronomical Zodiac, not the astrologial Zodiac. I obtained actual
astronomical data from a Web service for a region
surrounding the brightest star in each of the 13 Zodiacal constellations.
Right ascension is mapped to time, declination is mapped to pitch,
visual magnitude is mapped to loudness, and spectral type is mapped to
choice of insrument.

I manually adjusted the placement of the constellations, their pitch ranges, and
so on to obtain a more successful result. This is in the same spirit as
Cage's instructions to his performers. However, I made a major departure from
Cage in that I assign different different harmonies to each constellation. 
Thus, my piece is not atonal but quasi-tonal.

Version 2 of this piece uses the CsoundAC.Scale class to generate the 
harmony. Each of the constellations is in a different key, and within 
each constellation, there is a separate chord progression.

Copyright (C) 2014-2020 by Michael Gogins.
All rights reserved.

'''
print(__doc__)

print('Set "rendering" to:     "soundfile" or "audio".')
rendering = 'soundfile'
print('Rendering option:       \"%s\"' % rendering)
print
print('Set dac_name for system.')
dac_name = "dac:plughw:1,0"
print('dac_name:               \"{}\"'.format(dac_name))
print

import CsoundAC
import os
import random
import signal
import string
import sys
import traceback

CsoundAC.System_setMessageLevel(15)

scriptFilename = sys.argv[0]
print('Full Python script:     \"%s\"' % scriptFilename)
title, ext = os.path.splitext(os.path.basename(scriptFilename))

model = CsoundAC.MusicModel()
model.setTitle("Zodiac-v2")
model.setAuthor("Michael Gogins")
model.setArtist("Michael Gogins")
print("")
model.generateAllNames()
print("")
CsoundAC.System_setMessageLevel(3)

score = model.getScore()

commandsForRendering = {
    'soundfile':    '--sample-accurate -r 96000 --ksmps=128 -+msg_color=0 -dm227 -RWZdfo %s' % (model.getOutputSoundfileFilepath()),
    'audio':        '--sample-accurate -r 48000 --ksmps=128 -+msg_color=0 -dm227 -o %s' % (dac_name),
}
csoundCommand = commandsForRendering[rendering]

model = CsoundAC.MusicModel()
model.setTitle("Zodiac-v2")
model.setAuthor("Michael Gogins")
model.setArtist("Michael Gogins")
model.generateAllNames()
score = model.getScore()

sections = []
# 0         1      2         3     4      5        6              7     8      9
# Filename, start, duration, bass, range, softest, dynamic range, left, width, list of root progressions
time = 0.
duration = 30.
print('Pisces...')
sections.append(['Alpha_Piscium.tsv',       time, duration, 36., 60., 60., 15., .1, .9, [-2, -2, -8, 2, -2, 3]])
time = time + duration + 4
duration = 30.
print('Aries...')
sections.append(['Alpha_Arietis.tsv',       time, duration, 36., 60., 60., 15., .1, .9, [-2, -2, -8, 2, -2, 3]])
time = time + duration + 4
duration = 30.
print('Taurus...')
sections.append(['Aldebaran.tsv',           time, duration, 36., 60., 60., 15., .1, .9, [-2, -12, -8, 2, -2, 3]])
time = time + duration + 4
duration = 30.
print('Gemini...')
sections.append(['Pollux.tsv',              time, duration, 36., 60., 60., 15., .1, .9, [-2, -2, -8, 2, -2, 3]])
time = time + duration + 4
duration = 30.
print('Cancer...')
sections.append(['Alpha_Cancri.tsv',        time, duration, 36., 60., 60., 15., .1, .9, [-2, -2, -8, 2, -2, 3]])
time = time + duration + 4
duration = 30.
print('Virgo...')
sections.append(['Spica.tsv',               time, duration, 36., 60., 60., 15., .1, .9, [-2, -2, -2, 2, -2, 3]])
time = time + duration + 4
duration = 30.
print('Libra...')
sections.append(['Alpha_Libris.tsv',        time, duration, 36., 60., 60., 15., .1, .9, [-2, -2, -8, 2, -2, 1]])
time = time + duration + 4
duration = 30.
print('Scorpio...')
sections.append(['Alpha_Scorpii.tsv',       time, duration, 36., 60., 60., 15., .1, .9, [-2, -2, -8, 2, -2, 1]])
time = time + duration + 4
duration = 30.
print('Ophiuchus...')
sections.append(['Rasalhague.tsv',          time, duration, 36., 60., 60., 15., .1, .9, [-2, -10, -2, -8, 2, -2, 3]])
time = time + duration + 4
duration = 30.
print('Sagittarius...')
sections.append(['Rukbat.tsv',              time, duration, 36., 60., 60., 15., .1, .9, [-2, -2, -8, 2, -2, 3]])
time = time + duration + 4
duration = 30.
print('Capricorn...')
sections.append(['Alpha_Capricornis.tsv',   time, duration, 36., 60., 60., 15., .1, .9, [-12, -12, -8, 2, -2, 3]])
time = time + duration + 4
duration = 30.
print('Aquarius...')
sections.append(['Alpha_Aquarii.tsv',       time, duration, 36., 60., 60., 15., .1, .9, [-2, -2, -8, 2, -2, 3]])
time = time + duration + 4
duration = 30.
print('Leo...')
sections.append(['Regulus.tsv',             time, duration, 36., 60., 60., 15., .1, .9, [-2, -6, 3, -2 -2, -8, 2, -2, 3]])

instrumentsForSpectralTypes = {' ':0, 'O':1, 'B':2, 'A':3, 'F':4, 'G':5, 'K':6, 'M':7}

scale = CsoundAC.Scale("D major")
chord = scale.chord(1, 4)

'''
Given a scale and chord, if the chord also exists in one or more other keys,
returns a random choice from those other keys.
'''
def modulate(scale, chord):
    modulations = scale.modulations(chord)
    new_key = scale
    count = len(modulations)
    new_key_index = 0
    if count > 0:
        index = 0
        new_key_index = random.randint(0, count - 1)
        for modulation in modulations:
            print("Possible modulation: {} {} {}".format(index, modulation.toString(), modulation.name()))
            if index == new_key_index:
                new_key = modulation
            index = index + 1
        print("              Chose: {} {} {}".format(new_key_index, new_key.toString(), new_key.name()))
    return new_key

def readCatalog(section, voiceleadingNode):
    global scale
    global chord
    print('SECTION', section)
    score = CsoundAC.ScoreNode()
    score.thisown = 0
    f = open(section[0])
    # Read the file until we run into '--'
    # which marks the last line of non-data.
    while(True):
        line = f.readline()
        if line.find('--') >= 0:
            # Read the file until done.
            # The fields we want are:
            # Right ascension   1
            # Declination       2
            # Visual magnitude  8
            # Spectral type     9
            while(True):
                line = f.readline()
                if not line:
                    break
                fields = line.split('\t')
                if not len(fields) > 8:
                    break
                # print(fields
                time = float(fields[1])
                key = float(fields[2])
                velocity = float(fields[8])
                # This was evidently a mistake.
                # pan = float(fields[6])
                pan = random.uniform(.05, .95)
                if(len(fields) > 9):
                    instrument = float(instrumentsForSpectralTypes[fields[9][0]])
                else:
                    instrument = 8
                score.getScore().append(time, velocity * 0.001, 144.0, instrument, key, velocity, 0.0, pan)
            print(score.getScore().toString())
            # Put the section into a Rescale node to position it in the piece.
            rescale = CsoundAC.Rescale()
            rescale.thisown = 0
            scoreTime = section[1]
            scoreDuration = section[2]
            shortest = 4.0
            durationRange = 8.0
            key = section[3]
            range = section[4]
            lowest = section[5]
            dynamicRange = section[6]
            leftmost = section[7]
            width = section[8]
            print('time:          ',scoreTime)
            print('duration:      ',scoreDuration)
            print('shortest:      ',shortest)
            print('duration range:',durationRange)
            print('key:           ',key)
            print('range:         ',range)
            print('lowest:        ',lowest)
            print('dynamic range: ',dynamicRange)
            print('leftmost:      ',leftmost)
            print('width:         ',width)
            print
            rescale.setRescale(CsoundAC.Event.TIME,      True, True, scoreTime, scoreDuration)
            rescale.setRescale(CsoundAC.Event.DURATION,  True, True, shortest,  durationRange)
            rescale.setRescale(CsoundAC.Event.KEY,       True, True, key,       range)
            rescale.setRescale(CsoundAC.Event.VELOCITY,  True, True, lowest,    dynamicRange)
            rescale.setRescale(CsoundAC.Event.PHASE,     True, True, leftmost,  width)
            # Now generate the harmony as a function of scoreDuration and add the chords.
            progression = section[9]
            secondsPerChord = scoreDuration / len(progression)
            chordTime = scoreTime
            for steps in progression:
                chord = scale.transpose_degrees(chord, steps)
                print("Time: {:9.4f} chord: {} name: {}".format(chordTime, chord.toString(), chord.eOP().name()))
                voiceleadingNode.chord(chord, chordTime)
                chordTime = chordTime + secondsPerChord
            rescale.addChild(score)
            scale = modulate(scale, chord)
            return rescale

rescale = CsoundAC.Rescale()
model.addChild(rescale)
rescale.setRescale(CsoundAC.Event.INSTRUMENT, True, True,  1.0,  7.0)
rescale.setRescale(CsoundAC.Event.DURATION,   True, True,  6.0,  8.0)
rescale.setRescale(CsoundAC.Event.VELOCITY,   True, True, 60.0,  8.0)
sectionNumber = 0
for section in sections:
    sectionNumber = sectionNumber + 1
    print("Section %3d: %s" % (sectionNumber, section))
    voiceleadingNode = CsoundAC.VoiceleadingNode()
    voiceleadingNode.thisown = 0
    subscore = readCatalog(section, voiceleadingNode)
    voiceleadingNode.addChild(subscore)
    rescale.addChild(voiceleadingNode)

csoundOrchestra = '''

sr = 48000
ksmps = 128
nchnls = 2
0dbfs = 32767

massign 0, 7
massign 1, 7

gi2fqc init cpspch(7.09)
gi3fqc init cpspch(10.0)
giseed init 0.5

; Waveform for the string-pad
giwave ftgen 1, 0, 65537, 10, 1, .5, .33, 0.25, .0, 0.1, .1, 0.1
gisine ftgen 2, 0, 65537, 10, 1
giwtsin init gisine
giharpsichord ftgen 0, 0, 65537, 7, -1, 1024, 1, 1024, -1 ; Kelley harpsichord.
gicosine ftgen 0, 0, 65537, 11, 1 ; Cosine wave. Get that noise down on the most widely used table!
giexponentialrise ftgen 0, 0, 65537, 5, .001, 513, 1 ; Exponential rise.
githirteen ftgen 0, 0, 65537, 9, 1, .3, 0
giln ftgen 0, 0, 65537, -12, 20.0 ; Unscaled ln(I(x)) from 0 to 20.0.
gibergeman ftgen 0, 0, 65537, 10, .28, 1, .74, .66, .78, .48, .05, .33, 0.12, .08, .01, .54, 0.19, .08, .05, 0.16, .01, 0.11, .3, .02, 0.2 ; Bergeman f1
gicookblank ftgen 0, 0, 65537, 10, 0 ; Blank wavetable for some Cook FM opcodes.
gicook3 ftgen 0, 0, 65537, 10, 1, .4, 0.2, 0.1, 0.1, .05
gikellyflute ftgen 0, 0, 65537, 10, 1, 0.25, 0.1 ; Kelley flute.
gichebychev ftgen 0, 0, 65537, -7, -1, 150, 0.1, 110, 0, 252, 0
giffitch1 ftgen 0, 0, 65537, 10, 1
giffitch2 ftgen 0, 0, 65537, 5, 1, 1024, .01
giffitch3 ftgen 0, 0, 65537, 5, 1, 1024, .001
 ; Rotor Tables
gitonewheel1 ftgen 0, 0, 65537, 10, 1, .02, .01
gitonewheel2 ftgen 0, 0, 65537, 10, 1, 0, 0.2, 0, 0.1, 0, .05, 0, .02
 ; Rotating Speaker Filter Envelopes
gitonewheel3 ftgen 0, 0, 65537, 7, 0, 110, 0, 18, 1, 18, 0, 110, 0
gitonewheel4 ftgen 0, 0, 65537, 7, 0, 80, 0.2, 16, 1, 64, 1, 16, 0.2, 80, 0
 ; Distortion Tables
gitonewheel5 ftgen 0, 0, 65537, 8, -.8, 336, -.78, 800, -.7, 5920, .7, 800, .78, 336, .8
gitonewheel6 ftgen 0, 0, 65537, 8 -.8, 336, -.76, 3000, -.7, 1520, .7, 3000, .76, 336, .8
 ; Table for Reed Physical Model
gireedtable ftgen 0, 0, 256, 7, 1, 80, 1, 156, -1, 40, -1
 ; Tables for simple granular synthesis
gigrtab ftgen 0, 0, 65537, 10, 1, 0.3, .1, 0, .2, .02, 0, .1, .04
giwintab ftgen 0, 0, 65537, 10, 1, 0, .5, 0, .33, 0, .25, 0, .2, 0, .167
 ; Tables for waveshaping drone
giharmonics ftgen 0, 0, 65537, 10, 1, 0, 2, 0, 0, 1
gidistortion ftgen 0, 0, 65537, 13, 1, 1, 0, 1, 0, 1
 ; Tables for Lee Zakian flute
gif1 ftgen 0, 0, 65537, 10, 1
gif2 ftgen 0, 0, 16, -2, 40, 40, 80, 160, 320, 640, 1280, 2560, 5120, 10240, 10240
gif26 ftgen 0, 0, 65537, -10, 2000, 489, 74, 219, 125, 9, 33, 5, 5
gif27 ftgen 0, 0, 65537, -10, 2729, 1926, 346, 662, 537, 110, 61, 29, 7
gif28 ftgen 0, 0, 65537, -10, 2558, 2012, 390, 361, 534, 139, 53, 22, 10, 13, 10
gif29 ftgen 0, 0, 65537, -10, 12318, 8844, 1841, 1636, 256, 150, 60, 46, 11
gif30 ftgen 0, 0, 65537, -10, 1229, 16, 34, 57, 32
gif31 ftgen 0, 0, 65537, -10, 163, 31, 1, 50, 31
gif32 ftgen 0, 0, 65537, -10, 4128, 883, 354, 79, 59, 23
gif33 ftgen 0, 0, 65537, -10, 1924, 930, 251, 50, 25, 14
gif34 ftgen 0, 0, 65537, -10, 94, 6, 22, 8
gif35 ftgen 0, 0, 65537, -10, 2661, 87, 33, 18
gif36 ftgen 0, 0, 65537, -10, 174, 12
gif37 ftgen 0, 0, 65537, -10, 314, 13

connect "Harpsichord", "outleft", "ReverbSC", "inleft"
connect "Harpsichord", "outright", "ReverbSC", "inright"
connect "FilteredChorus", "outleft", "ReverbSC", "inleft"
connect "FilteredChorus", "outright", "ReverbSC", "inright"
connect "FilteredChorus2", "outleft", "ReverbSC", "inleft"
connect "FilteredChorus2", "outright", "ReverbSC", "inright"
connect "Eight", "outleft", "ReverbSC", "inleft"
connect "Eight", "outright", "ReverbSC", "inright"
connect "FMModerate", "outleft", "ReverbSC", "inleft"
connect "FMModerate", "outright", "ReverbSC", "inright"
connect "Xing", "outleft", "ReverbSC", "inleft"
connect "Xing", "outright", "ReverbSC", "inright"
connect "Xanadu3", "outleft", "ReverbSC", "inleft"
connect "Xanadu3", "outright", "ReverbSC", "inright"
connect "ReverbSC", "outleft", "MasterOutput", "inleft"
connect "ReverbSC", "outright", "MasterOutput", "inright"

;           Insno           Delay   Pitch mod   Cutoff  Wet
alwayson    "ReverbSC",     .48,    .005,       16000,  .25

;           Insno Start     Fadeout  Clip
alwayson    "MasterOutput", .004,    .1,      2


opcode NoteOn, ikii, iii
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General purpose instrument control UDO.
; Returns the pitch at i-rate, the pitch at k-rate
; (with addition of smoothed MIDI pitch bend, if any),
; decibels full scale scaled from MIDI velocity,
; and the amplitude scaled such that 127 == 0 dBFS.
;
; If an instrument is balanced, then its solo peak
; amplitude at MIDI velocity 127 should be exactly
; 0 dBFS. If the instrument is too loud (or too soft)
; at velocity 127, set imeasuredDBFS to the peak level
; reported by Csound; e.g. for the following messsage:
;
; rtevent: T 12.257 TT 12.257 M: +3.35 +3.60
; number of samples out of range: 511 552
;
; set the imeasuredDBFS parameter in the NoteOn call
; in the instrument to 3.6. This will noprmalize the
; instrument.
ikey,ivelocity,idecibels xin
; Convert MIDI key number to cycles per second.
iHz = cpsmidinn(ikey)
; Modify with MIDI pitch bend, if any.
kpitchbend pchbend -6.0, +6.0
kpitchbend = kpitchbend + 6.0
iinitialpb init i(kpitchbend)
;print(iinitialpb
; Smooth out the stepping in the MIDI control signal.
ksmoothbend port kpitchbend, 0.125, iinitialpb
kKey = ikey + ksmoothbend
kHz = cpsmidinn(kKey)
; ratio = pow(10, dB / 20)
iratio = pow(10, idecibels / 20)
iamplitude = ampdb(ivelocity) * iratio
; print(ivelocity, idecibels, iamplitude, iratio
xout iHz, kHz, iamplitude, ivelocity
endop

opcode Modulation, k, j
; Returns +- one octave of pitch bend in MIDI
; key numbers.
kpitchbend init 0
xout kpitchbend
endop

opcode AR, ia, iiii
; Outputs new p3, arate envelope for
; attack time,
; attack level,
; release time (should usually be p3),
; slope exponent.
; Handles real-time by indefinitely extending
; sustain time and p3.
iat,ial,irt,islope xin
ip3 = p3;iat + irt
aenvelope transeg 0.0, iat, islope, ial, irt, islope, 0.0
xout ip3, aenvelope
endop

opcode ASR, ia, iiiiii
; Outputs new p3, arate envelope for
; attack time,
; attack level,
; sustain time (should usually be p3),
; sustain level,
; release time,
; slope exponent.
; Handles real-time by indefinitely extending
; sustain time and p3.
iat,ial,ist,isl,irt,islope xin
ip3 = iat + ist + irt
aenvelope transeg 0.0, iat, islope, ial, ist, islope, isl, irt, islope, 0.0
xout ip3, aenvelope
endop

opcode ADSR, ia, iiiiiiii
; Outputs new p3, arate envelope for
; attack time,
; attack level,
; decay time,
; decay level,
; sustain time (should usually be p3),
; sustain level,
; release time,
; release level,
; slope exponent.
; Handles real-time by indefinitely extending
; sustain time and p3.
iat,ial,idt,idl,ist,irt,irl,islope xin
ip3 = p3;iat + idt + ist + irt
aenvelope transeg 0.0, iat, islope, ial, idt, islope, idl, ist, islope, irl, irt, islope, 0.0
xout ip3, aenvelope
endop

instr Harpsichord ; James Kelley
prints "Harpsichord     "
iHz,kHz,iamplitude,idB NoteOn p4, p5, 4
p3=10
adamping linseg 0, .003, 1, p3 - .008, 1, .005, 0
aenvelope transeg 1.0, 40.0, -19.0, 0.0
apluck pluck iamplitude, kHz, iHz, 0, 1
apluck = apluck * aenvelope
aharp poscil3 aenvelope, kHz, giharpsichord
aharp2 balance apluck, aharp
asignal = (apluck + aharp2) * iamplitude
aleft, aright pan2 asignal, p7
prints "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\\n", p1, p2, p3, p4, p5, p7
outleta "outleft", aleft * adamping
outleta "outright", aright * adamping
endin

instr FilteredChorus
prints "Filtered Chorus "
adamping linseg 0, .003, 1, p3 - .008, 1, .005, 0
iHz,kHz,iamplitude,idB NoteOn p4, p5, 62
koctave = octcps(kHz)
idb = 1.5
ip5 = gibergeman
ip3 = p3
ip6 = 0.9
ip7 = 1.4
kp8 = cpsoct(koctave - .01)
kp9 = cpsoct(koctave + .01)
isc = idb * .333
ak1 line 40,ip3,800
ak2 line 440,ip3,220
;ak3 linen isc,ip6,ip3,ip7
ak3 linseg 0, ip6, isc, ip3 - (ip6 + ip7), isc, 0
ak4 line 800,ip3,40
ak5 line 220,ip3,440
;ak6 linen isc,ip6,ip3,ip7
ak6 linseg 0, ip6, isc, ip3 - (ip6 + ip7), isc, 0
a5 poscil3 ak3,kp8,ip5
a6 poscil3 ak3,kp8*.999,ip5
a7 poscil3 ak3,kp8*1.001,ip5
a1=a5+a6+a7
a8 poscil3 ak6,kp9,ip5
a9 poscil3 ak6,kp9*.999,ip5
a10 oscil3 ak6,kp9*1.001,ip5
a11=a8+a9+a10
a2 butterbp a1,ak1,40
a3 butterbp a2,ak5,ak2*.8
a4 balance a3,a1
a12 butterbp a11,ak4,40
a13 butterbp a12,ak2,ak5*.8
a14 balance a13,a11
;outs a4,a14
outleta "outleft", a4 * adamping * iamplitude
outleta "outright", a14 * adamping * iamplitude
prints "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\\n", p1, p2, p3, p4, p5, p7
endin

instr FilteredChorus2 ; Michael Bergeman
prints "Filtered Chor2 "
adamping linseg 0, .003, 1, p3 - .008, 1, .005, 0
iHz,kHz,iamplitude,idB NoteOn p4, p5, -6
koctave = octcps(kHz)
idb = 1.5
ip5 = gibergeman
ip3 = p3
ip6 = 0.9
ip7 = 1.4
kp8 = cpsoct(koctave - .01)
kp9 = cpsoct(koctave + .01)
isc = idb * .333
ak1 linseg 40, ip3, 800, p3, 800, 0.06, 0.0
ak2 linseg 440, ip3, 220, p3, 220, 0.06, 0.0
ak3 linseg 0.0, ip6, 800, ip7, 200.0, p3, 200, 0.06, 0.0
ak4 linseg 800, ip3, 40, p3, 40, 0.06, 0.0
ak5 linseg 220, ip3, 440, p3, 440, 0.06, 0.0
ak6 linseg isc, ip6, p3, ip7, p3, 0.06, 0.0
ak7 linseg 0.0, ip6, 1, ip7, .3, p3, .1, 0.06, 0.0
a5 poscil3 ak3, kp8, ip5
a6 poscil3 ak3, kp8 * 0.999, ip5
a7 poscil3 ak3, kp8 * 1.001, ip5
a1 = a5 + a6 + a7
a8 poscil3 ak6, kp9, ip5
a9 poscil3 ak6, kp9 * 0.999, ip5
a10 poscil3 ak6, kp9 * 1.001, ip5
a11 = a8 + a9 + a10
a2 butterbp a1, ak1, 40
a3 butterbp a2, ak5, ak2 * 0.8
a4 balance a3, a1
a12 butterbp a11, ak4, 40
a13 butterbp a12, ak2, ak5 * 0.8
a14 balance a13, a11
a15 reverb2 a4, 5, 0.3
a16 reverb2 a4, 4, 0.2
a17 = (a15 + a4) * ak7
a18 = (a16 + a4) * ak7
asignal = (a17 + a18) * iamplitude
aleft, aright pan2 asignal, p7
outleta "outleft", aleft * adamping
outleta "outright", aright * adamping
prints "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\\n", p1, p2, p3, p4, p5, p7
endin

instr FMModerate ; FM moderate index 3, Michael Gogins
prints "FM Moderate Ind "
adamping linseg 0, .003, 1, p3 - .008, 1, .005, 0
iHz,kHz,iamplitude,idB NoteOn p4, p5, 62
iattack = 0.002
isustain = p3
idecay = 4.5
irelease = 0.05
icarrier = 1
imodulator = 1.5
ifmamplitude = 2
index = 3.5
ifrequencyb = iHz * 1.003
icarrierb = icarrier * 1.004
aindenv transeg 0.0, iattack, -7.0, 1.0, idecay, -7.0, 0.025, isustain, 0.0, 0.025, irelease, -7.0, 0.0
aindex = aindenv * index * ifmamplitude
; ares foscili xamp, kcps, xcar, xmod, kndx, ifn [, iphs]
aouta foscili 1.0, iHz, icarrier, imodulator, index, 1
aoutb foscili 1.0, ifrequencyb, icarrierb, imodulator, index, 1
; Plus amplitude correction.
afmout = (aouta + aoutb) * aindenv
asignal = afmout * iamplitude
aleft, aright pan2 asignal, p7
outleta "outleft", aleft * adamping
outleta "outright", aright * adamping
prints "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\\n", p1, p2, p3, p4, p5, p7
endin

gkgrainDensity init 120
gkgrainDuration init 0.2
gkgrainAmplitudeRange init .05
gkgrainFrequencyRange init 8 / 1200
instr Eight
 //////////////////////////////////////////////
 // Original by Hans Mikelson.
 // Adapted by Michael Gogins.
 //////////////////////////////////////////////
i_instrument = p1
i_time = p2
i_duration = p3
i_midikey = p4
i_midivelocity = p5
i_phase = p6
i_pan = p6
i_depth = p8
i_height = p9
i_pitchclassset = p10
i_homogeneity = p11
adamping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
ifrequency = cpsmidinn(i_midikey)
iamplitude = ampdb(i_midivelocity) / 6
igrtab ftgenonce 0, 0, 65537, 10, 1, .3, .1, 0, .2, .02, 0, .1, .04
iwintab ftgenonce 0, 0, 65537, 10, 1, 0, .5, 0, .33, 0, .25, 0, .2, 0, .167
iHz = ifrequency
ihertz = iHz
ip5 = iHz
ip6 = igrtab
ip7 = iwintab
ip8 = 0.033
ip8 = .002
ip9 = 150
ip9 = 100
ip10 = 1.6
ip10 = 3
idur = p3
iamp = iamplitude ; p4
ifqc = iHz ; cpspch(p5)
igrtab = ip6
iwintab = ip7
ifrng = ip8
idens = ip9
ifade = ip10
igdur = 0.2
iattack = .2
idecay = .4
isustain = p3 - (iattack + idecay)
kenvelope transeg 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
kamp = kenvelope
; ares grain xamp, xpitch, xdens,          kampoff,               kpitchoff,                    kgdur,           igfn,   iwfn,    imgdur [, igrnd]
aoutl grain 1,  ifqc,   gkgrainDensity, gkgrainAmplitudeRange, ifqc * gkgrainFrequencyRange, gkgrainDuration, igrtab, iwintab, 5
aoutr grain 1, ifqc, gkgrainDensity, gkgrainAmplitudeRange, ifqc * gkgrainFrequencyRange, gkgrainDuration, igrtab, iwintab, 5
;aoutl grain 1, ifqc, gkgrainDensity, gkgrainAmplitudeRange, gkgrainFrequencyRange, gkgrainDuration, igrtab, iwintab, 5
;aoutr grain 1, ifqc, gkgrainDensity, gkgrainAmplitudeRange, gkgrainFrequencyRange, gkgrainDuration, igrtab, iwintab, 5
aleft = aoutl * kamp * iamplitude
aright = aoutr * kamp * iamplitude
outleta "outleft", aleft * adamping
outleta "outright", aright * adamping
prints "Grain           "
prints "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\\n", p1, p2, p3, p4, p5, p7
endin

instr Xing ; by Andrew Horner
prints "Xing            "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; p4 pitch in octave.pch
; original pitch = A6
; range = C6 - C7
; extended range = F4 - C7
adamping linseg 0, .003, 1, p3 - .008, 1, .005, 0
iHz,kHz,iamplitude,idB NoteOn p4, p5, 75
isine = 1
iinstrument = p1
istarttime = p2
ioctave = p4
idur = p3
kfreq = kHz
iamp = 1
inorm = 32310
aamp1 linseg 0,.001,5200,.001,800,.001,3000,.0025,1100,.002,2800,.0015,1500,.001,2100,.011,1600,.03,1400,.95,700,1,320,1,180,1,90,1,40,1,20,1,12,1,6,1,3,1,0,1,0
adevamp1 linseg 0, .05, .3, idur - .05, 0
adev1 poscil3 adevamp1, 6.7, gisine, .8
amp1 = aamp1 * (1 + adev1)
aamp2 linseg 0,.0009,22000,.0005,7300,.0009,11000,.0004,5500,.0006,15000,.0004,5500,.0008,2200,.055,7300,.02,8500,.38,5000,.5,300,.5,73,.5,5.,5,0,1,1
adevamp2 linseg 0,.12,.5,idur-.12,0
adev2 poscil3 adevamp2, 10.5, gisine, 0
amp2 = aamp2 * (1 + adev2)
aamp3 linseg 0,.001,3000,.001,1000,.0017,12000,.0013,3700,.001,12500,.0018,3000,.0012,1200,.001,1400,.0017,6000,.0023,200,.001,3000,.001,1200,.0015,8000,.001,1800,.0015,6000,.08,1200,.2,200,.2,40,.2,10,.4,0,1,0
adevamp3 linseg 0, .02, .8, idur - .02, 0
adev3 poscil3 adevamp3, 70, gisine ,0
amp3 = aamp3 * (1 + adev3)
awt1 poscil3 amp1, kfreq, gisine
awt2 poscil3 amp2, 2.7 * kfreq, gisine
awt3 poscil3 amp3, 4.95 * kfreq, gisine
asig = awt1 + awt2 + awt3
arel linenr 1,0, idur, .06
asignal = asig * arel * (iamp / inorm) * iamplitude
aleft, aright pan2 asignal, p7
outleta "outleft", aleft * adamping
outleta "outright", aright * adamping
prints "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\\n", p1, p2, p3, p4, p5, p7
endin

instr Xanadu3 ; Xanadu instr 3
prints "FM Mod Det Chor "
adamping linseg 0, .003, 1, p3 - .008, 1, .005, 0
iHz,kHz,iamplitude,idB NoteOn p4, p5, 55
ishift = 1.5 / 1200.0
kpch = kHz
koct = octcps(kpch)
;amodi linseg 0, p3/3, 5, p3/3, 3, p3/3, 0 ; ADSR envelope for I
amodi linseg 0, p3/3, 1.37, p3/3, 0.43, p3/3, 0 ; ADSR envelope for I
ip6 = 1.2
ip7 = 0.8
amodr linseg ip6, p3, ip7 ; r moves from p6->p7 in p3 sec.
a1 = amodi * (amodr - 1 / amodr) / 2
a1ndx = abs(a1 * 2 / 20) ; a1*2 is normalized from 0-1.
a2 = amodi * (amodr + 1 / amodr) / 2
a3 tablei a1ndx, giln, 1 ; lookup tbl in f3, normal index
ao1 poscil3 a1, kpch, gicosine
a4 = exp(-0.5 * a3 + ao1)
ao2 poscil3 a2 * kpch, kpch, gicosine
aoutl poscil3 1 * a4, ao2 + cpsoct(ishift), gisine
aoutr poscil3 1 * a4, ao2 + cpsoct(ishift), gisine
aleft = aoutl * iamplitude
aright = aoutr * iamplitude
outleta "outleft", aleft * adamping
outleta "outright", aright * adamping
prints "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\\n", p1, p2, p3, p4, p5, p7
endin

instr FMBell ; FM Bell
prints "FM Bell         "
adamping linseg 0, .001, 1, p3 - .006, 1, .005, 0
iHz,kHz,iamplitude,idB NoteOn p4, p5, 50
kc1 = 5
kc2 = 7
kvdepth = 0.0125
kvrate = 5.1
ifn1 = 1
ifn2 = 2
ifn3 = 1
ifn4 = 1
ivfn = 1
aout fmbell 1, iHz, kc1, kc2, kvdepth, kvrate, ifn1, ifn2, ifn3, ifn4, ivfn
aleft, aright pan2 aout, p7
outleta "outleft", aleft * adamping * iamplitude
outleta "outright", aright * adamping * iamplitude
prints "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\\n", p1, p2, p3, p4, p5, p7
endin

instr ReverbSC ; Reverb by Sean Costello / J. Lato
prints "Reverb          "
ainleft inleta "inleft"
ainright inleta "inright"
idelay = p4
ipitchmod = p5
icutoff = p6
iwet = p7
idry = 1 - iwet
aoutleft, aoutright reverbsc ainleft, ainright, idelay, icutoff, sr, ipitchmod
aoutleft = aoutleft * iwet + aoutleft * idry
aoutright = aoutright * iwet + aoutleft * idry
outleta "outleft", aoutleft
outleta "outright", aoutright
endin

instr MasterOutput ; Master output
igain = p4
ifade = p5
iclip = p6
aleft inleta "inleft"
aright inleta "inright"
; Applies a bass enhancement, compression and fadeout
; to the whole piece, outputs signals, and clears the mixer.
; Receive audio from the master mixer buss.
; Enhance the bass.
aleft butterlp aleft, 100
aright butterlp aright, 100
aleft = aleft * 1.5 + aleft
aright= aright * 1.5 + aright
aleft = aleft * igain
aright = aright * igain
;aleft clip aleft, 0, iclip
;aright clip aright, 0, iclip
; Remove DC bias.
aleft dcblock aleft
aright dcblock aright
; Output audio.
outs aleft, aright
prints "Master Output   "
prints "instr %4d t %9.4f d %9.4f gain %9.4f fade %9.4f clip %9.4f\\n", p1, p2, p3, igain, ifade, iclip
endin
'''

model.setCsoundOrchestra(csoundOrchestra)
model.setCsoundCommand(csoundCommand)
model.generate()
model.createCsoundScore()
model.performMaster()
if rendering == 'soundfile':
    model.translateMaster()

