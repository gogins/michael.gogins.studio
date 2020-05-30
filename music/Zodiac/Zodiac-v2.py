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
harmony. Some of the constellations are in different keys, and within 
each constellation, there is a separate chord progression.

Copyright (C) 2014-2020 by Michael Gogins.
All rights reserved.

'''
print(__doc__)

print('Set "rendering" to:     "soundfile" or "audio".')
rendering = "soundfile"
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

random.seed(59382)

CsoundAC.System_setMessageLevel(15)

scriptFilename = sys.argv[0]
print('Full Python script:     \"%s\"' % scriptFilename)
title, ext = os.path.splitext(os.path.basename(scriptFilename))

model = CsoundAC.MusicModel()
model.setTitle("Zodiac-v2")
model.setAuthor("Michael Gogins")
model.setArtist("Michael Gogins")
model.setYear("2020")
print("")
model.generateAllNames()
print("")
CsoundAC.System_setMessageLevel(3)

score = model.getScore()

commandsForRendering = {
    'soundfile':    '--sample-accurate -r 96000 --ksmps=20  -+msg_color=0 -dm227 -RWZdfo %s' % (model.getOutputSoundfileFilepath()),
    'audio':        '--sample-accurate -r 48000 --ksmps=128 -+msg_color=0 -dm227      -o %s' % (dac_name),
}
csoundCommand = commandsForRendering[rendering]

model = CsoundAC.MusicModel()
model.setTitle("Zodiac-v2")
model.setAuthor("Michael Gogins")
model.setArtist("Michael Gogins")
model.setYear("2020")
model.generateAllNames()
score = model.getScore()

sections = []
# 0         1      2         3     4      5        6              7     8      9
# Filename, start, duration, bass, range, softest, dynamic range, left, width, list of root progressions
time = 0.
duration = 30.
print('Pisces...')
sections.append(['Alpha_Piscium.tsv',       time, duration, 36., 60., 60., 15., .1, .9, [-2, -2, -10, -2, 4]])
time = time + duration + 4
duration = 30.
print('Aries...')
sections.append(['Alpha_Arietis.tsv',       time, duration, 36., 60., 60., 15., .1, .9, [-2, -2, -2, -2, -2, 3]])
time = time + duration + 4
duration = 30.
print('Taurus...')
sections.append(['Aldebaran.tsv',           time, duration, 36., 60., 60., 15., .1, .9, [-2, -2, -8, 2, -8, 3]])
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
sections.append(['Alpha_Libris.tsv',        time, duration, 36., 60., 60., 15., .1, .9, [-2, -4, -8, 2, -4, 3]])
time = time + duration + 4
duration = 30.
print('Scorpio...')
sections.append(['Alpha_Scorpii.tsv',       time, duration, 36., 60., 60., 15., .1, .9, [-2, -2, -8, 2, -8, 3]])
time = time + duration + 4
duration = 30.
print('Ophiuchus...')
sections.append(['Rasalhague.tsv',          time, duration, 36., 60., 60., 15., .1, .9, [-2, -10, -2, -8, 3, -2, 3]])
time = time + duration + 4
duration = 30.
print('Sagittarius...')
sections.append(['Rukbat.tsv',              time, duration, 36., 60., 60., 15., .1, .9, [-6, -2, -8, -4, -2, 3]])
time = time + duration + 4
duration = 30.
print('Capricorn...')
sections.append(['Alpha_Capricornis.tsv',   time, duration, 36., 60., 60., 15., .1, .9, [-2, -2, -8, 2, -8, 3]])
time = time + duration + 4
duration = 30.
print('Aquarius...')
sections.append(['Alpha_Aquarii.tsv',       time, duration, 36., 60., 60., 15., .1, .9, [-2, -2, -8, 2, -2, 1]])
time = time + duration + 4
duration = 30.
print('Leo...')
sections.append(['Regulus.tsv',             time, duration, 36., 60., 60., 15., .1, .9, [-2, -6, 3, -2, -8, 3, 0, 0]])

instrumentsForSpectralTypes = {' ':0, 'O':1, 'B':2, 'A':3, 'F':4, 'G':5, 'K':6, 'M':7}

scale = CsoundAC.Scale("A major")
chord = scale.chord(1, 5)

'''
Given a scale and chord, if that chord also exists in one or more other keys,
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
    # Put the section into a Rescale node to position it in the piece.
    rescale = CsoundAC.Rescale()
    rescale.thisown = 0
    rescale.addChild(score)
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
                score_ = score.getScore()
                score_[score_.size() -1].setProperty("section", str(section))
            #print(score.getScore().toString())
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
            changes = random.randint(6, 15)
            progression = random.choices([-2, -4, -6, -8, -10, -12, 3, 6], [10, 3, 2, 1, 1, 1, 8, 3], k=changes)
            secondsPerChord = scoreDuration / len(progression)
            chordTime = scoreTime
            for steps in progression:
                print("Time: {:9.4f} chord: {} name: {}".format(chordTime, chord.toString(), chord.eOP().name()))
                voiceleadingNode.chord(chord, chordTime)
                chord = scale.transpose_degrees(chord, steps)
                chordTime = chordTime + secondsPerChord
            scale = modulate(scale, chord)
            return rescale

# Where the constellations overlap, the score could get bitonal. To minimize 
# that, we put the chords _outside_ the collection of constellations.

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
0dbfs = 1

alwayson    "ReverbSC"
alwayson    "MasterOutput"

connect "Harpsichord", "outleft", "ReverbSC", "inleft"
connect "Harpsichord", "outright", "ReverbSC", "inright"
connect "FilteredSines2", "outleft", "ReverbSC", "inleft"
connect "FilteredSines2", "outright", "ReverbSC", "inright"
connect "FilteredSines3", "outleft", "ReverbSC", "inleft"
connect "FilteredSines3", "outright", "ReverbSC", "inright"
connect "Blower", "outleft", "ReverbSC", "inleft"
connect "Blower", "outright", "ReverbSC", "inright"
connect "FMBell", "outleft", "ReverbSC", "inleft"
connect "FMBell", "outright", "ReverbSC", "inright"
connect "FMModerate", "outleft", "ReverbSC", "inleft"
connect "FMModerate", "outright", "ReverbSC", "inright"
connect "Xing", "outleft", "ReverbSC", "inleft"
connect "Xing", "outright", "ReverbSC", "inright"
connect "FMModulatedChorus", "outleft", "ReverbSC", "inleft"
connect "FMModulatedChorus", "outright", "ReverbSC", "inright"
connect "ReverbSC", "outleft", "MasterOutput", "inleft"
connect "ReverbSC", "outright", "MasterOutput", "inright"

gk_Harpsichord_level init -13
gk_Harpsichord_midi_dynamic_range init 127
gi_Harpsichord_harptable ftgen 0, 0, 65537, 7, -1, 1024, 1, 1024, -1
instr Harpsichord
i_instrument = p1
i_time = p2
p3 = 10
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_Harpsichord_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 82.4
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Harpsichord_level)
iHz = cpsmidinn(i_midi_key)
kHz = k(iHz)
aenvelope transeg 1.0, 40.0, -25.0, 0.0
apluck pluck i_amplitude, kHz, iHz, 0, 1
aharp poscil aenvelope, kHz, gi_Harpsichord_harptable
aharp2 balance apluck, aharp
a_signal	= (apluck + aharp2) * aenvelope
i_attack = .002
i_sustain = p3
i_release = 0.01
xtratim i_attack + i_release
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_signal * a_declicking * k_gain
#ifdef USE_SPATIALIZATION
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d %s\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1), strget(p12)
endin

gk_FilteredSines2_level init -4
gi_FilteredSines2_attack init 1
gi_FilteredSines2_release init 1
gi_FilteredSines2_bergeman ftgen 0, 0, 65537, 10, .28, 1, .74, .66, .78, .48, .05, .33, 0.12, .08, .01, .54, 0.19, .08, .05, 0.16, .01, 0.11, .3, .02, 0.2
instr FilteredSines2
; Author: Michael Bergeman
; Modified by: Michael Gogins
xtratim gi_FilteredSines2_attack + gi_FilteredSines2_release
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_overall_amps = 166
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
i_frequency = cpsmidinn(i_midi_key)
k_gain = ampdb(gk_FilteredSines2_level)
kHz = k(i_frequency)
koctave = octcps(kHz)
iattack init gi_FilteredSines2_attack
isustain init p3
irelease init gi_FilteredSines2_release
idb = 1.5
ip5 = gi_FilteredSines2_bergeman
ip3 = 5.0
ip6 = 0.9
ip7 = 1.4
kp8 = cpsoct(koctave - .01)
kp9 = cpsoct(koctave + .01)
isc = idb * .333
k1 linseg 40, ip3, 800, p3, 800, 0.06, 0.0
k2 linseg 440, ip3, 220, p3, 220, 0.06, 0.0
k3 linseg 0.0, ip6, 800, ip7, 200.0, p3, 200, 0.06, 0.0
k4 linseg 800, ip3, 40, p3, 40, 0.06, 0.0
k5 linseg 220, ip3, 440, p3, 440, 0.06, 0.0
k6 linseg isc, ip6, p3, ip7, p3, 0.06, 0.0
k7 linseg 0.0, ip6, 1, ip7, .3, p3, .1, 0.06, 0.0
a5 poscil k3, kp8, ip5
a6 poscil k3, kp8 * 0.999, ip5
a7 poscil k3, kp8 * 1.001, ip5
a1 = a5 + a6 + a7
a8 poscil k6, kp9, ip5
a9 poscil k6, kp9 * 0.999, ip5
a10 poscil k6, kp9 * 1.001, ip5
a11 = a8 + a9 + a10
a2 butterbp a1, k1, 40
a3 butterbp a2, k5, k2 * 0.8
a4 balance a3, a1
a12 butterbp a11, k4, 40
a13 butterbp a12, k2, k5 * 0.8
a14 balance a13, a11
a15 reverb2 a4, 5, 0.3
a16 reverb2 a4, 4, 0.2
a17 = (a15 + a4) * k7
a18 = (a16 + a4) * k7
a_signal = (a17 + a18)
i_attack = .002
i_sustain = p3
i_release = 0.01
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_signal * i_amplitude * a_declicking * k_gain * 1.88
#ifdef USE_SPATIALIZATION
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d %s\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1), strget(p12)
endin

gk_FilteredSines3_level init -2
gi_FilteredSines3_attack init 1
gi_FilteredSines3_release init 1
gi_FilteredSines3_bergeman ftgen 0, 0, 65537, 10, .28, 1, .74, .66, .78, .48, .05, .33, 0.12, .08, .01, .54, 0.19, .08, .05, 0.16, .01, 0.11, .3, .02, 0.2
instr FilteredSines3
; Author: Michael Bergeman
; Modified by: Michael Gogins
xtratim gi_FilteredSines3_attack + gi_FilteredSines3_release
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_overall_amps = 166
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
i_frequency = cpsmidinn(i_midi_key)
k_gain = ampdb(gk_FilteredSines3_level)
kHz = k(i_frequency)
koctave = octcps(kHz)
iattack init gi_FilteredSines3_attack
isustain init p3
irelease init gi_FilteredSines3_release
idb = 1.5
ip5 = gi_FilteredSines3_bergeman
ip3 = 5.0
ip6 = 0.9
ip7 = 1.4
kp8 = cpsoct(koctave - .011)
kp9 = cpsoct(koctave + .009)
isc = idb * .333
k1 linseg 40, ip3, 800, p3, 800, 0.06, 0.0
k2 linseg 440, ip3, 220, p3, 220, 0.06, 0.0
k3 linseg 0.0, ip6, 800, ip7, 200.0, p3, 200, 0.06, 0.0
k4 linseg 800, ip3, 40, p3, 40, 0.06, 0.0
k5 linseg 220, ip3, 440, p3, 440, 0.06, 0.0
k6 linseg isc, ip6, p3, ip7, p3, 0.06, 0.0
k7 linseg 0.0, ip6, 1, ip7, .3, p3, .1, 0.06, 0.0
a5 poscil k3, kp8, ip5
a6 poscil k3, kp8 * 0.999, ip5
a7 poscil k3, kp8 * 1.001, ip5
a1 = a5 + a6 + a7
a8 poscil k6, kp9, ip5
a9 poscil k6, kp9 * 0.999, ip5
a10 poscil k6, kp9 * 1.001, ip5
a11 = a8 + a9 + a10
a2 butterbp a1, k1, 40
a3 butterbp a2, k5, k2 * 0.8
a4 balance a3, a1
a12 butterbp a11, k4, 40
a13 butterbp a12, k2, k5 * 0.8
a14 balance a13, a11
a15 reverb2 a4, 5, 0.3
a16 reverb2 a4, 4, 0.2
a17 = (a15 + a4) * k7
a18 = (a16 + a4) * k7
a_signal = (a17 + a18)
i_attack = .002
i_sustain = p3
i_release = 0.01
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_signal * i_amplitude * a_declicking * k_gain * 1.88
#ifdef USE_SPATIALIZATION
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d %s\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1), strget(p12)
endin

gk_FMModerate_level init -4
gk_FMModerate_midi_dynamic_range init 127
gi_FMModerate_cosine ftgen 0, 0, 65537, 11, 1
instr FMModerate
; Authors: Michael Gogins
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_Harpsichord_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
i_overall_amps = 86 ; To start - change to observed value.
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_FMModerate_level)
iattack = 0.002
isustain = p3
idecay = 4.5
irelease = 0.05
xtratim iattack + idecay + irelease
icarrier = 1
imodulator = 1.5
ifmamplitude = 2
index = 3.5
ifrequencyb = i_frequency * 1.003
icarrierb = icarrier * 1.004
aindenv transegr 0.0, iattack, -7.0, 1.0, idecay, -10.0, 0.025, isustain, 0.0, 0.025, irelease, -7.0, 0.0
aindex = aindenv * index * ifmamplitude
aouta foscili 1.0, i_frequency, icarrier, imodulator, index, gi_FMModerate_cosine
aoutb foscili 1.0, ifrequencyb, icarrierb, imodulator, index, gi_FMModerate_cosine
a_signal = (aouta + aoutb) * aindenv
i_attack = .002
i_sustain = p3
i_release = 0.05
xtratim i_attack + i_sustain + i_release
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_signal * i_amplitude * a_declicking * k_gain
#ifdef USE_SPATIALIZATION
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d %s\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1), strget(p12)
endin

gk_Blower_level init 45
gk_Blower_grainDensity init 120
gk_Blower_grainDuration init 0.2
gk_Blower_grainAmplitudeRange init .05
gk_Blower_grainFrequencyRange init 8 / 1200
gk_Blower_midi_dynamic_range init 127
gi_Blower_grtab ftgen 0, 0, 65537, 10, 1, .3, .1, 0, .2, .02, 0, .1, .04
gi_Blower_wintab ftgen 0, 0, 65537, 10, 1, 0, .5, 0, .33, 0, .25, 0, .2, 0, .167
instr Blower
//////////////////////////////////////////////
// Original by Hans Mikelson.
// Adapted by Michael Gogins.
//////////////////////////////////////////////
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_Blower_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 132
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Blower_level)
iHz = i_frequency
ihertz = iHz
ip4 = i_amplitude
ip5 = iHz
ip6 = gi_Blower_grtab
ip7 = gi_Blower_wintab
ip8 = 0.033
ip8 = .002
ip9 = 150
ip9 = 100
ip10 = 1.6
ip10 = 3
idur = p3
iamp = i_amplitude ; p4
ifqc = iHz ; cpspch(p5)
igrtab = ip6
iwintab = ip7
ifrng = ip8
idens = ip9
ifade = ip10
igdur = 0.2
iattack = 0.5
i_sustain = p3
idecay = 1.5
xtratim iattack + idecay
kenvelope transegr 0.0, iattack / 2.0, 1.5, .5, iattack / 2.0, -1.5, 1, i_sustain, 0.0, 1, idecay / 2.0, 1.5, .5, idecay / 2.0, -1.5, 0
; kamp linseg 0, ifade, 1, idur - 2 * ifade, 1, ifade, 0
kamp = kenvelope
; Amp Fqc Dense AmpOff PitchOff GrDur GrTable WinTable MaxGrDur
aoutl grain ip4, ifqc, gk_Blower_grainDensity, gk_Blower_grainAmplitudeRange, gk_Blower_grainFrequencyRange, gk_Blower_grainDuration, igrtab, iwintab, 5
aoutr grain ip4, ifqc, gk_Blower_grainDensity, gk_Blower_grainAmplitudeRange, gk_Blower_grainFrequencyRange, gk_Blower_grainDuration, igrtab, iwintab, 5
a_signal = aoutl + aoutr
i_attack = .002
i_release = 0.01
xtratim i_attack + i_release
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_signal * i_amplitude * a_declicking * k_gain
#ifdef USE_SPATIALIZATION
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d %s\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1), strget(p12)
endin

gk_Xing_level init 0
instr Xing
; Author: Andrew Horner
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_overall_amps = 75
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
i_frequency = cpsmidinn(i_midi_key)
k_gain = ampdb(gk_Xing_level  )
isine ftgenonce 0, 0, 65537, 10, 1
iinstrument = p1
istarttime = p2
ioctave = p4
idur = p3
kfreq = k(i_frequency)
iamp = 1
inorm = 32310
aamp1 linseg 0,.001,5200,.001,800,.001,3000,.0025,1100,.002,2800,.0015,1500,.001,2100,.011,1600,.03,1400,.95,700,1,320,1,180,1,90,1,40,1,20,1,12,1,6,1,3,1,0,1,0
adevamp1 linseg 0, .05, .3, idur - .05, 0
adev1 poscil adevamp1, 6.7, isine, .8
amp1 = aamp1 * (1 + adev1)
aamp2 linseg 0,.0009,22000,.0005,7300,.0009,11000,.0004,5500,.0006,15000,.0004,5500,.0008,2200,.055,7300,.02,8500,.38,5000,.5,300,.5,73,.5,5.,5,0,1,1
adevamp2 linseg 0,.12,.5,idur-.12,0
adev2 poscil adevamp2, 10.5, isine, 0
amp2 = aamp2 * (1 + adev2)
aamp3 linseg 0,.001,3000,.001,1000,.0017,12000,.0013,3700,.001,12500,.0018,3000,.0012,1200,.001,1400,.0017,6000,.0023,200,.001,3000,.001,1200,.0015,8000,.001,1800,.0015,6000,.08,1200,.2,200,.2,40,.2,10,.4,0,1,0
adevamp3 linseg 0, .02, .8, idur - .02, 0
adev3 poscil adevamp3, 70, isine ,0
amp3 = aamp3 * (1 + adev3)
awt1 poscil amp1, i_frequency, isine
awt2 poscil amp2, 2.7 * i_frequency, isine
awt3 poscil amp3, 4.95 * i_frequency, isine
asig = awt1 + awt2 + awt3
arel linenr 1,0, idur, .06
a_signal = asig * arel * (iamp / inorm)
i_attack = .002
i_sustain = p3
i_release = 0.01
xtratim i_attack + i_release
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_signal * i_amplitude * a_declicking * k_gain
#ifdef USE_SPATIALIZATION
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d %s\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1), strget(p12)
endin

gk_FMModulatedChorus_level init 0
gi_FMModulatedChorus_attack init 0.003
gi_FMModulatedChorus_release init 0.01
gk_FMModulatedChorus_midi_dynamic_range init 127
gi_FMModulatedChorus_ln ftgen 0, 0, 65537, -12, 20.0 ; Unscaled ln(I(x)) from 0 to 20.0.
gi_FMModulatedChorus_cosine ftgen 0, 0, 65537, 11, 1 ; Cosine wave. Get that noise down on the most widely used table!
gi_FMModulatedChorus_sine ftgen 0, 0, 65537, 10, 1
instr FMModulatedChorus
//////////////////////////////////////////////
// Original by Hans Mikelson.
// Adapted by Michael Gogins.
//////////////////////////////////////////////
i_instrument = p1
i_time = p2
i_sustain = p3
xtratim gi_FMModulatedChorus_attack + gi_FMModulatedChorus_release
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_FMModulatedChorus_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 85.2
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_FMModulatedChorus_level)
i_modr_start = 0.3
i_modr_end = 4.2
; shift it.
ishift = 5.0 / 12000
; convert parameter 5 to oct.
ioct = i_midi_key
akadsr linsegr 0, gi_FMModulatedChorus_attack, 1, i_sustain, 1, gi_FMModulatedChorus_release, 0
akmodi linseg 0, gi_FMModulatedChorus_attack, 5, i_sustain, 2, gi_FMModulatedChorus_release, 0
; r moves from i_modr_start to i_modr_end in p3 secs.
akmodr linseg i_modr_start, i_sustain, i_modr_end
a1 = akmodi * (akmodr - 1 / akmodr) / 2
; a1*2 is argument normalized from 0-1.
a1ndx = abs(a1 * 2 / 20)
a2 = akmodi * (akmodr + 1 / akmodr) / 2
a3 tablei a1ndx, gi_FMModulatedChorus_ln, 1
ao1 oscili a1, i_frequency, gi_FMModulatedChorus_cosine
a4 = exp(-0.5 * a3 + ao1)
; Cosine
ao2 oscili a2 * i_frequency, i_frequency, gi_FMModulatedChorus_cosine
; Final output left
aoutl oscili 1 * akadsr * a4, ao2 + cpsmidinn(ioct + ishift), gi_FMModulatedChorus_sine
; Final output right
aoutr oscili 1 * akadsr * a4, ao2 + cpsmidinn(ioct - ishift), gi_FMModulatedChorus_sine
asignal = aoutl + aoutr
a_signal = asignal * i_amplitude
a_declicking linsegr 0, gi_FMModulatedChorus_attack, 1, i_sustain, 1, gi_FMModulatedChorus_release, 0
a_signal = a_signal * i_amplitude * a_declicking * k_gain
#ifdef USE_SPATIALIZATION
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d %s\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1), strget(p12)
endin

gk_FMBell_level init 0
gi_FMBell_cosine ftgen 0, 0, 65537, 11, 1
instr FMBell
; Authors: John ffitch, Michael Gogins
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
i_overall_amps = 90
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_FMBell_level)
kc1 = 5
kc2 = 5
kvdepth = 0.025
kvrate = 5.1
ifn1 = gi_FMBell_cosine
ifn2 = gi_FMBell_cosine
ifn3 = gi_FMBell_cosine
ifn4 = gi_FMBell_cosine
ivfn = gi_FMBell_cosine
aout fmbell i_amplitude, i_frequency, kc1, kc2, kvdepth, kvrate, ifn1, ifn2, ifn3, ifn4, ivfn
aenv transegr 0.0, .003, -6, 1.0, 9, -6, 0
a_signal = aout * aenv
i_attack = .0005  
i_sustain = p3
i_release = 0.01
xtratim i_attack + i_release
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_signal * a_declicking * k_gain * 1.2
#ifdef USE_SPATIALIZATION
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d %s\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1), strget(p12)
endin

gk_Reverb_feedback init 0.875
gk_Reverb_wet init 0.25
gi_Reverb_delay_modulation init 0.0075
gk_Reverb_frequency_cutoff init 14000
instr ReverbSC
gk_Reverb_dry = 1.0 - gk_Reverb_wet
aleftin init 0
arightin init 0
aleftout init 0
arightout init 0
aleftin inleta "inleft"
arightin inleta "inright"
aleftout, arightout reverbsc aleftin, arightin, gk_Reverb_feedback, gk_Reverb_frequency_cutoff, sr, gi_Reverb_delay_modulation
aleftoutmix = aleftin * gk_Reverb_dry + aleftout * gk_Reverb_wet
arightoutmix = arightin * gk_Reverb_dry + arightout * gk_Reverb_wet
outleta "outleft", aleftoutmix
outleta "outright", arightoutmix
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_MasterOutput_level init 6
gS_MasterOutput_filename init ""
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
i_amplitude_adjustment = ampdbfs(-3) / 32767
i_filename_length strlen gS_MasterOutput_filename
if i_filename_length > 0 goto filename_exists
goto filename_endif
filename_exists:
prints sprintf("Output filename: %s\\n", gS_MasterOutput_filename)
fout gS_MasterOutput_filename, 18, aleft * i_amplitude_adjustment, aright * i_amplitude_adjustment
filename_endif:
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin
'''

model.setCsoundOrchestra(csoundOrchestra)
model.setCsoundCommand(csoundCommand)
model.generate()
model.performMaster()
if rendering == 'soundfile':
    model.translateMaster()

