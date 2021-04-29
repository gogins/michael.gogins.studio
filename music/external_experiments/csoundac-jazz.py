'''
Author: Michael Gogins
'''

import CsoundAC
import os
import random
# Using the same random seed for each performance makes the performance 
# deterministic, not random.
random.seed(221)
import signal
import string
import sys
import traceback

print('Set "rendering" to:     "soundfile" or "audio".')
print
rendering = "audio"

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

import musx_csoundac

################################################################################
"""
This tutorial presents an implementation of an simple automatic jazz
program. The code is derived from a program originally written by
Erik Flister at CCRMA, Stanford University, as a project for his
undergraduate computer music class. His original program has been
simplified and adapted here to work with General MIDI
instruments. Flister's improviser generates music for a jazz trio
(piano, acoustic bass and percussion).

To run this script cd to the parent directory of musx_demos/ and do:
```bash
python3 -m musx_demos.jazz
```
"""


from musx.midi import MidiNote, MidiSeq, MidiFile
from musx.midi.gm import AcousticGrandPiano, AcousticBass
from musx.generators import cycle, choose, jumble
from musx.rhythm import intempo
from musx.scheduler import Scheduler
from musx.ran import odds, pick, between
from musx.tools import playfile, setmidiplayer
from musx.scales import keynum
import types


jazz_scale = [0, 2, 3, 5, 7, 9, 10, 12, 14]
"""The scale used by the improvisor."""


jazz_changes = keynum('bf3 ef4 bf3 bf ef4 ef bf3 bf f4 ef bf3 bf')
"""The chord changes for the piano and bass parts."""


jazz_tempo = 120
"""The tempo of the compostion."""


def jazz_high_hat(q, tmpo, ampl):
    """
    Plays the High Hat on the second and fourth quarter of every measure and
    rests on the first and third beats. Each sound lasts for the duration one
    triplet eighth note i.e. 1/3 of a beat.
    """
    rhy = intempo(1, tmpo)
    dur = intempo(1/3, tmpo)
    amp = .5
    pat = cycle(['r', 42, 'r', 42]) # 'r' is rest
    for _ in range(4):
        x = next(pat)
        if x != 'r':
            m = MidiNote(time=q.now, dur=dur, key=x, amp=amp * ampl, chan=9)
            q.out.addevent(m)
        yield rhy


def jazz_drums(q, tmpo, ampl):
    """
    Randomly selects between playing the snare, the bass drum or resting one
    quarter of the time. One tenth of the time it produces a very loud tone.
    """
    elec_snare = 40
    bass_drum = 35
    knums = choose(['r', elec_snare, bass_drum], [.25, 1, 1])
    rhys = cycle([2/3, 1/3])
    amps = choose([.7, .95], [1, .1])
    for _ in range(8):
        k = next(knums)
        a = next(amps)
        r = intempo(next(rhys), tmpo)
        if k != 'r':
            m = MidiNote(time=q.now, dur=r, key=k, amp=a * ampl, chan=9)
            q.out.addevent(m)
        yield r


def jazz_cymbals(q, tmpo, ampl):
    """
    The cymbals process performs a constant stream of triplet eighths in
    which the ride1 cymbal is played on the beginning of every quarter
    note. The second and third triplets of each beat are either rests or
    a random choice between ride1, ride2 or a rest.  This is the beat
    map for a measure of the process, where '1' means the ride cymbal 1 is
    played, '-' means a rest, and 'x' means a random choice between ride1,
    ride2 or a rest:

    ```text
    Triplet 8th: 1  2  3    4  5  6    7  8  9   10 11 12
    Cymbals:     1  -  x    1  -  1    1  x  x    1  x  1 
    ```
    """
    ride1 = 51
    ride2 = 59
    rhy = intempo(1/3, tmpo)
    amps = cycle([.6, .5, .9, .7, .5, 1, .6, .5, .9, .7, .5, 1])

    def subpat(wt):
        r1 = choose([ride1, 'r'], [1, wt])
        r2 = choose([ride2, 'r'], [1, wt])
        return choose([r1, r2], [1.5, 1])

    # the events that happen on each triplet of the measure
    meas = {0: ride1,  1: 'r',        2: subpat(5),
            3: ride1,  4: 'r',        5: ride1,
            6: ride1,  7: subpat(7),  8: subpat(7),
            9: ride1, 10: subpat(3), 11: ride1}
    for b in meas:
        k = meas[b]
        if k != 'r':
            if type(k) is not int: # k is a subpattern
                k = next(next(k))
            if k != 'r':
                a = next(amps)
                m = MidiNote(time=q.now, dur=rhy, key=k, amp=a*ampl, chan=9)
                q.out.addevent(m)
        yield rhy


def jazz_piano(q, on, tmpo, ampl):
    """
    The jazz piano improvises jazz chords based on a pattern of root
    changes and a scale pattern that is transposed to each root. The
    piano randomly choose between playing triplet eighths or straight
    eights for a given measure.
    """
    reps = odds(.65, 8, 12)
    scal = jumble(jazz_scale)
    rhys = cycle([2/3, 1/3] if reps == 8 else [1/3])
    for _ in range(reps):
        r = intempo(next(rhys), tmpo)
        l = [] if odds(2/5) else [next(scal) for _ in range(between(1,9))]
        for k in l:
            a = pick(.4, .5, .6, .7, .8)
            m = MidiNote(time=q.now, dur=r, key=on+k, amp=a, chan=0)
            q.out.addevent(m)
        yield r


def jazz_bass(q, on, tmpo, ampl):
    """
    The bass part plays a melodic line built out of tones from the jazz-scale's
    tonic seventh chord alternating with color tones outside the tonic chord.
    The bass plays a series of 12 triplets per measure, on each triplet only one of
    the two sets is possible. On all but the first triplet a rest is also possible.
    """
    # 5 possible patterns for triplets 1-4
    a = choose(['trrc', 'trrr', 'trtc', 'tctc', 'tctr'], [1.0, .25, .22, .065, .014])
    # 5 possible patterns for 5-7
    b = choose(['rrt', 'rrr', 'rct', 'tct', 'tcr'], [1.0, .25, .22, .038, .007])
    # 5 possible patterns for 8-10
    c = choose(['rrc', 'rtc', 'rrr', 'ctc', 'ctr'], [1.0, .415, .25, .11, .018])
    # two possible values for 11
    d = choose(['r', 't'], [1, .25])
    # two possible values for 12
    e = choose(['r', 'c'], [1, .25])
    # the measure map
    meas = next(a) + next(b) + next(c) + next(d) + next(e)

    rhy = intempo(1/3, tmpo)
    tonics = choose([jazz_scale[i] for i in [0, 2, 4, 6, 7]])
    colors = choose([jazz_scale[i] for i in [1, 3, 5, 6, 8]])
    amps = cycle([.5, .4, 1.0, .9, .4, .9, .5, .4, 1.0, .9, .5, .9])
    durs = cycle([2/3, 1/3, 1/3])

    for x in meas:
        k = -1
        if x == 't':
            k = next(tonics)
        elif x == 'c':
            k = next(colors)
        if k > -1:
            a = next(amps)
            d = next(durs)
            m = MidiNote(time=q.now, dur=d, key=on+k, amp=ampl*a, chan=1)
            q.out.addevent(m)
        yield rhy


def jazz_combo(q, measures, tempo):
    """
    The conductor process adds combo parts for each meaure to the schedule 
    generate sound. By adding parts at each measure the conductor could make
    changes to the overall texture, amplitude etc, as the pieces progresses.
    """ 
    roots = cycle(jazz_changes)
    ampl = .9
    for meas in range(measures):
        root = next(roots)
        if  0 == meas % 12:
           ampl = between(.5, 1)
        q.compose(jazz_piano(q, root, tempo, ampl))
        q.compose(jazz_cymbals(q, tempo, ampl))
        q.compose(jazz_high_hat(q, tempo, ampl))
        q.compose(jazz_drums(q, tempo, ampl))
        q.compose(jazz_bass(q, root-12, tempo, ampl))
        yield intempo(4,tempo)
    
# It's good practice to add any metadata such as tempo, midi instrument
# assignments, micro tuning, etc. to track 0 in your midi file.
t0 = MidiSeq.metaseq(ins={0: AcousticGrandPiano, 1: AcousticBass})
# Track 1 will hold the composition.
t1 = MidiSeq()
# Create a scheduler and give it t1 as its output object.
q = Scheduler(t1)

# c=[]
# for t in range(0, 15, 2): # t -> 0 2 4 6 8 10 12 14
#     c += [[t, jazz_high_hat(q, 120, .9)], 
#           [t, jazz_drums(q, 120, .9)],
#           [t, jazz_cymbals(q, 120, .9)],
#           [t, jazz_piano(q, 58, 120, .9)],
#           [t, jazz_bass(q, 46, 120, .9)]
#           ]

q.compose(jazz_combo(q, 48, 120))
# Write a midi file with our track data.
f = MidiFile("jazz.mid", [t0, t1]).write()
# To automatially play demos use setmidiplayer() to assign a shell
# command that will play midi files on your computer. Example:
#   setmidiplayer("fluidsynth -iq -g1 /usr/local/sf/MuseScore_General.sf2")
print(f"Wrote '{f.pathname}'.")

csoundac_score_node = CsoundAC.ScoreNode()
csoundac_score = csoundac_score_node.getScore()
musx_csoundac.to_csoundac_score(f, csoundac_score)

print("Generated:")
print(csoundac_score.getCsoundScore())

orc = '''
sr = 48000
ksmps = 128
nchnls = 2
0dbfs = 100

; Ensure the same random stream for each rendering.
; rand, randh, randi, rnd(x) and birnd(x) are not affected by seed.

seed 38493

gi_Pianoteq vstinit "/home/mkg/Pianoteq\ 7/x86-64bit/Pianoteq\ 7.so", 0

alwayson "PianoOutPianoteq"
alwayson "ReverbSC"
alwayson "MasterOutput"

connect "ChebyshevMelody", "outleft", "ReverbSC", "inleft"
connect "ChebyshevMelody", "outright", "ReverbSC", "inright"
connect "FMWaterBell", "outleft", "ReverbSC", "inleft"
connect "FMWaterBell", "outright", "ReverbSC", "inright"
connect "Harpsichord", "outleft", "ReverbSC", "inleft"
connect "Harpsichord", "outright", "ReverbSC", "inright"
connect "Rhodes", "outleft", "ReverbSC", "inleft"
connect "Rhodes", "outright", "ReverbSC", "inright"
connect "ZakianFlute", "outleft", "ReverbSC", "inleft"
connect "ZakianFlute", "outright", "ReverbSC", "inright"

connect "PianoOutPianoteq", "outleft", "ReverbSC", "inleft"
connect "PianoOutPianoteq", "outright", "ReverbSC", "inright"
connect "ReverbSC", "outleft", "MasterOutput", "inleft"
connect "ReverbSC", "outright", "MasterOutput", "inright"

gk_PianoNotePianoteq_midi_dynamic_range init 127
instr PianoNotePianoteq
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_PianoNotePianoteq_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
i_homogeneity = p11
instances active p1
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
i_pitch_correction = 44100 / sr
; prints "Pitch factor:   %9.4f\\n", i_pitch_correction
vstnote gi_Pianoteq, 0, i_midi_key, i_midi_velocity, i_duration
endin

gk_ZakianFlute_midi_dynamic_range init 80
gk_ZakianFlute_level init 0
gk_ZakianFlute_pan init .5
gi_ZakianFLute_seed init .5
gi_ZakianFLute_f2  ftgen 0, 0, 16, -2, 40, 40, 80, 160, 320, 640, 1280, 2560, 5120, 10240, 10240
gi_ZakianFlute_f26 ftgen 0, 0, 65537, -10, 2000, 489, 74, 219, 125, 9, 33, 5, 5
gi_ZakianFlute_f27 ftgen 0, 0, 65537, -10, 2729, 1926, 346, 662, 537, 110, 61, 29, 7
gi_ZakianFlute_f28 ftgen 0, 0, 65537, -10, 2558, 2012, 390, 361, 534, 139, 53, 22, 10, 13, 10
gi_ZakianFlute_f29 ftgen 0, 0, 65537, -10, 12318, 8844, 1841, 1636, 256, 150, 60, 46, 11
gi_ZakianFlute_f30 ftgen 0, 0, 65537, -10, 1229, 16, 34, 57, 32
gi_ZakianFlute_f31 ftgen 0, 0, 65537, -10, 163, 31, 1, 50, 31
gi_ZakianFlute_f32 ftgen 0, 0, 65537, -10, 4128, 883, 354, 79, 59, 23
gi_ZakianFlute_f33 ftgen 0, 0, 65537, -10, 1924, 930, 251, 50, 25, 14
gi_ZakianFlute_f34 ftgen 0, 0, 65537, -10, 94, 6, 22, 8
gi_ZakianFlute_f35 ftgen 0, 0, 65537, -10, 2661, 87, 33, 18
gi_ZakianFlute_f36 ftgen 0, 0, 65537, -10, 174, 12
gi_ZakianFlute_f37 ftgen 0, 0, 65537, -10, 314, 13
gi_ZakianFlute_wtsin ftgen 0, 0, 65537, 10, 1

instr ZakianFlute
; Author: Lee Zakian
; Adapted by: Michael Gogins
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_overall_amps = 65.2
i_normalization = ampdb(-i_overall_amps) / 2
i_midi_dynamic_range = i(gk_ZakianFlute_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_ZakianFlute_level)
iattack = .002
isustain = p3
irelease = .3
xtratim iattack + irelease
iHz = cpsmidinn(i_midi_key)
kHz = k(iHz)
aenvelope transeg 1.0, 20.0, -10.0, 0.05
ip3 = (p3 < 3.0 ? p3 : 3.0)
; parameters
; p4 overall amplitude scaling factor
ip4 init i_amplitude
; p5 pitch in Hertz (normal pitch range: C4-C7)
ip5 init iHz
; p6 percent vibrato depth, recommended values in range [-1., +1.]
ip6 init 0.5
; 0.0 -> no vibrato
; +1. -> 1% vibrato depth, where vibrato rate increases slightly
; -1. -> 1% vibrato depth, where vibrato rate decreases slightly
; p7 attack time in seconds
; recommended value: .12 for slurred notes, .06 for tongued notes
; (.03 for short notes)
ip7 init .08
; p8 decay time in seconds
; recommended value: .1 (.05 for short notes)
ip8 init .08
; p9 overall brightness / filter cutoff factor
; 1 -> least bright / minimum filter cutoff frequency (40 Hz)
; 9 -> brightest / maximum filter cutoff frequency (10,240Hz)
ip9 init 5
; initial variables
iampscale = ip4 ; overall amplitude scaling factor
ifreq = ip5 ; pitch in Hertz
ivibdepth = abs(ip6*ifreq/100.0) ; vibrato depth relative to fundamental frequency
iattack = ip7 * (1.1 - .2*gi_ZakianFLute_seed) ; attack time with up to +-10% random deviation
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947) ; reset gi_ZakianFLute_seed
idecay = ip8 * (1.1 - .2*gi_ZakianFLute_seed) ; decay time with up to +-10% random deviation
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
ifiltcut tablei ip9, gi_ZakianFLute_f2 ; lowpass filter cutoff frequency
iattack = (iattack < 6/kr ? 6/kr : iattack) ; minimal attack length
idecay = (idecay < 6/kr ? 6/kr : idecay) ; minimal decay length
isustain = p3 - iattack - idecay
p3 = (isustain < 5/kr ? iattack+idecay+5/kr : p3) ; minimal sustain length
isustain = (isustain < 5/kr ? 5/kr : isustain)
iatt = iattack/6
isus = isustain/4
idec = idecay/6
iphase = gi_ZakianFLute_seed ; use same phase for all wavetables
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
; vibrato block
; kvibdepth linseg .1, .8*p3, 1, .2*p3, .7
kvibdepth linseg .1, .8*ip3, 1, isustain, 1, .2*ip3, .7
kvibdepth = kvibdepth* ivibdepth ; vibrato depth
kvibdepthr randi .1*kvibdepth, 5, gi_ZakianFLute_seed ; up to 10% vibrato depth variation
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
kvibdepth = kvibdepth + kvibdepthr
ivibr1 = gi_ZakianFLute_seed ; vibrato rate
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
ivibr2 = gi_ZakianFLute_seed
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
if ip6 < 0 goto vibrato1
kvibrate linseg 2.5+ivibr1, p3, 4.5+ivibr2 ; if p6 positive vibrato gets faster
 goto vibrato2
vibrato1:
ivibr3 = gi_ZakianFLute_seed
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
kvibrate linseg 3.5+ivibr1, .1, 4.5+ivibr2, p3-.1, 2.5+ivibr3 ; if p6 negative vibrato gets slower
vibrato2:
kvibrater randi .1*kvibrate, 5, gi_ZakianFLute_seed ; up to 10% vibrato rate variation
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
kvibrate = kvibrate + kvibrater
kvib oscili kvibdepth, kvibrate, gi_ZakianFlute_wtsin
ifdev1 = -.03 * gi_ZakianFLute_seed ; frequency deviation
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
ifdev2 = .003 * gi_ZakianFLute_seed
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
ifdev3 = -.0015 * gi_ZakianFLute_seed
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
ifdev4 = .012 * gi_ZakianFLute_seed
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
kfreqr linseg ifdev1, iattack, ifdev2, isustain, ifdev3, idecay, ifdev4
kfreq = kHz * (1 + kfreqr) + kvib
if ifreq < 427.28 goto range1 ; (cpspch(8.08) + cpspch(8.09))/2
if ifreq < 608.22 goto range2 ; (cpspch(9.02) + cpspch(9.03))/2
if ifreq < 1013.7 goto range3 ; (cpspch(9.11) + cpspch(10.00))/2
goto range4
; wavetable amplitude envelopes
range1: ; for low range tones
kamp1 linseg 0, iatt, 0.002, iatt, 0.045, iatt, 0.146, iatt, \
0.272, iatt, 0.072, iatt, 0.043, isus, 0.230, isus, 0.000, isus, \
0.118, isus, 0.923, idec, 1.191, idec, 0.794, idec, 0.418, idec, \
0.172, idec, 0.053, idec, 0
kamp2 linseg 0, iatt, 0.009, iatt, 0.022, iatt, -0.049, iatt, \
-0.120, iatt, 0.297, iatt, 1.890, isus, 1.543, isus, 0.000, isus, \
0.546, isus, 0.690, idec, -0.318, idec, -0.326, idec, -0.116, idec, \
-0.035, idec, -0.020, idec, 0
kamp3 linseg 0, iatt, 0.005, iatt, -0.026, iatt, 0.023, iatt, \
0.133, iatt, 0.060, iatt, -1.245, isus, -0.760, isus, 1.000, isus, \
0.360, isus, -0.526, idec, 0.165, idec, 0.184, idec, 0.060, idec, \
0.010, idec, 0.013, idec, 0
iwt1 = gi_ZakianFlute_f26 ; wavetable numbers
iwt2 = gi_ZakianFlute_f27
iwt3 = gi_ZakianFlute_f28
inorm = 3949
goto end
range2: ; for low mid-range tones
kamp1 linseg 0, iatt, 0.000, iatt, -0.005, iatt, 0.000, iatt, \
0.030, iatt, 0.198, iatt, 0.664, isus, 1.451, isus, 1.782, isus, \
1.316, isus, 0.817, idec, 0.284, idec, 0.171, idec, 0.082, idec, \
0.037, idec, 0.012, idec, 0
kamp2 linseg 0, iatt, 0.000, iatt, 0.320, iatt, 0.882, iatt, \
1.863, iatt, 4.175, iatt, 4.355, isus, -5.329, isus, -8.303, isus, \
-1.480, isus, -0.472, idec, 1.819, idec, -0.135, idec, -0.082, idec, \
-0.170, idec, -0.065, idec, 0
kamp3 linseg 0, iatt, 1.000, iatt, 0.520, iatt, -0.303, iatt, \
0.059, iatt, -4.103, iatt, -6.784, isus, 7.006, isus, 11, isus, \
12.495, isus, -0.562, idec, -4.946, idec, -0.587, idec, 0.440, idec, \
0.174, idec, -0.027, idec, 0
iwt1 = gi_ZakianFlute_f29
iwt2 = gi_ZakianFlute_f30
iwt3 = gi_ZakianFlute_f31
inorm = 27668.2
goto end
range3: ; for high mid-range tones
kamp1 linseg 0, iatt, 0.005, iatt, 0.000, iatt, -0.082, iatt, \
0.36, iatt, 0.581, iatt, 0.416, isus, 1.073, isus, 0.000, isus, \
0.356, isus, .86, idec, 0.532, idec, 0.162, idec, 0.076, idec, 0.064, \
idec, 0.031, idec, 0
kamp2 linseg 0, iatt, -0.005, iatt, 0.000, iatt, 0.205, iatt, \
-0.284, iatt, -0.208, iatt, 0.326, isus, -0.401, isus, 1.540, isus, \
0.589, isus, -0.486, idec, -0.016, idec, 0.141, idec, 0.105, idec, \
-0.003, idec, -0.023, idec, 0
kamp3 linseg 0, iatt, 0.722, iatt, 1.500, iatt, 3.697, iatt, \
0.080, iatt, -2.327, iatt, -0.684, isus, -2.638, isus, 0.000, isus, \
1.347, isus, 0.485, idec, -0.419, idec, -.700, idec, -0.278, idec, \
0.167, idec, -0.059, idec, 0
iwt1 = gi_ZakianFlute_f32
iwt2 = gi_ZakianFlute_f33
iwt3 = gi_ZakianFlute_f34
inorm = 3775
goto end
range4: ; for high range tones
kamp1 linseg 0, iatt, 0.000, iatt, 0.000, iatt, 0.211, iatt, \
0.526, iatt, 0.989, iatt, 1.216, isus, 1.727, isus, 1.881, isus, \
1.462, isus, 1.28, idec, 0.75, idec, 0.34, idec, 0.154, idec, 0.122, \
idec, 0.028, idec, 0
kamp2 linseg 0, iatt, 0.500, iatt, 0.000, iatt, 0.181, iatt, \
0.859, iatt, -0.205, iatt, -0.430, isus, -0.725, isus, -0.544, isus, \
-0.436, isus, -0.109, idec, -0.03, idec, -0.022, idec, -0.046, idec, \
-0.071, idec, -0.019, idec, 0
kamp3 linseg 0, iatt, 0.000, iatt, 1.000, iatt, 0.426, iatt, \
0.222, iatt, 0.175, iatt, -0.153, isus, 0.355, isus, 0.175, isus, \
0.16, isus, -0.246, idec, -0.045, idec, -0.072, idec, 0.057, idec, \
-0.024, idec, 0.002, idec, 0
iwt1 = gi_ZakianFlute_f35
iwt2 = gi_ZakianFlute_f36
iwt3 = gi_ZakianFlute_f37
inorm = 4909.05
goto end
end:
kampr1 randi .02*kamp1, 10, gi_ZakianFLute_seed ; up to 2% wavetable amplitude variation
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
kamp1 = kamp1 + kampr1
kampr2 randi .02*kamp2, 10, gi_ZakianFLute_seed ; up to 2% wavetable amplitude variation
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
kamp2 = kamp2 + kampr2
kampr3 randi .02*kamp3, 10, gi_ZakianFLute_seed ; up to 2% wavetable amplitude variation
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
kamp3 = kamp3 + kampr3
awt1 poscil kamp1, kfreq, iwt1, iphase ; wavetable lookup
awt2 poscil kamp2, kfreq, iwt2, iphase
awt3 poscil kamp3, kfreq, iwt3, iphase
asig = awt1 + awt2 + awt3
asig = asig*(iampscale/inorm)
kcut linseg 0, iattack, ifiltcut, isustain, ifiltcut, idecay, 0 ; lowpass filter for brightness control
afilt tone asig, kcut
a_signal balance afilt, asig
i_attack = .002
i_sustain = p3
i_release = 0.01
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
a_signal *= .7
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

//////////////////////////////////////////////
// Original by Steven Yi.
// Adapted by Michael Gogins.
//////////////////////////////////////////////
gk_FMWaterBell_level init 0
gi_FMWaterBell_attack init 0.002
gi_FMWaterBell_release init 0.01
gi_FMWaterBell_sustain init 20
gi_FMWaterBell_sustain_level init .1
gk_FMWaterBell_index init .5
gk_FMWaterBell_crossfade init .5
gk_FMWaterBell_vibrato_depth init 0.05
gk_FMWaterBell_vibrato_rate init 6
gk_FMWaterBell_midi_dynamic_range init 127
gi_FMWaterBell_cosine ftgen 0, 0, 65537, 11, 1
instr FMWaterBell
i_instrument = p1
i_time = p2
i_duration = p3
; One of the envelopes in this instrument should be releasing, and use this:
i_sustain = 1000
;xtratim gi_FMWaterBell_attack + gi_FMWaterBell_release
xtratim gi_FMWaterBell_attack + gi_FMWaterBell_release
i_midi_key = p4
i_midi_dynamic_range = i(gk_FMWaterBell_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 81
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization * 1.6
k_gain = ampdb(gk_FMWaterBell_level)
a_signal fmbell	1, i_frequency, gk_FMWaterBell_index, gk_FMWaterBell_crossfade, gk_FMWaterBell_vibrato_depth, gk_FMWaterBell_vibrato_rate, gi_FMWaterBell_cosine, gi_FMWaterBell_cosine, gi_FMWaterBell_cosine, gi_FMWaterBell_cosine, gi_FMWaterBell_cosine ;, gi_FMWaterBell_sustain
;a_envelope linsegr 0, gi_FMWaterBell_attack, 1, i_sustain, gi_FMWaterBell_sustain_level, gi_FMWaterBell_release, 0
a_envelope linsegr 0, gi_FMWaterBell_attack, 1, i_sustain, 1, gi_FMWaterBell_release, 0
; ares transegr ia, idur, itype, ib [, idur2] [, itype] [, ic] ...
; a_envelope transegr 0, gi_FMWaterBell_attack, 12, 1, i_sustain, 12, gi_FMWaterBell_sustain_level, gi_FMWaterBell_release, 12, 0
a_signal = a_signal * i_amplitude * a_envelope * k_gain
;_signal = a_signal * i_amplitude * k_gain
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
; printks "FMWaterBell    i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\\n", 1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_out_left)), dbamp(rms(a_out_right))
endin

gk_Harpsichord_midi_dynamic_range init 127
gk_Harpsichord_level init 0
gk_Harpsichord_pick init .075
gk_Harpsichord_reflection init .5
gk_Harpsichord_pluck init .75
gi_Harpsichord_harptable ftgen 0, 0, 65537, 7, -1, 1024, 1, 1024, -1
instr Harpsichord
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
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 110
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Harpsichord_level)
iHz = cpsmidinn(i_midi_key)
kHz = k(iHz)
aenvelope transeg 1.0, 40.0, -25.0, 0.0
apluck pluck i_amplitude, kHz, iHz, 0, 1
aharp poscil aenvelope, kHz, gi_Harpsichord_harptable
aharp2 balance apluck, aharp
a_signal	= (apluck + aharp2)
i_attack = .0005
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
;printks "Harpsichord      %9.4f   %9.4f\\n", 0.5, a_out_left, a_out_right
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_ChebyshevMelody_level init 0
gi_ChebyshevMelody_attack init 0.003
gi_ChebyshevMelody_release init 0.01
gk_ChebyshevMelody_midi_dynamic_range init 127
gk_ChebyshevMelody_level init 0
gi_ChebyshevMelody_chebyshev ftgen 0, 0, 65537, -7, -1, 150, 0.1, 110, 0, 252, 0
gi_ChebyshevMelody_sine ftgen 0, 0, 65537, 10, 1
gi_ChebyshevMelody_cook3 ftgen 0, 0, 65537, 10, 1, .4, 0.2, 0.1, 0.1, .05
instr ChebyshevMelody
///////////////////////////////////////////////////////
// Original by Jon Nelson.
// Adapted by Michael Gogins.
///////////////////////////////////////////////////////
i_instrument = p1
i_time = p2
i_sustain = p3
xtratim gi_ChebyshevMelody_attack + gi_ChebyshevMelody_release
i_midi_key = p4
i_midi_dynamic_range = i(gk_ChebyshevMelody_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
// Spatial location is specified in Ambisonic coordinates.
k_space_front_to_back = p6
// AKA stereo pan.
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
// Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 72
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_ChebyshevMelody_level)

iattack = .01
isustain = p3
irelease = .01
p3 = iattack + isustain + irelease
iHz = cpsmidinn(i_midi_key)
iamplitude = ampdb(i_midi_velocity) * 7.
ip6 = gi_ChebyshevMelody_chebyshev
kHz = k(iHz)
idB = i_midi_velocity
i1 = iHz
k100 randi 1,0.05
ak101 poscil 1, 5 + k100, gi_ChebyshevMelody_sine
ak102 linseg 0, .5, 1, p3, 1
k100 = i1 + (ak101 * ak102)
; Envelope for driving oscillator.
ip3 init 3.0
; k1 linenr 0.5, ip3 * .3, ip3 * 2, 0.01
ak1 linseg 0, ip3 * .3, .5, ip3 * 2, 0.01, isustain, 0.01, irelease, 0
; k2 line 1, p3, .5
ak2 linseg 1.0, ip3, .5, isustain, .5, irelease, 0
ak1 = ak2 * ak1
; Amplitude envelope.
ak10 expseg 0.0001, iattack, 1.0, isustain, 0.8, irelease, .0001
ak10 = (ak10 - .0001)
; Power to partials.
k20 linseg 1.485, iattack, 1.5, (i_sustain + irelease), 1.485
; a1-3 are for cheby with p6=1-4
a1 poscil ak1, k100 - .25, gi_ChebyshevMelody_cook3
; Tables a1 to fn13, others normalize,
a2 tablei a1, ip6, 1, .5
a3 balance a2, a1
; Try other waveforms as well.
a4 foscili 1, k100 + .04, 1, 2.000, k20, gi_ChebyshevMelody_sine
a5 poscil 1, k100, gi_ChebyshevMelody_sine
a6 = ((a3 * .1) + (a4 * .1) + (a5 * .8)) * ak10
a7 comb a6, .5, 1 / i1
a8 = (a6 * .9) + (a7 * .1)
asignal balance a8, a1
a_declicking linsegr 0, gi_ChebyshevMelody_attack, 1, i_sustain, 1, gi_ChebyshevMelody_release, 0
a_signal = asignal * i_amplitude * a_declicking * k_gain
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
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
printks "ChebyshevMelody i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\\n", .1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_out_left)), dbamp(rms(a_out_right))
endin

gk_Rhodes_level init 0
gk_Rhodes_midi_dynamic_range init 127
gi_Rhodes_sine ftgen 0, 0, 65537, 10, 1
gi_Rhodes_cosine ftgen 0, 0, 65537, 11, 1
gi_Rhodes_blank ftgen 0, 0, 65537, 10, 0 ; Blank wavetable for some Cook FM opcodes.
instr Rhodes
; Authors: Perry Cook, John ffitch, Michael Gogins
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_Rhodes_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
i_midi_velocity = p5
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
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
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_PianoOutPianoteq_level init 0
gi_PianoOutPianoteq_print init 1
gk_PianoOutPianoteq_front_to_back init 0
gk_PianoOutPianoteq_left_to_right init 1/7
gk_PianoOutPianoteq_bottom_to_top init 0
instr PianoOutPianoteq
; Should be "D4 Daily Practice".
vstprogset gi_Pianoteq, 0
; Sustain off.
vstparamset gi_Pianoteq, 0, 0
; Reverb off.
vstparamset gi_Pianoteq, 72, 0
k_gain = ampdb(gk_PianoOutPianoteq_level)
i_overall_amps = 89
i_normalization = ampdb(-i_overall_amps) * 2
i_amplitude = ampdb(80) * i_normalization
if gi_PianoOutPianoteq_print == 1 then
  vstinfo gi_Pianoteq
endif
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
ainleft init 0
ainright init 0
aoutleft, aoutright vstaudio gi_Pianoteq, ainleft, ainright
a_signal = aoutleft + aoutright
a_signal *= k_gain
a_signal *= i_amplitude
a_out_left, a_out_right pan2 a_signal, gk_PianoOutPianoteq_left_to_right
#ifdef USE_SPATIALIZATION
a_signal = a_out_left + a_out_right
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, gk_PianoOutPianoteq_front_to_back, gk_PianoOutPianoteq_left_to_right, gk_PianoOutPianoteq_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
; printks "PianoOutPt     L %9.4f R %9.4f l %9.4f\\n", 0.5, a_out_left, a_out_right, gk_Piano_level
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_Reverb_feedback init 0.875
gk_Reverb_wet init 0.5
gi_Reverb_delay_modulation init 0.0075
gk_Reverb_frequency_cutoff init 15000
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
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

; It is important for levels to be evenly balanced _on average_! This 
; enables instruments to come forward and recede according to their 
; response to MIDI velocity.

gk_PianoOutPianoteq_level init  -8 
gk_ZakianFlute_level init       25
gk_FMWaterBell_level init       24;16
gk_ChebyshevMelody_level init   28;19
gk_Harpsichord_level init       27
gk_Rhodes_level init            31

gk_Reverb_wet init 0.25
gk_Reverb_feedback init 0.85
gi_Reverb_delay_modulation init 0.0875
gk_Reverb_frequency_cutoff init 14000
'''
rescale = CsoundAC.Rescale()
rescale.addChild(csoundac_score_node)
rescale.setRescale(CsoundAC.Event.INSTRUMENT, bool(1), bool(1), 1, 3.99)
rescale.setRescale(CsoundAC.Event.VELOCITY, bool(1), bool(1), 50, 20)
model.addChild(rescale)
model.setCsoundOrchestra(orc)
model.setCsoundCommand(csound_command)
model.generate()
# Fix ending. Instruments would drop out, sustain till the end. Some notes 
# course will decay first.
score = model.getScore()
score_duration = score.getDuration() + 4.
sounding = set()
score.save(model.getMidifileFilepath())
model.performMaster()
if rendering == 'soundfile':
    model.translateMaster()

