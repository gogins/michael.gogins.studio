import copy
import CsoundAC
import math
import os
import string
import sys
sys.path.append('/Users/michaelgogins/csound-ac/reaper')
import types
import time
from reaper_python import *
for p in sys.path:
    RPR_ShowConsoleMsg(p + '\n')
import ac_reaper

began = time.time()

def order(c):
    chord = list(c)
    chord.sort()
    return tuple(chord)

# Return the pitch-class for pitch or chord n.
def pc(n):
    t = type(n)
    if t in (tuple, list):
        l = []
        for e in n:
            l.append(pc(e))
        return tuple(l)
    else:
        return n % 12.0
    
# Perform pitch-class transposition of pitch or chord p
# by n semitones.
def T(p, n):
    t = type(p)
    if t in (tuple, list):
        l = []
        for e in p:
            l.append(T(e, n))
        return tuple(l)
    else:
        return pc(p + n)
        
# Invert the pitch or chord p around n.
def I(p, n):
    t = type(p)
    if t in (tuple, list):
        l = []
        for e in p:
            l.append(I(e, n))
        return tuple(l)
    else:
        return pc(-p + n)
        
# Invert chord c by exchange.
def K(c):
    n = c[0] + c[1]
    return I(c, n)
    
# Return True if Y is a T-form of X.
# g is the generator of T.
def Tform(X, Y, g=1.0):
    i = 0.0
    pcsx = pc(X)
    while i < 12.0:
        ty = T(Y, i)
        pcsty = pc(ty)
        if pcsx == pcsty:
            return True
        i = i + g
    return False
        
# Return True if Y is an I-form of X.
# g is the generator of I.
def Iform(X, Y, g=1.0):
    i = 0.0
    pcsx = pc(X)
    while i < 12.0:
        iy = I(Y, i)
        pcsiy = pc(iy)
        if pcsx == pcsiy:
            return True
        i = i + g
    return False
    
# Contextually transpose chord c with respect to chord s by n.
# g is the generator of transpositions.
def Q(c, n, s, g = 1.0):
    if Tform(c, s, g):
        #print('%s is a T-form of %s' % (c, s))
        return T(c, n)
    if Iform(c, s, g):
        #print('%s is an I-form of %s' % (c, s))
        return T(c, -n)
    return None
        
voicingsForPcs = {}

def incrementVoicing(chord, range_):
    for place in range(len(chord)):
        pitch = chord[place] + 12.0
        if pitch <= range_:
            chord[place] = pitch
            return tuple(chord)
        prime = CsoundAC.Voicelead.primeChord(chord)
        chord[place] = prime[place]
    return None

def decrementVoicing(chord, range_):
    for place in range(len(chord)):
        pitch = chord[place] - 12.0
        if pitch >= 0:
            chord[place] = pitch
            return tuple(chord)
        prime = list(CsoundAC.Voicelead.primeChord(chord))
        top = False
        while not top:
            for i in range(len(prime)):
                if prime[i] + 12.0 > range_:
                    top = True
                    break
                prime[i] = prime[i] + 12.0
        chord[place] = prime[place]
    return None

def voice(chord, v, range):
    pcs = order(pc(chord))
    if pcs not in voicingsForPcs:
        voicings = []
        upiterator = list(pcs)
        downiterator = list(pcs)
        bottom = pcs
        while True:
            c = decrementVoicing(downiterator, range)
            if c:
                bottom = order(c)
            else:
                break
        voicings.append(tuple(bottom))
        upiterator = list(bottom)
        while True:
            c = incrementVoicing(upiterator, range)
            if c:
                voicings.append(c)
            else:
                break
        voicingsForPcs[pcs] = tuple(voicings)
    voicings = voicingsForPcs[pcs]
    r = v % len(voicings)
    return voicings[r]
        
class Turtle(object):
    def __init__(self):
        # The size of R.
        self.range = 60.0
        # The state of this as a pitch-class segment or chord in O.
        self.O = ()
        # The state of this as the index of octavewise permutations of O in R.
        self.r = 0
        # The time of this, used at writing time.
        self.time = 0.0
        # The duration of this.
        self.duration = 1.0
        # The loudness of this.
        self.loudness = 60.0
        # Voicelead from prior chord?
        self.voicelead = False
        # The pitches implied by O, range, and r.
        self.R = None
        self.instrumentsForVoices = {}
        for i in range(12):
            self.instrumentsForVoices[i] = i
    def write(self, score):
        for i in range(len(self.R)):
            t = self.time
            d = self.duration
            s = 144.0
            k = self.R[i]
            v = self.loudness
            p = i
            instrument = self.instrumentsForVoices[i]
            #print self.instrumentsForVoices
            print('NOTE %3d:  i: %3d  t: %9.4f  d: %9.4f  s: %5.1f  k: %5.2f  v: %5.2f  p: %5.2f' % (i, instrument, t, d, s, k, v, p))
            score.append(t, d, 144.0, float(instrument), k, v, 0, p)
        print
    def voice(self, priorChord):
        pass
    def __str__(self):
        return 'TURTLE: t: %9.3f  d: %9.3f  l: %9.3f v: %s  R: %9.3f  r: %9.3f  O: %s' % (self.time, self.duration, self.loudness, self.voicelead, self.range, self.r, self.O)

# A CsoundAC.ScoreNode that generates notes
# using a context-free deterministic Lindenmayer system
# with a vocabulary of voice-leading operations
# extending neo-Riemannian operations to the 
# Generalized Contextual Group.
class GeneralizedContextualGroup(CsoundAC.ScoreNode):
    def __init__(self, debug = False):
        CsoundAC.ScoreNode.__init__(self)
        self.debug = debug
        self.merge = True
        self.generations = 3
        self.axiom = ""
        self.rules = {}
        self.production = ""
        self.generations = 3
        self.tie = False
        self.avoidParallelFifths = False
        self.score = self.getScore()
        self.voiceleadingNode = CsoundAC.VoiceleadingNode()
        self.rescaleTimes = False
        self.commands = {}
        self.commands['['] = self.commandPush
        self.commands[']'] = self.commandPop
        self.commands['D'] = self.commandD
        self.commands['I'] = self.commandI
        self.commands['K'] = self.commandK
        self.commands['L'] = self.commandL
        self.commands['P'] = self.commandP
        self.commands['Q'] = self.commandQ
        self.commands['R'] = self.commandR
        self.commands['S'] = self.commandS
        self.commands['T'] = self.commandT
        self.commands['V'] = self.commandV
        self.commands['W'] = self.commandW
    def setIterationCount(self, count):
        self.generations = count
    def addRule(self, token, replacement):
        self.rules[token] = replacement
    def setAxiom(self, axiom):
        self.axiom = axiom
    def generate(self):
        print('Generating Lindenmayer system production...')
        self.produce()
        print('Interpreting Lindenmayer system production...')
        self.interpret()
        print('Writing generated chords to score...')
        self.write()
        print('Finished Lindenmayer system.')
    def trace(self):
        if self.debug:
            print(self.turtle)
    def produce(self):
        self.production = self.axiom
        self.chords = []
        for generation in range(self.generations):
            tokens = self.production.split()
            self.production = ''
            for token in tokens:
                if token in self.rules:
                    self.production = '%s %s' % (self.production, self.rules[token])
                else:
                    self.production = '%s %s' % (self.production, token)
                    self.production += ' '
    def commandPush(self, token):
        self.stack.append(copy.deepcopy(self.turtle))
    def commandPop(self, token):
        self.turtle = self.stack.pop()
    def commandD(self, token):
        if len(token) == 2:
            if token[1] == '*':
                self.turtle.duration = self.turtle.duration * 2.0
            elif token[1] == '/':
                self.turtle.duration = self.turtle.duration / 2.0
        else:
            if token[1] == '*':
                self.turtle.duration = self.turtle.duration * float(token[2:])
            elif token[1] == '/':
                self.turtle.duration = self.turtle.duration / float(token[2:])
    def commandI(self, token):
        arrangement = string.split(token[1:], '=')
        v = int(arrangement[0])
        i = int(arrangement[1])
        print('Assigning instrument %s for voice %s.' % (i, v))
        self.turtle.instrumentsForVoices[v] = i
    def commandK(self, token):
        self.turtle.O = K(self.turtle.O)
    def commandL(self, token):
         if len(token) == 2:
            if token[1] == '*':
                self.turtle.loudness = self.turtle.loudness * 2.0
            elif token[1] == '/':
                self.turtle.loudness = self.turtle.loudness / 2.0
         else:
            if token[1] == '*':
                self.turtle.loudness = self.turtle.loudness * float(token[2:])
            elif token[1] == '/':
                self.turtle.loudness = self.turtle.loudness / float(token[2:])
    # Mutate the turtle chord from the current PTV (if any)
    # to a new PTV that is as close a voiceleading to the current PTV as possible.
    def commandP(self, token):
        if token[1] == '=':
            chord = token[2:]
            print('New chord:', chord)
            pcs = CsoundAC.Conversions_nameToPitches(chord)
        else:
            pcs = eval(token[1:])
        print('New PCS:', pcs)
        P = CsoundAC.Voicelead_pitchClassSetToPandT(pcs)[0]
        # Must voicelead from current PTV to closest new PTV.
        if not self.turtle.O:
            ptv = (P, 0, 0)
            print('args', int(ptv[0]), int(ptv[1]), int(ptv[2]), int(0), int(self.turtle.range), int(12))
            self.turtle.O = CsoundAC.Voicelead_ptvToChord(int(ptv[0]), int(ptv[1]), int(ptv[2]), int(0), int(self.turtle.range), int(12))
        else:
            pt = CsoundAC.Voicelead.pitchClassSetToPandT(self.turtle.O)
            ptv = (pt[0], pt[1], self.turtle.r)
            currentChord = CsoundAC.Voicelead_ptvToChord(int(ptv[0]), int(ptv[1]), int(ptv[2]), int(0), int(self.turtle.range), int(12))
            print('Current chord:', currentChord)
            newChord = CsoundAC.Voicelead_voicelead(self.turtle.O, pcs, int(0), int(self.turtle.range), True, int(12))
            print('New chord:', newChord)
            self.turtle.O = pc(newChord)
        print('New PTV:', ptv)
        print('New turtle.O:', self.turtle.O)
        self.modality = copy.copy(self.turtle.O)
    def commandQ(self, token):
        n = float(token[1:])
        self.turtle.O = Q(self.turtle.O, n, self.modality)
    def commandR(self, token):
        self.turtle.range = float(token[1:])
    def commandS(self, token):
        self.modality = eval(token[1:])
        self.turtle.O = copy.copy(self.modality)
    def commandT(self, token):
        n = 1
        if len(token) > 2:
            n = int(token[1:])
        self.turtle.O = T(self.turtle.O, n)
    def commandV(self, token):
        if len(token) == 2:
            if token[1] == '+':
                self.turtle.r = self.turtle.r + 1
            elif token[1] == '-':
                self.turtle.r = self.turtle.r - 1
        else:
            self.turtle.r = self.turtle.r + float(token[1:])
        while self.turtle.r < 0:
            self.turtle.r = self.turtle.r + self.turtle.range
        while self.turtle.r >= self.turtle.range:
            self.turtle.r = self.turtle.r - self.turtle.range
    def commandW(self, token):
        chord = copy.deepcopy(self.turtle)
        if len(token) == 1 or token[1] == 'C':
            chord.voicelead = False
        else:
            chord.voicelead = True
        if len(token) > 2:
            n = int(token[2:])
        else:
            n = 1
        for i in range(n):
            self.chords.append(chord)
    def interpret(self):
        self.turtle = Turtle()
        self.stack = []
        tokens = self.production.split()
        self.time = 0.0
        for token in tokens:
            if self.debug:
                 print('COMMAND: %s' % (token))
            command = token[0]
            if command in self.commands:
                self.commands[command](token)
                self.trace()
            if self.debug:
                print
    def write(self):
        for i in range(1, len(self.chords)):
            prior = self.chords[i - 1]
            chord = self.chords[i]
            opcs = order(pc(chord.O))
            pt = CsoundAC.Voicelead.pitchClassSetToPandT(opcs)
            print('WRITING CHORD...')
            print('Number:   ', (i + 1))
            print('Prime:    ', pt[0])
            print('Transpose:', pt[1])
            print('r:        ', chord.r)
            print('Prior:    ', prior.R)
            print('Current:  ', chord.O)
            chord.time = prior.time + prior.duration
            if not chord.voicelead:
                chord.R = CsoundAC.Voicelead.ptvToChord(int(pt[0]), int(pt[1]), int(chord.r), int(0), int(chord.range))
                print('Voiced:   ', chord.R)
            else:
                if prior.R:
                    if order(prior.O) == order(chord.O):
                        chord.R = prior.R
                        print('Tied:     ', chord.R)
                    else:
                        chord.R = CsoundAC.Voicelead.voicelead(order(prior.R), chord.O, 0, chord.range, True)
                        print('Voice-led:', chord.R)
                else: 
                    chord.R = CsoundAC.Voicelead.ptvToChord(int(pt[0]), int(pt[1]), int(chord.r), int(0), int(chord.range))
                    print('Voiced:   ', chord.R)
            rpcs = order(pc(chord.R))
            if opcs != rpcs:
                print('*** ERROR: Voicing or voice-leading changed PCS!')
            for i in range(len(rpcs)):
                if rpcs[i] > chord.range:
                    print('*** ERROR: Voice out of range!')
            print
            chord.write(self.score)
        if self.merge and True:
            print('Began merging overlapping notes with %d notes...' % len(self.score))
            print
            delete = set()
            n = len(self.score);
            for soonerI in range(n):
                if soonerI not in delete:
                    print
                    sooner = self.score[soonerI]
                    sooner.thisown=0
                    soonerEnds = sooner.getOffTime()
                    for laterI in range(soonerI + 1, n, 1):
                        if laterI not in delete:
                            later = self.score[laterI]
                            laterBegins = later.getTime()
                            if later.getKey() == sooner.getKey() and later.getChannel() == sooner.getChannel():
                                if soonerEnds >= laterBegins:
                                    print('TIE     %5d %s  Ends: %9.4f  With:' % (soonerI, sooner.toString(), soonerEnds))
                                    print('        %5d %s  Ends: %9.4f ' % (laterI, later.toString(), later.getOffTime()))
                                    sooner.setDuration(later.getOffTime() - sooner.getTime())
                                    soonerEnds = sooner.getOffTime()
                                    delete.add(laterI)
                                else:
                                    break
                    print('WRITE   %5d %s  Ends: %9.4f' % (soonerI, sooner.toString(), sooner.getOffTime()))                
            for i in range(n - 1, 0, -1):
                if i in delete:
                    del self.score[i]
            print('Ended merging overlapping notes with %d notes.' % len(self.score))
            print
         

gcg = GeneralizedContextualGroup()
gcg.setAxiom('S(0,4,7,10,14) R60 V+209 1 B')  
gcg.addRule('B', 'T-3 B V+21 K W V+109 V+1 V+11 WV V-11 [ T-13 K Q1 W D/2.1 B [ T+3 ] V-1 B V-8 D/2.1 ] WV V-91 D*1.0 1 V-4 T3 WV')
gcg.setIterationCount(4) 
gcg.debug = False 
gcg.generate()
score = gcg.getScore() 
score.setDuration(180)
print(score.getCsoundScore())  
ac_reaper.score_to_midiitem(score, 36)

