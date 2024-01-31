'''
This module uses a simple probabilistic
iterated function system (IFS)
to approximate the measure on the IFS attractor.
Each point in the iteration becomes a grain of sound
in a pre-allocated soundfile; 
x maps to time, y maps to frequency, 
and the density of the measure maps to amplitude.
In theory, any sound can be rendered in this way.

Is it possible to use the deterministic algorithm or the MCRM?
That would be less noisy.
'''
import CsoundAC
from   scipy import *
import scipy.signal.waveforms
import time
import math
from   numpy import *
import os
import random
# Use both numpy and psyco to get all possible speed
# out of Python.
import psyco
psyco.full()

class IfsToSoundfile(object):
    def __init__(self, iterations = 1000000, durationSeconds = 120.0, minimumHz = 32.0, maximumHz = 16000.0, grainDuration = 0.01, grainAmplitude = 0.001):
        self.iterations = iterations
        self.durationSeconds = durationSeconds
        self.minimumHz = minimumHz
        self.maximumHz = maximumHz
        self.grainDuration = grainDuration
        self.grainAmplitude = grainAmplitude
        self.sizeIterations = -200000
        self.hutchinsonTransforms = []
        self.weights = []
    # Make a homogeneous affine transformation matrix
    # and add it to the Hutchinson operator.
    def addTransform(self, a, b, c, d, e, f, w):
        self.hutchinsonTransforms.append(array([[a, b, e], [c, d, f], [0.0, 0.0, 1.0]], dtype = float64))
        self.weights.append(w)
    def formatTransform(self, t):
        return 'a: %9.3f b: %9.3f c: %9.3f d: %9.3f e: %9.3f f: %9.3f' % (t[0, 0], t[0, 1], t[1, 0], t[1, 1], t[0, 2], t[1, 2])
    def formatPoint(self, p):
        return 'x: %9.3f y: %9.3f' % (p[0], p[1])
    # Initialize the initial point of the IFS,
    # and normalize the probability weights
    # for each transform in the Hutchinson operator.
    def initialize(self):
        self.iteration = 0
        self.point =    array([0, 0, 1], dtype = float64)
        self.min =      array([0, 0, 1], dtype = float64)
        self.max =      array([0, 0, 1], dtype = float64)
        self.range =    array([0, 0, 1], dtype = float64)
        self.scale =    array([0, 0, 1], dtype = float64)
        self.hutchinsonWeights = zeros(len(self.weights), dtype = float64)
        s = 0.0
        for i in xrange(len(self.weights)):
            s = s + self.weights[i]
        for i in xrange(len(self.weights)):
            self.hutchinsonWeights[i] = self.weights[i] / s
        for i in xrange(len(self.weights)):
            print 'Transform %4d: %s  Weight: %9.3f' % (i, self.formatTransform(self.hutchinsonTransforms[i]), self.hutchinsonWeights[i])
    # Choose a transform from the normalized weights array using the Monte Carlo method.
    def spin(self):
        i = 0
        s = 0.0
        r = random.random()
        for i in xrange(len(self.weights)):
            s = s + self.hutchinsonWeights[i]
            if s >= r:
                break
        return self.hutchinsonTransforms[i]                
    # Create an output soundfile.
    def create(self):
        filename = './ifs-a.wav'
        if os.path.exists(filename):
            os.remove(filename)
        self.began = time.time()
        print 'Began', time.ctime(self.began)
        self.soundfile = CsoundAC.Soundfile()
        self.soundfile.create(filename, 88200, 1)
        # Create a blank output soundfile
        # with additional time padded at beginning and end.
        self.soundfile.blank(self.durationSeconds + 2.0)
        print 'Blank', time.ctime()
    # Iterate the 'chaos game' algorithm;
    # the first round of iterations determines
    # the bounds of the attractor, the second round
    # of iterations rescales the measure
    # to fit within the predetermined time and
    # frequency range, and renders the soundfile.
    def iterate(self):
        self.initialize()
        self.create()
        for self.iteration in xrange(self.sizeIterations, 0):
            transform = self.spin()
            self.point = dot(transform, self.point)
            if self.iteration % 1000 == 0:
                print 'Iteration: %9d' % self.iteration
                print 'Point:     %s' % self.formatPoint(self.point)
                print 'Transform: %s' % self.formatTransform(transform)
                print
            for i in xrange(2):
                if self.point[i] > self.max[0]:
                    self.max[i] = self.point[i]
                    self.range[i] = self.max[i] - self.min[i]
                if self.point[i] < self.min[0]:
                    self.min[i] = self.point[i]
                    self.range[i] = self.max[i] - self.min[i]
        self.scale[0] = self.durationSeconds / self.range[0]
        self.scale[1] = (self.maximumHz - self.minimumHz) / self.range[1]
        print 'Scale:', self.scale
        maximumTime = self.durationSeconds + 1.0 - self.grainDuration
        minimumTime = self.grainDuration
        for self.iteration in xrange(self.iterations):
            # (H,h) = W(u),
            # where W = Hutchinson operator
            # and u = a uniformly distributed random variable in [0, 1).
            transform = self.spin()
            # p = p . H + h,
            # implemented using a homogeneous affine transform
            # for efficiency.
            self.point = dot(transform, self.point)
            # Rescale the point to fit within the predetermined
            # time/frequency bounds of the soundfile.
            center = ((self.point[0] - self.min[0]) * self.scale[0]) + 1.0
            Hz = ((self.point[1] - self.min[1]) * self.scale[1]) + self.minimumHz
            # If the point is within bounds,
            # mix it as a grain of sound into the soundfile.
            if center > minimumTime and center < maximumTime:
                self.soundfile.cosineGrain(center, self.grainDuration, Hz, self.grainAmplitude, 0.0, 0.0)
            else:
                print 'Grain out of range:'
                print '  Iteration: %9d  Time: %9.3f  Hz: %9.3f' % (self.iteration, center, Hz)
                print '  Point:     %s', self.formatPoint(self.point)
                print '  Transform: %s', self.formatTransform(transform)
            if self.iteration % 10000 == 0:
                print 'Iteration: %9d  Time: %9.3f  Hz: %9.3f' % (self.iteration, center, Hz)
        self.soundfile.close()
        ended = time.time()
        print 'Ended', time.ctime(ended)
        elapsed = ended - self.began
        print 'Elapsed time     %s' % (time.strftime('%H:%M:%S', time.gmtime(elapsed)))

if __name__ == '__main__':
    print __doc__
    ifs = IfsToSoundfile()
    ifs.addTransform(0.5, 0.0, 0.0, 0.5, 0.0, 0.0, 1.0)
    ifs.addTransform(0.5, 0.0, 0.0, 0.5, 0.5, 0.0, 2.0)
    ifs.addTransform(0.5, 0.0, 0.0, 0.5, 0.0, 0.51, 1.0)
    ifs.iterate()
