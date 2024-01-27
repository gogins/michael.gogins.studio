import CsoundAC
from   scipy import *
import scipy.signal.waveforms
import time
import math
from   numpy import *
import os
import random
import sys
# Use both numpy and psyco to get all possible speed
# out of Python.
import psyco
psyco.full()

print 'CREATING FILENAMES...'
print

scriptFilename = sys.argv[0]
print 'Full Python script:  %s' % scriptFilename
title = os.path.basename(scriptFilename)
print 'Base Python script:  %s' % title
directory = os.path.dirname(scriptFilename)
if len(directory):
	print 'Working directory:   %s' % directory
	os.chdir(directory)
print 'Working directory:   %s' % directory
soundfileName = title + '.wav'
print 'Soundfile name:      %s' % soundfileName
soundfilePlayer = r'D:\utah\opt\Audacity\audacity.exe'
print 'Soundfile player:    %s' % soundfilePlayer
print

class IfsToSoundfile(object):
	def __init__(self, iterations = 10, durationSeconds = 120.0, minimumHz = 27.5, maximumHz = 4000.0, width=8000, height=400):
		self.iterations = iterations
		self.width = width
		self.height = height
		self.durationSeconds = durationSeconds
		self.minimumHz = minimumHz
		self.maximumHz = maximumHz
		self.columnSeconds = (self.durationSeconds / float(self.width))
		self.grainSeconds = 2.0 * self.columnSeconds
		self.hutchinsonTransforms = []
		self.weights = []
		self.sizeIterations = 200000
	# Make a homogeneous affine transformation matrix
	# and add it to the Hutchinson operator.
	def addTransform(self, a, b, c, d, e, f, w):
		transform = array([[a, b, e], [c, d, f], [0.0, 0.0, 1.0]], dtype = float64)
		self.hutchinsonTransforms.append(transform)
		self.weights.append(w)
	def formatTransform(self, t):
		return 'a: %9.3f b: %9.3f c: %9.3f d: %9.3f e: %9.3f f: %9.3f' % (t[0, 0], t[0, 1], t[1, 0], t[1, 1], t[0, 2], t[1, 2])
	def formatPoint(self, p):
		return 'x: %9.3f y: %9.3f' % (p[0], p[1])
	# Initialize the initial point of the IFS,
	# and normalize the probability weights
	# for each transform in the Hutchinson operator.
	def initialize(self):
		print 'INITIALIZING...'
		print
		self.sizeIteration = 0
		self.point =    array([0, 0, 1], dtype = float64)
		self.min =      array([0, 0, 1], dtype = float64)
		self.max =      array([0, 0, 1], dtype = float64)
		self.range =    array([0, 0, 1], dtype = float64)
		self.scale =    array([0, 0, 1], dtype = float64)
		self.iteration = 0
		self.point =    array([0, 0, 1], dtype = float64)
		self.hutchinsonWeights = zeros(len(self.weights), dtype = float64)
		s = 0.0
		for i in xrange(len(self.weights)):
			s = s + self.weights[i]
		for i in xrange(len(self.weights)):
			self.hutchinsonWeights[i] = self.weights[i] / s
		for i in xrange(len(self.weights)):
			print 'Transform %4d: %s  Weight: %9.3f' % (i, self.formatTransform(self.hutchinsonTransforms[i]), self.weights[i])
		print
		self.target = ones((self.width, self.height), dtype = float64)
		self.source = zeros((self.width, self.height), dtype = float64)
		print 'Allocated measure arrays:'
		print self.source
		print self.target
		print
	# Create an output soundfile.
	def create(self):
		print 'CREATING BLANK SOUNDFILE...'
		print
		if os.path.exists(soundfileName):
			os.remove(soundfileName)
		self.began = time.time()
		print 'Began', time.ctime(self.began)
		self.soundfile = CsoundAC.Soundfile()
		self.soundfile.create(soundfileName, 88200, 1)
		# Create a blank output soundfile
		# with additional time padded at beginning and end.
		self.soundfile.blank(self.durationSeconds + 2.0)
		print 'Blank', time.ctime()
		print
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
	# Measure M is x,y; attractor A is X,Y; scale S is A / M; min is min(A).
	def apply(self, source, target):
		for x in xrange(self.width):
			for y in xrange(self.height):
				if source[x, y] > 0.0:
					for t in xrange(len(self.hutchinsonTransforms)):
						transform = self.hutchinsonTransforms[t]
						X0 = (float(x) * self.scale[0]) + self.min[0]
						Y0 = (float(y) * self.scale[1]) + self.min[1]
						X1 = transform[0, 0] * X0 + transform[0, 1] * Y0 + transform[0, 2]
						Y1 = transform[1, 0] * X0 + transform[1, 1] * Y0 + transform[1, 2]
						x1 = int((X1 - self.min[0]) / self.scale[0])
						y1 = int((Y1 - self.min[1]) / self.scale[1])
						if 0 <= x1 and x1 < self.width and 0 <= y1 and y1 < self.height:
							target[x1, y1] = target[x1, y1] + self.hutchinsonWeights[t]
	def clear(self, m):
		m.fill(0.0)
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
	def sizeIterate(self):
		print 'FINDING SIZE OF ATTRACTOR WITH THE RANDOM ALGORITHM...'
		print
		self.max = array(self.point)
		self.min = array(self.point)
		for self.iteration in xrange(self.sizeIterations):
			transform = self.spin()
			self.point = dot(transform, self.point)
			if self.iteration > 1000:
				if self.iteration % 1000 == 0:
					print 'Iteration: %8d' % self.iteration
					print 'Point:     %s'  % self.formatPoint(self.point)
					print 'Transform: %s'  % self.formatTransform(transform)
					print
				for i in xrange(2):
					if self.point[i] > self.max[i]:
						self.max[i] = self.point[i]
						self.range[i] = self.max[i] - self.min[i]
					if self.point[i] < self.min[i]:
						self.min[i] = self.point[i]
						self.range[i] = self.max[i] - self.min[i]
		self.scale[0] = self.range[0] / float(self.width)
		self.scale[1] = self.range[1] / float(self.height)
		print 'Max:  ', self.max            
		print 'Min:  ', self.min            
		print 'Range:', self.range            
		print 'Scale:', self.scale 
		print        
	def iterate(self):
		self.initialize()
		self.create()
		self.sizeIterate()
		self.deterministicIterate()
	def deterministicIterate(self):
		print 'COMPUTING ATTRACTOR WITH THE DETERMINISTIC ALGORITHM...'
		print
		for self.iteration in xrange(self.iterations):
			temp = self.source
			self.source = self.target
			self.target = temp
			print 'Iteration: %4d' % (self.iteration + 1)
			self.clear(self.target)
			self.apply(self.source, self.target)
		print
		self.render(self.target)
	def render(self, m):
		print 'RENDERING ATTRACTOR WITH PHASE-SYNCHRONOUS GRAINS...'
		print
		print                 'Column duration: %9.4f' % self.columnSeconds
		print                 'Grain duration:  %9.4f' % self.grainSeconds
		print
		hzSize = self.maximumHz / self.height
		for x in xrange(self.width):
			t = 0.5 + float(x) * self.columnSeconds
			if x % 100 == 0:
				print 'Column:          %9d  time: %9.4f' % (x, t)
			for y in xrange(self.height):
				Hz = 30.0 + float(y) * hzSize
				# Ensure that grains at the same frequency
				# are synchronous in phase.
				wavelength = 1.0 / Hz
				wavelengths = t / wavelength
				fraction, wholecycles = math.modf(wavelengths)
				phase = fraction * 2.0 * math.pi
				self.soundfile.cosineGrain(t, self.grainSeconds, Hz, float(m[x, y]) * 0.001, phase, 0.0)
		print
		self.soundfile.close()
		ended = time.time()
		print 'Ended', time.ctime(ended)
		print
		elapsed = ended - self.began
		#print 'Elapsed time     %s' % (time.strftime('%H:%M:%S', time.gmtime(elapsed))
		self.play()
	def play(self):
		os.spawnl(os.P_NOWAIT, soundfilePlayer, soundfilePlayer, r'"%s"' % (os.getcwd() + os.sep + soundfileName))

if __name__ == '__main__':
	ifs = IfsToSoundfile()
	#                   a       b       c       d       e       f       w
##    ifs.addTransform(   0.5,    0.0,    0.0,    0.5,    0.1,    0.1,    1.0 )
##    ifs.addTransform(   0.51,   0.0,    0.0,    0.0,    0.5,    0.5,    0.5 )
##    ifs.addTransform(   0.5,    0.0,    0.0,    0.45,   0.0,    0.49,   1.0 )
##    ifs.addTransform(   0.5,    0.0,    0.1,    0.5,    0.5,    0.49,   1.0 )
	ifs.addTransform(   0.5,    0.0,    0.0,    0.5,    0.0,    0.5,    1.0 )
	ifs.addTransform(   0.5,    0.0,    0.0,    0.5,    0.5,    0.0,    1.0 )
	ifs.addTransform(   0.5,    0.0,    0.0,    0.5,   -0.5,    0.0,    1.0 )
##	ifs.addTransform(   0.382,  0.0,    0.0,    0.382,  0.3072, 0.6190,    1.0 )
##	ifs.addTransform(   0.382,  0.0,    0.0,    0.382,  0.6033, 0.4044,    1.0 )
##	ifs.addTransform(   0.382,  0.0,    0.0,    0.382,  0.0139, 0.4044,    1.0 )
##	ifs.addTransform(   0.382,  0.0,    0.0,    0.382,  0.1253, 0.0595,    1.0 )
##	ifs.addTransform(   0.382,  0.0,    0.0,    0.382,  0.4920, 0.0595,    1.0 )
	ifs.iterate()
