import CsoundVST
from scipy import *
import scipy.signal.waveforms
import time
import os
import psyco
psyco.full()

filename = './ifs.wav'
if os.path.exists(filename):
    os.remove(filename)
started = time.time()
print 'Began', time.ctime(started)
soundfile = CsoundVST.Soundfile()
soundfile.create(filename, 44100, 1)
soundfile.blank(240.0)
print 'Blank', time.ctime()
xy = 0.5
xc = .9995
xy1 = 0.0
yy = 0.5
yc = .99958
yy1 = 0.0
duration = 0.05
for i in xrange(0, 100):
    xy = 4.0 * xy * xc * (1.0 - xy)
    yy = 4.0 * yy * yc * (1.0 - yy)
for i in xrange(1, 100001):
    xy = 4.0 * xy * xc * (1.0 - xy)
    yy = 4.0 * yy * yc * (1.0 - yy)
    center = 0.5 + xy * 239.0
    Hz = 32.0 + yy * 8000.0
    a = 0.001
    print 'Grain %6d: center %9.3f duration %9.3f Hz %9.3f a %9.12f' % (i, center, duration, Hz, a)
    soundfile.cosineGrain(center, duration, Hz, a, 0.0, 0.0)
soundfile.close()
ended = time.time()
print 'Ended', time.ctime(ended)
duration = ended - started
print 'Elapsed time     %s' % (time.strftime('%H:%M:%S', time.gmtime(duration)))









