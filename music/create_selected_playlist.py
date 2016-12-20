'''
Builds an m3u playlist of all soundfiles on my computer that might
conceivably be pieces of mine. Small files are omitted, files in certain
directories are omitted, later files are preferred to earlier files.

Sample:

#EXTM3U
#EXTINF:0,01-19991216a.wav
D:\Dropbox\Michael Gogins\Semblance\01-19991216a.wav

'''

import os
import os.path
import string
import sys
import time
import traceback

output = r'sifting1.m3u'

filepaths = r'''d:\Dropbox\Michael Gogins\Semblance\01-19991216a.wav
d:\Dropbox\Michael Gogins\Garden of Algorithms\01-Cloud Strata.wav
d:\Dropbox\Michael Gogins\Summertrees\01-Various _ 01_fractal23-a.mml.master.a.wav
d:\Dropbox\Michael Gogins\Semblance\02-20000611B-b.wav
d:\Dropbox\Michael Gogins\Garden of Algorithms\02-Triptych.wav
d:\Dropbox\Michael Gogins\Summertrees\02-Various _ 02_f--2002-03-09--20-54-18.018.mml-a.csd.master.wav
d:\Dropbox\Michael Gogins\Semblance\03-csound-2004-06-11-17.43.24.py.wav
d:\Dropbox\Michael Gogins\Garden of Algorithms\03-Dark Tower.wav
d:\Dropbox\2015-02-15 Spectrum NYC\03-Various _ 03_f--2002-03-08--13-48-05.005-a.mml.master.wav
d:\Dropbox\Michael Gogins\Garden of Algorithms\04-Bending Arches.wav
d:\Dropbox\Michael Gogins\Semblance\04-csound-2004-12-12-19.55.12.py.wav
d:\Dropbox\Michael Gogins\Summertrees\04-Various _ 04_B-2001-10-27-a.mml-a.csd.py.master.wav
d:\Dropbox\Michael Gogins\Garden of Algorithms\05-Broken Differential.wav
d:\Dropbox\Michael Gogins\Semblance\05-csound-2005-03-06-03-38.19.py.wav
d:\Dropbox\Michael Gogins\Summertrees\05-Various _ 05_f--2002-01-10--23-12-54.054-b.mml.master.wav
d:\Dropbox\Michael Gogins\Garden of Algorithms\06-970104e.wav
d:\Dropbox\Michael Gogins\Semblance\06-f-2002-01-28-17-37-42.042.mml.wav
d:\Dropbox\Michael Gogins\Summertrees\06-Various _ 06_drone.py.wav
d:\Dropbox\Michael Gogins\Garden of Algorithms\07-Chaotic Squares.wav
d:\Dropbox\Michael Gogins\Semblance\07-f-2002-03-02-22-35-05.005.mml.wav
d:\Dropbox\Michael Gogins\Summertrees\07-Various _ 07_Attractor1.mml-b.master.wav
d:\Dropbox\Michael Gogins\Garden of Algorithms\08-LinMuse21.wav
d:\Dropbox\Michael Gogins\Semblance\08-silence-2003-07-07-19.52.47.py.wav
d:\Dropbox\Michael Gogins\Summertrees\08-Various _ 08_csound[2005-04-16][15.29.17]-a.py.master.wav
d:\Dropbox\Michael Gogins\Garden of Algorithms\09-Hex.wav
d:\Dropbox\Michael Gogins\Semblance\09-zodiac.wav
d:\Dropbox\Michael Gogins\Garden of Algorithms\10-MC 3.wav
d:\Dropbox\Michael Gogins\Garden of Algorithms\11-Piano Fall.wav
d:\Dropbox\Michael Gogins\Garden of Algorithms\12-Three Trees.wav
d:\Dropbox\2011-LAC-MKG\Aeolus_Study.3.native_reverb.lua.norm.wav
d:\Dropbox\2010-ICMC-MKG\soundifs\Apophysis-091206-3.wav
d:\Dropbox\Talks\Brad's class\My Way\Blue_Leaves_4e.norm.wav
d:\Dropbox\music\Blue_Leaves\Blue_Leaves_5a_1.norm.wav
d:\Dropbox\2013-Csound-MKG\Gogins Submissions\Blue Leaves 6\Blue_Leaves_6.norm.wav
d:\Dropbox\music\Blue_LeavesBlue_Leaves_1.wav
d:\Dropbox\2013-Csound-MKG\Gogins Submissions\Drone IV\Drone-IV-1377971544.tagged.wav
d:\Dropbox\2014-LAC-MKG\Submission\Drone-VIII-d.wav
d:\Dropbox\studio\01_workshop\drone.py.wav
d:\Dropbox\2015-02-15 Spectrum NYC\Hesperide.wav
d:\Dropbox\2010-ICMC-MKG\soundifs\ifs-2008-01-23-a.py.stereo.wav
d:\Dropbox\2010-ICMC-MKG\soundifs\ifs-i.py.wav
d:\Dropbox\2010-ICMC-MKG\soundifs\ifs-n.py.wav
d:\Dropbox\2014-11-05 Spectrum NYC\Ladon-1-d.wav
d:\Dropbox\2014-CircuitBridges-MKG\mkg-2007-01-20-b.py.wav
d:\Dropbox\music\mkg-2008-06-15-d\mkg-2008-06-15-d-3.wav
d:\Dropbox\music\mkg-2008-06-15-d\mkg-2008-06-15-d.wav
d:\Dropbox\studio\01_workshop\mkg-2008-06-20-a-1.wav
d:\Dropbox\studio\01_workshop\mkg-2008-09-16-c.wav
d:\Dropbox\studio\01_workshop\mkg-2008-12-25-b.wav
d:\Dropbox\studio\01_workshop\mkg-2009-01-10-b.wav
d:\Dropbox\Talks\Brad's class\My Way\mkg-2009-01-10-c.wav
d:\Dropbox\2009-ICMC-MKG\mkg-2009-02-03-d.wav
d:\Dropbox\2014-Vanderbilt_Planetarium-MKG\mkg-2009-09-14-o-1.py.wav
d:\Dropbox\studio\mkg-2009-09-14-r.py.wav
d:\Dropbox\music\mkg-2010-03-16-e\mkg-2010-03-16-e-1.py.wav
d:\Dropbox\music\qt_drone\Mountain_Drone.norm.wav
d:\Dropbox\2015-02-15 Spectrum NYC\Semblance 09 chaotic dynamical system.wav
d:\Dropbox\2016-06-13 NYCEMF\Pieces\Fixed Media\Sevier.6.wav
d:\Dropbox\2010-ICMC-MKG\soundifs\soundifs.py.wav
d:\Dropbox\2015-06-22 NYCEMF\Telamon.wav
d:\Dropbox\2014-06-15 NYCEMS Salon\Two Dualities.wav
d:\Dropbox\2014-Queens_College-MKG\Untouching-1.norm.wav
d:\Dropbox\Michael Gogins\Semblance\01-19991216a.wav
'''

filecount = 0
playlist = open(output, 'w')
playlist.write('#EXTM3U\n')
pathnames = filepaths.split('\n')
pathnames.sort()
for pathname in pathnames:
    filecount = filecount + 1
    basename = os.path.basename(pathname)
    playlist.write('#EXTINF:-1,%s\n' % basename)
    playlist.write('%s\n' % pathname)
    print
playlist.write('\n')

print 'Finished with', filecount, 'files.'

