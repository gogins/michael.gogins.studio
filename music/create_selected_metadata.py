'''
Attempts to build all required metadata of listed works.

'''

import datetime
import os
import os.path
import soundfile
import string
import sys
import time
import traceback

output = r'sifting.m3u'
sox_path = r'C:\Program_Files_x86\sox-14-4-2'
sndfile_path = r'C:\Program_Files\Mega-Nerd\libsndfile\bin'

filepaths = r'''d:\Dropbox\2011-LAC-MKG\Aeolus_Study.3.native_reverb.lua.norm.wav
d:\Dropbox\2014-Vanderbilt_Planetarium-MKG\Apophysis-090301-1.wav
d:\Dropbox\2010-ICMC-MKG\soundifs\Apophysis-090301-100.grains.wav
d:\Dropbox\2010-ICMC-MKG\soundifs\Apophysis-090301-102.wav
d:\Dropbox\2010-ICMC-MKG\soundifs\Apophysis-090301-17.grains.wav
d:\Dropbox\2010-ICMC-MKG\soundifs\Apophysis-091201-101.wav
d:\Dropbox\2010-ICMC-MKG\soundifs\Apophysis-091202-101.wav
d:\Dropbox\2010-ICMC-MKG\soundifs\Apophysis-091203-103.wav
d:\Dropbox\2010-ICMC-MKG\soundifs\Apophysis-091206-101.wav
d:\Dropbox\2010-ICMC-MKG\soundifs\Apophysis-091206-3.wav
d:\Dropbox\2010-ICMC-MKG\soundifs\Apophysis-091206-a.wav
d:\Dropbox\2010-ICMC-MKG\soundifs\Apophysis-091206-c.wav
d:\Dropbox\2010-ICMC-MKG\soundifs\Apophysis-091228-1.wav
d:\Dropbox\2010-ICMC-MKG\soundifs\Apophysis-091228-2.wav
d:\Dropbox\2010-ICMC-MKG\soundifs\Apophysis-091228-4.wav
d:\Dropbox\Talks\Brads class\My Way\Blue_Leaves_4e.norm.wav
d:\Dropbox\music\Blue_Leaves\Blue_Leaves_5a_1.norm.wav
d:\Dropbox\2013-Csound-MKG\Gogins Submissions\Blue Leaves 6\Blue_Leaves_6.norm.wav
d:\Dropbox\music\Blue_LeavesBlue_Leaves_1.wav
d:\Dropbox\2013-Csound-MKG\Gogins Submissions\Drone IV\Drone-IV-1377971544.tagged.wav
d:\Dropbox\2014-LAC-MKG\Submission\Drone-VIII-d.wav
d:\Dropbox\studio\01_workshop\drone.py.wav
d:\Dropbox\2015-02-15 Spectrum NYC\Hesperide.wav
d:\Dropbox\2010-ICMC-MKG\soundifs\ifs-2008-01-22-a.py.wav
d:\Dropbox\2010-ICMC-MKG\soundifs\ifs-2008-01-23-a.py.stereo.wav
d:\Dropbox\2010-ICMC-MKG\soundifs\ifs-2008-05-23-a.py.stereo.wav
d:\Dropbox\2010-ICMC-MKG\soundifs\ifs-2009-03-02-a.py.stereo.wav
d:\Dropbox\2010-ICMC-MKG\soundifs\ifs-2009-03-04-a.py.stereo.wav
d:\Dropbox\2010-ICMC-MKG\soundifs\ifs-b.py.wav
d:\Dropbox\2010-ICMC-MKG\soundifs\ifs-d.py.wav
d:\Dropbox\2010-ICMC-MKG\soundifs\ifs-e.py.wav
d:\Dropbox\2010-ICMC-MKG\soundifs\ifs-i.py.wav
d:\Dropbox\2010-ICMC-MKG\soundifs\ifs-j.py.stereo.wav
d:\Dropbox\2010-ICMC-MKG\soundifs\ifs-m.py.wav
d:\Dropbox\2010-ICMC-MKG\soundifs\ifs-n.py.wav
d:\Dropbox\2010-ICMC-MKG\soundifs\ifs.wav
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
d:\Dropbox\music\Yellow_Leaves\Yellow_Leaves.4.lua.norm.wav
d:\Dropbox\2014-Vanderbilt_Planetarium-MKG\zodiac-2014-04-19a.master.wav'''

filepaths = filepaths.split('\n')
filecount = 0
for filepath in filepaths:
    print filepath
    basename = os.path.basename(filepath)
    ctime = os.path.getctime(filepath)
    year = datetime.datetime.fromtimestamp(ctime).strftime('%Y')
    #print 'basename:', basename, 'year:', year
    sox_command = '''%s\\sox %s -n spectrogram -t"%s" -c"Copyright (C) %s by Michael Gogins (http://michaelgogins.tumblr.com), Irreducible Productions (ASCAP)" -o"%s.png"''' % (sox_path, filepath, basename, year, filepath)
    print sox_command
    os.system(sox_command)
    #print soundfile.info(filepath, True)
    filecount = filecount + 1
    print

print 'Finished with', filecount, 'files.'

