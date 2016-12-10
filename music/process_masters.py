# -*- coding: utf-8 -*-
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
lame_path = r'C:\Program_Files_x86\"Lame For Audacity"'

database = r'''Michael Gogins	2001	Garden of Algorithms	01:08	1	Cloud Strata	10:01.00	d:\Dropbox\Michael Gogins\Garden of Algorithms\01-Cloud Strata.wav	01/14/14 03:03 AM		Publish	0.00		Registered		Published by CDBaby			Published
Michael Gogins	2001	Garden of Algorithms		2	Triptych	03:32.00	d:\Dropbox\Michael Gogins\Garden of Algorithms\02-Triptych.wav	01/14/14 03:04 AM		Publish	0.00		Registered		Published by CDBaby			Published
Michael Gogins	2001	Garden of Algorithms		3	Dark Tower	05:13.00	d:\Dropbox\Michael Gogins\Garden of Algorithms\03-Dark Tower.wav	01/14/14 03:04 AM		Publish	0.00		Registered		Published by CDBaby			Published
Michael Gogins	2001	Garden of Algorithms		4	Bending Arches	02:43.00	d:\Dropbox\Michael Gogins\Garden of Algorithms\04-Bending Arches.wav	01/14/14 03:04 AM		Publish	0.00		Registered		Published by CDBaby			Published
Michael Gogins	2001	Garden of Algorithms		5	Broken Differential	06:48.00	d:\Dropbox\Michael Gogins\Garden of Algorithms\05-Broken Differential.wav	01/14/14 03:04 AM		Publish	0.00		Registered		Published by CDBaby			Published
Michael Gogins	2001	Garden of Algorithms		6	970104e	03:58.00	d:\Dropbox\Michael Gogins\Garden of Algorithms\06-970104e.wav	01/14/14 03:05 AM		Publish	0.00		Registered		Published by CDBaby			Published
Michael Gogins	2001	Garden of Algorithms		7	Chaotic Squares	04:05.00	d:\Dropbox\Michael Gogins\Garden of Algorithms\07-Chaotic Squares.wav	01/14/14 03:05 AM		Publish	0.00		Registered		Published by CDBaby			Published
Michael Gogins	2001	Garden of Algorithms		8	LinMuse 21	06:10.00	d:\Dropbox\Michael Gogins\Garden of Algorithms\08-LinMuse21.wav	01/14/14 03:05 AM		Publish	0.00		Registered		Published by CDBaby			Published
Michael Gogins	2001	Garden of Algorithms		9	Hex	08:02.00	d:\Dropbox\Michael Gogins\Garden of Algorithms\09-Hex.wav	01/14/14 03:06 AM		Publish	0.00		Registered		Published by CDBaby			Published
Michael Gogins	2001	Garden of Algorithms		10	MC3	05:02.00	d:\Dropbox\Michael Gogins\Garden of Algorithms\10-MC 3.wav	01/14/14 03:06 AM		Publish	0.00		Registered		Published by CDBaby			Published
Michael Gogins	2001	Garden of Algorithms		11	Piano Fall	07:27.00	d:\Dropbox\Michael Gogins\Garden of Algorithms\11-Piano Fall.wav	01/14/14 03:06 AM		Publish	0.00		Registered		Published by CDBaby			Published
Michael Gogins	2001	Garden of Algorithms		12	Three Trees	05:15.00	d:\Dropbox\Michael Gogins\Garden of Algorithms\12-Three Trees.wav	01/14/14 03:07 AM		Publish	0.00		Registered		Published by CDBaby			Published
Michael Gogins	2006	Semblance	00:54	1	19991216a	04:02.00	d:\Dropbox\Michael Gogins\Semblance\01-19991216a.wav	08/03/10 01:18 AM		Publish	0.00			Accepted as composite	Published by CDBaby			Published
Michael Gogins	2006	Semblance		2	20000611B-b	07:33.00	d:\Dropbox\Michael Gogins\Semblance\02-20000611B-b.wav	08/03/10 01:18 AM		Publish	0.00			Accepted as composite	Published by CDBaby			Published
Michael Gogins	2006	Semblance		3	csound-2004-06-11-17.43.24.py	06:41.00	d:\Dropbox\Michael Gogins\Semblance\03-csound-2004-06-11-17.43.24.py.wav	01/14/14 02:53 AM		Publish	0.00				Published by CDBaby			Published
Michael Gogins	2006	Semblance		4	csound-2004-12-12-19.55.12.py	05:44.00	d:\Dropbox\Michael Gogins\Semblance\04-csound-2004-12-12-19.55.12.py.wav	01/14/14 02:53 AM		Publish	0.00			Accepted as composite	Published by CDBaby			Published
Michael Gogins	2006	Semblance		5	csound-2005-03-06-03-38.19.py	09:10.00	d:\Dropbox\Michael Gogins\Semblance\05-csound-2005-03-06-03-38.19.py.wav	01/14/14 02:54 AM		Publish	0.00			Accepted as composite	Published by CDBaby			Published
Michael Gogins	2006	Semblance		6	f-2002-01-28-17-37-42.042.mml	03:36.00	d:\Dropbox\Michael Gogins\Semblance\06-f-2002-01-28-17-37-42.042.mml.wav	01/14/14 02:54 AM		Publish	0.00			Accepted as composite	Published by CDBaby			Published
Michael Gogins	2006	Semblance		7	f-2002-03-02-22-35-05.005.mml	05:02.00	d:\Dropbox\Michael Gogins\Semblance\07-f-2002-03-02-22-35-05.005.mml.wav	01/14/14 02:55 AM		Publish	0.00			Accepted as composite	Published by CDBaby			Published
Michael Gogins	2006	Semblance		8	silence-2003-07-07-19.52.47.py	05:36.00	d:\Dropbox\Michael Gogins\Semblance\08-silence-2003-07-07-19.52.47.py.wav	01/14/14 02:55 AM		Publish	0.00				Published by CDBaby			Published
Michael Gogins	2006	Semblance		9	Zodiac	07:31.00	d:\Dropbox\Michael Gogins\Semblance\09-zodiac.wav	08/03/10 01:18 AM		Publish	0.00			Accepted as composite	Published by CDBaby			Published
Michael Gogins	2016	Blue and Yellow Leaves	00:57	1	Two Dualities	01:05.00	d:\Dropbox\2014-06-15 NYCEMS Salon\Two Dualities.wav	05/17/14 06:36 PM		Publish	0.00					Published
Michael Gogins	2016	Blue and Yellow Leaves		2	Blue Leaves 4e	06:01.00	d:\Dropbox\Talks\Brads class\My Way\Blue_Leaves_4e.norm.wav	03/09/13 12:07 PM		Publish	0.00						Published
Michael Gogins	2016	Blue and Yellow Leaves		3	Blue Leaves 5a-1	06:01.00	d:\Dropbox\music\Blue_Leaves\Blue_Leaves_5a_1.norm.wav	03/30/13 10:50 AM		Publish	1.00
Michael Gogins	2016	Blue and Yellow Leaves		4	Blue Leaves 1	04:06.00	d:\Dropbox\music\Blue_LeavesBlue_Leaves_1.wav	03/12/14 01:03 AM		Publish	1.00
Michael Gogins	2016	Blue and Yellow Leaves		5	Yellow Leaves 4	05:09.00	d:\Dropbox\music\Yellow_Leaves\Yellow_Leaves.4.lua.wav	03/09/13 12:02 PM		Publish	1.00
Michael Gogins	2016	Blue and Yellow Leaves		6	mkg-2009-02-03-d	07:56.00	d:\Dropbox\2009-ICMC-MKG\mkg-2009-02-03-d.wav	08/03/10 03:05 AM		Publish	1.00
Michael Gogins	2016	Blue and Yellow Leaves		7	mkg-2009-09-14-r.py	09:06.00	d:\Dropbox\studio\mkg-2009-09-14-r.py.wav	08/03/10 03:11 AM		Publish	1.00
Michael Gogins	2016	Blue and Yellow Leaves		8	Untouching-1	05:06.00	d:\Dropbox\2014-Queens_College-MKG\Untouching-1.norm.wav	03/14/13 12:51 AM		Publish	1.00
Michael Gogins	2016	Blue and Yellow Leaves		9	mkg-2007-01-20-b.py	04:59.00	d:\Dropbox\2014-CircuitBridges-MKG\mkg-2007-01-20-b.py.wav	08/03/10 01:18 AM		Publish	1.00
Michael Gogins	2016	Blue and Yellow Leaves		10	mkg-2008-12-25-b.wav	00:52.00	d:\Dropbox\studio\01_workshop\mkg-2008-12-25-b.wav	08/03/10 01:14 AM		Publish	1.00
Michael Gogins	2016	Garden of the Hesperides	00:23	1	Hesperide	07:20.00	d:\Dropbox\2015-02-15 Spectrum NYC\Hesperide.wav	01/04/15 05:28 PM		Publish	1.00
Michael Gogins	2016	Garden of the Hesperides		2	Ladon	08:22.00	d:\Dropbox\2014-11-05 Spectrum NYC\Ladon-1-d.wav	12/02/14 12:01 AM		Publish	1.00
Michael Gogins	2016	Garden of the Hesperides		3	Telamon	08:02.00	d:\Dropbox\2015-06-22 NYCEMF\Telamon.wav	02/08/15 12:24 PM		Publish	0.00						Published
Michael Gogins	2016	Sevier	00:53	1	Sevier	09:00.00	d:\Dropbox\2016-06-13 NYCEMF\Pieces\Fixed Media\Sevier.6.wav	06/04/16 12:32 AM		Publish	0.00					Publish
Michael Gogins	2016	Sevier		2	Aeolus Study No. 3	04:35.00	d:\Dropbox\2011-LAC-MKG\Aeolus_Study.3.native_reverb.lua.norm.wav	03/09/13 12:08 PM		Publish	1.00
Michael Gogins	2016	Sevier		3	Drone VII-D	08:04.00	d:\Dropbox\2014-LAC-MKG\Submission\Drone-VIII-d.wav	02/03/14 02:46 AM		Publish	1.00
Michael Gogins	2016	Sevier		4	Chaotic Dynamical System Study	04:03.00	d:\Dropbox\2015-02-15 Spectrum NYC\Semblance 09 chaotic dynamical system.wav	01/03/15 07:15 PM		Publish	1.00
Michael Gogins	2016	Sevier		5	mkg-2008-06-15-d-3	06:05.00	d:\Dropbox\music\mkg-2008-06-15-d\mkg-2008-06-15-d-3.wav	03/09/13 12:05 PM		Publish	1.00
Michael Gogins	2016	Sevier		6	mkg-2008-06-20-a-1	02:47.00	d:\Dropbox\studio\01_workshop\mkg-2008-06-20-a-1.wav	08/03/10 01:13 AM		Publish	1.00
Michael Gogins	2016	Sevier		7	mkg-2008-09-16-c	06:47.00	d:\Dropbox\studio\01_workshop\mkg-2008-09-16-c.wav	08/03/10 01:13 AM		Publish	1.00
Michael Gogins	2016	Sevier		8	mkg-2009-01-10-b	06:07.00	d:\Dropbox\studio\01_workshop\mkg-2009-01-10-b.wav	08/03/10 01:14 AM		Publish	1.00
Michael Gogins	2016	Sevier		9	mkg-2009-01-10-c	06:05.00	d:\Dropbox\Talks\Brads class\My Way\mkg-2009-01-10-c.wav	08/03/10 01:14 AM		Publish	1.00
Michael Gogins	2016	Sound Fractals	00:27	1	Sound Fractals	13:14.00	d:\Dropbox\2010-ICMC-MKG\soundifs\soundifs.py.wav	08/03/10 03:07 AM		Publish	1.00
Michael Gogins	2016	Sound Fractals		2	Apophysis-091206-3	02:01.00	d:\Dropbox\2010-ICMC-MKG\soundifs\Apophysis-091206-3.wav	08/03/10 03:07 AM		Publish	1.00
Michael Gogins	2016	Sound Fractals		3	Ifs-n.py	02:01.00	d:\Dropbox\2010-ICMC-MKG\soundifs\ifs-n.py.wav	08/03/10 01:16 AM		Publish	1.00
Michael Gogins	2016	Sound Fractals		4	Ifs-i.py	05:02.00	d:\Dropbox\2010-ICMC-MKG\soundifs\ifs-i.py.wav	08/03/10 01:16 AM		Publish	1.00
Michael Gogins	2016	Sound Fractals		5	ifs-2008-01-23-a.py.wav	05:04.00	d:\Dropbox\2010-ICMC-MKG\soundifs\ifs-2008-01-23-a.py.stereo.wav	08/03/10 01:15 AM		Publish	1.00
Michael Gogins	2016	Summertrees	00:48	1	Fractal 23	05:01.00	d:\Dropbox\Michael Gogins\Summertrees\01-Various _ 01_fractal23-a.mml.master.a.wav	01/14/14 03:12 AM		Publish	1.00			Accepted as composite
Michael Gogins	2016	Summertrees		2	f--2002-03-09--20-54-18.018.mml-a	07:01.00	d:\Dropbox\Michael Gogins\Summertrees\02-Various _ 02_f--2002-03-09--20-54-18.018.mml-a.csd.master.wav	01/14/14 03:13 AM		Publish	1.00			Accepted as composite
Michael Gogins	2016	Summertrees		3	f--2002-03-08--13-48-05.005-a.mml	07:02.00	d:\Dropbox\Michael Gogins\Summertrees\03-Various _ 03_f--2002-03-08--13-48-05.005-a.mml.master.wav	01/14/14 03:12 AM		Publish	1.00			Accepted as composite
Michael Gogins	2016	Summertrees		4	B-2001-10-27-a.mml-a	06:48.00	d:\Dropbox\Michael Gogins\Summertrees\04-Various _ 04_B-2001-10-27-a.mml-a.csd.py.master.wav	01/14/14 03:14 AM		Publish	1.00			Accepted as composite
Michael Gogins	2016	Summertrees		5	f--2002-01-10--23-12-54.054-b.mml	03:24.00	d:\Dropbox\Michael Gogins\Summertrees\05-Various _ 05_f--2002-01-10--23-12-54.054-b.mml.master.wav	01/14/14 03:14 AM		Publish	1.00			Accepted as composite
Michael Gogins	2016	Summertrees		6	drone.py.wav	09:50.00	d:\Dropbox\Michael Gogins\Summertrees\06-Various _ 06_drone.py.wav	01/14/14 03:14 AM		Publish	1.00			Accepted as composite
Michael Gogins	2016	Summertrees		7	Attractor1.mml-b	04:08.00	d:\Dropbox\Michael Gogins\Summertrees\07-Various _ 07_Attractor1.mml-b.master.wav	01/14/14 03:15 AM		Publish	1.00			Accepted as composite
Michael Gogins	2016	Summertrees		8	csound[2005-04-16][15.29.17]-a.py	05:09.00	d:\Dropbox\Michael Gogins\Summertrees\08-Various _ 08_csound[2005-04-16][15.29.17]-a.py.master.wav	01/14/14 03:15 AM		Publish	1.00			Accepted as composite
'''
# Test:
database = '''
Michael Gogins	2016	Garden of the Hesperides		3	Telamon - Copy	08:02.00	d:\Dropbox\\2015-06-22 NYCEMF\Telamon - Copy.wav	02/08/15 12:24 PM		Publish	0.00						Published
'''
lines = database.split('\n')
filecount = 0
for line in lines:
    line = line.replace('\\', '/');
    parts = line.split('\t')
    if len(parts) >= 7:
        author = parts[0]
        year = parts[1]
        album = parts[2]
        track = int(parts[4])
        title = parts[5]
        filepath = parts[7]
        directory, basename = os.path.split(filepath)
        rootname = os.path.splitext(basename)[0]
        label = '%s -- %s -- Track %s -- %s' % (author, album, track, title)
        master_filename = '%s.master.wav' % label
        spectrogram_filename = '%s.png' % label
        cd_quality_filename = '%s.cd.wav' % label
        mp3_filename = '%s.mp3' % label
        wmv_filename = '%s.wmv' % label
        print 'Original file:          ', filepath
        print 'Basename:               ', basename
        print 'Rootname:               ', rootname
        print 'Title:                  ', title
        print 'Master filename:        ', master_filename
        print 'Spectrogram filename:   ', spectrogram_filename
        print 'CD quality filename:    ', cd_quality_filename
        print 'MP3 filename:           ', mp3_filename
        print 'WMV filename:           ', wmv_filename
        sox_normalize_command = '''%s\\sox -S "%s" "%s" gain -n -3''' % (sox_path, filepath, master_filename)
        print 'sox_normalize command:  ', sox_normalize_command
        os.system(sox_normalize_command)
        sox_spectrogram_command = '''%s\\sox -S "%s" -n spectrogram -t"%s" -c"Copyright (C) %s by Michael Gogins (http://michaelgogins.tumblr.com), Irreducible Productions (ASCAP)" -o"%s"''' % (sox_path, master_filename, label, year, spectrogram_filename)
        print 'sox_spectrogram_command:', sox_spectrogram_command
        os.system(sox_spectrogram_command)
        sox_cd_command = '''%s\\sox -S "%s" -b 16 -r 44100 "%s"''' % (sox_path, master_filename, cd_quality_filename)
        print 'sox_cd_command:         ', sox_cd_command
        os.system(sox_cd_command)
        print
        filecount = filecount + 1
        print

print 'Finished with', filecount, 'files.'

