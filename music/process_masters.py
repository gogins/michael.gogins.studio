# -*- coding: utf-8 -*-
'''
P R O C E S S   M A S T E R S
Author: Michael Gogins

Attempts to process masters for all works listed in a tab-delimited text
database. Processing produces normalized, cd-audio, mp3, and wmv files
with metadata and sortable, self-explanatory filenames in a single directory.
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
bwfmetaedit_path = r'''C:\Program Files\BWF_MetaEdit_CLI_1.3.1_Windows_x64'''
# YouTube accepts MOV, MP4 (MPEG4), AVI, WMV, FLV, 3GP, MPEGPS, WebM
# To use FFmpeg see https://bbs.archlinux.org/viewtopic.php?id=168433 and https://www.virag.si/2015/06/encoding-videos-for-youtube-with-ffmpeg/
# ffmpeg -loop 1 -framerate 2 -i input.png -i audio.ogg -c:v libx264 -preset medium -tune stillimage -crf 18 -c:a copy -shortest -pix_fmt yuv420p output.mkv
# ffmpeg -i <input file> -codec:v libx264 -crf 21 -bf 2 -flags +cgop -pix_fmt yuv420p -codec:a aac -strict -2 -b:a 384k -r:a 48000 -movflags faststart <output_name>.mp4
ffmpeg_program = r'''C:\Program_Files\ffmpeg-3.2-win64-shared\bin\ffmpeg.exe'''
# ffmpeg tags are documented here: http://jonhall.info/how_to/create_id3_tags_using_ffmpeg

cd_had_titles_in_reverse_order = r'''
Michael Gogins	2001	Garden of Algorithms	01:08	1	Cloud Strata	10:01.00	d:\Dropbox\Michael Gogins\Garden of Algorithms\01-Cloud Strata.wav	01/14/14 03:03 AM		Publish	0.00		Registered		Published by CDBaby			Published
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
'''
database = r'''
Michael Gogins	2001	Garden of Algorithms	01:08	1	Cloud Strata	10:01.00	d:\Dropbox\Michael Gogins\Garden of Algorithms\01-Cloud Strata.wav	01/14/14 03:03 AM		Publish	0.00		Registered		Published by CDBaby			Published
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
Michael Gogins	2017	NYCEMF		1	Black_Mountain.19.html	09:15.00	C:/Users/restore/michael.gogins.studio/music/Black Mountain 19.wav	04/25/17 05:12 PM		Publish	0.00						#Published
'''
database = r'''
Michael Gogins	2017	NYCEMF		1	Black_Mountain.7d.html	09:15.00	C:/Users/restore/michael.gogins.studio/music/Black Mountain 7d.wav	04/25/17 05:00 AM		Publish	0.00						#Published
'''
database = r'''
Michael Gogins	2017	NYCEMF		1	Black_Mountain.jak.html	09:15.00	~/michael.gogins.studio/music/Black Mountain.jak.wav	04/25/17 05:00 AM		Publish	0.00						#Published
'''
database = r'''
Michael Gogins	2017	NYCEMF		1	Black_Mountain.7d.html	09:15.00	C:/Users/restore/michael.gogins.studio/music/Black Mountain 7d.wav	04/25/17 05:00 AM		Publish	0.00						#Published
Michael Gogins	2017	NYCEMF		1	Black_Mountain.7e.html	09:15.00	C:/Users/restore/michael.gogins.studio/music/Black Mountain 7e.wav	04/25/29 05:00 AM		Publish	0.00						#Published
'''

notes = 'Electroacoustic Music'
lines = database.split('\n')
cwd = os.getcwd()
print 'cwd:                    ', cwd
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
        mp4_filename = '%s.mp4' % label
        print 'Original file:          ', filepath
        print 'Basename:               ', basename
        print 'Rootname:               ', rootname
        print 'Title:                  ', title
        print 'Master filename:        ', master_filename
        print 'Spectrogram filename:   ', spectrogram_filename
        print 'CD quality filename:    ', cd_quality_filename
        print 'MP3 filename:           ', mp3_filename
        print 'MP4 filename:           ', mp4_filename
        bext_description       = notes
        bext_originator        = 'Michael Gogins'
        bext_orig_ref          = basename
        #bext_umid              = xxx
        #bext_orig_date         = xxx
        #bext_orig_time         = xxx
        #bext_coding_hist       = xxx
        #bext_time_ref          = xxx
        str_comment            = notes
        str_title              = title
        str_copyright          = 'Copyright (C) %s by Michael Gogins' % year
        str_artist             = 'Michael Gogins'
        str_date               = year
        str_album              = album
        str_license            = 'ASCAP'
        sox_normalize_command = '''%s\\sox -S "%s" "%s" gain -n -3''' % (sox_path, filepath, master_filename + 'untagged.wav')
        print 'sox_normalize command:  ', sox_normalize_command
        os.system(sox_normalize_command)
        tag_wav_command = '''%s\\sndfile-metadata-set "%s" --bext-description "%s" --bext-originator "%s" --bext-orig-ref "%s" --str-comment "%s" --str-title "%s" --str-copyright "%s" --str-artist  "%s" --str-date "%s" --str-album "%s" --str-license "%s" "%s"''' % (sndfile_path, master_filename + 'untagged.wav', bext_description, bext_originator, bext_orig_ref, str_comment, str_title, str_copyright, str_artist, str_date, str_album, str_license, master_filename)
        print 'tag_wav_command:        ', tag_wav_command
        os.system(tag_wav_command)
        sox_spectrogram_command = '''%s\\sox -S "%s" -n spectrogram -o "%s" -t"%s" -c"%s"''' % (sox_path, master_filename, spectrogram_filename, label, str_copyright + ' (Irreducible Productions, ASCAP)')
        print 'sox_spectrogram_command:', sox_spectrogram_command
        os.system(sox_spectrogram_command)
        sox_cd_command = '''%s\\sox -S "%s" -b 16 -r 44100 "%s"''' % (sox_path, master_filename, cd_quality_filename + 'untagged.wav')
        print 'sox_cd_command:         ', sox_cd_command
        os.system(sox_cd_command)
        tag_wav_command = '''%s\\sndfile-metadata-set "%s" --bext-description "%s" --bext-originator "%s" --bext-orig-ref "%s" --str-comment "%s" --str-title "%s" --str-copyright "%s" --str-artist  "%s" --str-date "%s" --str-album "%s" --str-license "%s" "%s"''' % (sndfile_path, cd_quality_filename + 'untagged.wav', bext_description, bext_originator, bext_orig_ref, str_comment, str_title, str_copyright, str_artist, str_date, str_album, str_license, cd_quality_filename)
        print 'tag_wav_command:        ', tag_wav_command
        os.system(tag_wav_command)
        mp3_command = '''%s\\lame.exe --add-id3v2 --tt "%s" --ta "%s" --tl "%s" --ty "%s" --tc "%s" --tn "%s" --tg "%s"  "%s" "%s"''' % (lame_path, title, "Michael Gogins", album, year, notes, track, "Electroacoustic", master_filename, mp3_filename)
        print 'mp3_command:            ', mp3_command
        os.system(mp3_command)
        mp4_command = '''"%s" -r 1 -i "%s" -i "%s" -codec:a aac -strict -2 -b:a 384k -r:a 48000 -c:v libx264 -b:v 500k "%s"''' % (ffmpeg_program, os.path.join(cwd, spectrogram_filename), os.path.join(cwd, master_filename), os.path.join(cwd, mp4_filename))
        mp4_metadata =  '-metadata title="%s" ' % title
        mp4_metadata += '-metadata album="%s" ' % album
        mp4_metadata += '-metadata date="%s" ' % year
        mp4_metadata += '-metadata track="%s" ' % track
        mp4_metadata += '-metadata genre="%s" ' % "Electroacoustic Music"
        mp4_metadata += '-metadata publisher="%s" ' % 'Irreducible Productions, ASCAP'
        mp4_metadata += '-metadata copyright="%s" ' % str_copyright
        mp4_metadata += '-metadata composer="%s" ' % 'Michael Gogins'
        mp4_metadata += '-metadata composer="%s" ' % 'Michael Gogins'
        mp4_metadata += '-metadata artist="%s" ' % 'Michael Gogins'
        mp4_command = '''"%s" -y -loop 1 -framerate 2 -i "%s" -i "%s" -c:v libx264 -preset medium -tune stillimage -crf 18 -codec:a aac -strict -2 -b:a 384k -r:a 48000 -shortest -pix_fmt yuv420p -vf "scale=trunc(iw/2)*2:trunc(ih/2)*2" %s "%s"''' % (ffmpeg_program, os.path.join(cwd, spectrogram_filename), os.path.join(cwd, master_filename), mp4_metadata, os.path.join(cwd, mp4_filename))
        mp4_command = mp4_command.replace('\\', '/')
        print 'mp4_command:            ', mp4_command
        # Limited length for os.system command, so...
        with open('temp.bat', 'w') as f:
            f.write(mp4_command)
            f.write('\n')
            f.close()
        os.system('temp.bat')
        filecount = filecount + 1
        os.system('del *wavuntagged.wav')
        print
print 'Finished with', filecount, 'files.'

