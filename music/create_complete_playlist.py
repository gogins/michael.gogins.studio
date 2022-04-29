'''
Builds an m3u playlist of all soundfiles on my computer that might
conceivably be pieces of mine. Small files are omitted, files in certain
directories are omitted, later files are preferred to earlier files.

Sample:

#EXTM3U
#EXTINF:0,01-19991216a.wav
D:\Dropbox\Michael Gogins\Semblance\01-19991216a.wav

'''

import datetime
import os
import string
import sys
import time
import traceback

output = r'complete-%s.m3u' % datetime.date.today()
spreadsheet = r'complete_soundfiles-%s.tsv' % datetime.date.today()

rootdirs = '/Users/michaelgogins'.split()
omitdirs = 'synthv-editor performance-mode Trash csound-extended-vst4cs Downloads jak_stretch Nancarrow_Renderings imparting_harmonies rawwaves orc eSupport attic Attic music-attic Examples Music MUSIC examples winabx csound-extended ssdir Hrabovsky'.split()
extensions = '.wav .aif .aiff'.split()

def omit(omitdirs, filepath):
    for omitdir in omitdirs:
        omitdir = os.sep + omitdir + os.sep
        if filepath.find(omitdir) != -1:
            return True
        if filepath.find(".cd.wav") != -1:
            return True
        if filepath.find("test.wav") != -1:
            return True
        if filepath.find("Heidi Rogers") != -1:
            return True
        if filepath.find("TASCAM") != -1:
            return True
        if filepath.find("process-palette") != -1:
            return True
        if filepath.find(".CHK.wav") != -1:
            return True
        if filepath.find(".output.wav") != -1:
            return True
        if filepath.find(".wavuntagged.wav") != -1:
            return True
        if filepath.find("csound-extended") != -1:
            return True
    return False

timesForBasenames = {}

def add(pathname):
    filename, extension = os.path.splitext(pathname)
    basename = os.path.basename(pathname)
    if extension.lower() in extensions:
        if omit(omitdirs, pathname) == False:
            filestat = os.stat(pathname)
            # Piece should be at least a minute long.
            if filestat.st_size > 10000000:
            # The most recent version will be picked.
                if basename not in timesForBasenames:
                    timesForBasenames[basename] = {}
                timesForBasenames[basename][filestat.st_ctime] = pathname
                print(pathname)

for rootdir in rootdirs:
    for root, subdirs, files in os.walk(rootdir):
        for filename in files:
            try:
                pathname = os.path.join(root, filename)
                add(pathname)
            except:
                traceback.print_exc()

filecount = 0
playlist = open(output, 'w')
spreadsheet_output = open(spreadsheet, 'w')
playlist.write('#EXTM3U\n')
basenames = sorted(timesForBasenames.keys())
for basename in basenames:
    timesAndPaths = timesForBasenames[basename]
    times = sorted(timesAndPaths.keys())
    times.reverse()
    filecount = filecount + 1
    for tyme in times:
        timestring = time.strftime('%Y-%m-%d %H:%M:%S', time.gmtime(tyme))
        print('[%5d %s] %s\n                            %s' % (filecount, timestring, basename, timesAndPaths[tyme]))
    playlist.write('#EXTINF:-1,%s\n' % basename)
    pathname = timesAndPaths[times[0]]
    playlist.write('%s\n' % pathname)
    spreadsheet_output.write('Michael Gogins\t%s\t%s\t%s\n' % (basename, pathname, timestring))
    print()
playlist.write('\n')

print('Finished with', filecount, 'files.')

