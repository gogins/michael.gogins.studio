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
import string
import sys
import time
import traceback

output = r'complete.m3u'
spreadsheet = r'complete_soundfiles.tsv'

rootdirs = string.split('c:\\Users\\mike c:\\Users\\restore d:\\Dropbox f:\\ D:\\msys64\\home\\restore\\michael.gogins.studio')
omitdirs = string.split('rawwaves orc eSupport attic Attic music-attic Examples Music MUSIC examples winabx csound-csound6-git ssdir Hrabovsky')
extensions = string.split('.wav .aif .aiff')

def omit(omitdirs, filepath):
    for omitdir in omitdirs:
        omitdir = os.sep + omitdir + os.sep
        if filepath.find(omitdir) != -1:
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
basenames = timesForBasenames.keys()
basenames.sort()
for basename in basenames:
    timesAndPaths = timesForBasenames[basename]
    times = timesAndPaths.keys()
    times.sort()
    times.reverse()
    filecount = filecount + 1
    for tyme in times:
        timestring = time.strftime('%Y-%m-%d %H:%M:%S', time.gmtime(tyme))
        print '[%5d %s] %s' % (filecount, timestring, basename)
        print '                           ', timesAndPaths[tyme]
    playlist.write('#EXTINF:-1,%s\n' % basename)
    pathname = timesAndPaths[times[0]]
    playlist.write('%s\n' % pathname)
    spreadsheet_output.write('Michael Gogins\t%s\t%s\t%s\n' % (basename, pathname, timestring))
    print
playlist.write('\n')

print 'Finished with', filecount, 'files.'

