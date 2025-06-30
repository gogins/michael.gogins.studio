'''
Builds a table of all files that might be sources for pieces of mine. Once a 
piece has been identified, a search for output .wav, .aiff, .mid, or .smf will 
be made. Metadata also will be read. Progress will be printed to aid in 
improving the script. Outputs are a table of pieces with columns in the 
following order: 

Title
Date composed
My URL
Public URL
Duration

This table can have multiple rows for the same title, so the table is joined 
by title.

And an .m3u playlist of the highest resolution rendering found, e.g.:

#EXTM3U
#EXTINF:0,01-19991216a.wav
D:\\Dropbox\Michael Gogins\Semblance\01-19991216a.wav

The script will create two empty tables, sources and outputs, and crawl the 
filesystem except for omitted directories. If there is metadata in an output, 
print the title, and add it to that entry.

When the filesystem crawl has been completed, match sources with outputs to 
create the final output table. In theory the basename of the filepath is the 
title of the piece, but that isn't always the case. Therefore, the join should 
try to match metadata titles with source basenames first, then source 
basenames with output basenames.

When that table has been processed, write the following files:

sources.tsv
playlist.m3u

'''

import datetime
import os
import pymediainfo
import string
import sys
import time
import traceback

output = r'complete-%s.m3u' % datetime.date.today()
spreadsheet = r'complete_pieces-%s.tsv' % datetime.date.today()

rootdirs = '/Users/michaelgogins'.split()
# One must simply run this script over and over, and keep adding to omitdirs until only actual pieces are found.
# Keep this in alphabetical order just to make it easier to check.
omitdirs = '''Android
Attic
Downloads
Examples
Hrabovsky
JUCE
Jacko_csd
Library
MUSIC
Music
My-Csound-Wwise
Nancarrow_Renderings
Steinberg
Strudel
Trash
VST
VST_SDK
ace_builds
attic
boost_1_88_0
build
build-macos
cabbage
cabbage_csd.vst3
cache
cm
consolidation
coverage
csound
csound-ac
csound-android
csound-cxx-opcodes
csound-examples
csound-examples
csound-extended
csound-extended-vst4cs
csound-nwjs
csound-vst
csound-vst2
csound-vst3
csound-vst3-opcodes
csound-wasm
csound-webserver-opcodes
csoundvst
de
dependencies
deps
docs
dual-test
eSupport
emsdk
esdocs
examples
extensions
functions
ghc
imparting_harmonies
jak_stretch
jsdocs
kolmogorov
learn
libmusicxml
libsndfile
manual
music-attic
musx
node_modules
nwbuild-cloud-5
orc
p5.js
performance-mode
plugins
rawwaves
recipes
ssdir
strudel
synthv-editor
technical-manual
tempy
test
tests
venvs
vst
vstsdk-2.4
winabx'''
omitdirs = omitdirs.split()
print()
for dir in omitdirs:
    print(dir)
print()

source_extensions = '.html .csd .cpp .bas .pas'.split()
audio_extensions = '.wav .aif .aiff'.split()

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
sources = set()
outputs = set()

def add(pathname):
    filename, extension = os.path.splitext(pathname)
    basename = os.path.basename(pathname)
    if omit(omitdirs, pathname) == False:
        if extension.lower() in audio_extensions:
            filestat = os.stat(pathname)
            # Piece should be at least a minute long.
            if filestat.st_size > 10000000:
            # The most recent version will be picked.
                if basename not in timesForBasenames:
                    timesForBasenames[basename] = {}
                timesForBasenames[basename][filestat.st_ctime] = pathname
                outputs.add(pathname)
                print("Audio: ", pathname)
        if extension.lower() in source_extensions:
            sources.add(pathname)
            print("Source:", pathname)
        

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

