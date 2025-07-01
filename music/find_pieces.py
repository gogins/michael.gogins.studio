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
name of the track, but that isn't always the case. Therefore, the join should 
try to match metadata titles with source basenames first, then source 
basenames with output basenames.

In future ensure the full pathname of the source is encoded as a tag.

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

playlist_filename = r'complete-%s.m3u' % datetime.date.today()
compositions_filename = r'complete_pieces-%s.tsv' % datetime.date.today()

rootdirs = '/Users/michaelgogins'.split()
# One must simply run this script over and over, and keep adding to omit_directories until only actual pieces are found.
# Keep this in alphabetical order just to make it easier to edit.
omit_directories = '''Android
Art Hunkins
Attic
BlackHole
CsoundPlugin
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
Notes from the Metalevel
Steinberg
Strudel
Trash
VST
VST_SDK
VSTPlugins
ace-builds
asiosdk2
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
csound-vst4cs-opcodes
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
exercises
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
nwbuild-poustinia
orc
p5.js
performance-mode
plugins
python-abx
rawwaves
recipes
shader-web-background
ssdir
strudel
synthv-editor
technical-manual
tempy
test
tests
understand
venvs
vst
vst4cs
vstsdk-2.4
whatthefrack
winabx
workshop'''
omit_directories = omit_directories.split()
print()
for directory in omit_directories:
    print(directory)
print()

def parse_tags(audio_file):
    try:
        print(pymediainfo.MediaInfo.parse(pathname, output="text", full=False))
        # media_info = pymediainfo.MediaInfo.parse(audio_file)
        # for track in media_info.tracks:
        #     if track.track_type == "Audio":
        #         print(f"Duration: {track.duration} ms")
        #         print(f"Sample rate: {track.sampling_rate} Hz")
        #         print(f"Channels: {track.channel_s}")
        #         print(f"Codec: {track.codec}")        
    except:
        pass

composition_extensions = '.html .csd .cpp .bas .pas'.split()
audio_extensions = '.wav .aif .aiff'.split()

def omit(omit_directories, filepath):
    for omit_directory in omit_directories:
        omit_directory = os.sep + omit_directory + os.sep
        if filepath.find(omit_directory) != -1:
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
compositions = set()
soundfiles = set()

def add(pathname):
    filename, extension = os.path.splitext(pathname)
    basename = os.path.basename(pathname)
    if omit(omit_directories, pathname) == False:
        if extension.lower() in audio_extensions:
            filestat = os.stat(pathname)
            # Piece should be at least a minute long.
            if filestat.st_size > 10000000:
            # The most recent version will be picked.
                if basename not in timesForBasenames:
                    timesForBasenames[basename] = {}
                timesForBasenames[basename][filestat.st_ctime] = pathname
                soundfiles.add(pathname)
                print("Soundfile:  ", pathname)
                parse_tags(pathname)
        if extension.lower() in composition_extensions:
            compositions.add(pathname)
            print("Composition?", pathname)

for rootdir in rootdirs:
    for root, subdirs, files in os.walk(rootdir):
        for filename in files:
            try:
                pathname = os.path.join(root, filename)
                add(pathname)
            except:
                traceback.print_exc()

print("\nBuilding a playlist of all audio files...\n")
filecount = 0
playlist = open(playlist_filename, 'w')
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
    print()
playlist.write('\n')
print('Finished with', filecount, 'audio files.')

print("\nBuilding a table of all composition files...\n")
filecount = 0
composition_files = open(compositions_filename, 'w')
compositions = sorted(compositions)
for composition in compositions:
    # Here we should try to match compositions with outputs, and perhaps even 
    # with online publications.
    composition_files.write('%s\n' % composition)
    filecount = filecount + 1

print('Finished with', filecount, 'composition files.')



