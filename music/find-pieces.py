'''
Builds a table of all files that might be pieces of mine. 

The script creates two empty tables, sources and soundfiles, and crawls 
the filesystem (except for omitted directories) to populate these tables. 

When the filesystem crawl has been completed, soundfiles are matched with 
sources. The title in the soundfile metadata is used for the title of the 
piece. If a title is not found, a match with the soundfile basename is tried. 
Then an outer join is produced, sorted first by composition basename, then by 
soundfile basename.

In future, ensure the full pathname of the composition source code is encoded 
as a tag in the soundfile!

The outputs are as follows.

# playlist.m3u

The playlist has entries like this:

#EXTM3U
#EXTINF:0,01-19991216a.wav
D:\\Dropbox\\Michael Gogins\\Semblance\\01-19991216a.wav

One may load this playlist into a media player and listen to any piece at 
any point in time.

# compositions.tsv

This file containing the outer join table. One may scan this table 
for missing matches; it may then be possible to manually create missing
matches, usually by re-rendering or re-post-processing the piece.
'''

import datetime
import googlesearch
import os
import ffmpeg
import re
import string
import sys
import time
import traceback

playlist_filename = r'complete-%s.m3u' % datetime.date.today()
compositions_filename = r'complete_pieces-%s.tsv' % datetime.date.today()

# rootdirs = '/Users/michaelgogins'.split()
rootdirs = '/Users/michaelgogins/Dropbox /Users/michaelgogins/michael.gogins.studio /Users/michaelgogins/cloud-5 /Users/michaelgogins/from-backups'.split()

# One must simply run this script over and over, and keep adding to omit_directories until only actual pieces are found.
# Keep this in alphabetical order just to make it easier to edit.
omit_directories = '''.venv
Android
Art Hunkins
Attic
BlackHole
CMakeFiles
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
adapt-these
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
papers
performance-mode
pipx
plugins
python-abx
python-soundfile
rawwaves
recipes
shader-web-background
spatialization
ssdir
strudel
synthv-editor
technical-manual
tempy
test
tests
understand
venv
venvs
vst
vst4cs
vstsdk-2.4
whatthefrack
winabx
workshop'''
omit_directories = omit_directories.split()

'''
# This doesn't work well, as it is soon throttled by Google.
# However, manual searches for pieces can be done.
results = googlesearch.search('"Michael Gogins" site:sonus.ca', num_results=10)
for result in results:
    print(result)
exit()
'''
def parse_tags(audio_file):
    try:
        metadata = ffmpeg.probe(audio_file)
        tags = metadata.get('format', {}).get('tags', {})
        composer = tags.get('artist', 'Composer not found')
        title = tags.get('title', 'Title not found')
        duration = float(metadata.get('format', {}).get('duration', -1))
        date = tags.get('date', 'Date not found')
        return (composer, title, duration, date)
    except ffmpeg.Error as e:
        print("ffmpeg error:", e.stderr.decode())

composition_extensions = '.html .csd .lua .lisp .cpp .bas .pas .rpp .py'.split()
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
        if filepath.find("-generated.csd") != -1:
            return True
    return False

compositions = {}
soundfiles = {}

# Csound and Reaper files can probably always be considered compositions.
# Other files containing "csound" or "Gogins" likewise.
# What else?

def add(pathname):
    basename = os.path.basename(pathname)
    filename, extension = os.path.splitext(basename)
    extension = extension.lower()
    if omit(omit_directories, pathname) == False:
        if extension in audio_extensions:
            filestat = os.stat(pathname)
            # Piece should be at least a minute long.
            if filestat.st_size > 10000000:
                soundfiles[pathname] = basename
                print("Soundfile:  ", pathname)
        if extension in composition_extensions:
            if extension == '.csd':
                compositions[pathname] = basename
            elif extension == '.rpp':
                compositions[pathname] = basename
            else:
                with open(pathname, 'r') as f:
                    text = f.read()
                    if re.search(r'\bcsound\b', text, re.IGNORECASE):
                        compositions[pathname] = basename
                    elif 'Gogins' in text:
                        compositions[pathname] = basename
            print("Source code:", pathname)

for rootdir in rootdirs:
    for root, subdirs, files in os.walk(rootdir):
        for filename in files:
            try:
                pathname = os.path.join(root, filename)
                add(pathname)
            except:
                traceback.print_exc()

soundfiles  = dict(sorted(soundfiles.items()))
compositions  = dict(sorted(compositions.items()))

print("\nBuilding a playlist of all audio files...\n")

filecount = 0
playlist = open(playlist_filename, 'w')
playlist.write('#EXTM3U\n')
for soundfile in soundfiles.keys():
    basename = os.path.basename(soundfile)
    filecount = filecount + 1
    playlist.write('#EXTINF:-1,%s\n' % basename)
    playlist.write('%s\n' % soundfile)
    print(f"Playlist: {soundfile}")
playlist.write('\n')

print("\nFinding compositions for audio files...\n")

# The title is expected to be stored as metadata in the soundfile, 
# but if the title tag is not found, perhaps the filename encodes the title.

matches = 0
compositions_matched = set()
soundfiles_matched = set()
compositions_tsv = open(compositions_filename, 'w')
compositions_tsv.write("Composer\tTitle\tSoundfile\tComposition\tDuration\tDate\n")
for soundfile in soundfiles.keys():
    composer, title, duration, date = parse_tags(soundfile)
    for composition in compositions.keys():
        if title in composition:
            matches = matches + 1
            print(f"Match:             {matches}")
            print(f"\tComposer:  {composer}")
            print(f"\tTitle:     {title}")
            print(f"\tSoundfile: {soundfile}")
            print(f"\tSource:    {composition}")
            print(f"\tDuration:  {duration}")
            print(f"\tDate:      {date}")
            compositions_matched.add(composition)
            soundfiles_matched.add(soundfile)
            compositions_tsv.write(f"{composer}\t{title}\t{soundfile}\t{composition}\t{duration}\t{date}\n")
        else:
            basename = os.path.basename(soundfile)
            filename, extension = os.path.splitext(basename)
            filename = "/" + filename
            if filename in composition:
                matches = matches + 1
                print(f"Match:             {matches}")
                print(f"\tComposer:  {composer}")
                print(f"\tFilename:  {filename}")
                print(f"\tSoundfile: {soundfile}")
                print(f"\tSource:    {composition}")
                print(f"\tDuration:  {duration}")
                print(f"\tDate:      {date}")
                compositions_matched.add(composition)
                soundfiles_matched.add(soundfile)
                compositions_tsv.write(f"{composer}\t{title}\t{soundfile}\t{composition}\t{duration}\t{date}\n")
for composition in compositions.keys():
    if composition not in compositions_matched:
        print(f"Source:    {composition}")
        compositions_tsv.write(f"\t\t\t{composition}\t\t\n")
for soundfile in soundfiles.keys():
    if soundfile not in soundfiles_matched:
        print(f"Soundfile: {soundfile}")
        compositions_tsv.write(f"\t\t{soundfile}\t\t\t\n")

print(f'\nPossible compositions ({len(compositions.keys())}) saved here:\n\t{compositions_filename}')
print(f'Playlist ({len(soundfiles.keys())}) saved here:\n\t{playlist_filename}')
print(f'Matched {matches} compositions with soundfiles.\n')



