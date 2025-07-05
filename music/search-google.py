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
rootdirs = '/Users/michaelgogins/Dropbox /Users/michaelgogins/michael.gogins.studio /Users/michaelgogins/cloud-5'.split()

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


# This doesn't work well, as it is soon throttled by Google.
# However, manual searches for pieces can be done.
results = googlesearch.search('"Michael Gogins" site:music.youtube.com', num_results=40, advanced=True)
for result in results:
    if result.title.startswith("Michael Gogins"):
        print(result.url, result.title)
exit()

