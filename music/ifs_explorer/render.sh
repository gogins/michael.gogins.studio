#!/bin/bash

csound ifs_explorer.csd -o$1.wav
sox $1.wav -n spectrogram -o "$1.png" -t "$2"
xdg-open "$1.png"
aplay $1.wav