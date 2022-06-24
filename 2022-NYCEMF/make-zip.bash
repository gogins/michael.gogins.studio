#!/bin/bash
rm -f advanced_csound.zip
find advanced-csound -name "Michael_Gogins*" -ls
find advanced-csound -name "test.*" -ls
find advanced-csound -name "*.dSYM/" -ls
find advanced-csound -name "score.srt" -ls
find advanced-csound/exercises/commandline -name "*.wav" -ls
find advanced-csound/exercises/languages -name "*.wav" -ls
find advanced-csound/exercises/resolution -name "*.wav" -ls

find advanced-csound -name "Michael_Gogins*" -delete
find advanced-csound -name "test.*" -delete
find advanced-csound -name "*.dSYM/" -delete
find advanced-csound -name "score.srt" -delete
find advanced-csound/exercises/commandline -name "*.wav" -delete
find advanced-csound/exercises/languages -name "*.wav" -delete
find advanced-csound/exercises/resolution -name "*.wav" -delete

zip advanced_csound.zip -r advanced-csound/*

ls -ll advanced_csound.zip

