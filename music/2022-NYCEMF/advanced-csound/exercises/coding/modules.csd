<CsoundSynthesizer>
<CsOptions>
-m165 -d -RWomodules.wav
</CsOptions>
<CsInstruments>

/* Written by Michael Gogins */
; Initialize the global variables.
sr = 48000
ksmps = 128
nchnls = 2

; Connect up the instruments to create a signal flow graph.

connect "SimpleSine",   "outleft",     "Reverberator",     	"inleft"
connect "SimpleSine",   "outright",    "Reverberator",     	"inright"

connect "Moogy",        "outleft",     "Reverberator",     	"inleft"
connect "Moogy",        "outright",    "Reverberator",     	"inright"

connect "Reverberator", "outleft",     "Compressor",       	"inleft"
connect "Reverberator", "outright",    "Compressor",       	"inright"

connect "Compressor",   "outleft",     "Soundfile",       	"inleft"
connect "Compressor",   "outright",    "Soundfile",       	"inright"

; Turn on the "effect" units in the signal flow graph.

alwayson "Reverberator", 0.91, 12000
alwayson "Compressor"
alwayson "Soundfile"

gi_SimpleSine_sine ftgenonce 0, 0, 65567, 10, 1
instr SimpleSine
  ihz = cpsmidinn(p4)
  iamplitude = ampdb(p5)
  print ihz, iamplitude
  a1 poscil3 iamplitude, ihz, gi_SimpleSine_sine
  aenv madsr 0.05, 0.1, 0.5, 0.2
  asignal = a1 * aenv
  ; Stereo audio outlet to be routed in the orchestra header.
  outleta "outleft", asignal * 0.25
  outleta "outright", asignal * 0.75
endin

instr Moogy
  ihz = cpsmidinn(p4)
  iamplitude = ampdb(p5)
  ; Use ftgenonce instead of ftgen, ftgentmp, or f statement.
  isine ftgenonce 0, 0, 4096, 10, 1
  asignal vco iamplitude, ihz, 1, 0.5, isine
  kfco line 200, p3, 2000
  krez init 0.9
  asignal moogvcf asignal, kfco, krez, 100000
  ; Stereo audio outlet to be routed in the orchestra header.
  outleta "outleft", asignal * 0.75
  outleta "outright", asignal * 0.25
endin

instr Reverberator
  ; Stereo input.
  aleftin inleta "inleft"
  arightin inleta "inright"
  idelay = p4
  icutoff = p5
  aleftout, arightout reverbsc aleftin, arightin, idelay, icutoff
  ; Stereo output.
  outleta "outleft", aleftout
  outleta "outright", arightout 
endin

instr Compressor
  ; Stereo input.
  aleftin inleta "inleft"
  arightin inleta "inright"
  kthreshold = 25000
  icomp1 = 0.5
  icomp2 = 0.763
  irtime = 0.1
  iftime = 0.1
  aleftout dam aleftin, kthreshold, icomp1, icomp2, irtime, iftime
  arightout dam arightin, kthreshold, icomp1, icomp2, irtime, iftime
  ; Stereo output.
  outleta "outleft", aleftout 
  outleta "outright", arightout 
endin

instr Soundfile
  ; Stereo input.
  aleftin inleta "inleft"
  arightin inleta "inright"
  outs aleftin, arightin
endin

</CsInstruments>
<CsScore>
; Not necessary to activate "effects" or create f-tables in the score!
; Overlapping notes to create new instances of instruments.
i "SimpleSine" 1 5 60 85
i "SimpleSine" 2 5 64 80
i "Moogy" 3 5 67 75
i "Moogy" 4 5 71 70
</CsScore>
</CsoundSynthesizer>


