<CsoundSynthesizer>
<CsLicense>
Drone-VIII-e
Copyright (C) 2014 by Michael Gogins.
All rights reserved.
</CsLicense>
<CsOptions>
-RWDfo Drone-VIII-e-1.wav -m227 --midi-key=4 --midi-velocity=5 -+id_artist=Michael_Gogins -+id_copyright=Copr_2019_Michael_Gogins -+id_title=Drone-VIII-e-1 
</CsOptions>
<CsInstruments>
sr = 96000
ksmps = 100
nchnls = 2
0dbfs = 10000

lua_exec {{
package.path = package.path .. ";/home/mkg/michael.gogins.studio/music/Tzimtzum/silencio/lua/?.lua;"
local ffi = require("ffi")
local math = require("math")
local string = require("string")
local silencio = require ("Silencio")
local LindenmayerPITV = require("LindenmayerPITV")
local ChordSpace = require ("ChordSpace")
local duration = 1
-- local csoundApi = ffi.load("csoundandroid.so")
local csoundApi = ffi.load("csound64.so")
-- Declare the parts of the Csound API that we need.
ffi.cdef[[
 int csoundGetKsmps(void *);
 double csoundGetSr(void *);
 int csoundMessage(void *, const char *format, ...);
 int csoundInputMessage(void *, const char *message);
 int csoundScoreEvent(void *csound, char type, const double *pFields, long numFields);
 int csoundReadScore(void *csound, const char *message);
]]
-- Print that we have acquired the global Csound object.
csoundApi.csoundMessage(csound, 'csound: %s\\n', tostring(csound))
-- Prove that the Csound API and object are working.
csoundApi.csoundMessage(csound, 'sr: %f\\n', csoundApi.csoundGetSr(csound))
local lindenmayer = LindenmayerPITV:new()
csoundApi.csoundMessage(csound, 'lindenmayer: %s\\n', tostring(lindenmayer))
local score = lindenmayer.score
score:setCsound(csound, csoundApi)
csoundApi.csoundMessage(csound, 'score: %s\\n', tostring(score))
lindenmayer:initialize(4, 4)
lindenmayer.duration = 300
--lindenmayer.axiom = 'P=C7 V=543 I=0 T=0 d=1 v=10 p=0.5 a P=CM7 T+5 a L T+3 L C C C'
  lindenmayer.axiom = 'P=C7 V=543 I=0 T=0 d=1 v=10 p=0.5 a P=CM7 T+5 d+1 L C C'
  lindenmayer.axiom = 'P=C7 V=1343 I=0 T=0 d=1 v=10 p=0.5 a P=CM7 T+5 d+1 L C C'
  lindenmayer.axiom = 'P=C7 V=1343 I=0 T=0 d=1.75 v=10 p=0.5 a P=CM7 T+5 d+1 L C'
--lindenmayer.rules['a'] = 'a L T+3 v+2   V+2 a v-2 C C I+1 C V-2 P=CM7 a L T+5 L d+.25 a P=C7 d-.5 I+1 T+2 '
  lindenmayer.rules['a'] = 'a L T+4 v+2   V+126 a v-2 C V-2 P=CM7 a L T+2 L K d+.25 V-80 a P=C7 d-.5  T+2 '    	              
  lindenmayer.rules['a'] = 'a L T+4 v+2   V+126 K a v-2 C V-2 P=CM7 a L T+2 L d+.25 V-80 a P=C7 d-.5  T+2 '  
  lindenmayer.rules['a'] = 'a K L v+2 K L V+126 a v-2 C V-2 P=CM7 d+.25 T-5 V-25 a L T+2 L d-.375 V-79 a P=C7 K '  
  lindenmayer.rules['a'] = 'a K L v+2 K L V+126 a v-2 C V-2 P=CM7 d+.25 T-5 V-26 a L T+2 L d-.375 V-79 a P=C7 K '  
  lindenmayer.rules['a'] = 'a K L v+2 K L V+125 a v-2 C V-2 P=CM7 d+.25 T-5 V-29 a L T+2 L d-.375 V-70 a P=C7 K '  
  lindenmayer.rules['a'] = 'a K L v+2 K L V+125 a v-2 C V-2 P=CM7 d+.25 T+4 V-23 a L T+2 L d-.375 V-70 a P=C7 K '  
  lindenmayer.rules['a'] = 'a K L v+2 K L V+125 a v-2 C V-2 P=CM7 d+.25 T+4 V-28 a L T+2 L d-.375 V-70 a P=C7 K '    
  lindenmayer.rules['a'] = 'a K L v+2 K L V+125 a v-2 C V-10 P=CM7 d+.25 T+7 V-26 a L T+2 L d-.375 V-70 a P=C7 K '    
  lindenmayer.rules['a'] = 'a K L v+2 K L a v-2 V-4 C P=CM7 d+.25 T+7 a L T+2 V-1 C d-.375 a P=C7 K '    
  lindenmayer.rules['a'] = 'a K L v+2 K L           a v-2    V-4 C P=CM7 d+.25 T+7        a   T+2 V-1 C d-.325 a P=C7  '    
  lindenmayer.rules['a'] = 'a K L v+2 K L V+125 a v-2 C V-2    P=CM7 d+.25 T-5 V-29 a C T+2 L d-.325 V-70 a P=C7 K '  
  lindenmayer.rules['a'] = 'a K L v+2 K L V+6     a v-2 C V-2    P=CM7 d+.25 T-5  V-29 a C T+2 L d-.325 V-70 a P=C7 K '  
  lindenmayer.rules['a'] = 'a K L v+2 K L V+125 a v-2 C V-2    P=CM7 d+.25 T-5  V-29 a L T+2 L d-.325 V-70 a P=C7 K '    
  lindenmayer.iterations = 2
lindenmayer:generate()
for key, value in ipairs(score) do
	value[PAN] = math.random()
end
score:setScale(KEY, 24.0)
score:setScale(CHANNEL, 0, 3)
score:setScale(VELOCITY, 40.0, 6)
score:setScale(PAN, 0.1, 0.8)
score:setDuration(60 * 6.5)
score:temper(12.0)
score:tieOverlaps()
score:print()
scales = score:findScales()
print('minima:', scales[1])
print('ranges:', scales[2])
score:sendToCsound(false)
csoundApi.csoundMessage(csound, 'Finished generating score.\\n')
}}

connect "Droner", "outleft", "ReverbLeft", "inleft"
connect "Droner", "outright", "ReverbRight", "inright"
connect "Blower", "outleft", "ReverbLeft", "inleft"
connect "Blower", "outright", "ReverbRight", "inright"
connect "Sweeper", "outleft", "ReverbLeft", "inleft"
connect "Sweeper", "outright", "ReverbRight", "inright"
connect "Buzzer", "outleft", "ReverbLeft", "inleft"
connect "Buzzer", "outright", "ReverbRight", "inright"
connect "ReverbLeft", "outleft", "MasterOutput", "inleft"
connect "ReverbRight", "outright", "MasterOutput", "inright"

alwayson "ReverbLeft"
alwayson "ReverbRight"
alwayson "MasterOutput"
alwayson "Controls"

gkwaveform init 0
gkDroner1 init 1
gkDroner2 init .8
gkDroner3 init 0
gkDroner4 init .1
gkDroner5 init .1
gkDroner6 init 0
gkDroner7 init 0
gkDroner8 init .05
gkDroner9 init 0
gkDroner10 init 0
gkDronerLevel init 3
gkDronerPan init .5
gkDronerEnabled init 1
instr Droner
if gkDronerEnabled == 1 then
insno = p1
istart = p2
iduration = p3
ikey = p4
ivelocity = p5
iphase = p6
ipan = p7
iamp = ampdb(ivelocity)
ioverlap = p3 / 4
iattack = ioverlap
idecay = ioverlap
isustain = p3 - ioverlap
p3 = iattack + isustain + idecay
kenvelope transeg 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
ihertz = cpsmidinn(ikey)
isine ftgenonce 0, 0, 65537, 10, 1, 0, .02
if gkwaveform == 0 then
asignal poscil3 1, ihertz, isine
endif
if gkwaveform == 1 then
asignal vco2 1, ihertz, 8 ; integrated saw
endif
if gkwaveform == 2 then
asignal vco2 1, ihertz, 12 ; triangle
endif
asignal chebyshevpoly asignal, gkDroner1, gkDroner2, gkDroner3, gkDroner4, gkDroner5, gkDroner6, gkDroner7, gkDroner8, gkDroner9, gkDroner10
asignal = asignal * kenvelope * gkDronerLevel * 0.01
aleft, aright pan2 asignal, ipan
adamping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
aleft = adamping * aleft * asignal
aright = adamping * aright * asignal
outleta "outleft", aleft
outleta "outright", aright
prints "Droner  %2d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
endif
if gkDronerEnabled != 1 then
adummy init 0
outleta "outleft", adummy
outleta "outright", adummy
endif
endin

gkbritel init 0
gkbriteh init 2.9
gkbritels init .2 / 3
gkbritehs init 2.5 / 4
gkSweeperLevel init 1
gkSweeperEnabled init 1
instr Sweeper
if gkSweeperEnabled == 1 then
insno = p1
istart = p2
iduration = p3
ikey = p4
ivelocity = p5
iphase = p6
ipan = p7
iamp = ampdb(ivelocity) / 4
gisine ftgenonce 0, 0, 65537, 10, 1
gioctfn ftgenonce 0, 0, 65537, -19, 1, 0.5, 270, 0.5
ioverlap = p3 / 4
iattack = ioverlap
idecay = ioverlap
isustain = p3 - ioverlap
p3 = iattack + isustain + idecay
kenvelope transeg 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
ihertz = cpsmidinn(ikey)
icps = ihertz
kamp expseg 0.001,0.02,0.2,p3-0.01,0.001
ktonemoddep jspline 0.01,0.05,0.2
ktonemodrte jspline 6,0.1,0.2
ktone poscil3 ktonemoddep, ktonemodrte, gisine
kbrite rspline gkbritel, gkbriteh, gkbritels, gkbritehs
ibasfreq init icps
ioctcnt init 3
iphs init 0
;a1 hsboscil kamp, ktone, kbrite, ibasfreq, gisine, gioctfn, ioctcnt, iphs
a1 hsboscil kenvelope, ktone, kbrite, ibasfreq, gisine, gioctfn, ioctcnt, iphs
amod poscil3 0.25, ibasfreq*(1/3), gisine
arm = a1*amod
kmix expseg 0.001, 0.01, rnd(1), rnd(3)+0.3, 0.001
kmix=.25
a1 ntrpol a1, arm, kmix
;a1 pareq a1/10, 400, 15, .707
;a1 tone a1, 500
kpanrte jspline 5, 0.05, 0.1
kpandep jspline 0.9, 0.2, 0.4
kpan poscil3 kpandep, kpanrte, gisine
a1,a2 pan2 a1, ipan
a1 delay a1, rnd(0.1)
a2 delay a2, rnd(0.11)
kenv linsegr 1, 1, 0
kenv = kenvelope * gkSweeperLevel
aleft = a1*kenv*.02
aright = a2*kenv*.02
adamping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
aleft = adamping * aleft
aright = adamping * aright
outleta "outleft", aleft
outleta "outright", aright
prints "Sweeper %2d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
endif
if gkSweeperEnabled != 1 then
adummy init 0
outleta "outleft", adummy
outleta "outright", adummy
endif
endin

gkgrainDensity init 150
gkgrainDuration init 0.2
gkgrainAmplitudeRange init 100
gkgrainFrequencyRange init .033
gkBlowerLevel init 4
gkBlowerPan init .9875
gkBlowerEnabled init 1
instr Blower
 //////////////////////////////////////////////
 // Original by Hans Mikelson.
 // Adapted by Michael Gogins.
 //////////////////////////////////////////////
 if gkBlowerEnabled == 1 then
i_instrument = p1
i_time = p2
i_duration = p3
i_midikey = p4
i_midivelocity = p5
i_phase = p6
i_pan = p6
i_depth = p8
i_height = p9
i_pitchclassset = p10
i_homogeneity = p11
ifrequency = cpsmidinn(i_midikey)
iamplitude = ampdb(i_midivelocity) / 200
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; f1 0 65537 1 "hahaha.aif" 0 4 0
 ; f2 0 1024 7 0 224 1 800 0
 ; f3 0 8192 7 1 8192 -1
 ; f4 0 1024 7 0 512 1 512 0
 ; f5 0 1024 10 1 .3 .1 0 .2 .02 0 .1 .04
 ; f6 0 1024 10 1 0 .5 0 .33 0 .25 0 .2 0 .167
 ; a0 14 50
 ; p1 p2 p3 p4 p5 p6 p7 p8 p9 p10
 ; Start Dur Amp Freq GrTab WinTab FqcRng Dens Fade
 ; i1 0.0 6.5 700 9.00 5 4 .210 200 1.8
 ; i1 3.2 3.5 800 7.08 . 4 .042 100 0.8
 ; i1 5.1 5.2 600 7.10 . 4 .0320 100 0.9
 ; i1 7.2 6.6 900 8.03 . 4 .021 150 1.6
 ; i1 21.3 4.5 1000 9.00 . 4 .031 150 1.2
 ; i1 26.5 13.5 1100 6.09 . 4 .121 150 1.5
 ; i1 30.7 9.3 900 8.05 . 4 .014 150 2.5
 ; i1 34.2 8.8 700 10.02 . 4 .14 150 1.6
igrtab ftgenonce 0, 0, 65537, 10, 1, .3, .1, 0, .2, .02, 0, .1, .04
iwintab ftgenonce 0, 0, 65537, 10, 1, 0, .5, 0, .33, 0, .25, 0, .2, 0, .167
iHz = ifrequency
ihertz = iHz
ip4 = iamplitude
ip5 = iHz
ip6 = igrtab
ip7 = iwintab
ip8 = 0.033
ip8 = .002
ip9 = 150
ip9 = 100
ip10 = 1.6
ip10 = 3
idur = p3
iamp = iamplitude ; p4
ifqc = iHz ; cpspch(p5)
igrtab = ip6
iwintab = ip7
ifrng = ip8
idens = ip9
ifade = ip10
igdur = 0.2
ioverlap = p3 / 4
iattack = ioverlap
idecay = ioverlap
isustain = p3 - ioverlap
p3 = iattack + isustain + idecay
kenvelope transeg 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
; kamp linseg 0, ifade, 1, idur - 2 * ifade, 1, ifade, 0
kamp = kenvelope * gkBlowerLevel
; Amp Fqc Dense AmpOff kBlowerLevel GrDur GrTable WinTable MaxGrDur
aoutl grain ip4, ifqc, gkgrainDensity, gkgrainAmplitudeRange, ifqc * gkgrainFrequencyRange, gkgrainDuration, igrtab, iwintab, 5
aoutr grain ip4, ifqc, gkgrainDensity, gkgrainAmplitudeRange, ifqc * gkgrainFrequencyRange, gkgrainDuration, igrtab, iwintab, 5
aleft = aoutl * kamp * iamplitude
aright = aoutr * kamp * iamplitude
adamping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
aleft = adamping * aleft
aright = adamping * aright
asignal = aleft + aright
aleft, aright pan2 asignal, i_pan
outleta "outleft", aleft
outleta "outright", aright
prints "Blower  %2d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
endif
if gkBlowerEnabled != 1 then
adummy init 0
outleta "outleft", adummy
outleta "outright", adummy
endif
endin

gkBuzzerHarmonics init 2
gkBuzzerLevel init 1
gkBuzzerPan init .9875
gkBuzzerEnabled init 1
instr Buzzer
if gkBuzzerEnabled == 1 then
insno = p1
istart = p2
iduration = p3
ikey = p4
ivelocity = p5
iphase = p6
ipan = p7
iamp = ampdb(ivelocity)
ioverlap = p3 / 4
iattack = ioverlap
idecay = ioverlap
isustain = p3 - ioverlap
p3 = iattack + isustain + idecay
aenvelope transeg 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
ihertz = cpsmidinn(ikey)
;asignal gbuzz kenvelope, ihertz, 3, gkFirstHarmonic, gkDistortFactor, gisine
isine ftgenonce 0, 0, 65537, 10, 1
asignal buzz aenvelope, ihertz, gkBuzzerHarmonics, isine
asignal = asignal * gkBuzzerLevel
;asignal vco2 kenvelope, ihertz, 12
;asignal poscil3 kenvelope, ihertz, giharmonics
;asignal distort asignal, gkDistortFactor * .4, giwaveshaping
aleft, aright pan2 asignal, ipan
adamping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
aleft = adamping * aleft
aright = adamping * aright
outleta "outleft", aleft
outleta "outright", aright
prints "Buzzer  %2d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
endif
if gkBuzzerEnabled != 1 then
adummy init 0
outleta "outleft", adummy
outleta "outright", adummy
endif
endin

opcode ReverbSC, a, akkkk
asignal, kReverbFeedback, kReverbModulation, kReverbWet, kReverbModulationFrequency xin
; p4 = gain of reverb. Adjust empirically
; for desired reverb time. .6 gives
; a good small "live" room sound, .8
; a small hall, .9 a large hall,
; .99 an enormous stone cavern.

; p5 = amount of random pitch modulation
; for the delay lines. 1 is the "normal"
; amount, but this may be too high for
; held pitches such as piano tones.
; Adjust to taste.

; p6 = cutoff frequency of lowpass filters
; in feedback loops of delay lines,
; in Hz. Lower cutoff frequencies results
; in a sound with more high-frequency
; damping.

; 8 delay line FDN reverb, with feedback matrix based upon
; physical modeling scattering junction of 8 lossless waveguides
; of equal characteristic impedance. Based on Julius O. Smith III,
; "A New Approach to Digital Reverberation using Closed Waveguide
; Networks," Proceedings of the International Computer Music
; Conference 1985, p. 47-53 (also available as a seperate
; publication from CCRMA), as well as some more recent papers by
; Smith and others.
; Coded by Sean Costello, October 1999
itone = 16000
afilt1 init 0
afilt2 init 0
afilt3 init 0
afilt4 init 0
afilt5 init 0
afilt6 init 0
afilt7 init 0
afilt8 init 0
idel1 = (2473.000/sr)
idel2 = (2767.000/sr)
idel3 = (3217.000/sr)
idel4 = (3557.000/sr)
idel5 = (3907.000/sr)
idel6 = (4127.000/sr)
idel7 = (2143.000/sr)
idel8 = (1933.000/sr)
; k1-k8 are used to add random pitch modulation to the
; delay lines. Helps eliminate metallic overtones
; in the reverb sound.
; ares rspline xrangeMin, xrangeMax, kcpsMin, kcpsMax 
; ares randi xamp, xcps [, iseed] [, isize] [, ioffset] 
ak1 rspline -.001  * kReverbModulationFrequency, .001  * kReverbModulationFrequency, 3.1,   4.06
ak2 rspline -.0011 * kReverbModulationFrequency, .0011 * kReverbModulationFrequency, 3.5,   4.9
ak3 rspline -.0017 * kReverbModulationFrequency, .0017 * kReverbModulationFrequency, 1.11,  1.7
ak4 rspline -.0006 * kReverbModulationFrequency, .0006 * kReverbModulationFrequency, 3.973, 4.3
ak5 rspline -.001  * kReverbModulationFrequency, .001  * kReverbModulationFrequency, 2.341, 2.63
ak6 rspline -.0011 * kReverbModulationFrequency, .0011 * kReverbModulationFrequency, 1.897, 2.7
ak7 rspline -.0017 * kReverbModulationFrequency, .0017 * kReverbModulationFrequency, 0.891, 0.9
ak8 rspline -.0006 * kReverbModulationFrequency, .0006 * kReverbModulationFrequency, 3.221, 3.44
;ak1 randi .001, 3.1, .06
;ak2 randi .0011, 3.5, .9
;ak3 randi .0017, 1.11, .7
;ak4 randi .0006, 3.973, .3
;ak5 randi .001, 2.341, .63
;ak6 randi .0011, 1.897, .7
;ak7 randi .0017, 0.891, .9
;ak8 randi .0006, 3.221, .44
;ak1 lfo .001, .31, 1
;ak1 += .06
;ak2 lfo .0011, .35, 1
;ak2 += .9
;ak3 lfo .0017, .111, 1
;ak3 += .7
;ak4 lfo .0006, .3973, 1
;ak4 += .3
;ak5 lfo .001, .2341, 1
;ak5 += .63
;ak6 lfo .0011, .1897, 1
;ak6 += .7
;ak7 lfo .0017, .0891, 1
;ak7 += .0
;ak8 lfo .0006, .3221, 1
;ak8 += .44
; apj is used to calculate "resultant junction pressure" for
; the scattering junction of 8 lossless waveguides
; of equal characteristic impedance. If you wish to
; add more delay lines, simply add them to the following
; equation, and replace the .25 by 2/N, where N is the
; number of delay lines.
apj = .25 * (afilt1 + afilt2 + afilt3 + afilt4 + afilt5 + afilt6 + afilt7 + afilt8)
adum1 delayr 1
adel1 deltapi idel1 + ak1 * kReverbModulation
 delayw asignal + apj - afilt1
adum2 delayr 1
adel2 deltapi idel2 + ak2 * kReverbModulation
 delayw asignal + apj - afilt2
adum3 delayr 1
adel3 deltapi idel3 + ak3 * kReverbModulation
 delayw asignal + apj - afilt3
adum4 delayr 1
adel4 deltapi idel4 + ak4 * kReverbModulation
 delayw asignal + apj - afilt4
adum5 delayr 1
adel5 deltapi idel5 + ak5 * kReverbModulation
 delayw asignal + apj - afilt5
adum6 delayr 1
adel6 deltapi idel6 + ak6 * kReverbModulation
 delayw asignal + apj - afilt6
adum7 delayr 1
adel7 deltapi idel7 + ak7 * kReverbModulation
 delayw asignal + apj - afilt7
adum8 delayr 1
adel8 deltapi idel8 + ak8 * kReverbModulation
 delayw asignal + apj - afilt8
; 1st order lowpass filters in feedback
; loops of delay lines.
afilt1 tone adel1 * kReverbFeedback, itone
afilt2 tone adel2 * kReverbFeedback, itone
afilt3 tone adel3 * kReverbFeedback, itone
afilt4 tone adel4 * kReverbFeedback, itone
afilt5 tone adel5 * kReverbFeedback, itone
afilt6 tone adel6 * kReverbFeedback, itone
afilt7 tone adel7 * kReverbFeedback, itone
afilt8 tone adel8 * kReverbFeedback, itone
; The outputs of the delay lines are summed
; and sent to the stereo outputs. This could
; easily be modified for a 4 or 8-channel
; sound system.
aout1 = (afilt1 + afilt3 + afilt5 + afilt7)
aout2 = (afilt2 + afilt4 + afilt6 + afilt8)
aoutput = aout1 + aout2
aoutwet = aoutput * kReverbWet
aoutdry = aoutput * (1 - kReverbWet)
aoutput = aoutwet + aoutdry
xout aoutput
endop

gkReverbFeedback init 0.7
gkReverbModulation init 0.7
gkReverbWet init 1
gkReverbModulationFrequency init 0.5
instr ReverbLeft
asignal inleta "inleft"
aoutput ReverbSC asignal, gkReverbFeedback, gkReverbModulation, gkReverbWet, gkReverbModulationFrequency
outleta "outleft", aoutput
endin

instr ReverbRight
asignal inleta "inright"
aoutput ReverbSC asignal, gkReverbFeedback, gkReverbModulation, gkReverbWet, gkReverbModulationFrequency
outleta "outright", aoutput
endin

gkMasterLevelLeft init 4
gkMasterLevelRight init 4
instr MasterOutput
aleft inleta "inleft"
aleft *= gkMasterLevelLeft
aright inleta "inright"
aright *= gkMasterLevelRight
aleft dcblock aleft
aright dcblock aright
krmsleft downsamp aleft, ksmps
kdbleft dbfsamp krmsleft
outvalue "kdbleft", kdbleft
krmsright downsamp aright, ksmps
kdbright dbfsamp krmsright
outvalue "kdbright", kdbright
outs aleft, aright
idate date
Sfilename sprintf "Drone-VIII-e-1-%d.wav", idate
prints sprintf("Filename: %s\n", Sfilename)
//fout Sfilename, 16, aleft, aright
//dispfft aleft + aright, 0.1, 2048, 0, 1
endin

instr Controls
gkDroner1 invalue "gkDroner1"
gkDroner2 invalue "gkDroner2"
gkDroner3 invalue "gkDroner3"
gkDroner4 invalue "gkDroner4"
gkDroner5 invalue "gkDroner5"
gkDroner6 invalue "gkDroner6"
gkDroner7 invalue "gkDroner7"
gkDroner8 invalue "gkDroner8"
gkDroner9 invalue "gkDroner9"
gkDroner10 invalue "gkDroner10"
gkDronerLevel invalue "gkDronerLevel"
gkDronerEnabled invalue "gkDronerEnabled"
gkSweeperLevel invalue "gkSweeperLevel"
gkSweeperEnabled invalue "gkSweeperEnabled"
gkgrainDensity invalue "gkgrainDensity"
gkgrainDuration invalue "gkgrainDuration"
gkgrainAmplitudeRange invalue "gkgrainAmplitudeRange"
gkgrainFrequencyRange invalue "gkgrainFrequencyRange"
gkBlowerLevel invalue "gkBlowerLevel"
gkBlowerEnabled invalue "gkBlowerEnabled"
gkReverbFeedbackDela invalue "gkReverbFeedback"
gkReverbModulation invalue "gkReverbModulation"
gkReverbModulation invalue "gkReverbModulationFrequency"
gkReverbWet invalue "gkReverbWet"
gkMasterLevelLeft invalue "gkMasterLevelLeft"
gkMasterLevelRight invalue "gkMasterLevelRight"
gkBuzzerHarmonics invalue "gkBuzzerHarmonics"
gkBuzzerLevel invalue "gkBuzzerLevel"
gkBuzzerEnabled invalue "gkBuzzerEnabled"
endin

</CsInstruments>
<CsScore>
f 0 [6 * 60 + 33]
</CsScore>
</CsoundSynthesizer>






<bsbPanel>
 <label>Widgets</label>
 <objectName/>
 <x>0</x>
 <y>0</y>
 <width>1140</width>
 <height>831</height>
 <visible>true</visible>
 <uuid/>
 <bgcolor mode="nobackground">
  <r>234</r>
  <g>253</g>
  <b>255</b>
 </bgcolor>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>gkDroner3</objectName>
  <x>184</x>
  <y>85</y>
  <width>260</width>
  <height>25</height>
  <uuid>{5e269dfb-9532-41d3-83d8-d42cfde539f7}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.58076923</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>19</x>
  <y>85</y>
  <width>166</width>
  <height>25</height>
  <uuid>{e87c67e0-c4b2-407f-bb2f-242be34a52ec}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gkDroner3</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>16</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>gkDroner4</objectName>
  <x>184</x>
  <y>108</y>
  <width>261</width>
  <height>25</height>
  <uuid>{91144bb6-31e1-44e5-9d98-53b6c9c93532}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.09578544</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>19</x>
  <y>108</y>
  <width>167</width>
  <height>25</height>
  <uuid>{236a0eb0-fd30-47c2-8845-25315155f74b}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gkDroner4</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>16</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>gkDroner5</objectName>
  <x>184</x>
  <y>131</y>
  <width>261</width>
  <height>25</height>
  <uuid>{ad3f9dd8-7147-4f62-9770-e09aa418e837}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.17241379</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>19</x>
  <y>131</y>
  <width>167</width>
  <height>26</height>
  <uuid>{8c8248ab-8c18-484d-b822-91f9f4956fdb}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gkDroner5</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>16</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>gkDroner6</objectName>
  <x>184</x>
  <y>154</y>
  <width>261</width>
  <height>27</height>
  <uuid>{93eb0db1-8e07-421f-8121-357e84ff85e4}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.06896552</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>19</x>
  <y>155</y>
  <width>167</width>
  <height>26</height>
  <uuid>{2bdfdb11-1cc5-40fa-af6e-774ea1cab987}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gkDroner6</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>16</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>gkDroner7</objectName>
  <x>184</x>
  <y>179</y>
  <width>261</width>
  <height>26</height>
  <uuid>{f7ecf097-1755-4681-8f8f-d4b1acfdc09a}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.22988506</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>19</x>
  <y>179</y>
  <width>167</width>
  <height>26</height>
  <uuid>{13642f66-f23b-4727-86a3-74ba1ffb5cf1}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gkDroner7</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>16</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBController">
  <objectName>gkReverbFeedback</objectName>
  <x>550</x>
  <y>37</y>
  <width>503</width>
  <height>153</height>
  <uuid>{7bdad897-657c-4a54-9206-709cc8534ebb}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <objectName2>gkReverbModulation</objectName2>
  <xMin>0.00000000</xMin>
  <xMax>1.00000000</xMax>
  <yMin>0.00000000</yMin>
  <yMax>2.00000000</yMax>
  <xValue>0.88469185</xValue>
  <yValue>0.43137255</yValue>
  <type>crosshair</type>
  <pointsize>1</pointsize>
  <fadeSpeed>0.00000000</fadeSpeed>
  <mouseControl act="press">jump</mouseControl>
  <color>
   <r>0</r>
   <g>234</g>
   <b>0</b>
  </color>
  <randomizable group="0" mode="both">false</randomizable>
  <bgcolor>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
 </bsbObject>
 <bsbObject version="2" type="BSBDisplay">
  <objectName>gkDroner3</objectName>
  <x>442</x>
  <y>85</y>
  <width>81</width>
  <height>25</height>
  <uuid>{7ef693ee-baeb-4c01-bbea-44d8b884b57f}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>0.581</label>
  <alignment>right</alignment>
  <font>Arial</font>
  <fontsize>15</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBDisplay">
  <objectName>gkDroner7</objectName>
  <x>443</x>
  <y>179</y>
  <width>80</width>
  <height>26</height>
  <uuid>{1ebffaf7-0a51-4395-af00-21494aad303c}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>0.230</label>
  <alignment>right</alignment>
  <font>Arial</font>
  <fontsize>15</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBDisplay">
  <objectName>gkDroner6</objectName>
  <x>443</x>
  <y>154</y>
  <width>80</width>
  <height>27</height>
  <uuid>{4a8f2804-f935-48ce-a461-6e0d42a1a9e3}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>0.069</label>
  <alignment>right</alignment>
  <font>Arial</font>
  <fontsize>15</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBDisplay">
  <objectName>gkDroner5</objectName>
  <x>443</x>
  <y>131</y>
  <width>80</width>
  <height>25</height>
  <uuid>{d26c20e9-dfb7-4d91-bea8-456064879ffb}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>0.172</label>
  <alignment>right</alignment>
  <font>Arial</font>
  <fontsize>15</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBDisplay">
  <objectName>gkDroner4</objectName>
  <x>443</x>
  <y>108</y>
  <width>80</width>
  <height>25</height>
  <uuid>{c5bc6516-4212-4bfc-a3ab-9267190771ff}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>0.096</label>
  <alignment>right</alignment>
  <font>Arial</font>
  <fontsize>15</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBDisplay">
  <objectName>gkReverbModulation</objectName>
  <x>973</x>
  <y>4</y>
  <width>80</width>
  <height>25</height>
  <uuid>{875228fc-e82f-43c2-83bb-5cc963facc98}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>0.431</label>
  <alignment>right</alignment>
  <font>Arial</font>
  <fontsize>15</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBDisplay">
  <objectName>gkReverbFeedback</objectName>
  <x>1060</x>
  <y>38</y>
  <width>80</width>
  <height>25</height>
  <uuid>{ae997c24-fc31-43b0-89aa-db0a8ffde4b7}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>0.885</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>15</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBScope">
  <objectName/>
  <x>548</x>
  <y>443</y>
  <width>507</width>
  <height>144</height>
  <uuid>{f79d2676-2f95-4b20-94c4-f63775334ac9}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <value>-255.00000000</value>
  <type>scope</type>
  <zoomx>2.00000000</zoomx>
  <zoomy>1.00000000</zoomy>
  <dispx>1.00000000</dispx>
  <dispy>1.00000000</dispy>
  <mode>0.00000000</mode>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>551</x>
  <y>3</y>
  <width>422</width>
  <height>27</height>
  <uuid>{ea4be3fd-e964-4cf3-b364-3eb75c8a52f4}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>Costello Reverb</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>16</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>19</x>
  <y>2</y>
  <width>425</width>
  <height>38</height>
  <uuid>{5a6fe890-03db-4dec-8481-327980bd6cbe}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>Droner
</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>16</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>gkDronerLevel</objectName>
  <x>184</x>
  <y>272</y>
  <width>261</width>
  <height>26</height>
  <uuid>{8c3fdf6a-67e8-49b7-82f6-bfc69adf78c3}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>0.00000000</minimum>
  <maximum>10.00000000</maximum>
  <value>3.52490421</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>19</x>
  <y>272</y>
  <width>167</width>
  <height>26</height>
  <uuid>{e20f7609-b6ed-4208-8eaa-1f2da81a4ce5}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gkDronerLevel</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>16</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBDisplay">
  <objectName>gkDronerLevel</objectName>
  <x>443</x>
  <y>272</y>
  <width>80</width>
  <height>26</height>
  <uuid>{830bdf8a-8ddf-42f1-a0b4-3429333017ef}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>3.525</label>
  <alignment>right</alignment>
  <font>Arial</font>
  <fontsize>15</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>gkSweeperLevel</objectName>
  <x>183</x>
  <y>357</y>
  <width>261</width>
  <height>30</height>
  <uuid>{4e1d87ce-deb5-4208-84cc-ae2ae51b81d1}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>0.00000000</minimum>
  <maximum>10.00000000</maximum>
  <value>0.30651341</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>19</x>
  <y>357</y>
  <width>166</width>
  <height>30</height>
  <uuid>{4525842f-b74c-4379-a417-414a0d9898ed}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gkSweeperLevel</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>16</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBDisplay">
  <objectName>gkSweeperLevel</objectName>
  <x>442</x>
  <y>357</y>
  <width>80</width>
  <height>30</height>
  <uuid>{36bff934-5628-4661-a6d4-b875978cb9e0}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>0.307</label>
  <alignment>right</alignment>
  <font>Arial</font>
  <fontsize>15</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>19</x>
  <y>316</y>
  <width>426</width>
  <height>43</height>
  <uuid>{18531c0a-9dfa-468f-b13f-d2e09e76d10c}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>Sweeper
</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>16</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>gkgrainDensity</objectName>
  <x>184</x>
  <y>442</y>
  <width>263</width>
  <height>27</height>
  <uuid>{1023f505-dfd7-4138-b43b-85de8a0d0eac}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>0.00000000</minimum>
  <maximum>400.00000000</maximum>
  <value>311.78707224</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>19</x>
  <y>442</y>
  <width>167</width>
  <height>27</height>
  <uuid>{950542aa-2f7c-470c-a7d3-e17a00798f21}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gkgrainDensity</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>16</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>gkgrainDuration</objectName>
  <x>184</x>
  <y>467</y>
  <width>263</width>
  <height>27</height>
  <uuid>{342d7f7b-0299-481a-b729-e8a27e389537}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>0.00000000</minimum>
  <maximum>0.25000000</maximum>
  <value>0.04752852</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>19</x>
  <y>467</y>
  <width>167</width>
  <height>27</height>
  <uuid>{c4bbb464-4c46-4faf-b271-c0fc44f33232}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gkgrainDuration</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>16</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>gkgrainAmplitudeRange</objectName>
  <x>184</x>
  <y>492</y>
  <width>263</width>
  <height>27</height>
  <uuid>{4d2eadbd-2b72-41a6-9308-42e4deec7e23}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>0.00000000</minimum>
  <maximum>200.00000000</maximum>
  <value>20.53231939</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>19</x>
  <y>492</y>
  <width>167</width>
  <height>27</height>
  <uuid>{e2dd43b7-fd95-43dc-b22d-d4a7bbf55e8c}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gkgrainAmplitudeRange</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>16</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>gkgrainFrequencyRange</objectName>
  <x>184</x>
  <y>517</y>
  <width>263</width>
  <height>26</height>
  <uuid>{3f31ad33-5ce5-4fb6-97df-cb81cbc01b9b}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>0.00000000</minimum>
  <maximum>0.10000000</maximum>
  <value>0.03726236</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>19</x>
  <y>517</y>
  <width>167</width>
  <height>26</height>
  <uuid>{44a6e617-e671-4653-a35c-cf0b887917ae}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gkgrainFrequencyRange</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>16</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>gkBlowerLevel</objectName>
  <x>184</x>
  <y>541</y>
  <width>263</width>
  <height>26</height>
  <uuid>{3abbf610-33bb-474d-b878-299fe22e1193}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>0.00000000</minimum>
  <maximum>10.00000000</maximum>
  <value>0.38022814</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>19</x>
  <y>541</y>
  <width>167</width>
  <height>26</height>
  <uuid>{68702dc9-2a1b-46eb-8692-a4f73c94b5f7}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gkBlowerLevel</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>16</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBDisplay">
  <objectName>gkgrainDensity</objectName>
  <x>445</x>
  <y>442</y>
  <width>78</width>
  <height>27</height>
  <uuid>{6d6ff9f5-f094-45a8-8ac7-c5731ef98439}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>311.787</label>
  <alignment>right</alignment>
  <font>Arial</font>
  <fontsize>15</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBDisplay">
  <objectName>gkBlowerLevel</objectName>
  <x>445</x>
  <y>541</y>
  <width>78</width>
  <height>26</height>
  <uuid>{410c2dab-dc3d-43cd-a281-042e786f8f0a}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>0.380</label>
  <alignment>right</alignment>
  <font>Arial</font>
  <fontsize>15</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBDisplay">
  <objectName>gkgrainFrequencyRange</objectName>
  <x>445</x>
  <y>517</y>
  <width>78</width>
  <height>26</height>
  <uuid>{fe0c528f-1d23-49ba-ba79-1e4f5136bc59}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>0.037</label>
  <alignment>right</alignment>
  <font>Arial</font>
  <fontsize>15</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBDisplay">
  <objectName>gkgrainAmplitudeRange</objectName>
  <x>445</x>
  <y>492</y>
  <width>78</width>
  <height>27</height>
  <uuid>{1ced5131-f967-44b5-a643-c1e5ca010d92}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>20.532</label>
  <alignment>right</alignment>
  <font>Arial</font>
  <fontsize>15</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBDisplay">
  <objectName>gkgrainDuration</objectName>
  <x>445</x>
  <y>467</y>
  <width>78</width>
  <height>27</height>
  <uuid>{ccc72bc6-d467-4d49-98db-1ceb481947cf}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>0.048</label>
  <alignment>right</alignment>
  <font>Arial</font>
  <fontsize>15</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>19</x>
  <y>404</y>
  <width>428</width>
  <height>40</height>
  <uuid>{8ec04663-4b4d-4cc7-967a-4a122d32057b}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>Blower
</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>16</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBConsole">
  <objectName/>
  <x>548</x>
  <y>585</y>
  <width>525</width>
  <height>246</height>
  <uuid>{2a2e7ba5-15e4-4011-ab2e-15f65210adff}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <font>Courier</font>
  <fontsize>8</fontsize>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>550</x>
  <y>304</y>
  <width>167</width>
  <height>26</height>
  <uuid>{675a2ee5-e365-48b2-88b0-98ff1a5e8add}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gkMasterLevelLeft</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>16</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBDisplay">
  <objectName>gkMasterLevelLeft</objectName>
  <x>976</x>
  <y>304</y>
  <width>78</width>
  <height>26</height>
  <uuid>{a20b0e42-aebd-457e-9278-22895083db4b}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>7.985</label>
  <alignment>right</alignment>
  <font>Arial</font>
  <fontsize>15</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>550</x>
  <y>271</y>
  <width>428</width>
  <height>35</height>
  <uuid>{d55d4717-226c-4b13-ab04-fb1fa924be4a}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>Master
</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>16</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>gkMasterLevelLeft</objectName>
  <x>715</x>
  <y>304</y>
  <width>263</width>
  <height>26</height>
  <uuid>{691248c1-a95a-458d-89a2-8331df804b47}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>0.00000000</minimum>
  <maximum>100.00000000</maximum>
  <value>7.98479087</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>550</x>
  <y>203</y>
  <width>167</width>
  <height>26</height>
  <uuid>{453d3fb5-149a-4a9b-80a9-4167fbb67953}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gkReverbModulationFrequency</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>16</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBDisplay">
  <objectName>gkReverbModulationFrequency</objectName>
  <x>972</x>
  <y>203</y>
  <width>78</width>
  <height>26</height>
  <uuid>{51de1d91-2c36-478b-b1b8-231b3e8518a0}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>1.139</label>
  <alignment>right</alignment>
  <font>Arial</font>
  <fontsize>15</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>gkReverbModulationFrequency</objectName>
  <x>715</x>
  <y>203</y>
  <width>259</width>
  <height>26</height>
  <uuid>{d51377f8-9137-47d2-8303-59a08970e62e}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>0.00000000</minimum>
  <maximum>5.00000000</maximum>
  <value>1.13899614</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBCheckBox">
  <objectName>gkDronerEnabled</objectName>
  <x>503</x>
  <y>2</y>
  <width>17</width>
  <height>19</height>
  <uuid>{f0aa58a2-2c40-4269-ab9b-f51272137c85}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <selected>true</selected>
  <label/>
  <pressedValue>1</pressedValue>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBCheckBox">
  <objectName>gkSweeperEnabled</objectName>
  <x>504</x>
  <y>314</y>
  <width>18</width>
  <height>20</height>
  <uuid>{f6ea7af8-44ca-45fc-ba5c-602ccd9e73ee}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <selected>true</selected>
  <label/>
  <pressedValue>1</pressedValue>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBCheckBox">
  <objectName>gkBlowerEnabled</objectName>
  <x>505</x>
  <y>403</y>
  <width>17</width>
  <height>19</height>
  <uuid>{c72606dc-0d61-4abc-b60d-ad925d804fa9}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <selected>true</selected>
  <label/>
  <pressedValue>1</pressedValue>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>gkBuzzerHarmonics</objectName>
  <x>184</x>
  <y>623</y>
  <width>263</width>
  <height>27</height>
  <uuid>{8fb0c469-2589-42e5-867a-5e6790c976df}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>0.00000000</minimum>
  <maximum>20.00000000</maximum>
  <value>5.55133080</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>19</x>
  <y>623</y>
  <width>167</width>
  <height>27</height>
  <uuid>{e8436594-94ec-4ca2-83d3-3a57eec145f9}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gkBuzzerHarmonics</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>16</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>gkBuzzerLevel</objectName>
  <x>184</x>
  <y>648</y>
  <width>263</width>
  <height>27</height>
  <uuid>{b0e3903c-6f62-4c0b-882b-077a30c8a644}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>0.00000000</minimum>
  <maximum>10.00000000</maximum>
  <value>0.11406844</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>19</x>
  <y>648</y>
  <width>167</width>
  <height>27</height>
  <uuid>{49fb544d-c694-4d21-931b-d72a697c0e8e}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gkBuzzerLevel</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>16</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBDisplay">
  <objectName>gkBuzzerHarmonics</objectName>
  <x>445</x>
  <y>623</y>
  <width>78</width>
  <height>27</height>
  <uuid>{9381b604-d2ff-4c02-8df1-4557a017d7d1}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>5.551</label>
  <alignment>right</alignment>
  <font>Arial</font>
  <fontsize>15</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBDisplay">
  <objectName>gkBuzzerLevel</objectName>
  <x>445</x>
  <y>648</y>
  <width>78</width>
  <height>27</height>
  <uuid>{5c4ca4df-24a3-44d4-8f7b-58844cb1ce8c}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>0.114</label>
  <alignment>right</alignment>
  <font>Arial</font>
  <fontsize>15</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>19</x>
  <y>585</y>
  <width>428</width>
  <height>40</height>
  <uuid>{8beeb7e3-6270-45e6-b707-cd6c89a27e23}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>Buzzer
</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>16</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBCheckBox">
  <objectName>gkBuzzerEnabled</objectName>
  <x>505</x>
  <y>584</y>
  <width>17</width>
  <height>19</height>
  <uuid>{5e8c123c-0d3c-499e-b911-a57dfe9213b2}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <selected>true</selected>
  <label/>
  <pressedValue>1</pressedValue>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>550</x>
  <y>364</y>
  <width>167</width>
  <height>26</height>
  <uuid>{17f8cf09-a712-4d92-a0f6-cb26fda5c61b}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gkMasterLevelRight</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>16</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBDisplay">
  <objectName>gkMasterLevelRight</objectName>
  <x>976</x>
  <y>364</y>
  <width>78</width>
  <height>26</height>
  <uuid>{2c7e2c86-3072-4634-bfd5-311b36628ac2}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>7.985</label>
  <alignment>right</alignment>
  <font>Arial</font>
  <fontsize>15</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>gkMasterLevelRight</objectName>
  <x>715</x>
  <y>364</y>
  <width>263</width>
  <height>26</height>
  <uuid>{e326f81e-a029-4dc4-a9d1-110c5274002d}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>0.00000000</minimum>
  <maximum>100.00000000</maximum>
  <value>7.98479087</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>550</x>
  <y>227</y>
  <width>167</width>
  <height>26</height>
  <uuid>{abeef7e9-322d-490a-92a8-158b69e2a824}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gkReverbWet</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>16</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBDisplay">
  <objectName>gkReverbWet</objectName>
  <x>972</x>
  <y>227</y>
  <width>78</width>
  <height>26</height>
  <uuid>{9acf1a44-0a4a-4f92-a013-6116f6bde06b}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>0.826</label>
  <alignment>right</alignment>
  <font>Arial</font>
  <fontsize>15</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>gkReverbWet</objectName>
  <x>715</x>
  <y>227</y>
  <width>259</width>
  <height>26</height>
  <uuid>{a7a2f68f-f206-4a3d-a73d-2eefa61e7bac}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.82625483</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>gkDroner8</objectName>
  <x>184</x>
  <y>203</y>
  <width>260</width>
  <height>25</height>
  <uuid>{1ee428fe-e04e-4847-8c46-57aa35213e5e}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.09615385</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>19</x>
  <y>203</y>
  <width>166</width>
  <height>25</height>
  <uuid>{18829d66-5336-4df3-a277-150e8c452df9}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gkDroner8</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>16</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>gkDroner1</objectName>
  <x>184</x>
  <y>38</y>
  <width>261</width>
  <height>25</height>
  <uuid>{c6206b70-bf6d-4a1d-9983-94c1df2ee3a5}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.09195402</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>19</x>
  <y>38</y>
  <width>167</width>
  <height>25</height>
  <uuid>{ab7aa679-06a4-47ed-a2c5-c23c98bd6ae6}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gkDroner1</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>16</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>gkDroner2</objectName>
  <x>184</x>
  <y>61</y>
  <width>261</width>
  <height>25</height>
  <uuid>{bb4df8a8-e0eb-4bf1-9f87-17a4a703d59d}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.24904215</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>19</x>
  <y>61</y>
  <width>167</width>
  <height>26</height>
  <uuid>{00e7c30b-229f-4501-830e-78cd6c87f9b0}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gkDroner2</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>16</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBDisplay">
  <objectName>gkDroner8</objectName>
  <x>442</x>
  <y>203</y>
  <width>81</width>
  <height>25</height>
  <uuid>{264a53d4-1b1e-4cd0-b722-29cb248d275b}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>0.096</label>
  <alignment>right</alignment>
  <font>Arial</font>
  <fontsize>15</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBDisplay">
  <objectName>gkDroner2</objectName>
  <x>443</x>
  <y>61</y>
  <width>80</width>
  <height>25</height>
  <uuid>{14a8898b-f382-408e-b0c2-def94f8c2762}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>0.249</label>
  <alignment>right</alignment>
  <font>Arial</font>
  <fontsize>15</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBDisplay">
  <objectName>gkDroner1</objectName>
  <x>443</x>
  <y>38</y>
  <width>80</width>
  <height>25</height>
  <uuid>{d676dae0-5f29-4002-9cba-2a4e8fe490db}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>0.092</label>
  <alignment>right</alignment>
  <font>Arial</font>
  <fontsize>15</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>gkDroner9</objectName>
  <x>184</x>
  <y>226</y>
  <width>260</width>
  <height>25</height>
  <uuid>{f674ed81-3f7a-481e-b1c4-d53bb2ddad6f}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>19</x>
  <y>226</y>
  <width>166</width>
  <height>25</height>
  <uuid>{a5b4c3fc-d3ad-4654-9b8f-c4108a6f6b4f}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gkDroner9</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>16</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBDisplay">
  <objectName>gkDroner9</objectName>
  <x>442</x>
  <y>226</y>
  <width>81</width>
  <height>25</height>
  <uuid>{10d728de-f4d2-4268-8fb3-c217e9112374}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>0.000</label>
  <alignment>right</alignment>
  <font>Arial</font>
  <fontsize>15</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBHSlider">
  <objectName>gkDroner10</objectName>
  <x>184</x>
  <y>249</y>
  <width>260</width>
  <height>25</height>
  <uuid>{7292e910-4c85-4ee0-b3b8-768d3fbdfa76}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>19</x>
  <y>249</y>
  <width>166</width>
  <height>25</height>
  <uuid>{eec5de45-4612-4e84-9e09-999f06e808a9}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gkDroner10</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>16</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBDisplay">
  <objectName>gkDroner10</objectName>
  <x>442</x>
  <y>249</y>
  <width>81</width>
  <height>25</height>
  <uuid>{9b8fbe94-c970-4a76-b976-52291e54b7d1}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>0.000</label>
  <alignment>right</alignment>
  <font>Arial</font>
  <fontsize>15</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBController">
  <objectName>kdbleft</objectName>
  <x>715</x>
  <y>334</y>
  <width>263</width>
  <height>26</height>
  <uuid>{ed70beeb-a6d3-4da6-9225-f4454ad255e9}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <objectName2>kdbleft0</objectName2>
  <xMin>-90.00000000</xMin>
  <xMax>0.00000000</xMax>
  <yMin>-90.00000000</yMin>
  <yMax>0.00000000</yMax>
  <xValue>-38.95329349</xValue>
  <yValue>-22.40886286</yValue>
  <type>fill</type>
  <pointsize>1</pointsize>
  <fadeSpeed>0.00000000</fadeSpeed>
  <mouseControl act="press">jump</mouseControl>
  <color>
   <r>0</r>
   <g>234</g>
   <b>0</b>
  </color>
  <randomizable group="0" mode="both">false</randomizable>
  <bgcolor>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
 </bsbObject>
 <bsbObject version="2" type="BSBController">
  <objectName>kdbright</objectName>
  <x>715</x>
  <y>394</y>
  <width>263</width>
  <height>26</height>
  <uuid>{a59f717d-026d-4c69-ae16-7381996cd9aa}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <objectName2>kdbright0</objectName2>
  <xMin>-90.00000000</xMin>
  <xMax>0.00000000</xMax>
  <yMin>-90.00000000</yMin>
  <yMax>0.00000000</yMax>
  <xValue>-55.38261679</xValue>
  <yValue>-22.40886286</yValue>
  <type>fill</type>
  <pointsize>1</pointsize>
  <fadeSpeed>0.00000000</fadeSpeed>
  <mouseControl act="press">jump</mouseControl>
  <color>
   <r>0</r>
   <g>234</g>
   <b>0</b>
  </color>
  <randomizable group="0" mode="both">false</randomizable>
  <bgcolor>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
 </bsbObject>
 <bsbObject version="2" type="BSBDisplay">
  <objectName>kdbleft</objectName>
  <x>976</x>
  <y>334</y>
  <width>78</width>
  <height>26</height>
  <uuid>{ae4ed258-4a80-4c82-834c-3411431a707e}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>-38.953</label>
  <alignment>right</alignment>
  <font>Arial</font>
  <fontsize>15</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>255</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>85</g>
   <b>127</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>0</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBDisplay">
  <objectName>kdbright</objectName>
  <x>976</x>
  <y>394</y>
  <width>78</width>
  <height>26</height>
  <uuid>{4beb881e-f2e0-4b3f-9bce-2e44bbdd3826}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>-55.383</label>
  <alignment>right</alignment>
  <font>Arial</font>
  <fontsize>15</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>255</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>85</g>
   <b>127</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
</bsbPanel>
<bsbPresets>
</bsbPresets>
