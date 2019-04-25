<CsoundSynthesizer>
<CsLicense>
Drone-VIII-e
Copyright (C) 2014 by Michael Gogins.
All rights reserved.
</CsLicense>
<CsOptions>
-RWdfo Drone-VIII-e.wav -m195 -+msg_color=0 --midi-key=4 --midi-velocity=5 -+id_artist=Michael_Gogins -+id_copyright=Copr_2014_Michael_Gogins -+id_title=Drone-VIII-e 
</CsOptions>
<CsInstruments>

sr = 96000
ksmps = 100
nchnls = 2
0dbfs = 2
seed 39284

lua_exec {{
package.path = package.path .. ";/home/mkg/csound-extended/silencio/lua/?.lua;"
local ffi = require("ffi")
local math = require("math")
local string = require("string")
local silencio = require ("Silencio")
local LindenmayerPITV = require("LindenmayerPITV")
local ChordSpace = require ("ChordSpace")
local duration = 1
local csoundApi = ffi.load("libcsound64.so")
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
math.randomseed(39284)
lindenmayer:initialize(4, 5)
lindenmayer.duration = 300
lindenmayer.axiom = 'P=C7 V=890 I=0 T=0 d=1 v=10 p=0.5 a P=CM7 T+5 d+1 C C'
-- lindenmayer.rules['a'] = 'a K L v+2 K L V+126 a v-2 C V-2 P=CM7 d+.25 T-5 V-205 a L T+2 L d-.325 V-9 a P=C7  K '  
lindenmayer.rules['a'] = 'a K L v+2 K L V+126 a v-2 C V-2 P=CM7 d+.25 T-5 V-205 a L T+2 L d-.325 V-9 a P=C7  K '  
lindenmayer.iterations = 2
lindenmayer:generate()
for key, value in ipairs(score) do
    local channel = math.floor(value[CHANNEL])
    local pan = channel / 5
    local pan = pan + 1/5
	value[PAN+1] = pan
end
score:setScale(KEY, 24.0)
-- score:setScale(CHANNEL, 0, 3.999)
score:setScale(CHANNEL, 0, 3.999)
-- score:setScale(VELOCITY, 40.0, 5.5)
score:setScale(VELOCITY, 40.0, 2.75)
score:setScale(PAN, 0.0, 1.0)
score:sort()
score:setDuration(60 * 10)
score:setScale(TIME, 1)
score:temper(12.0)
score:tieOverlaps()
score:print()
score:renderMidi()
scales = score:findScales()
print('minima:  ', scales[1])
print('ranges:  ', scales[2])
print('duration:', score:getDuration())
score:sendToCsound(false)
csoundApi.csoundMessage(csound, 'Finished generating score.\\n')
}}

sr = 96000
ksmps = 100
nchnls = 2
0dbfs = 3

#include "silencio/patches/Droner.inc"
#include "silencio/patches/Blower2.inc"
#include "silencio/patches/Sweeper.inc"
#include "silencio/patches/Buzzer.inc"
#include "silencio/patches/ReverbSC.inc"
#include "silencio/patches/ParametricEQ.inc"

instr MasterEnvelope
iattack init p4
isustain init p5
irelease init p6
aleft inleta "inleft"
aright inleta "inright"
aenvelope linseg 0, iattack, 1, isustain, 1, irelease, 0
aleft = aleft * aenvelope
aright = aright * aenvelope
outleta "outleft", aleft
outleta "outright", aright
prints "MasterEnvelope i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p7, active(p1)
endin

#include "silencio/patches/MasterOutput.inc"

instr TurnOff
scoreline_i "e"
endin

gi_Droner_waveform init 1
gk_Droner_partial1 init 4
gk_Droner_partial2 init .02
gk_Droner_partial3 init .05
gk_Droner_partial4 init .003
gk_Droner_partial5 init 0
gk_Droner_partial6 init .1
gk_Droner_partial7 init 0
gk_Droner_partial8 init 0
gk_Droner_partial9 init 0
gk_Droner_partial10 init 0
gk_Droner_level init -2
gk_Droner_level init (0 + 8 - 13 -3)
gk_Droner_pan init .5

gk_Blower2_grainDensity init 400
gk_Blower2_grainDuration init 0.02
gk_Blower2_grainAmplitudeRange init .01
gk_Blower2_grainCentsRange init 4
gk_Blower2_level init (-36 + 12 -2)
gk_Blower2_midi_dynamic_range init 127

gk_Sweeper_midi_dynamic_range init 127
gk_Sweeper_attack init .125
gk_Sweeper_release init .25
gk_Sweeper_britel init 1
gk_Sweeper_briteh init 3
gk_Sweeper_britels init 0.2
gk_Sweeper_britehs init 0.6
gk_Sweeper_britel init -2
gk_Sweeper_briteh init 2
gk_Sweeper_britels init (8 / 24)
gk_Sweeper_britehs init (8 / 3)
gk_Sweeper_level init (-2 -6)

gk_Buzzer_attack init .125
gk_Buzzer_release init .25
gk_Buzzer_harmonics init 2
gk_Buzzer_level init (-13 -6)
gk_Buzzer_midi_dynamic_range init 127

gk_ParametricEQ_Frequency init 3500
gk_ParametricEQ_Gain init 0
gk_ParametricEQ_Q init .707
gi_ParametricEQ_Mode init 0

gk_Reverb_feedback init 0.9975

gi_Reverb_delay_modulation init .02
gk_Reverb_frequency_cutoff init 11000
gk_Reverb_Wet init .5

gk_MasterOutput_level init (8 +6 +6)

connect "Droner",               "outleft",  "ReverbSC",         "inleft"
connect "Droner",               "outright", "ReverbSC",         "inright"
connect "Blower2",              "outleft",  "ReverbSC",         "inleft"
connect "Blower2",              "outright", "ReverbSC",         "inright"
connect "Sweeper",              "outleft",  "ReverbSC",         "inleft"
connect "Sweeper",              "outright", "ReverbSC",         "inright"
connect "Buzzer",               "outleft",  "ReverbSC",         "inleft"
connect "Buzzer",               "outright", "ReverbSC",         "inright"
connect "ReverbSC",             "outleft",  "ParametricEQ",     "inleft"
connect "ReverbSC",             "outright", "ParametricEQ",     "inright"
connect "ParametricEQ",         "outleft",  "MasterEnvelope",   "inleft"
connect "ParametricEQ",         "outright", "MasterEnvelope",   "inright"
connect "MasterEnvelope",       "outleft",  "MasterOutput",     "inleft"
connect "MasterEnvelope",       "outright", "MasterOutput",     "inright"

schedule "TurnOff", 626, 1
alwayson "ReverbSC"
alwayson "ParametricEQ"
alwayson "MasterEnvelope", .01, 620, 5
alwayson "MasterOutput"

</CsInstruments>
<CsScore>
f 0 [10 * 6 + 45]
</CsScore>
</CsoundSynthesizer>
