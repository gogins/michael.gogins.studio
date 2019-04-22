<CsoundSynthesizer>
<CsLicense>
Drone-VIII-e
Copyright (C) 2014 by Michael Gogins.
All rights reserved.
</CsLicense>
<CsOptions>
-RWdfo Drone-VIII-e.wav -m195 --midi-key=4 --midi-velocity=5 -+id_artist=Michael_Gogins -+id_copyright=Copr_2014_Michael_Gogins -+id_title=Drone-VIII-e 
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
lindenmayer:initialize(4, 3)
lindenmayer.duration = 300
lindenmayer.axiom = 'P=C7 V=543 I=0 T=0 d=1 v=10 p=0.5 a P=CM7 T+5 d+1 C C'
-- lindenmayer.rules['a'] = 'a K C v+2 K C v+126 a v-2 C v-2 P=CM7 d+.25 T-5 V-25 a L T+2 L d-.325 V-79 a P=C7  K '  
lindenmayer.rules['a'] = 'a K L v+2 K L V+126 a v-2 C V-2 P=CM7 d+.25 T-5 V-25 a L T+2 L d-.325 V-79 a P=C7  K '  
lindenmayer.iterations = 2
lindenmayer:generate()
for key, value in ipairs(score) do
	value[PAN+1] = math.random()
end
score:setScale(KEY, 24.0)
--score:setScale(CHANNEL, 0, 3.999)
score:setScale(CHANNEL, 0, 1.999)
score:setScale(VELOCITY, 40.0, 5.5)
score:setScale(PAN, 0.0, 1.0)
score:setDuration(60 * 10)
score:setScale(TIME, 1)
score:temper(12.0)
score:tieOverlaps()
score:print()
score:renderMidi()
scales = score:findScales()
print('minima:', scales[1])
print('ranges:', scales[2])
score:sendToCsound(false)
csoundApi.csoundMessage(csound, 'Finished generating score.\\n')
}}
#define USE_REVERBSC #1#

#include "silencio/patches/Droner.inc"
#include "silencio/patches/Sweeper.inc"
#include "silencio/patches/Blower.inc"
#include "silencio/patches/Buzzer.inc"
#ifdef USE_REVERBSC
#include "silencio/patches/ReverbSC.inc"
#else
#include "silencio/patches/Reverberator1.inc"
#include "silencio/patches/Reverberator2.inc"
#endif
#include "silencio/patches/MasterOutput.inc"

gi_Droner_waveform init 0
gk_Droner_partial1 init 1
gk_Droner_partial2 init 2
gk_Droner_partial3 init 0
gk_Droner_partial4 init .1
gk_Droner_partial5 init 0
gk_Droner_partial6 init .7
gk_Droner_partial7 init 0
gk_Droner_partial8 init .05
gk_Droner_partial9 init .1
gk_Droner_partial10 init 0
gk_Droner_level init -14
gk_Droner_pan init .5

gk_Blower_grainDensity init 300
gk_Blower_grainDuration init 0.05
gk_Blower_grainAmplitudeRange init 100
gk_Blower_grainFrequencyRange init .33
gk_Blower_level init -4
gk_Blower_midi_dynamic_range init 127

gk_Sweeper_midi_dynamic_range init 127
gk_Sweeper_attack init .125
gk_Sweeper_release init .25
gk_Sweeper_britel init 1
gk_Sweeper_briteh init 4
gk_Sweeper_britels init 1
gk_Sweeper_britehs init 0.6
gk_Sweeper_level init -1

gk_Buzzer_attack init .125
gk_Buzzer_release init .25
gk_Buzzer_harmonics init 3
gk_Buzzer_level init -3
gk_Buzzer_midi_dynamic_range init 127

gk_Reverb_feedback init 0.995
gi_Reverb_delay_modulation init 0.01
gk_Reverb_feedback init 0.997
gi_Reverb_delay_modulation init .02
gk_Reverb_frequency_cutoff init 11000
gk_Reverb_Wet init 1

gk_MasterOutput_level init 6

#ifdef USE_REVERBSC
connect "Droner",           "outleft",  "ReverbSC",     "inleft"
connect "Droner",           "outright", "ReverbSC",     "inright"
connect "Blower",           "outleft",  "ReverbSC",     "inleft"
connect "Blower",           "outright", "ReverbSC",     "inright"
connect "Sweeper",          "outleft",  "ReverbSC",     "inleft"
connect "Sweeper",          "outright", "ReverbSC",     "inright"
connect "Buzzer",           "outleft",  "ReverbSC",     "inleft"
connect "Buzzer",           "outright", "ReverbSC",     "inright"
connect "ReverbSC",         "outleft",  "MasterOutput", "inleft"
connect "ReverbSC",         "outright", "MasterOutput", "inright"
alwayson "ReverbSC"
#else
connect "Droner",           "outleft",  "Reverb1",    "in"
connect "Droner",           "outright", "Reverb2",    "in"
connect "Blower",           "outleft",  "Reverb1",    "in"
connect "Blower",           "outright", "Reverb2",    "in"
connect "Sweeper",          "outleft",  "Reverb1",    "in"
connect "Sweeper",          "outright", "Reverb2",    "in"
connect "Buzzer",           "outleft",  "Reverb1",    "in"
connect "Buzzer",           "outright", "Reverb2",    "in"
connect "Reverb1",    "out",      "MasterOutput",     "inleft"
connect "Reverb2",    "out",      "MasterOutput",     "inright"
alwayson "Reverb1",    i(gk_Reverb_feedback),    i(gi_Reverb_delay_modulation),   i(gk_Reverb_frequency_cutoff),  i(gk_Reverb_Wet)
alwayson "Reverb2",    i(gk_Reverb_feedback),    i(gi_Reverb_delay_modulation),   i(gk_Reverb_frequency_cutoff),  i(gk_Reverb_Wet)
#endif

alwayson "MasterOutput"
;alwayson "Controls"


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
f 0 430
</CsScore>
</CsoundSynthesizer>
