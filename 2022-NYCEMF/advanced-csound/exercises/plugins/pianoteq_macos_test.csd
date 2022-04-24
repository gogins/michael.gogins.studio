<CsoundSynthesizer>
<CsOptions>
-z195 -d -odac0
</CsOptions>
<CsInstruments>
sr = 48000
ksmps = 128
nchnls = 2
0dbfs = 2

gi_Pianoteq vst3init "/Library/Audio/Plug-Ins/VST3/Pianoteq 7.vst3", "Pianoteq 7", 1
vst3info gi_Pianoteq

</CsInstruments>
<CsScore>
f 0 30
</CsScore>
</CsoundSynthesizer>
