<CsoundSyntheizer>
<CsLicense>

</CsLicense>
<CsOptions>
-m195 ; --opcode-lib="../../build-macos/lib/Debug/libvst3_plugins.dylib" -z1
</CsOptions>
<CsInstruments>

sr = 48000
ksmps = 128
nchnls = 2
0dbfs = 20

gi_maximum_voices init 20

//////////////////////////////////////////////////////////////////////////////
gi_FaustBubble_compiled faustcompile {{

declare name "bubble";
declare description "Production of a water drop bubble sound.";
declare license "MIT";
declare copyright "(c) 2017: Yann Orlarey, GRAME";

import("stdfaust.lib");

//---------------------------`bubble`--------------------------
// bubble(f0, trig) : produces a water drop bubble sound
//
// #### Usage
//
// ```
// bubble(f0, trig) : _
// ```
//
// Where:
//
// * ` f0 `: base frequency of bubble sound
// * `trig`: trigs the bubble sound on the rising front
//
// #### Example
//
// ```
// button("drop") : bubble(600) : _
// ```
//
// #### Reference:
//
// <http://www.cs.ubc.ca/~kvdoel/publications/tap05.pdf>
//------------------------------------------------------------

bubble(f0,trig) = os.osc(f) * (exp(-damp*time) : si.smooth(0.99))
	with {
		damp = 0.043*f0 + 0.0014*f0^(3/2);
		f = f0*(1+sigma*time);
		sigma = eta * damp;
		eta = 0.075;
		time = 0 : (select2(trig>trig'):+(1)) ~ _ : ba.samp2sec;
	};

process = button("drop") : bubble(hslider("v:bubble/freq", 600, 150, 2000, 1)) <: dm.freeverb_demo;

}}, "--import-dir \"/home/mkg/faust/libraries\"", 0

print gi_FaustBubble_compiled

gk_FaustBubble_level chnexport "gk_FaustBubble_level", 3
gk_FaustBubble_midi_dynamic_range chnexport "gk_FaustBubble_midi_dynamic_range", 3

gk_FaustBubble_level init 0
gk_FaustBubble_midi_dynamic_range init 20

instr FaustBubble
i_attack = .005
i_sustain = p3
i_release = .1
xtratim i_attack + i_release
i_instrument = p1
i_time = p2
i_midi_key = p4
while i_midi_key < 49 do
  i_midi_key = i_midi_key + 12
od
while i_midi_key > 99 do
  i_midi_key = i_midi_key - 12
od
i_midi_dynamic_range = i(gk_FaustBubble_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 68
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_FaustBubble_level)
i_faust_dsp faustdsp gi_FaustBubble_compiled
k_frequency cpsmidinn i_midi_key
faustctl i_faust_dsp, "freq", k_frequency
faustctl i_faust_dsp, "drop", k(1)
a_left, a_right faustplay i_faust_dsp
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_left = a_left * i_amplitude * a_declicking * k_gain
a_right = a_right * i_amplitude * a_declicking * k_gain

#ifdef USE_SPATIALIZATION
a_signal = a_left + a_right
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
outleta "outleft", a_left
outleta "outright", a_right
#end
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin
connect "FaustBubble", "outleft",  "MasterOutput", "inleft"
connect "FaustBubble", "outright", "MasterOutput", "inright"
gk_FaustBubble_level init 0
maxalloc "FaustBubble", gi_maximum_voices
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
gk_MasterOutput_level chnexport "gk_MasterOutput_level", 3 ; 0
gS_MasterOutput_filename chnexport "gS_MasterOutput_filename", 3 ; ""

gk_MasterOutput_level init 0
gS_MasterOutput_filename init ""

instr MasterOutput
aleft inleta "inleft"
aright inleta "inright"
k_gain = ampdb(gk_MasterOutput_level)
printks2 "Master gain: %f\n", k_gain
iamp init 1
aleft butterlp aleft, 18000
aright butterlp aright, 18000
outs aleft * k_gain, aright * k_gain
; We want something that will play on my phone.
i_amplitude_adjustment = ampdbfs(-3) / 32767
i_filename_length strlen gS_MasterOutput_filename
if i_filename_length > 0 goto filename_exists
goto filename_endif
filename_exists:
prints sprintf("Output filename: %s\n", gS_MasterOutput_filename)
fout gS_MasterOutput_filename, 18, aleft * i_amplitude_adjustment, aright * i_amplitude_adjustment
filename_endif:
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin
gk_MasterOutput_level init 10
gS_MasterOutput_filename init ""
alwayson "MasterOutput"
//////////////////////////////////////////////////////////////////////////////

instr ScoreGenerator



endin

</CsInstruments>
<CsScore>
f 0 120
i "FaustBubble" 1 1 60 60
</CsScore>
</CsoundSynthesizer>
