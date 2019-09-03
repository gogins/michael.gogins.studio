<CsoundSynthesizer>
<CsOptions>
-m195 --displays --midi-key=4 --midi-velocity=5 
</CsOptions>
<CsInstruments>
sr = 96000
ksmps = 128
nchnls = 2
0dbfs = 1

alwayson "aeolus_output"

gi_aeolus aeolus_init "stops-0.3.0", "Aeolus", "waves", 0, 15, 0

instr aeolus_note, 1, 2, 3, 4, 5
prints "Aeolus note: i: %9.4f time: %9.4f duration: %9.4f key: %9.4f velocity: %9.4f\n", p1, p2, p3, p4, p5
aeolus_note gi_aeolus, 1, p4, p5
endin

instr aeolus_preset
prints "Aeolus preset: i: %9.4f time: %9.4f duration: %9.4f bank:%9.4f preset:%9.4f\n", p1, p2, p3, p4, p5
aeolus_preset gi_aeolus, p4, p5
endin

instr aeolus_stop
prints "aeolus_stop: i: %9.4f time: %9.4f duration: %9.4f stop: %9.4f\n", p1, p2, p3, p4
i_stop init p4
aeolus_stop gi_aeolus, i_stop
endin

instr aeolus_output
aeolus_preset gi_aeolus, 0, 0
// All our stops are in the Pedal division, MIDI channel 1.
aeolus_group_mode gi_aeolus, 0, 3
a_out[] init 2
a_out aeolus_out gi_aeolus
// Use reverbsc instead of the built-in Aeolus reverb.
i_reverb_feedback init .80
i_reverb_highpass init 9000
i_wet init .33334
i_dry init 1 - i_wet
a_left, a_right reverbsc a_out[0], a_out[1], i_reverb_feedback, i_reverb_highpass
outs a_out[0] * i_dry + a_left * i_wet, a_out[1] * i_dry + a_right * i_wet
endin

</CsInstruments>
<CsScore>
f 0 3600
; Original tempo in the MIDI file -- seems too fast to me.
; t 0 119
; Use to correlate beats with seconds for finding sections in the score editor or soundfile sonogram view.
t 0 60
; Actual rendering tempo.
; t 0 103
; Changes of stops are put in by hand. Monkey with the Aeolus GUI to create presets and choices of stops,
; and be sure to actually save your edits. Keep in mind that Csound score times are BEATS not seconds,
; and are seconds ONLY at tempo 60. It is best to set the tempo to 60 in a score editor after importing
; the MIDI and select the first beat of each section to get the time to change the presets. Then you can
; go back and change the rendering tempo in Csound.
i "aeolus_stop" 10 1 3
i "aeolus_stop" 11 1 4
i "aeolus_stop" 12 1 5
i "aeolus_stop" 10 1 6

</CsScore>
</CsoundSynthesizer>
