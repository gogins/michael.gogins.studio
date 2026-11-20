<CsoundSynthesizer>
<CsOptions>
-d
</CsOptions>
<CsInstruments>
sr = 48000
ksmps = 32
nchnls = 2
0dbfs = 1

gi_sine ftgen 0, 0, 65536, 10, 1

instr 1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
i_depth = p6
i_pan = p7
i_height = p8
i_phase = p9

k_amplitude = ampdbfs(-6) * (i_midi_velocity / 127)
a_decay expon 1, i_duration, 0.0001

; Just CM9 above the C of this octave: C 1/1, D 9/8, E 5/4, G 3/2, B 15/8.
; Any other pitch class stays in twelve-tone equal temperament.
i_pc = i_midi_key % 12
i_fundamental = cpsmidinn(i_midi_key - i_pc)
if i_pc == 0 then
  i_frequency = i_fundamental
elseif i_pc == 2 then
  i_frequency = i_fundamental * 9 / 8
elseif i_pc == 4 then
  i_frequency = i_fundamental * 5 / 4
elseif i_pc == 7 then
  i_frequency = i_fundamental * 3 / 2
elseif i_pc == 11 then
  i_frequency = i_fundamental * 15 / 8
else
  i_frequency = cpsmidinn(i_midi_key)
endif

; Sharp attack for clank/clang, at audio rate. A k-rate linseg steps once
; per ksmps (0.67 ms here), so a 1 ms rise was one jump. The clang and
; body used to start at full amplitude, which is a click. A few milliseconds
; from zero keeps the clang and removes the step. The last segment ends at 0.
i_attack_rest = i_duration - 0.052
if i_attack_rest < 0.01 then
  i_attack_rest = 0.01
endif
a_attack linseg 0, 0.003, 1, 0.004, 0.6, 0.015, 0.3, 0.03, 0.15, i_attack_rest, 0
a_clang_env = a_attack * a_decay
a_clang_attack linseg 0, 0.002, 1
a_body_attack linseg 0, 0.008, 1

; More prominent high metallic partials for clank
a_partial1 oscili k_amplitude * a_clang_env * 0.4, i_frequency * 1.0, gi_sine
a_partial2 oscili k_amplitude * a_clang_env * 0.35, i_frequency * 1.89, gi_sine, i_phase * 0.1
a_partial3 oscili k_amplitude * a_clang_env * 0.32, i_frequency * 2.67, gi_sine, i_phase * 0.2
a_partial4 oscili k_amplitude * a_clang_env * 0.28, i_frequency * 3.47, gi_sine, i_phase * 0.3
a_partial5 oscili k_amplitude * a_clang_env * 0.24, i_frequency * 4.21, gi_sine, i_phase * 0.4
a_partial6 oscili k_amplitude * a_clang_env * 0.20, i_frequency * 5.09, gi_sine, i_phase * 0.5
a_partial7 oscili k_amplitude * a_clang_env * 0.18, i_frequency * 5.93, gi_sine, i_phase * 0.6
a_partial8 oscili k_amplitude * a_clang_env * 0.15, i_frequency * 6.71, gi_sine, i_phase * 0.7

; Very fast decaying metallic clang partials, faded in from silence
a_decay_fast expon 1, 0.3, 0.0001
a_clang_gain = a_decay_fast * a_clang_attack
a_clang1 oscili k_amplitude * a_clang_gain * 0.4, i_frequency * 8.23, gi_sine, i_phase * 0.11
a_clang2 oscili k_amplitude * a_clang_gain * 0.35, i_frequency * 9.87, gi_sine, i_phase * 0.22
a_clang3 oscili k_amplitude * a_clang_gain * 0.3, i_frequency * 11.44, gi_sine, i_phase * 0.33
a_clang4 oscili k_amplitude * a_clang_gain * 0.25, i_frequency * 13.17, gi_sine, i_phase * 0.44
a_clang5 oscili k_amplitude * a_clang_gain * 0.22, i_frequency * 15.63, gi_sine, i_phase * 0.55

; Slower decaying bell body, also faded in from silence
a_decay_slow expon 1, i_duration, 0.001
a_body_gain = a_decay_slow * a_body_attack
a_partial9 oscili k_amplitude * a_body_gain * 0.08, i_frequency * 7.58, gi_sine, i_phase * 0.8
a_partial10 oscili k_amplitude * a_body_gain * 0.06, i_frequency * 8.34, gi_sine, i_phase * 0.9
a_partial11 oscili k_amplitude * a_body_gain * 0.05, i_frequency * 9.21, gi_sine, i_phase * 0.15
a_partial12 oscili k_amplitude * a_body_gain * 0.04, i_frequency * 10.07, gi_sine, i_phase * 0.25

; Slower phasing partials
k_lfo1 oscil 0.02, 0.07, gi_sine
k_lfo2 oscil 0.03, 0.11, gi_sine
a_partial13 oscili k_amplitude * a_body_gain * 0.03, i_frequency * 11.23 * (1 + k_lfo1), gi_sine
a_partial14 oscili k_amplitude * a_body_gain * 0.02, i_frequency * 12.51 * (1 + k_lfo2), gi_sine

; Sum partials
a_bell = a_partial1 + a_partial2 + a_partial3 + a_partial4 + a_partial5 + a_partial6 + a_partial7 + a_partial8 + a_partial9 + a_partial10 + a_partial11 + a_partial12 + a_partial13 + a_partial14
a_clang_sum = a_clang1 + a_clang2 + a_clang3 + a_clang4 + a_clang5

; More prominent ring modulation for metallic clang
a_ring1 = a_partial1 * a_partial3
a_ring2 = a_partial2 * a_partial5
a_ring3 = a_partial4 * a_partial7
a_ring4 = a_clang1 * a_clang3
a_bell = a_bell + a_clang_sum + (a_ring1 + a_ring2 + a_ring3 + a_ring4) * 0.15

; Apply reverb
a_reverb_left, a_reverb_right reverbsc a_bell, a_bell, 0.97, 12000
a_mix_left = a_bell * 0.3 + a_reverb_left * 0.7
a_mix_right = a_bell * 0.3 + a_reverb_right * 0.7

; Pan and output
a_left = a_mix_left * i_pan
a_right = a_mix_right * (1 - i_pan)

outs a_left * 0.5, a_right * 0.5
endin

</CsInstruments>
<CsScore>
; Just CM9, one stroke every 2 seconds, each an octave higher:
; C1 1/1, E2 5/4, G3 3/2, B4 15/8, D6 9/8. Each rings 120 seconds.
; The sequence starts on C1 so the highest clang partial stays under Nyquist.
; p10 is the pitch-class set {0, 2, 4, 7, 11}.
i 1 0 120 24 110 0.5 0.5 0.5 0.30 2197 0.5
i 1 2 120 40 110 0.5 0.5 0.5 0.42 2197 0.5
i 1 4 120 55 110 0.5 0.5 0.5 0.54 2197 0.5
i 1 6 120 71 110 0.5 0.5 0.5 0.66 2197 0.5
i 1 8 120 86 110 0.5 0.5 0.5 0.78 2197 0.5
; At 40 seconds, five strokes, two seconds apart, mirroring the ascent.
; Start on D6, then each stroke is a just fourth and an octave lower (* 3/8).
; Fractional keys are that chain. p10 adds A and F# to the CM9 set.
i 1 40 120 86 110 0.5 0.5 0.5 0.16 2773 0.5
i 1 42 120 69.058650 110 0.5 0.5 0.5 0.32 2773 0.5
i 1 44 120 52.078200 110 0.5 0.5 0.5 0.48 2773 0.5
i 1 46 120 35.097750 110 0.5 0.5 0.5 0.64 2773 0.5
i 1 48 120 18.117300 110 0.5 0.5 0.5 0.80 2773 0.5
; Thirty seconds after the lowest note, five strokes at twice the tempo.
; Start on that F#0, then each stroke is a just major third and an octave
; higher (* 5/2). p10 adds A# to the set.
i 1 78 120 18.117300 110 0.5 0.5 0.5 0.18 3797 0.5
i 1 79 120 33.980437 110 0.5 0.5 0.5 0.31 3797 0.5
i 1 80 120 49.843574 110 0.5 0.5 0.5 0.44 3797 0.5
i 1 81 120 65.706711 110 0.5 0.5 0.5 0.57 3797 0.5
i 1 82 120 81.569849 110 0.5 0.5 0.5 0.70 3797 0.5
e 203
</CsScore>
</CsoundSynthesizer>
