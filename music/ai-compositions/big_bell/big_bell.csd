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

; Cubic sine. A partial at or above 0.45 * sr is not run: its frequency
; argument is 0, so it cannot fold back into the audio band.
opcode nyquist_sine, a, aii
  a_amp, i_cps, i_phase xin
  i_ok = (i_cps > 0 && i_cps < sr * 0.45 ? 1 : 0)
  a_sig poscil3 a_amp * i_ok, i_cps * i_ok, gi_sine, i_phase
  xout a_sig
endop

opcode nyquist_glide, a, aaii
  a_amp, a_cps, i_max, i_phase xin
  i_ok = (i_max > 0 && i_max < sr * 0.45 ? 1 : 0)
  a_sig poscil3 a_amp * i_ok, a_cps * i_ok, gi_sine, i_phase
  xout a_sig
endop

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

i_band = sr * 0.45

; More prominent high metallic partials for clank
a_partial1 nyquist_sine k_amplitude * a_clang_env * 0.4, i_frequency * 1.0, 0
a_partial2 nyquist_sine k_amplitude * a_clang_env * 0.35, i_frequency * 1.89, i_phase * 0.1
a_partial3 nyquist_sine k_amplitude * a_clang_env * 0.32, i_frequency * 2.67, i_phase * 0.2
a_partial4 nyquist_sine k_amplitude * a_clang_env * 0.28, i_frequency * 3.47, i_phase * 0.3
a_partial5 nyquist_sine k_amplitude * a_clang_env * 0.24, i_frequency * 4.21, i_phase * 0.4
a_partial6 nyquist_sine k_amplitude * a_clang_env * 0.20, i_frequency * 5.09, i_phase * 0.5
a_partial7 nyquist_sine k_amplitude * a_clang_env * 0.18, i_frequency * 5.93, i_phase * 0.6
a_partial8 nyquist_sine k_amplitude * a_clang_env * 0.15, i_frequency * 6.71, i_phase * 0.7

; Strike. The old clang fell by 10^4 in 0.30 s (exponent about 31/s).
; This falls by 10^5 in 0.05 s (exponent about 230/s): a harder bell attack.
; The extra partials are inharmonic and live only on this strike.
a_strike_fall expon 1, 0.05, 0.00001
a_strike = a_clang_attack * a_strike_fall
a_clang1 nyquist_sine k_amplitude * a_strike * 0.40, i_frequency * 8.23, i_phase * 0.11
a_clang2 nyquist_sine k_amplitude * a_strike * 0.35, i_frequency * 9.87, i_phase * 0.22
a_clang3 nyquist_sine k_amplitude * a_strike * 0.30, i_frequency * 11.44, i_phase * 0.33
a_clang4 nyquist_sine k_amplitude * a_strike * 0.25, i_frequency * 13.17, i_phase * 0.44
a_clang5 nyquist_sine k_amplitude * a_strike * 0.22, i_frequency * 15.63, i_phase * 0.55
a_strike1 nyquist_sine k_amplitude * a_strike * 0.16, i_frequency * 2.27, i_phase * 0.17
a_strike2 nyquist_sine k_amplitude * a_strike * 0.14, i_frequency * 3.11, i_phase * 0.27
a_strike3 nyquist_sine k_amplitude * a_strike * 0.12, i_frequency * 3.83, i_phase * 0.37
a_strike4 nyquist_sine k_amplitude * a_strike * 0.10, i_frequency * 4.57, i_phase * 0.47
a_strike5 nyquist_sine k_amplitude * a_strike * 0.09, i_frequency * 5.51, i_phase * 0.57
a_strike6 nyquist_sine k_amplitude * a_strike * 0.08, i_frequency * 6.23, i_phase * 0.67
a_strike7 nyquist_sine k_amplitude * a_strike * 0.07, i_frequency * 7.07, i_phase * 0.73
a_strike8 nyquist_sine k_amplitude * a_strike * 0.06, i_frequency * 7.91, i_phase * 0.81
a_strike9 nyquist_sine k_amplitude * a_strike * 0.05, i_frequency * 9.53, i_phase * 0.13
a_strike10 nyquist_sine k_amplitude * a_strike * 0.045, i_frequency * 11.09, i_phase * 0.23
a_strike11 nyquist_sine k_amplitude * a_strike * 0.04, i_frequency * 12.77, i_phase * 0.39
a_strike12 nyquist_sine k_amplitude * a_strike * 0.035, i_frequency * 14.41, i_phase * 0.51
a_strike13 nyquist_sine k_amplitude * a_strike * 0.03, i_frequency * 16.33, i_phase * 0.63
a_strike14 nyquist_sine k_amplitude * a_strike * 0.025, i_frequency * 18.29, i_phase * 0.77

; Slower decaying bell body, also faded in from silence
a_decay_slow expon 1, i_duration, 0.001
a_body_gain = a_decay_slow * a_body_attack
a_partial9 nyquist_sine k_amplitude * a_body_gain * 0.08, i_frequency * 7.58, i_phase * 0.8
a_partial10 nyquist_sine k_amplitude * a_body_gain * 0.06, i_frequency * 8.34, i_phase * 0.9
a_partial11 nyquist_sine k_amplitude * a_body_gain * 0.05, i_frequency * 9.21, i_phase * 0.15
a_partial12 nyquist_sine k_amplitude * a_body_gain * 0.04, i_frequency * 10.07, i_phase * 0.25

; Slower phasing partials, at audio rate so the drift does not step
a_lfo1 oscili 0.02, 0.07, gi_sine
a_lfo2 oscili 0.03, 0.11, gi_sine
a_partial13 nyquist_glide k_amplitude * a_body_gain * 0.03, i_frequency * 11.23 * (1 + a_lfo1), i_frequency * 11.23 * 1.02, 0
a_partial14 nyquist_glide k_amplitude * a_body_gain * 0.02, i_frequency * 12.51 * (1 + a_lfo2), i_frequency * 12.51 * 1.03, 0

; Sum partials
a_bell = a_partial1 + a_partial2 + a_partial3 + a_partial4 + a_partial5 + a_partial6 + a_partial7 + a_partial8 + a_partial9 + a_partial10 + a_partial11 + a_partial12 + a_partial13 + a_partial14
a_clang_sum = a_clang1 + a_clang2 + a_clang3 + a_clang4 + a_clang5
a_strike_sum = a_strike1 + a_strike2 + a_strike3 + a_strike4 + a_strike5 + a_strike6 + a_strike7 + a_strike8 + a_strike9 + a_strike10 + a_strike11 + a_strike12 + a_strike13 + a_strike14

; Ring modulation only when the sum tone itself is under Nyquist
a_ring1 = a_partial1 * a_partial3 * ((i_frequency * (1.0 + 2.67)) < i_band ? 1 : 0)
a_ring2 = a_partial2 * a_partial5 * ((i_frequency * (1.89 + 4.21)) < i_band ? 1 : 0)
a_ring3 = a_partial4 * a_partial7 * ((i_frequency * (3.47 + 5.93)) < i_band ? 1 : 0)
a_ring4 = a_clang1 * a_clang3 * ((i_frequency * (8.23 + 11.44)) < i_band ? 1 : 0)
a_bell = a_bell + a_clang_sum + a_strike_sum + (a_ring1 + a_ring2 + a_ring3 + a_ring4) * 0.15

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
; Two more steps of the same third-plus-octave. Partials at or above
; Nyquist are silent, so these notes do not fold over.
i 1 83 120 97.432986 110 0.5 0.5 0.5 0.83 3831 0.5
i 1 84 120 113.296123 110 0.5 0.5 0.5 0.96 3831 0.5
e 205
</CsScore>
</CsoundSynthesizer>
