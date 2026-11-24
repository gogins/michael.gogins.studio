<CsoundSynthesizer>
<CsOptions>
-d
</CsOptions>
<CsInstruments>

sr = 48000
ksmps = 16
nchnls = 2
0dbfs = 1

; nu -- a short study: bells in D Dorian over a drone.
;
; Score p-fields:
;   p1 instrument    p2 time (seconds)    p3 duration (seconds)
;   p4 MIDI key      p5 MIDI velocity
;   p6 depth, front to back (0 near .. 1 far)
;   p7 pan, left to right (0 left .. 1 right)
;   p8 height        p9 phase    p10 pitch-class set    p11 homogeneity

gaReverbLeft init 0
gaReverbRight init 0

; Bell: two-operator FM, struck.
instr 1
  iHz = cpsmidinn(p4)
  iAmp = ampdbfs(-19 + 18 * (p5 / 127))
  iDepth = limit(p6, 0, 1)
  iPan = limit(p7, 0, 1)
  aAttack linseg 0, 0.004, 1, p3, 1
  aDecay expseg 1, p3, 0.001
  kIndex expseg 3.2, p3 * 0.6, 0.15, p3 * 0.4, 0.1
  aModulator poscil kIndex * iHz * 3.5, iHz * 3.5
  aCarrier poscil aAttack * aDecay * iAmp, iHz + aModulator
  aLeft, aRight pan2 aCarrier, iPan
  iDirect = 1 - 0.6 * iDepth
  iSend = 0.2 + 0.6 * iDepth
  out aLeft * iDirect, aRight * iDirect
  gaReverbLeft += aLeft * iSend
  gaReverbRight += aRight * iSend
endin

; Drone: two detuned sawtooths and a sub-octave sine through a slow filter.
instr 2
  iHz = cpsmidinn(p4)
  iAmp = ampdbfs(-26 + 18 * (p5 / 127))
  iPan = limit(p7, 0, 1)
  aEnvelope linseg 0, p3 * 0.3, 1, p3 * 0.4, 1, p3 * 0.3, 0
  aSawLow vco2 1, iHz * 0.998
  aSawHigh vco2 1, iHz * 1.002
  aSub poscil 1, iHz * 0.5
  kCutoff linseg 300, p3 * 0.5, 900, p3 * 0.5, 300
  aFiltered moogladder (aSawLow + aSawHigh) * 0.3 + aSub * 0.5, kCutoff, 0.2
  aSignal = aFiltered * aEnvelope * iAmp
  aLeft, aRight pan2 aSignal, iPan
  out aLeft, aRight
  gaReverbLeft += aLeft * 0.3
  gaReverbRight += aRight * 0.3
endin

; Breathy wind pad: noise filtered and shaped with LFO modulation.
instr 3
  iHz = cpsmidinn(p4)
  iAmp = ampdbfs(-28 + 18 * (p5 / 127))
  iPan = limit(p7, 0, 1)
  aEnvelope linseg 0, p3 * 0.35, 1, p3 * 0.3, 1, p3 * 0.35, 0
  aNoise noise 1, 0
  kLFO1 lfo 0.3, 0.11
  kLFO2 lfo 0.25, 0.17
  kCutoff = iHz * (1.8 + kLFO1)
  kResonance = 0.15 + kLFO2 * 0.1
  aFiltered1 moogladder aNoise, kCutoff, kResonance
  aFiltered2 moogladder aNoise, iHz * 0.9, 0.25
  aMix = (aFiltered1 * 0.6 + aFiltered2 * 0.4) * aEnvelope * iAmp
  aLeft, aRight pan2 aMix, iPan
  out aLeft, aRight
  gaReverbLeft += aLeft * 0.45
  gaReverbRight += aRight * 0.45
endin

; Reverberation for everything sent to the reverb bus.
instr 99
  aLeft, aRight reverbsc gaReverbLeft, gaReverbRight, 0.86, 9000
  out aLeft * 0.6, aRight * 0.6
  clear gaReverbLeft, gaReverbRight
endin

</CsInstruments>
<CsScore>
i 99 0 122
i 2 0 15 38 72 0.5 0.5 0 0 4095 1 
i 3 0 15 62 68 0.5 0.6 0 0 4095 1
i 1 0.5 4 62 91 0.36 0.3 0 0 4095 1 
i 1 1.5 3 64 79 0.34 0.7 0 0 4095 1 
i 1 2.5 3 69 77 0.23 0.32 0 0 4095 1 
i 1 2.5 4 69 79 0.61 0.7 0 0 4095 1 
i 1 3.25 2.5 64 93 0.39 0.69 0 0 4095 1 
i 1 3.5 3 71 67 0.59 0.3 0 0 4095 1 
i 1 4.25 3 69 93 0.1 0.36 0 0 4095 1 
i 1 4.5 3 76 65 0.48 0.68 0 0 4095 1 
i 1 5.25 2.5 71 81 0.64 0.31 0 0 4095 1 
i 1 5.75 3 64 81 0.28 0.55 0 0 4095 1 
i 1 6.25 3 76 81 0.35 0.64 0 0 4095 1 
i 1 6.75 3 62 90 0.17 0.44 0 0 4095 1 
i 2 7 8 45 62 0.5 0.5 0 0 4095 1 
i 3 7 8 69 66 0.5 0.35 0 0 4095 1
i 1 7.75 2.5 64 88 0.3 0.46 0 0 4095 1 
i 1 7.75 3 71 69 0.53 0.45 0 0 4095 1 
i 1 8.75 2.5 67 82 0.19 0.64 0 0 4095 1 
i 1 8.75 3 69 78 0.42 0.56 0 0 4095 1 
i 1 9.5 2.5 71 92 0.33 0.32 0 0 4095 1 
i 1 9.75 2.5 71 76 0.55 0.54 0 0 4095 1 
i 1 10.75 2.5 74 70 0.44 0.36 0 0 4095 1 
i 1 11.5 2.5 77 80 0.58 0.68 0 0 4095 1 
i 2 15 13 50 67 0.5 0.5 0 0 4095 1 
i 3 15 13 74 64 0.5 0.65 0 0 4095 1
i 1 15.5 4 64 85 0.42 0.35 0 0 4095 1 
i 1 16.5 3 62 73 0.38 0.72 0 0 4095 1 
i 1 17.5 3 71 71 0.27 0.38 0 0 4095 1 
i 1 17.5 4 67 75 0.57 0.66 0 0 4095 1 
i 1 18.25 2.5 62 89 0.44 0.71 0 0 4095 1 
i 1 18.5 3 74 63 0.62 0.29 0 0 4095 1 
i 1 19.25 3 67 88 0.14 0.41 0 0 4095 1 
i 1 19.5 3 77 61 0.51 0.65 0 0 4095 1 
i 1 20.25 2.5 74 77 0.68 0.34 0 0 4095 1 
i 1 20.75 3 62 78 0.32 0.58 0 0 4095 1 
i 1 21.25 3 77 77 0.39 0.61 0 0 4095 1 
i 1 21.75 3 64 86 0.21 0.48 0 0 4095 1 
i 2 28 11 38 64 0.5 0.5 0 0 4095 1 
i 3 28 11 64 62 0.5 0.4 0 0 4095 1
i 1 28.75 2.5 62 84 0.34 0.49 0 0 4095 1 
i 1 28.75 3 74 66 0.56 0.47 0 0 4095 1 
i 1 29.75 2.5 69 79 0.23 0.67 0 0 4095 1 
i 1 29.75 3 67 74 0.46 0.59 0 0 4095 1 
i 1 30.5 2.5 74 88 0.37 0.35 0 0 4095 1 
i 1 30.75 2.5 74 72 0.59 0.57 0 0 4095 1 
i 1 31.75 2.5 76 68 0.48 0.39 0 0 4095 1 
i 1 32.5 2.5 79 76 0.62 0.71 0 0 4095 1 
i 2 39 14 45 69 0.5 0.5 0 0 4095 1 
i 3 39 14 71 67 0.5 0.58 0 0 4095 1
i 1 39.5 4 69 87 0.4 0.33 0 0 4095 1 
i 1 40.5 3 67 81 0.36 0.69 0 0 4095 1 
i 1 41.5 3 76 75 0.25 0.34 0 0 4095 1 
i 1 41.5 4 71 77 0.63 0.73 0 0 4095 1 
i 1 42.25 2.5 67 91 0.41 0.67 0 0 4095 1 
i 1 42.5 3 79 65 0.6 0.32 0 0 4095 1 
i 1 43.25 3 71 90 0.12 0.39 0 0 4095 1 
i 1 43.5 3 81 67 0.49 0.7 0 0 4095 1 
i 1 44.25 2.5 79 79 0.66 0.33 0 0 4095 1 
i 1 44.75 3 67 83 0.3 0.53 0 0 4095 1 
i 1 45.25 3 81 83 0.37 0.66 0 0 4095 1 
i 1 45.75 3 69 92 0.19 0.42 0 0 4095 1 
i 2 53 12 50 66 0.5 0.5 0 0 4095 1 
i 3 53 12 67 65 0.5 0.45 0 0 4095 1
i 1 53.75 2.5 67 90 0.32 0.44 0 0 4095 1 
i 1 53.75 3 79 68 0.54 0.43 0 0 4095 1 
i 1 54.75 2.5 71 84 0.21 0.62 0 0 4095 1 
i 1 54.75 3 74 80 0.4 0.54 0 0 4095 1 
i 1 55.5 2.5 79 94 0.35 0.3 0 0 4095 1 
i 1 55.75 2.5 79 78 0.57 0.52 0 0 4095 1 
i 1 56.75 2.5 81 74 0.46 0.34 0 0 4095 1 
i 1 57.5 2.5 83 82 0.6 0.66 0 0 4095 1 
i 2 65 15 38 70 0.5 0.5 0 0 4095 1 
i 3 65 15 76 69 0.5 0.52 0 0 4095 1
i 1 65.5 4 74 89 0.38 0.31 0 0 4095 1 
i 1 66.5 3 71 77 0.32 0.74 0 0 4095 1 
i 1 67.5 3 81 79 0.29 0.36 0 0 4095 1 
i 1 67.5 4 76 81 0.65 0.68 0 0 4095 1 
i 1 68.25 2.5 71 95 0.43 0.73 0 0 4095 1 
i 1 68.5 3 83 69 0.58 0.28 0 0 4095 1 
i 1 69.25 3 76 95 0.08 0.34 0 0 4095 1 
i 1 69.5 3 86 59 0.47 0.72 0 0 4095 1 
i 1 70.25 2.5 83 85 0.7 0.29 0 0 4095 1 
i 1 70.75 3 71 79 0.26 0.6 0 0 4095 1 
i 1 71.25 3 86 85 0.33 0.68 0 0 4095 1 
i 1 71.75 3 74 88 0.15 0.46 0 0 4095 1 
i 2 80 13 45 68 0.5 0.5 0 0 4095 1 
i 3 80 13 69 66 0.5 0.62 0 0 4095 1
i 1 80.75 2.5 71 86 0.28 0.5 0 0 4095 1 
i 1 80.75 3 83 70 0.52 0.49 0 0 4095 1 
i 1 81.75 2.5 76 80 0.17 0.69 0 0 4095 1 
i 1 81.75 3 74 76 0.44 0.61 0 0 4095 1 
i 1 82.5 2.5 83 90 0.31 0.34 0 0 4095 1 
i 1 82.75 2.5 83 74 0.53 0.56 0 0 4095 1 
i 1 83.75 2.5 86 66 0.42 0.38 0 0 4095 1 
i 1 84.5 2.5 88 78 0.56 0.7 0 0 4095 1 
i 2 93 14 50 65 0.5 0.5 0 0 4095 1 
i 3 93 14 74 63 0.5 0.38 0 0 4095 1
i 1 93.5 4 76 91 0.36 0.37 0 0 4095 1 
i 1 94.5 3 74 83 0.34 0.68 0 0 4095 1 
i 1 95.5 3 86 73 0.31 0.32 0 0 4095 1 
i 1 95.5 4 81 83 0.67 0.74 0 0 4095 1 
i 1 96.25 2.5 74 93 0.45 0.65 0 0 4095 1 
i 1 96.5 3 88 71 0.56 0.34 0 0 4095 1 
i 1 97.25 3 81 92 0.1 0.4 0 0 4095 1 
i 1 97.5 3 90 63 0.45 0.66 0 0 4095 1 
i 1 98.25 2.5 88 87 0.72 0.35 0 0 4095 1 
i 1 98.75 3 74 85 0.24 0.51 0 0 4095 1 
i 1 99.25 3 90 79 0.31 0.62 0 0 4095 1 
i 1 99.75 3 76 94 0.13 0.4 0 0 4095 1 
i 2 107 15 38 71 0.5 0.5 0 0 4095 1 
i 3 107 15 64 70 0.5 0.55 0 0 4095 1
i 1 107.75 2.5 74 92 0.26 0.52 0 0 4095 1 
i 1 107.75 3 88 72 0.5 0.51 0 0 4095 1 
i 1 108.75 2.5 79 86 0.15 0.7 0 0 4095 1 
i 1 108.75 3 81 82 0.38 0.58 0 0 4095 1 
i 1 109.5 2.5 88 96 0.29 0.36 0 0 4095 1 
i 1 109.75 2.5 88 80 0.51 0.6 0 0 4095 1 
i 1 110.75 2.5 90 76 0.4 0.4 0 0 4095 1 
i 1 111.5 2.5 93 84 0.54 0.74 0 0 4095 1 
e 122
</CsScore>
</CsoundSynthesizer>
