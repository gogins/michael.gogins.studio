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

; Reverberation for everything sent to the reverb bus.
instr 99
  aLeft, aRight reverbsc gaReverbLeft, gaReverbRight, 0.86, 9000
  out aLeft * 0.6, aRight * 0.6
  clear gaReverbLeft, gaReverbRight
endin

</CsInstruments>
<CsScore>
; Reverb for the whole piece.
i 99 0 18

; Drone on D2.
;    time dur key vel depth pan  height phase pitches homogeneity
i 2  0    15  38  84  0.5   0.5  0      0     0       1

; Bells: D Dorian, slow, alternating sides.
i 1  0.5  3.5 62  92  0.2   0.30 0      0     0       1
i 1  2.0  3.5 69  84  0.3   0.70 0      0     0       1
i 1  3.5  3.0 65  80  0.4   0.35 0      0     0       1
i 1  4.5  3.5 67  86  0.3   0.65 0      0     0       1
i 1  6.0  3.0 64  78  0.5   0.25 0      0     0       1
i 1  7.0  3.5 72  90  0.2   0.75 0      0     0       1
i 1  8.5  3.0 71  82  0.4   0.40 0      0     0       1
i 1  9.5  3.5 69  84  0.3   0.60 0      0     0       1
i 1 11.0  4.0 74  94  0.2   0.50 0      0     0       1
i 1 12.0  4.5 62  80  0.6   0.30 0      0     0       1
i 1 12.0  4.5 57  76  0.6   0.70 0      0     0       1
e 18
</CsScore>
</CsoundSynthesizer>
