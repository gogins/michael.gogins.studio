<CsoundSynthesizer>
<CsLicense>
Copyright (C) 2013 by Michael Gogins.
All rights reserved.

</CsLicense>
<CsOptions>
--m-amps=1 --m-range=1 --m-dB=1 --m-benchmarks=1 --m-warnings=0 -+msg_color=0 -d -odac 
</CsOptions>
<CsInstruments>
sr = 48000
ksmps = 100
nchnls = 2
0dbfs = 1000000

#define USE_SPATIALIZATION #1#
#define SPATIALIZE_GOGINS #1#

opcode instrument_position, kk, iii
i_onset, i_radius, i_rate xin
i_rate = (i_rate / 15.)
k_time times
// Depth.
k_x = i_radius * cos(i_onset + ((k_time - i_onset) * i_rate))
// Pan.
k_y = i_radius * sin(i_onset + ((k_time - i_onset) * i_rate))
xout k_x, k_y
endop

#include "Spatialize1.inc"

gk_BformatDecoder_MasterLevel init 20
gk_BformatDecoder_SpeakerRig init 1
gk_BformatDecoder2_SpeakerRig init 31
gk_Spatialize_SpeakerRigRadius init 5.0
gk_SpatialReverb_ReverbDecay init 0.96
gk_SpatialReverb_CutoffHz init sr
gk_SpatialReverb_RandomDelayModulation init 4.0
gk_LocalReverbByDistance_Wet init 0.75
; This is a fraction of the speaker rig radius.
gk_LocalReverbByDistance_FrontWall init 0.9
gk_LocalReverbByDistance_ReverbDecay init 0.6
gk_LocalReverbByDistance_CutoffHz init 20000
gk_LocalReverbByDistance_RandomDelayModulation init 1.0
gk_Spatialize_Verbose init 0

gkslider1 init 0
gkslider2 init .2
gkslider3 init .1
gkslider4 init .4
gkslider5 init 0

connect "Droner", "outbformat", "BformatDecoder2", "inbformat"
connect "Droner", "out", "SpatialReverb", "in"
connect "Blower", "outbformat", "BformatDecoder2", "inbformat"
connect "Blower", "out", "SpatialReverb", "in"
connect "Bower", "outbformat", "BformatDecoder2", "inbformat"
connect "Bower", "out", "SpatialReverb", "in"
connect "Buzzer", "outbformat", "BformatDecoder2", "inbformat"
connect "Buzzer", "out", "SpatialReverb", "in"
connect "Sweeper", "outbformat", "BformatDecoder2", "inbformat"
connect "Sweeper", "out", "SpatialReverb", "in"
connect "SpatialReverb", "outbformat", "BformatDecoder2", "inbformat"

alwayson "SpatialReverb"
alwayson "BformatDecoder2"

gk_Droner_Attack init 10
gk_Droner_1 init 0.5
gk_Droner_2 init 0.05
gk_Droner_3 init 0.1
gk_Droner_4 init 0.2
gk_Droner_5 init 0.1
gk_Droner_6 init 0.05
gk_Droner_7 init 0.1
gk_Droner_8 init 0.0
gk_Droner_9 init 0.0
gk_Droner_10 init 0.05
gk_Droner_x init 0.01
gk_Droner_y init 0.01
gk_Droner_z init 0.01
instr Droner
insno = p1
istart = p2
iduration = p3
ikey = p4
ivelocity = p5
iphase = p6
ipan = p7
k1 = gk_Droner_1
k2 = gk_Droner_2
k3 = gk_Droner_3
k4 = gk_Droner_4
k5 = gk_Droner_5
k6 = gk_Droner_6
k7 = gk_Droner_7
k8 = gk_Droner_8
k9 = gk_Droner_9
k10 = gk_Droner_10
iamp = ampdb(ivelocity)
iattack = i(gk_Droner_Attack)
idecay = iattack
isustain = p3 - iattack
xtratim idecay
aenvelope transeg 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
ihertz = cpsmidinn(ikey)
isine ftgenonce 0, 0, 65536, 10, 1, 0, .02
asignal poscil3 1, ihertz, isine
asignal chebyshevpoly asignal, 0, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10
asignal = asignal * aenvelope * 10
absignal[] init 16
kx jspline 12, 1/5, 1/20
ky jspline 12, 1/5, 1/20
kz jspline 12, 1/5, 1/20
kx = kx - 6
ky = ky - 6
kz = kz - 6
absignal, asend Spatialize asignal, gk_Droner_x + kx, gk_Droner_y + ky, gk_Droner_z + kz
outleta "out", asend
outletv "outbformat", absignal
kelapsed timeinsts
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_Bower_Attack = 10
instr Bower
insno = p1
istart = p2
iduration = p3
ikey = p4
ivelocity = p5
iphase = p6
ipan = p7
iamp = ampdb(ivelocity) * 200
iattack = i(gk_Bower_Attack)
idecay = iattack
isustain = p3 - iattack
xtratim idecay
kenvelope transeg 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
ihertz = cpsmidinn(ikey)
kamp = kenvelope
kfreq = ihertz
kpres = 0.25
; krat rspline 0.006,0.988,0.1,0.4
krat rspline 0.006,0.988,1,4
kvibf = 4.5
kvibamp = 0
iminfreq = 20
isine ftgenonce 0,0,65536,10,1
asignal wgbow kamp,kfreq,kpres,krat,kvibf,kvibamp,isine,iminfreq
absignal[] init 16
kx jspline 6, 1/5, 1/20
ky jspline 6, 1/5, 1/20
kz jspline 6, 1/5, 1/20
absignal, asend Spatialize asignal, gk_Droner_x, gk_Droner_y, gk_Droner_z
outleta "out", asend
outletv "outbformat", absignal
kelapsed timeinsts
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_Phaser_Attack init 10
gkratio1 init 1
gkratio2 init 1/3
gkindex1 init 1
gkindex2 init 0.0125
instr Phaser
insno = p1
istart = p2
iduration = p3
ikey = p4
ivelocity = p5
iphase = p6
ipan = p7
iamp = ampdb(ivelocity) * 8
iattack = i(gk_Phaser_Attack)
idecay = iattack
isustain = p3 - iattack
xtratim iattack + isustain + idecay
kenvelope transeg 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
ihertz = cpsmidinn(ikey)
isine ftgenonce 0,0,65536,10,1
khertz = ihertz
ifunction1 = isine
ifunction2 = isine
a1,a2 crosspm gkratio1, gkratio2, gkindex1, gkindex2, khertz, ifunction1, ifunction2
asignal = a1 + a2
absignal[] init 16
kx jspline 6, 1/5, 1/20
ky jspline 6, 1/5, 1/20
kz jspline 6, 1/5, 1/20
absignal, asend Spatialize asignal, kx, ky, kz
outleta "out", asend
outletv "outbformat", absignal
kelapsed timeinsts
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

#include "Sweeper1.inc"
gk_Sweeper_attack init 10
gk_Sweeper_release init 10

gk_Buzzer_Attack init 10
instr Buzzer
insno = p1
istart = p2
iduration = p3
ikey = p4
ivelocity = p5
iphase = p6
ipan = p7
iamp = ampdb(ivelocity) * 4
iattack = i(gk_Buzzer_Attack)
idecay = iattack
isustain = p3 - iattack
xtratim iattack + isustain + idecay
aenvelope transeg 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
ihertz = cpsmidinn(ikey)
;asignal gbuzz kenvelope, ihertz, 3, gkFirstHarmonic, gkDistortFactor, gisine
isine ftgenonce 0, 0, 65536, 10, 1
gkHarmonics = gkslider1 * 20
asignal buzz aenvelope, ihertz, gkHarmonics, isine
asignal = asignal * 3
;asignal vco2 kenvelope, ihertz, 12
;asignal poscil3 kenvelope, ihertz, giharmonics
;asignal distort asignal, gkDistortFactor * .4, giwaveshaping
aleft, aright pan2 asignal, ipan
adamping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
aleft = adamping * aleft
aright = adamping * aright
asignal = aleft + aright
absignal[] init 16
kx jspline 6, 1/5, 1/20
ky jspline 6, 1/5, 1/20
kz jspline 6, 1/5, 1/20
absignal, asend Spatialize asignal, kx, ky, kz
outleta "out", asend
outletv "outbformat", absignal
kelapsed timeinsts
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_Shiner_Attack init 10
instr Shiner
insno = p1
istart = p2
iduration = p3
ikey = p4
ivelocity = p5
iphase = p6
ipan = p7
iamp = ampdb(ivelocity) * 4
iattack = i(gk_Shiner_Attack)
idecay = iattack
isustain = p3 - iattack
xtratim iattack + isustain + idecay
kenvelope transeg 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
ihertz = cpsmidinn(ikey)
print insno, istart, iduration, ikey, ihertz, ivelocity, iamp, iphase, ipan
;asignal gbuzz kenvelope, ihertz, 3, gkFirstHarmonic, gkDistortFactor, gisine
gkHarmonics = gkslider1 * 20
;asignal buzz kenvelope, ihertz, gkHarmonics, gisine
;asignal = asignal
asignal vco2 kenvelope * 4, ihertz, 12
;asignal poscil3 kenvelope, ihertz, giharmonics
;asignal distort asignal, gkDistortFactor * .4, giwaveshaping
aleft, aright pan2 asignal, ipan
adamping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
aleft = adamping * aleft
aright = adamping * aright
asignal = aleft + aright
kfronttoback jspline 6, 1/5, 1/20
klefttoright jspline 6, 1/5, 1/20
kbottomtotop jspline 6, 1/5, 1/20
absignal[] init 16
absignal, aspatialreverbsend Spatialize asignal, kfronttoback, klefttoright, kbottomtotop
outletv "outbformat", absignal
outleta "out", aspatialreverbsend
kelapsed timeinsts
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

#include "Blower1.inc"

gk_Blower_grainDensity init 132.3332789825534
gk_Blower_grainDuration init 0.2854231208217838
gk_Blower_grainAmplitudeRange init 174.0746779716289
gk_Blower_grainFrequencyRange init 62.82406652535464
gk_Blower_level init -140
gk_Blower_front_to_back init 0
gk_Blower_left_to_right init 0.5
gk_Blower_bottom_to_top init 0

</CsInstruments>
<CsScore>

t 0 20

; p4 is just intonation in MIDI key numbers (numerator can be 0):
; [ ((numerator / denominator) * 12) + (octave * 12) + 24 ] 

; C E B
i "Droner"    0 60 [ (( 0 /  1) * 12) + (1 * 12) + 24 ] 60
i "Droner"    0 60 [ (( 4 /  5) * 12) + (2 * 12) + 24 ] 60
i "Droner"    0 60 [ ((15 / 28) * 12) + (3 * 12) + 24 ] 60
; C Ab E B
i "Blower"   30 30 [ (( 5 /  8) * 12) + (1 * 12) + 24 ] 60
; G F# B
i "Droner"   60 60 [ (( 2 /  3) * 12) + (1 * 12) + 24 ] 60
i "Droner"   60 60 [ ((32 / 45) * 12) + (2 * 12) + 24 ] 60
i "Droner"   60 30 [ ((15 / 28) * 12) + (3 * 12) + 24 ] 60
; G F B
i "Droner"   90 30 [ (( 3 /  4) * 12) + (3 * 12) + 24 ] 60
; C E B
i "Sweeper" 120 60 [ (( 0 /  1) * 12) + (1 * 12) + 24 ] 60
i "Sweeper" 120 60 [ (( 4 /  5) * 12) + (2 * 12) + 24 ] 60
i "Droner"  120 30 [ ((15 / 28) * 12) + (2 * 12) + 24 ] 60
i "Droner"  150 30 [ (( 0 /  1) * 12) + (2 * 12) + 24 ] 60
</CsScore>
</CsoundSynthesizer>

