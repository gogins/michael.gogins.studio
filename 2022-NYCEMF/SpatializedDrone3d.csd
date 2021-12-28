<CsoundSynthesizer>
<CsLicense>
Copyright (C) 2013 by Michael Gogins.
All rights reserved.
</CsLicense>
<CsOptions>
-+msg_color=0 -odac -m195 -d -odac
</CsOptions>
<CsInstruments>
sr = 96000
ksmps = 1
nchnls = 2
0dbfs = 100000

#define USE_SPATIALIZATION #1#

#include "Spatialize1.inc"

gk_BformatDecoder_SpeakerRig init 1
gk_Spatialize_SpeakerRigRadius init 7.0
gk_SpatialReverb_ReverbDecay init 0.8
gk_SpatialReverb_CutoffHz init sr
gk_SpatialReverb_RandomDelayModulation init .2
gk_LocalReverbByDistance_Wet init 0.5
; This is a fraction of the speaker rig radius.
gk_LocalReverbByDistance_FrontWall init 0.9
gk_LocalReverbByDistance_ReverbDecay init 0.6
gk_LocalReverbByDistance_CutoffHz init 20000
gk_LocalReverbByDistance_RandomDelayModulation init .2
gk_Spatialize_Verbose init 0

gkslider1 init 0
gkslider2 init 0
gkslider3 init 0
gkslider4 init 0
gkslider5 init 0

connect "Droner", "outbformat", "BformatDecoder2", "inbformat"
connect "Droner", "out", "SpatialReverb2", "in"
connect "Blower", "outbformat", "BformatDecoder2", "inbformat"
connect "Blower", "out", "SpatialReverb2", "in"
connect "Buzzer", "outbformat", "BformatDecoder2", "inbformat"
connect "Buzzer", "out", "SpatialReverb2", "in"
connect "Shiner", "outbformat", "BformatDecoder2", "inbformat"
connect "Shiner", "out", "SpatialReverb2", "in"
connect "SpatialReverb2", "outbformat", "BformatDecoder2", "inbformat"

alwayson "SpatialReverb"
alwayson "SpatialReverb2"
alwayson "BformatDecoder"
alwayson "BformatDecoder2"
alwayson "Controls"

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
gk_Droner_10 init 0.0
gk_Droner_x init 0
gk_Droner_y init 5
gk_Droner_z init 2
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
xtratim iattack + idecay
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
absignal, asend Spatialize asignal, gk_Droner_x + kx, gk_Droner_y + ky, gk_Droner_z + kz
outleta "out", asend
outletv "outbformat", absignal
kelapsed timeinsts
printks "Droner  i %7.2f t %7.2f [%7.2f] d %7.2f k %7.2f f %7.2f v %7.2f kx %7.2f ky %7.2f kz %7.2f A %9.2f\n", 1.0, p1, p2, kelapsed, p3, p4, ihertz, p5, kx, ky, kz, asignal
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
xtratim iattack + isustain + idecay
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
printks "Bower  i %7.2f t %7.2f [%7.2f] d %7.2f f %7.2f v %7.2f kx %7.2f ky %7.2f kz %7.2f A %7.2f\n", 1.0, p1, p2, kelapsed, p3, ihertz, p5, kx, ky, kz, asignal
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
printks "Phaser i %7.2f t %7.2f [%7.2f] d %7.2f k %7.2f %7.2f v %7.2f kx %7.2f ky %7.2f kz %7.2f A %7.2f\n", 1.0, p1, p2, kelapsed, p3, p4, ihertz, p5, kx, ky, kz, downsamp(asignal)
endin

gk_Sweeper_Attack init 10
gkbritel init 0
gkbriteh init 2.9
gkbritels init .2 / 3
gkbritehs init 2.5 / 2
instr Sweeper
insno = p1
istart = p2
iduration = p3
ikey = p4
ivelocity = p5
iphase = p6
ipan = p7
print insno, istart, iduration, ikey, ivelocity, iphase, ipan
iamp = ampdb(ivelocity)
gisine ftgenonce 0, 0, 65536, 10, 1
gioctfn ftgenonce 0, 0, 65536, -19, 1, 0.5, 270, 0.5
iattack = i(gk_Sweeper_Attack)
idecay = iattack
isustain = p3 - iattack
xtratim iattack + isustain + idecay
kenvelope transeg 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
ihertz = cpsmidinn(ikey)
icps = ihertz
kamp expseg 0.001,0.02,0.2,p3-0.01,0.001
ktonemoddep jspline 0.01,0.05,0.2
ktonemodrte jspline 6,0.1,0.2
ktone poscil3 ktonemoddep, ktonemodrte, gisine
kbrite rspline gkbritel, gkbriteh, gkbritels, gkbritehs
ibasfreq init icps
ioctcnt init 3
iphs init 0
;a1 hsboscil kamp, ktone, kbrite, ibasfreq, gisine, gioctfn, ioctcnt, iphs
a1 hsboscil kenvelope, ktone, kbrite, ibasfreq, gisine, gioctfn, ioctcnt, iphs
amod poscil3 0.25, ibasfreq*(1/3), gisine
arm = a1*amod
kmix expseg 0.001, 0.01, rnd(1), rnd(3)+0.3, 0.001
kmix=.25
a1 ntrpol a1, arm, kmix
;a1 pareq a1/10, 400, 15, .707
;a1 tone a1, 500
kpanrte jspline 5, 0.05, 0.1
kpandep jspline 0.9, 0.2, 0.4
kpan poscil3 kpandep, kpanrte, gisine
a1,a2 pan2 a1, kpan
a1 delay a1, rnd(0.1)
a2 delay a2, rnd(0.11)
kenv linsegr 1, 1, 0
kenv = kenvelope
aleft = a1*kenv*.02
aright = a2*kenv*.02
asignal = aleft + aright
absignal[] init 16
kx jspline 6, 1/5, 1/20
ky jspline 6, 1/5, 1/20
kz jspline 6, 1/5, 1/20
absignal, asend Spatialize asignal, kx, ky, kz
outleta "out", asend
outletv "outbformat", absignal
kelapsed timeinsts
printks "Sweeper i %7.2f t %7.2f [%7.2f] d %7.2f f %7.2f v %7.2f kx %7.2f ky %7.2f kz %7.2f A %7.2f\n", 1.0, p1, p2, kelapsed, p3, ihertz, p5, kx, ky, kz, downsamp(asignal)
endin

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
gkHarmonics = gkslider1 
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
printks "Buzzer  i %7.2f t %7.2f [%7.2f] d %7.2f k %7.2f %7.2f v %7.2f kx %7.2f ky %7.2f kz %7.2f A %7.2f\n", 1.0, p1, p2, kelapsed, p3, p4, ihertz, p5, kx, ky, kz, downsamp(asignal)
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
printks "Shiner i %7.2f t %7.2f [%7.2f] d %7.2f f %7.2f v %7.2f kx %7.2f ky %7.2f kz %7.2f A %7.2f\n", 1.0, p1, p2, kelapsed, p3, ihertz, p5, kfronttoback, klefttoright, kbottomtotop, downsamp(asignal)
endin

#include "Blower1.inc"


</CsInstruments>
<CsScore>

;t 0 27

; p4 is just intonation in MIDI key numbers (numerator can be 0):
; [ ((numerator / denominator) * 12) + (octave * 12) + 24 ] 

; C E B
i "Sweeper"   0 60 [ (( 0 /  1) * 12) + (1 * 12) + 24 ] 60
i "Buzzer"    0 60 [ (( 4 /  5) * 12) + (2 * 12) + 24 ] 60
i "Phaser"    0 60 [ ((15 / 28) * 12) + (3 * 12) + 24 ] 60
; C Ab E B
i "Sweeper"  30 30 [ (( 5 /  8) * 12) + (1 * 12) + 24 ] 60
; G F# B
i "Buzzer"   60 60 [ (( 2 /  3) * 12) + (1 * 12) + 24 ] 60
i "Buzzer"   60 60 [ ((32 / 45) * 12) + (2 * 12) + 24 ] 60
i "Buzzer"   60 30 [ ((15 / 28) * 12) + (3 * 12) + 24 ] 60
; G F B
i "Buzzer"   90 30 [ (( 3 /  4) * 12) + (3 * 12) + 24 ] 60
; C E B
i "Droner"  120 60 [ (( 0 /  1) * 12) + (1 * 12) + 24 ] 60
i "Droner"  120 60 [ (( 4 /  5) * 12) + (2 * 12) + 24 ] 60
i "Droner"  120 30 [ ((15 / 28) * 12) + (2 * 12) + 24 ] 60
i "Droner"  150 30 [ (( 0 /  1) * 12) + (2 * 12) + 24 ] 60
e 10.0
</CsScore>
</CsoundSynthesizer>






