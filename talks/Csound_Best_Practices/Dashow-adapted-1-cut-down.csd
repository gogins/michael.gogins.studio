<CsoundSynthesizer>
<CsOptions>
-odac
</CsOptions>
<CsInstruments>
sr = 48000
ksmps = 1
nchnls = 2

instr 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15
endin

instr DashowCool
ga15 init 0
ga16 init 0
i7 = int(frac(p12)*100+.5)
i8 = (p6<0? (cpspch(abs(p6))/1.021975)-1 : p6) 
p7 = (p6<0? (cpspch(p7)/1.021975)-1 : p7)
kk7 init i8
i22 = p7-i8
; Adjust the amplitude.
p28 = (p28=0? 2/int(p12) : p28/int(p12))
p4 = p4*p28
; Number of oscillators.
i21 = int(p12+.5)
; this code puts together zvfmult pch table, rand control and
; zvfmult rthm table, output of this is aimed at timout,envlpx,
; and linseg; timout sets reinit. i5 thru i20, i35-i38,
; kk1 thru kk6 are reserved for these units. kk3, kk4 now unused.
; kk5=stereo left, kk6=stereo right, i9=zvfmult output, a5 is envlpx
; output; k1 thru k4 reserved.
i35 = int(frac(abs(p10))*100+.5)
; Initialize the random range.
i20 = (i35-int(abs(p10))-.02)*.5
; Initialize the random center.
i5 = int(abs(p10))+i20
i20 = i20+1
; Initialize the old random number.
i6 = i35+1
i16 = p3/int(p14)
; Amplitude.
i19 = p4 
kk1 randi i20,431,frac(p11)
;series zvfmult inits
i10 = int(abs(p10))-1 ;loc1 pointer
it1 = int(frac(abs(p11))*100+.5)
i11 = (it1=0? i10 : it1-1) ;recyc init
;rthm zvfmult inits
i12 = int(p20)-1 ;loc2 pointer
i13 = (p21=0? i12 : p21-1) ;recyc init
i14 = 0 ;atax
i15 = 0 ;total
;zvamph inits
if p24=0 igoto env
i18 = int(p24)-1 ;pointer init
i17 = ftlen(int(frac(p24)*100+.5))
kk2 init int(p24)/i17
kk2 phasor p23/p3,i(kk2) ;for pos p23 cps
;envlpx init
env: i36 = (frac(p15)=0? 1 : frac(p15)*10) ;atten factor of ss
;nf zvfmult inits
i23 = int(p25)-1
i24 = (p26=0? i23 : p26-1) ;recyc
rinit:
i9 = p10 ;constant pch init
if p11=0 igoto j30
if p11>0 igoto skp1
rnd: i10 = int(i5+i(kk1))
it1 = int(abs(p9))
i10 = (i10<it1? it1 : i10)
i10 = i10+(i10=i6? 1 : 0)
i10 = (i10>i35? it1 : i10)
i6 = i10
skp1: i9 table i10,int(abs(p11))
i10 = i10+1
if i10<i35 igoto j30
i10 = i11
j30: i9 = cpspch(i9)
it1 = i8*i9
if p6>=0 igoto j33
kk7 init it1 ;min interval
i22 = p7*i9-it1 ;intrval dif
j33: kk8 init i9+p5*it1 ;init real pch+offset
;nfsig zvfmult
if p27=0 igoto nt1
i7 table i23,p27
i23 = i23+1
i23 = (i23<int(frac(abs(p25))*100+.5)? i23 : i24)
;rthm zvfmult
nt1: if p22=0 igoto nt2
;i16 must be init to p14 before here
skp2: it3 table i12,p22
i12 = i12+1
if i12<int(frac(p20)*100+.5) igoto to2
i12 = i13
to2: if i14>=int(p14) igoto j35
; loop for rthm values
i15 = i15+it3
i14 = i14+1
if i14<int(p14) igoto skp2
; exit from loop and set loc pointer for first attack
i12 = int(p20)-1
igoto skp2
j35: i16 = p3*it3/i15 ;dur value sent to envlpx, timout, linseg
nt2: if p24=0 igoto nt3 ;zvamph
i18 = i18+1
if i18<abs(p23) igoto j40
i18 = int(p24)
j40: i18 = (p23<0? i18 : i(kk2)*i17)
i19 table i18,frac(p24)*100+.5
i19 = i19*p4 ;final amp
;envlop
nt3: k2 linseg 0,p13,.25,i16-p13,1 ;env follower control
a5 envlpx i19,p13,i16,frac(p14)*10,p15,i36,1/p16 ;env table
timout 0,i16,j50
reinit rinit
rireturn
j50: ;stereo control
kk5 init p17 ;left prop
kk6 init 1-p17 ;right prop
if p19=0 goto j60
it1 = (p17=1? 1 : int(p17)*.1)
i37 = frac(p17) ;min
i38 = it1-i37 ;dif
k4 oscil i38,p18/p3,frac(p19)*100+.5,int(p19) ;wrong phase
kk5 = i37+k4
kk6 = 1-kk5
;at end of instr, write outs sig*kk5,sig*kk6
j60: if p9=0 goto j70
i1 = int(frac(p9)*100+.5) ;nf
i2 = ftlen(i1)
k5 phasor p8/p3,int(p9)/i2
k5 = (p8=0? k2 : k5)
k5 table k5*i2,i1
kk7 = i8+i22*k5
kk8 = i9+p5*kk7
j70: a1 poscil3 a5,kk8,i7,0
a2 poscil3 a5,kk8+kk7,i7,.3/i21
a3 poscil3 a5,kk8+kk7+kk7,i7,.6/i21
a4 = a1+a2+a3
if i21<5 goto j80
a1 poscil3 a5,kk8+3*kk7,i7,.9/i21
a2 poscil3 a5,kk8+4*kk7,i7,1.2/i21
a4 = a4+a1+a2
if i21<7 goto j80
a1 poscil3 a5,kk8+5*kk7,i7,1.5/i21
a2 poscil3 a5,kk8+6*kk7,i7,1.8/i21
a4 = a4+a1+a2
j80: ga16 = ga16+a4
outs a4*kk5,a4*kk6
a_out_left = a4 * kk5
a_out_right = a4 * kk6
outleta "outleft", a_out_left
outleta "outright", a_out_right
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

instr DashowCool

endin

</CsInstruments>
<CsScore>

; Sinusoids.
f1   0   [2*18 - 1]   10	 1
f2   0   [2*18 - 1]   10	 1         -.5    -.25   .1
f3   0   [2*18 - 1]   10	 1          .05   -.5    .05	 .5    .05   -.3   .05	 1
f3  31	 [2*18 - 1]   10     1          .15	  -.6    .2   -.5	   .3    -.4   .3     .3
f5   0   [2*18 - 1]   10	 1          .05   -.6
f6   0   [2*18 - 1]   10	 1         -.6     .15
; Exponential segments.
f7   0   [ 17]    5	 1        [16]     50
f8   0   [ 17]    5	 1        [16]     16
f9   0	 [ 64]    5     .0001   [32]	.16    [  5]   1	[27]	1
f10  0	 [ 64]    5    1        [16]   1000    [ 24]   1    [24]    1
f11 50	 [ 32]    5   20	      [32]	  1
f9  15	 [128]    5          .001    50	 .16   5    1	 20   .8    7	.001   7   1 20   .8   7    .001    7   1	5    1
; Linear segments.
f4   0   128    7	 0   64       1    64	  0
f9  31	 128    7    0   40   .1	 24   1    40	 .85   24   0
f9  39.5  64    7    1	  30	1    2	 0   30   0    2    1
f12 46    64	7    0	   12	  .05	 20   1    12	.9    20   0
; Transferred from pfields.
f14  0	  64   -2   7.05   8.06	 9.07	10.08	8.03   9.02    10.04  11.01 7.10	8.11   10.00   10.09   4   6   9   5   1   1   3   2   7  2   1   2   2 9   4	7   3	2   2  4   2   8   3   7   6   5   2   1   1

; p01 p02 p03 p04 p05 p06 p07 p08 p09 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30 p31 p32 p33 p34 p35 p36 p37 p38 p39 p40
; ins tim dur amp                     pch     pch
i16   .44   13.56 5000  0     .04   .75   1     .09   8.06  0     7.02  3.5  1.007 8	  70	1     0     0	  0	0     0     0	  0  0    0     0	  0
i16   .     .     .     .     .     .     .     .     10.08 .     5.06  .    .     .     .     0
i16   15    12    .     .     .     .8    .     .     10.04 .     7.02 
i16   .     .	  .	.     .     .	  .	.     9.02  .	  7.03  .    .     .     .     1
i16   27    4	  .	.     .     .	  .6	50.09 7.10  .	  .  .03  .     7	  20
i16   27.12 .     .     .     .     .     .     .     10.00 .     .  .    .     .	  .	0
i16   31.12 5.5   .     .     .     .     3     .09   10.09 .     7.02
i16   31.0  .     .     .     .     .     .     .     8.11  .     7.03  .    .     .     .     1
i16   36.5  .	  3000	.     -1.00 1.0005 22	.02   9.06  .	  5.02 
i16   36.62 .	  .	.     .     .	  28	255.06 7.05 .	  5.03  .    .     .	  .	0
i16   38.5  1     .     .     -.07  1.00  1     .07   9.02  .     5.02
i16   .     .     .     .     .     .     .     .     8.03  .     .  .    .     .     .     1
i16   39.5  6.75  .	.     .     .	  8	.09   .     .	  5.03
i16   .     6.5   .     .     .     .     6.5   .     9.02  .     .  .    .     .	  .	0
i16   42.12 4.65  .     .     .     .     .     .     10.09 .     5.02
i16   42    4.88  .     .     .     .     8     .     10.00 .     5.03  .    .     .     .     1
i16   46.88 7.12  5000	.     .04   .8	  6.5	.12   .     .	  7.03  .    1.4   .     16    .75
i16   46.77 7.23  .	.     .     .	  7.8	.     10.09 .	  7.02  .    .     .     .     .25
i16   46    8	  .	.     .     .	  5.5	.     9.02  .	  7.03  .    .     .	  .	0
i16   46.25 7.75  .	.     .     .	  6	.     8.03  .	  .  .    .     .	  .	1
;i13   0     54.1  .63	3     15    -6	  .04	.005  .05   29	  .04 .02  .02   0	  0	.5    0     0	  0	58    1     0	  0
s
f0 1
e

</CsScore>
</CsoundSynthesizer>
