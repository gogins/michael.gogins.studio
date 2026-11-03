<CsoundSynthesizer>
<CsOptions>
-m3 -fRW -odac
</CsOptions>
<CsInstruments>
; Copyright (c) 2007 by Michael Gogins. All rights reserved.
; Inspired by the compositions of LaMonte Young.

sr     		    =        	48000
ksmps  		    =        	10
nchnls 		    =        	2
0dbfs           =           20000

gareverb1  	    init     	0
gareverb2  	    init     	0

		        instr 1
iattack		    init		20
idecay		    init		20
p3              =           p3 + (iattack + idecay) / 2.0
isustain	    =           p3 - (iattack + idecay)
ifundamental    =		    p4
inumerator      =           p5
idenominator    =           p6
ivelocity       =           p7
ipan            =           p8
iratio          =           inumerator / idenominator
ihertz          =           ifundamental * iratio
iamp		    =		    ampdb(ivelocity)

kenvelope 	    transeg 	0.0, iattack, 0.0, iamp, isustain, 0.0, iamp, idecay, 0.0, 0.0
asignal		    poscil3 	kenvelope, ihertz, 1
asignal  	    distort 	asignal, 0.125, 2
aleft, aright 	pan2 		asignal , ipan
gareverb1 	    = 		    gareverb1 + aleft
gareverb2 	    = 		    gareverb1 + aright
		        endin
		

instr 3
aleft, aright 	reverbsc 	gareverb1, gareverb2, 0.9, 15000.0
		        outs		gareverb1 + aleft * 0.8, gareverb2 + aright * 0.8
gareverb1	    =		    0
gareverb2	    =		    0
endin

</CsInstruments>
<CsScore>

;                       A few harmonics...
f   1   0       65536  	10   	3   0   0   1   0   2 
;                       ...distorted by waveshaping.
f   2 	0 	    65536 	13 	    1 	1	0 	3 	0   2    

t   0   4

; p1     p2     p3        p4           p5         p6           p7        p8
; insno  onset  duration  fundamental  numerator  denominator  velocity  pan

;                                      Fundamental
i 1 	 0 	    60 	      60	       1          1            60        0.75

;                                      Major whole tone
i 1 	 0 	    60 	      60	       9          8            60        0.25

;                                      Septimal octave
i 1 	 0 	    60 	      60	       63         32           60        0.00

;                                      Fundamental
i 1 	 60 	60 	      53.5546875   1          1            63        0.75

;                                      Perfect fifth
i 1 	 60 	60 	      53.5546875   3          2            62        0.25

;                                      Harmonic seventh
i 1 	 60 	60 	      53.5546875   7          4            61        0.00

;                                      Fundamental
i 1 	 120 	60 	      60	       1          1            60        0.75

;                                      Major whole tone
i 1 	 120 	60 	      60	       9          8            60        0.25

;                                      Septimal octave
i 1 	 120 	60	      60	       63         32           60        0.875

;                                      Septimal octave
i 1 	 120 	60	      60	       32         63           60        0.125

i 3      0 	-1

s 10.0
e 10.0
</CsScore>
</CsoundSynthesizer>
