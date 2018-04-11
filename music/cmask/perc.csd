<CsoundSynthesizer>
<CsOptions>
-odac -m35 -d
</CsOptions>
<CsScore>
f 0 24
</CsScore>
<CsInstruments>
sr = 48000
ksmps = 100
nchnls = 10

Score cmask {{ 
{
f1 0 8193 10 1
f2 0 8193 5 1 8193 .003
f3 0 8193 8 .8 1000 1 2192  .3 5000 0
f4 0 8193 5 1 1193 0.02 7000 .01
f5 0 8 -2 0 .02 .04 .07 .09 
}

f 0 20  

p1 const 1

p2 
rnd exp 2
mask .1 .5 map 1
quant .1 .96
prec 4

p3 
range .4 .5
prec 2

p4 range 0 4

p5 range 7 9



f 0 20  

p1 const 2

p2 rnd lin -1
mask .3 1 map 1
quant .3 .96
prec 2


p3 range .4 .5 
prec 2

p4 range 0 1


f 0 20  

p1 const 3

p2 rnd beta .2 .5
mask .1 1 map 1
quant .2 .9
prec 2

p3 range .8 1.5 
prec 2

p4 range 0 3
}}

instr 1		;mallet ?

;p2 onset
;p3 duration
;p4 pitch (0-4)
;p5 octav (7-9)

kenv	oscil		1,1/p3,2
kindx	pow		kenv,6,.5
iton	table		p4,5
a1	foscil	kenv*8000,cpspch(p5+iton),1,4,kindx,1
	outs		a1*(1-p4/4),a1*p4/4
		
endin	

instr 2		;metal plate

;p2 onset
;p3 duration
;p4 pitch (0/1)

kindx	expon		1,p3,.001
a1 	rand		100
a2	oscil		10000*kindx,3000+1500*p4+a1*(1+kindx),1

	outs		a2*p4,a2*(1-p4)	
endin	

instr 3

;p2 onset
;p3 duration
;p4 pitch (0-3)

kenv	oscil		1,1/p3,3
kindx	oscil		2,1/p3,4
a1	foscil	kenv*11000,100+p4*20,1,1.4,kindx,1
	outs		a1,a1
	
endin	

</CsInstruments>
</CsoundSynthesizer>