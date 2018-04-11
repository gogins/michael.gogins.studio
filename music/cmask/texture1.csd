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
f1 0 8192 9 .25 1 0
f2 0 8193 10 1 
}

f 0 30

p1 const 1

p2 rnd uni
mask [.01 .002 ipl 0] [.1 .01 ipl 0]

p3 range .5 1

p4 rnd uni
mask [860 80 ipl -1.2] [940 2000 ipl 1] map 1
quant 100 .9 0

p5 mask [.4 0] [.6 1]


f 31 33

p1 const 1

p2 seg [.08 .8 ipl 2]

p3 seg [.1 2]

p4 range 300 400

p5 seg [0 1]
}}

instr	1

;p4 frequency
;p5 pan (0...1) 

ipanl	table	1-p5 ,1,1
ipanr	table	p5 ,1,1

k1	expon	1,p3,.01
a1	foscil	k1*4200,p4,1,2.41,k1*6,2
	
	outs	a1*ipanl, a1*ipanr
	
endin

</CsInstruments>
</CsoundSynthesizer>