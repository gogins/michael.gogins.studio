<CsoundSynthesizer>
<CsOptions>
-odac -m35 -d
</CsOptions>
<CsScore>
i 2 1 2.5

</CsScore>
<CsInstruments>
sr = 48000
ksmps = 100
nchnls = 10


instr 1

;p2 onset
;p3 duration
;p4 sound file pointer
;p5 speed factor (=transposition)

kenv	oscil		20000,1/p3,4
aindx	line		p4,p3,p4+p3*p5
asig	tablei	aindx*sr,1

	out		asig*kenv
	
endin	

instr 2
Score cmask {{ 
{
f1 0 131072 1 "schwermt.aif" 0 0 1
f4 0 8193 8 0 4096 1 4096 0
}

f 0 11  

p1 const 1

p2 const 0.01  	;constant grain interonset 10 ms

p3 const .02   	;constant grain duration 20 ms

p4 const .002	;1/5 tempo
accum on
prec 3

p5 const 1.5 	;fifth higher
}}
endin
</CsInstruments>
</CsoundSynthesizer>