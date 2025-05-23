<CsoundSynthesizer>
<CsOptions>
-odac -m35 -d
</CsOptions>
<CsScore>
f 0 13
</CsScore>
<CsInstruments>
sr = 48000
ksmps = 100
nchnls = 10

Score cmask {{ 
{
f1 0 131072 1 "schwermt.aif" 0 4 1
f2 0 8193 8 0 4096 1 4096 0
}

; for use with timestretch.orc !

f 0 10  

p1 const 1

p2 seg [.01 .12] 	;rising grain interonset 10 ...120 ms	
prec 2

p3 const .02 	;constant grain duration 20 ms

p4 const .01	;constant walk through the soundfile table
accum on
prec 2
}}

instr 1

;p2 onset
;p3 duration
;p4 sound file pointer

kenv	oscil		20000,1/p3,2
aindx	line		p4,p3,p3+p4
asig	tablei	aindx*sr,1

	out		asig*kenv
	
endin	

</CsInstruments>
</CsoundSynthesizer>