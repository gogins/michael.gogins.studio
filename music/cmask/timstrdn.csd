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

f 0 11

p1 const 1

p2 const .01 	;constant grain interonset 10 ms

p3 const .02 	;constant grain duration 20 ms   

p4 seg [.01 .0004 ipl 4]
accum on		;from normal to very small time steps	
prec 4		;= exponential slow down
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