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
;p4 speed factor (=transposition)

kenv	oscil		30000,1/p3,2
aindx	line		p2,p3,p2+p3*p4
asig	tablei	aindx*sr,1

	out		asig*kenv
	
endin	

instr 2
Score cmask {{ 
{
f1 0 131072 1 "schwermt.aif" 0 0 1 
f2 0 8193 8 0 4096 1 4096 0
}

; for use with pitchshift.orc !

f 0 2.2  

p1 const 1

p2 const .02 	;constant grain interonset 20 ms

p3 const .04 	;constant grain duration 40 ms

p4 range .75 1.5 	;random intervall
prec 2

}}
endin
</CsInstruments>
</CsoundSynthesizer>