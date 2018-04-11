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

p2 rnd uni		;random interonset, exponential rising density
mask [.2 .005 ipl 1.5] [.4 .01 ipl 1.5] map 1
prec 4	

p3 range .02 .05 	;random duration
prec 2

p4 range 0 2.1 	;random pointer
prec 3
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