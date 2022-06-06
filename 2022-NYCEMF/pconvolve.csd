<CsoundSynthesizer>
<CsOptions>
</CsOptions>
<CsInstruments>

sr = 41000
ksmps = 32
;nchnls = 2
;nchnls_i = 1
0dbfs  = 10

instr   1

kmix = .5	; Wet/dry mix. Vary as desired.
kvol  = .05*kmix	; Overall volume level of reverb. May need to adjust
		; when wet/dry mix is changed, to avoid clipping.
                     			
; do some safety checking to make sure we the parameters a good
kmix = (kmix < 0 || kmix > 1 ? .5 : kmix)
kvol  = (kvol < 0 ? 0 : .5*kvol*kmix)

; size of each convolution partion -- for best performance, this parameter needs to be tweaked
ipartitionsize = p4

; calculate latency of pconvolve opcode
idel = (ksmps < ipartitionsize ? ipartitionsize + ksmps : ipartitionsize)/sr
prints "Convolving with a latency of %f seconds%n", idel

; actual processing
;al in	;get live input
al soundin "planets.wav"
awetl, awetr pconvolve kvol*(al+al), "kickroll.wav", ipartitionsize
; Delay dry signal, to align it with the convoled sig
adryl delay (1-kmix)*al, idel
adryr delay (1-kmix)*al, idel
      outs adryl+awetl, adryr+awetr

endin
    
instr 2

imix = 0.5          	; Wet/dry mix. Vary as desired.
ivol = .05*imix     	; Overall volume level of reverb. May need to adjust
                	; when wet/dry mix is changed, to avoid clipping.
ipartitionsize = 1024	; size of each convolution partion
idel = (ksmps < ipartitionsize ? ipartitionsize + ksmps : ipartitionsize)/sr   ; latency of pconvolve opcode

kcount	init	idel*kr
; since we are using a soundin [instead of ins] we can
; do a kind of "look ahead" by looping during one k-pass
; without output, creating zero-latency
loop:
    asig soundin p4, 0
    awetl, awetr pconvolve ivol*(asig),"rv_stereo.wav", ipartitionsize
    adry delay	(1-imix)*asig,idel  ; Delay dry signal, to align it with
    kcount = kcount - 1
 if kcount > 0 kgoto loop
    outs awetl+adry, awetr+adry
		
endin
</CsInstruments>
<CsScore>

i 1 0 20 1024		;play live for 20 seconds

i 2 20 5 "fox.wav"
i 2 25 5 "flute.aiff"

e
</CsScore>
</CsoundSynthesizer>
