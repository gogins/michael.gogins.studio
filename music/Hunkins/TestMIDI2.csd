<CsoundSynthesizer>
<CsLicense>

A CARILLON FOR CREATION (2025) for realtime Csound
By Arthur B. Hunkins

Requires ASCII keyboard, and a selection of 6-8 MIDI note numbers 
corresponding to pitches from E3 - G5, non-duplicating and from a single 
octatonic scale. Performance is via MIDI-controlled 23+ note carillon.

ASCII keys must be *tapped*, not held.

ISSUE

Glad to have any feedback regarding my issue as demoed in the attached 
.csd.

The problem is simple: run as is, everything works as expected, except that 
the MIDI file written is empty. This is associated with the fact that the 
duration of performance (via ASC keyboard, the first 8 number keys) is 
infinite (z) and must be stopped by CTRL-C.

Changing the z infinite duration to a finite number (say 5 or 10) finishes 
the performance normally and writes an appropriate MIDI file. (Try it and 
see/verify.)

According to Victor L., z (CTRL-C) should write a proper MIDI file as well. 
This is the only issue. The audio performance depends on the right -Q value, 
but the MIDI file output seems totally independent of this.

Actually, as long as you have some kind of MIDI device plugged in (or an 
active MIDI cable), you can test this out without any audio at all. Just see 
whether the MIDI file (which if Csound runs, will be created) has any bytes!

Thanks for any insight. Victor suggests that "this is a Windows problem" -- 
which may well be true. (Do you find the same results with Mac or Linux?) 

</CsLicense>
<CsOptions>

; for Windows, Mac
-odac -m0d  -b128 -B2048 -Q1
; for Linux
;-odac -+rtaudio=alsa -+rtmidi=alsa -m0d -b128 -B2048 -Q hw:0,1
; to create a MIDI file, add the following line, substituting your desired filename
--midioutfile=MIDIFile2.mid

</CsOptions>
<CsInstruments>

#define	TRANSFAC #0# ; transposition factor (in semitones) to compensate for carillons pitched variously
	
; optionally insert your own MIDI note #'s below; they are associated with successive ASCII keys from (number keys) 1 to 8 on the top row.

giMIDI1	= 52
giMIDI2	= 55
giMIDI3	= 57
giMIDI4	= 61
giMIDI5	= 63
giMIDI6	= 66
giMIDI7	= 70
giMIDI8	= 72

sr	= 44100
ksmps	= 128

gitrans	= powoftwo($TRANSFAC / 12)

	instr 1

kASC	sensekey
	if kASC == -1 goto end
	if (kASC < 49) || (kASC > 56) goto end
	if kASC == 49 then
	event "i", 2, 0, -1
	elseif kASC == 50 then	
	event "i", 3, 0, -1
	elseif kASC == 51 then
	event "i", 4, 0, -1
	elseif kASC == 52 then	
	event "i", 5, 0, -1
	elseif kASC == 53 then	
	event "i", 6, 0, -1
	elseif kASC == 54 then	
	event "i", 7, 0, -1
	elseif kASC == 55 then	
	event "i", 8, 0, -1
	elseif kASC == 56 then	
	event "i", 9, 0, -1
	endif

end:	endin

	instr 2

	noteon 1, giMIDI1 + $TRANSFAC, 100

	endin

	instr 3

	noteon 1, giMIDI2 + $TRANSFAC, 100

	endin

	instr 4

	noteon 1, giMIDI3 + $TRANSFAC, 100

	endin

	instr 5

	noteon 1, giMIDI4 + $TRANSFAC, 100

	endin

	instr 6

	noteon 1, giMIDI5 + $TRANSFAC, 100

	endin

	instr 7

	noteon 1, giMIDI6 + $TRANSFAC, 100

	endin

	instr 8

	noteon 1, giMIDI7 + $TRANSFAC, 100

	endin

	instr 9

	noteon 1, giMIDI8 + $TRANSFAC, 100

	endin

</CsInstruments>
<CsScore>

; to create a MIDI file, take the approx duration in seconds you would like the file to last, and substitute this value for "z" below.
i1 0 z

e

</CsScore>
</CsoundSynthesizer>