#|
PANGUE LINGUA LAYERS
Michael Gogins
5 January 2019

Been trying to make something out of this jewel for decades!

To do:

--  Changes of range/transposition in the middle. Needs work. Note that 8vo preserves tune, others vary it.
--  End with reverb tail.
--  Better harmony if possible. 
--  The exactly correct tempo and duration and reverb. Maybe a little tweaking,

Program Notes

This piece consists of the tune of Pange Lingua, in Mode III, played in four 
voices that are layered at different modal transpositions and repeated at 
different tempos and lengths. I created the composition using the Lisp version 
of Common Music. I created the sound using Fons Adriaensen's software pipe 
organ, Aeolus, ported to a Csound opcode by me and run in Csound.
|#

(load "~/quicklisp/setup.lisp")
(ql:quickload "nudruz")
(in-package :cm)

(set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc)

(defparameter aeolus-orc #>qqq>
sr                              =                       48000
ksmps                           =                       64
nchnls                          =                       2
0dbfs                           =                       32768
iampdbfs                        init                    32768
                                prints                  "Default amplitude at 0 dBFS:  %9.4f\n", iampdbfs
idbafs                          init                    dbamp(iampdbfs)
                                prints                  "dbA at 0 dBFS:                 %9.4f\n", idbafs
iheadroom                       init                    6
                                prints                  "Headroom (dB):                 %9.4f\n", iheadroom
idbaheadroom                    init                    idbafs - iheadroom
                                prints                  "dbA at headroom:               %9.4f\n", idbaheadroom
iampheadroom                    init                    ampdb(idbaheadroom)
                                prints                  "Amplitude at headroom:        %9.4f\n", iampheadroom
                                prints                  "Balance so the overall amps at the end of performance is -6 dbfs.\n"

giFlatQ                         init                    sqrt(0.5)
giseed				            init                    0.5

gkReverberationEnabled          chnexport               "gkReverberationEnabled", 1
gkReverberationEnabled          init                    1
gkReverberationDelay            chnexport               "gkReverberationDelay", 1
gkReverberationDelay            init                    0.625
gkReverberationWet          	chnexport               "gkReverberationWet", 1
gkReverberationWet          	init                    0.25

gkMasterLevel                   chnexport               "gkMasterLevel", 1
gkMasterLevel                   init                    1.5

                                connect                 "AeolusOut",            "outleft", 	"Reverberation",        "inleft"
                                connect                 "AeolusOut",            "outright", "Reverberation",        "inright"
                                connect                 "Reverberation",        "outleft", 	"MasterOutput",         "inleft"
                                connect                 "Reverberation",        "outright", "MasterOutput",         "inright"

                                alwayson                "AeolusOut"
                                alwayson                "Reverberation"
                                alwayson                "MasterOutput"
                                                                
gi_aeolus                       aeolus_init             "stops-0.3.0", "Aeolus", "waves", 0, 12, 0

                                instr Aeolus_P 
                                //////////////////////////////////////////////
                                // By Michael Gogins.
                                //////////////////////////////////////////////
                                aeolus_note             gi_aeolus, p1 - 1, p4, p5
                                prints                  "Aeolus_P       i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p7, active(p1)
                                endin

                                instr Aeolus_I 
                                //////////////////////////////////////////////
                                // By Michael Gogins.
                                //////////////////////////////////////////////
                                aeolus_note             gi_aeolus, p1 - 1, p4, p5
                                prints                  "Aeolus_I       i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p7, active(p1)
                                endin

                                instr Aeolus_II 
                                //////////////////////////////////////////////
                                // By Michael Gogins.
                                //////////////////////////////////////////////
                                aeolus_note             gi_aeolus, p1 - 1, p4, p5
                                prints                  "Aeolus_II      i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p7, active(p1)
                                endin

                                instr Aeolus_III 
                                //////////////////////////////////////////////
                                // By Michael Gogins.
                                //////////////////////////////////////////////
                                aeolus_note             gi_aeolus, p1 - 1, p4, p5
                                prints                  "Aeolus_III     i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p7, active(p1)
                                endin
                                
                                instr AeolusPreset
                                aeolus_preset           gi_aeolus, 0, p4
                                prints                  "AeolusPreset   i %9.4f t %9.4f d %9.4f p %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p7, active(p1)
                                endin

                                //////////////////////////////////////////////
                                // OUTPUT INSTRUMENTS MUST GO BELOW HERE
                                //////////////////////////////////////////////

                                ; Send audio from the Aeolus to the output.
                                instr AeolusOut 
                                //////////////////////////////////////////////
                                // By Michael Gogins.
                                //////////////////////////////////////////////
                                aeolus_preset           gi_aeolus, 0, 0
a_out[]                         init                    2
a_out                           aeolus_out              gi_aeolus
                                out                     a_out
                                outleta                 "outleft",  a_out[0] * 0dbfs
                                outleta                 "outright", a_out[1] * 0dbfs
                                prints                  "AeolusOut      i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p7, active(p1)
                                endin        

                                instr                   Reverberation
                                //////////////////////////////////////////////
                                // By Michael Gogins.
                                ////////////////////////////////////2
ainleft                         inleta                  "inleft"
ainright                        inleta                  "inright"
if (gkReverberationEnabled == 0) goto reverberation_if_label
goto reverberation_else_label
reverberation_if_label:
aoutleft                        =                       ainleft
aoutright                       =                       ainright
kdry				            =			            1.0 - gkReverberationWet
goto reverberation_endif_label
reverberation_else_label:
awetleft, awetright             reverbsc                ainleft, ainright, gkReverberationDelay, 18000.0
aoutleft			            =			            ainleft *  kdry + awetleft  * gkReverberationWet
aoutright			            =			            ainright * kdry + awetright * gkReverberationWet
reverberation_endif_label:
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "Reverberation  i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p7, active(p1)
                                endin

                                instr                   MasterOutput
                                //////////////////////////////////////////////
                                // By Michael Gogins.
                                //////////////////////////////////////////////
ainleft                         inleta                  "inleft"
ainright                        inleta                  "inright"
aoutleft                        =                       gkMasterLevel * ainleft
aoutright                       =                       gkMasterLevel * ainright
                                outs                    aoutleft, aoutright
                                prints                  "MasterOutput   i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p7, active(p1)
                                endin
qqq)

; Each beat of "Pange Lingua" in Mode 3.

(defparameter Pange-Lingua
    (keynum '(e4 e e d g g a c5 c c c c c d c c c a4 c5 b4 a g g g g g a c5 b4 a g a a a a a a b g fs e a a a a a d d d d g g g e g a a g g g g a b g a g f d e e e e e e e)))

; Pitches of Mode 3, but without the F sharp.

(defparameter pitch-class-set-1  (new mode :degrees   '(c  d  e  f     g  a  b  c)))
(defparameter pitch-class-set-2  (new mode :degrees   '(c  d  ef f     g  af bf c)))
(defparameter pitch-class-set-3  (new mode :degrees   '(c  d  e  f     g  a  bf c)))

;; Nine repetitions of the tune in the bass.
;; The bass, voice 1, is the master voice that sequences the others at each repetition.

;; Nine repetitions...                                   1   2   3       4       5       6       7       8   9

(defparameter voice-1-tempos                         '(  2   2   4       4       4       4       4       2   2))
(defparameter voice-2-tempos                         '(  2   2   4       3     3     3     3     4       2   2))
(defparameter voice-3-tempos                         '(  2   2   2   2   2   2   1 1 1 1 2   2   2   2   2   2))
(defparameter voice-4-tempos                         '(  2   2   2   2   1 1 1 1 1 1 1 1 1 1 1 1 2   2   2   2))

(defparameter voice-1-transpositions (new cycle :of  '(-12 -12 -24     -21     -19     -16     -24     -12 -12))) 
(defparameter voice-2-transpositions (new cycle :of  '(  4   4 -20      -3      -3      -3     -15       4   4)))
(defparameter voice-3-transpositions (new cycle :of  '(  9   9   4       9      12       4       4       9   9)))
(defparameter voice-4-transpositions (new cycle :of  '( 12  12  12      12      12      12      12      12  12)))

(defparameter preset-cycle (new cycle :of            '(  0   1   2       3       4       5       6       7   0)))
(next preset-cycle)

(defparameter chord-cycle 
    (new cycle :of 
        (list 
            (transpose pitch-class-set-1  0) ; 1
            (transpose pitch-class-set-1  0) ; 2
            (transpose pitch-class-set-1  5) ; 3
            (transpose pitch-class-set-1  5) ; 4
            (transpose pitch-class-set-2  4) ; 5
            (transpose pitch-class-set-2  4) ; 5
            (transpose pitch-class-set-1  7) ; 7
            (transpose pitch-class-set-1  7) ; 8
            (transpose pitch-class-set-1  0) ; 9
        )
    )
)
(defparameter chord (next chord-cycle))

(format t "Length of 'Pange Lingua:' ~a~%" (length Pange-Lingua))

(defparameter pp-pulse 1/24)

(defparameter pp-tempo 46.0)
(defparameter stop-flag 0)

(defun bpm->seconds (bpm)
  (/ 60.0 bpm))

(defun rhythm->seconds (rhy tempo)
  (* rhy 4.0 (bpm->seconds tempo)))
  
(defun voice-1 (Pange-Lingua key-cycle-pop-tail amp channel time-offset transposition-cycle tempos is-master)
    (let* 
        (
            (rate (rhythm->seconds pp-pulse pp-tempo))
            ; Differential canon.
            (key-cycle (new cycle :keynums (subseq Pange-Lingua 0 (- (length Pange-Lingua) key-cycle-pop-tail))))
            (tempo-cycle (new cycle :of tempos))
            (tempo_ (next tempo-cycle))
            (transposition (next transposition-cycle))
        )
        (process 
            until (= stop-flag 1)
            for now_ = (now)
            for k = (next key-cycle)
            for key-adjusted = (keynum (+ transposition k) :through chord)
            for note_ = (new midi :time (+ time-offset (now) )
                :keynum key-adjusted
                :duration (* tempo_ rate)
                :amplitude amp
                :channel channel)
            output note_
            if (eop? key-cycle)
                set tempo_ = (next tempo-cycle)
            if (and is-master (eop? key-cycle))
                do
                    (progn
                        (output 
                            (new midi :time (+ time-offset (now) )
                                ; MIDI key represents the preset number.
                                :keynum (next preset-cycle)
                                :duration (* tempo_ rate)
                                :amplitude amp
                                :channel 4
                            )
                        )
                        (setf chord (next chord-cycle))
                        (setf transposition (next transposition-cycle)) 
                    )
            if (and is-master (eop? tempo-cycle))
                set stop-flag = 1
       wait (* tempo_ rate))
    )
)

(defun phasing (amp)
    (list 
        (voice-1 Pange-Lingua 3 amp 0 0 voice-1-transpositions voice-1-tempos t)
        (voice-1 Pange-Lingua 2 amp 1 0 voice-2-transpositions voice-2-tempos nil)
        (voice-1 Pange-Lingua 1 amp 2 0 voice-3-transpositions voice-3-tempos nil)
        (voice-1 Pange-Lingua 0 amp 3 0 voice-4-transpositions voice-4-tempos nil)
    )
)

(defparameter csound-seq (new seq :name "csound-seq"))

(events (phasing .5) csound-seq 1)

;(defparameter *piano-part* 
;  (new fomus:part
;   :name "Piano"
;   :partid 0 
;   :instr '(:piano :staves 3)))
;(defparameter partids (make-hash-table))
;(setf (gethash 1 partids) 0)
;(setf (gethash 2 partids) 0)
;(setf (gethash 3 partids) 0)
;(defparameter voices (make-hash-table))
;(setf (gethash 1 voices) '(1 2 3 4))
;(setf (gethash 2 voices) '(1 2 3 4))
;(setf (gethash 3 voices) '(1 2 3 4))
;(seq-to-lilypond csound-seq "Pange-Lingua.ly" *piano-part* partids voices)
(seq-to-midifile csound-seq "Pange-Lingua.mid")

(defparameter output "Pange-Lingua.wav")
;(defparameter output "dac")
(render-with-orc csound-seq aeolus-orc :output output :channel-offset 1 :velocity-scale 127 :csd-filename "Pange-Lingua.csd" :options "--midi-key=4 --midi-velocity=5 --0dbfs=1 -m0 -d -+msg_color=0")
(quit)



