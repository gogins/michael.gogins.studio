#|
PANGUE LINGUA SHUFFLED
Michael Gogins
5 January 2019

Been trying to make something out of this jewel for decades!

To do:

--  Double and triple time in the middle in some voices. OK,
--  Changes of stops using special instrument.
--  Changes of range/transposition in the middle. Needs work. Note that 8vo preserves tune, others vary it.
--  End roundly if possible, otherwise by ear. OK.
--  Better harmony if possible. Not OK. Next chord by time (modulus of now), or by next tempo in one voice.
--  Hocketting?
--  The exactly correct tempo and duration and reverb. Needs a little work.
|#

(load "~/quicklisp/setup.lisp")
(ql:quickload "nudruz")
(load "/home/mkg/michael.gogins.studio/music/all-in-one-orc.lisp")
(in-package :cm)

(set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc)

; Each beat of "Pangue Lingua" in Mode 3.

(defparameter Pangue-Lingua
    (keynum '(e4 e e d g g a c5 c c c c c d c c c a4 c5 b4 a g g g g g a c5 b4 a g a a a a a a b g fs e a a a a a d d d d g g g e g a a g g g g a b g a g f d e e e e e e e)))
  
(defparameter pitch-class-set-1  (new mode :degrees   '(c  d  e  f     g  a  b  c)))
(defparameter pitch-class-set-2  (new mode :degrees   '(c  d  ef       g  af bf c)))
(defparameter pitch-class-set-3  (new mode :degrees   '(c  ds e        gf    bf c)))

;; Nine repetitions of the tune in the bass.
;; The bass, voice 1, is the master voice that sequences the others at each repetition.

;; Nine repetitions...                      1   2   3       4       5       6       7       8   9

(defparameter voice-1-tempos            '(  2   2   4       4       4       4       4       2   2))
(defparameter voice-2-tempos            '(  2   2   4       3     3     3     3     4       2   2))
(defparameter voice-3-tempos            '(  2   2   2   2   2   2   1 1 1 1 2   2   2   2   2   2))
(defparameter voice-4-tempos            '(  2   2   2   2   1 1 1 1 1 1 1 1 1 1 1 1 2   2   2   2))

(defparameter voice-1-transpositions    '(-12 -12 -12     -24     -24     -24     -24     -24 -24)) 
(defparameter voice-2-transpositions    '(  4  -3  -3      -3       4      -3      -3      -3  -3))
(defparameter voice-3-transpositions    '(  9   4   4       6       9       4       4       4   4))
(defparameter voice-4-transpositions    '( 12   2   4       6       9      12      12      12  12))


(defparameter chord-cycle 
    (new cycle :of 
        (list 
            ; Out of order because of new cycles created in (*let.
            (transpose pitch-class-set-3  7) ; 7
            (transpose pitch-class-set-3  7) ; 8
            (transpose pitch-class-set-1  0) ; 9
            (transpose pitch-class-set-1  0) ; 1
            (transpose pitch-class-set-1  0) ; 2
            (transpose pitch-class-set-1  5) ; 3
            (transpose pitch-class-set-1  5) ; 4
            (transpose pitch-class-set-2 10) ; 5
            (transpose pitch-class-set-2 10) ; 6
        )
    )
)

(format t "Length of 'Pangue Lingua:' ~a~%" (length Pangue-Lingua))

(defparameter pp-pulse 1/24)

(defparameter pp-tempo 48.0)

(defun bpm->seconds (bpm)
  (/ 60.0 bpm))

(defun rhythm->seconds (rhy tempo)
  (* rhy 4.0 (bpm->seconds tempo)))
(defun voice-1 (Pangue-Lingua key-cycle-pop-tail amp channel time-offset transpositions tempos is-master)
    (let* 
        (
            (duration-seconds 360.0)
            (rate (rhythm->seconds pp-pulse pp-tempo))
            (key-cycle (new cycle :keynums (subseq Pangue-Lingua 0 (- (length Pangue-Lingua) key-cycle-pop-tail))))
            (tempo-cycle (new cycle :of tempos))
            (tempo_ (next tempo-cycle))
            (transposition-cycle (new cycle :of transpositions))
            (transposition (next transposition-cycle))
            (chord (next chord-cycle))
        )
        (process 
            for now_ = (now)
            until (eop? tempo-cycle)
            for k = (next key-cycle)
            for chord = (if (and is-master (eop? key-cycle))
                (next chord-cycle)
                (pattern-value chord-cycle))
            for tempo_ = (if (eop? key-cycle)
                (next tempo-cycle) 
                (pattern-value tempo-cycle))
            for transposition = (if (and is-master (eop? key-cycle))
                (next transposition-cycle) 
                (pattern-value transposition-cycle))
            for key-adjusted = (keynum (+ transposition k) :through chord)
            for note_ = (new midi :time (+ time-offset (now) )
                :keynum key-adjusted
                :duration ( * tempo_ rate)
                :amplitude amp
                :channel channel)
            output note_
        wait (* tempo_ rate))
    )
)

(defun phasing (amp)
    (list 
        (voice-1 Pangue-Lingua 0 amp 2 0 voice-1-transpositions voice-1-tempos t)
        (voice-1 Pangue-Lingua 3 amp 2 0 voice-2-transpositions voice-2-tempos nil)
        (voice-1 Pangue-Lingua 2 amp 2 0 voice-3-transpositions voice-3-tempos nil)
        (voice-1 Pangue-Lingua 1 amp 2 0 voice-4-transpositions voice-4-tempos nil)
    )
)

(defparameter csound-seq (new seq :name "csound-seq"))

(events (phasing .75) csound-seq 1)

(defparameter *piano-part* 
  (new fomus:part
   :name "Piano"
   :partid 0 
   :instr '(:piano :staves 3)))
(defparameter partids (make-hash-table))
(setf (gethash 1 partids) 0)
(setf (gethash 2 partids) 0)
(setf (gethash 3 partids) 0)
(defparameter voices (make-hash-table))
(setf (gethash 1 voices) '(1 2 3 4))
(setf (gethash 2 voices) '(1 2 3 4))
(setf (gethash 3 voices) '(1 2 3 4))
;(seq-to-lilypond csound-seq "Triphase-Aeolus-3.ly" *piano-part* partids voices)
(seq-to-midifile csound-seq "Triphase-Aeolus-3.mid")

(defparameter output "Triphase-Aeolus-3.wav")
(defparameter output "dac")
(render-with-orc csound-seq all-in-one-orc :output output :channel-offset 1 :velocity-scale 127 :csd-filename "Triphase-Aeolus-3.csd" :options "--midi-key=4 --midi-velocity=5 --0dbfs=1 -m195 -+msg_color=0")
(quit)



