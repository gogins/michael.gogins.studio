#|
PANGUE LINGUA SHUFFLED
Michael Gogins
5 January 2019

Been trying to make something out of this jewel for decades!

To do:

(1) Double and triple time in the middle in some voices. 
(2) Changes of stops using special instrument.
(3) Changes of range/transposition in the middle.
(4) End roundly if possible, otherwise by ear.
(5) Better harmony if possible.
(6) Hocketting?
(7) The exactly correct tempo and duration and reverb.
|#

(load "~/quicklisp/setup.lisp")
(ql:quickload "nudruz")
(load "/home/mkg/csound-extended/nudruz/sources/all-in-one-orc.lisp")
(in-package :cm)

(set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc)

(defparameter pitch-class-set-1  (new mode :degrees   '(c  d  e  f  fs g  a  b  c)))
(defparameter pitch-class-set-2  (new mode :degrees   '(c  d  ef       g  af bf c)))
(defparameter pitch-class-set-3  (new mode :degrees   '(c  ds e        gf    bf c)))

(defparameter voice-1-tempos    '(2   2   4       4       4       4       2   4     2   2))
(defparameter voice-2-tempos    '(2   2   4       3     3     3     3     2   4     2   2))
(defparameter voice-3-tempos    '(2   2   2   2   2   2   2   2   2   2   1 1 2  2  1 1 2))
(defparameter voice-4-tempos    '(2   2   2   2   1 1 1 1 1 1 1 1 1 1 1 1 1 1 2  2  1 1 2))

(defparameter voice-1-transpositions    '(-12 -24 -24 -24   0 -12 -24))
(defparameter voice-2-transpositions    '(-12 -12   4   4   0   0 -12))
(defparameter voice-3-transpositions    '(-12   0   9  21   0   9   0))
(defparameter voice-4-transpositions    '(-12   0  12  24   0  28  12))


(defparameter progression (new cycle :of 
    (list pitch-class-set-1 
    (list pitch-class-set-1 (transpose pitch-class-set-1 2) pitch-class-set-1 (transpose pitch-class-set-1 2) 
    (transpose pitch-class-set-1 7)))))

(defparameter pangue
    (keynum '(e4 e e d g g a c5 c c c c c d c c c a4 c5 b4 a g g g g g a c5 b4 a g a a a a a a b g fs e a a a a a d d d d g g g e g a a g g g g a b g a g f d e e e e)))
  
(format t "Length of pangue ~a~%" (length pangue))
(defparameter rhythms
    '(1 1 1 1 1 1 1 1))
    ;'(1 1 1 1 2 1 1 1 1 2 1 1 1 1 4 1 1 1 1 4 1 1 1 1))
    ;'(3 1 2 1 6 1 3 6 1 3 1 1 1 1 5 1 1 1 1 1 6 1 1 1 1 5 4 3 1 1 2 4 1 1 1 1 1 1 4))
    
(defparameter pp-pulse 1/24)

(defparameter pp-tempo 50.0)

(defun bpm->seconds (bpm)
  (/ 60.0 bpm))

(defun rhythm->seconds (rhy tempo)
  (* rhy 4.0 (bpm->seconds tempo)))
  
(defparameter pulses 0)
(defparameter total-pulses (* 4 72))

(defun voice-1 (pangue key-cycle-pop-tail amp channel time-offset transpositions tempos)
    (let* 
        (
            (duration-seconds 360.0)
            (key-cycle (new cycle :keynums (subseq pangue 0 (- (length pangue) key-cycle-pop-tail))))
            (rate (rhythm->seconds pp-pulse pp-tempo))
            (wait-factors (new cycle :of (subseq rhythms 0 (- (length rhythms) key-cycle-pop-tail))))
            (tempo-cycle (new cycle :of tempos))
            (tempo_ (next tempo-cycle))
            (chord (next progression))
            (transposition-cycle (new cycle :of transpositions))
            (transposition (next transposition-cycle))
        )
        (process 
            for now_ = (now)
            ;until (> now_ duration-seconds)
            until (eop? tempo-cycle)
            for progression-trigger = (rem (incf pulses) total-pulses)
            for chord = (if (equal progression-trigger 0) 
                (next progression)
                chord)
            for k = (next key-cycle)
            for tempo_ = (if (eop? key-cycle) 
                (next tempo-cycle) 
                tempo_)
            for transposition = (if (eop? key-cycle) 
                (next transposition-cycle) 
                transposition)
            for key-adjusted = (keynum (+ transposition k) :through chord)
            for note_ = (new midi :time (+ time-offset (now) )
                :keynum key-adjusted
                :duration (*  (* (next wait-factors)) (* tempo_ rate))
                :amplitude amp
                :channel channel)
            do (format t "Pulse: ~a Duration: ~a Chord: ~a Note: ~a~%" pulses tempo_ progression-trigger (keynum chord) note_)
            output note_
        wait (* (pattern-value wait-factors) (* tempo_ rate)))
    )
)

(defun phasing (amp transpose)
    (list 
        (voice-1 pangue 0 amp 3 0 voice-1-transpositions voice-1-tempos)
        (voice-1 pangue 1 amp 1 0 voice-2-transpositions voice-2-tempos)
        (voice-1 pangue 2 amp 2 0 voice-3-transpositions voice-3-tempos)
        (voice-1 pangue 3 amp 1 0 voice-4-transpositions voice-4-tempos)
    )
)

(defparameter csound-seq (new seq :name "csound-seq"))

(events (phasing .75 -1) csound-seq 1)

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



