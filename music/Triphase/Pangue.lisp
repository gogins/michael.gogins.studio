(load "~/quicklisp/setup.lisp")
(ql:quickload "nudruz")
(load "/home/mkg/csound-extended/nudruz/sources/all-in-one-orc.lisp")
(in-package :cm)

(set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc)

(defparameter chord-1  (new mode :degrees   '(c  d  e   f  g  a  b  c)))
(defparameter chord-2  (new mode :degrees   '(c  d  ef    g  af  bf c)))
(defparameter chord-3  (new mode :degrees   '(c  ds e     gf    bf c)))

(defparameter progression (new cycle :of (list chord-1 (transpose chord-1 2) chord-2 (transpose chord-2 2) (transpose chord-1 3))))

(defparameter pangue
    (keynum '(e4 e e d g g a c5 c c c c c d c c c a4 c5 b4 a g g g g g a c5 b4 a g a a a a a a b g fs e a a a a a d d d d g g g e g a a g g g g a b g a g f d e e e e)))
    ;(keynum '(e4 d g a c5 d c a4 c5 b4 a g a c5 b4 a g a b g fs e a d g e g a g a b g a g f d e)))
  
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

(defparameter chord (next progression))

(defun voice-1 (pangue key-cycle-pop-tail amp channel time-offset key-offset extra-wait)
    (let* 
        (
            (key-cycle (new cycle :keynums (subseq pangue 0 (- (length pangue) key-cycle-pop-tail))))
            (rate (rhythm->seconds pp-pulse pp-tempo))
            (wait-factors (new cycle :of (subseq rhythms 0 (- (length rhythms) key-cycle-pop-tail))))
            (duration-seconds 360.0)
        )
        (process 
            for now_ = (now)
            until (> now_ duration-seconds)
            for progression-trigger = (rem (incf pulses) 72)
            for chord = (if (equal progression-trigger 0) 
                (next progression)
                chord)
            for k = (next key-cycle)
            for k-adjusted = (keynum (+ key-offset k) :through chord)
            for note_ = (new midi :time (+ time-offset (now) )
                :keynum k-adjusted
                :duration (* (* extra-wait rate) (* (next wait-factors)))
                :amplitude amp
                :channel channel)
            do (format t "Pulse: ~a ~a Chord: ~a Note: ~a~%" pulses progression-trigger (keynum chord) note_)
            output note_
        wait (* (pattern-value wait-factors) (* extra-wait rate)))
    )
)

(defun phasing (amp transpose)
    (list 
        ;voice-1 pangue 0 amp 0 0 (+ transpose -12) 4)
        ;voice-1 pangue 1 amp 1 1 (+ transpose   0) 3)
        ;voice-1 pangue 2 amp 2 2 (+ transpose   5) 2)
        ;voice-1 pangue 3 amp 1 3 (+ transpose   9) 1)
        (voice-1 pangue 0 amp 3 0 (+ transpose -12) 2)
        (voice-1 pangue 1 amp 1 0 (+ transpose   0) 2)
        (voice-1 pangue 2 amp 2 0 (+ transpose   9) 2)
        (voice-1 pangue 3 amp 1 0 (+ transpose  16) 2)
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



