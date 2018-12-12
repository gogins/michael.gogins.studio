(require :asdf)
(asdf:load-system :nudruz)
(load "/home/mkg/csound-extended/nudruz/sources/all-in-one-orc.lisp")
(in-package :cm)

(set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc)

(defparameter CM9     (new mode :degrees   '(c     e     g     b  c)))
(defparameter Cm9     (new mode :degrees   '(c  d  ef  f g     bf c)))
(defparameter C7s9f5  (new mode :degrees   '(c  df e     gs    bf c)))

(defparameter progression (new cycle :of (list CM9 (transpose Cm9 2) CM9 (transpose Cm9 2) (transpose C7s9f5 11))))

(defparameter pat1
  (new cycle 
       :of (list (new chord 
                      :of (new rotation :notes '(c3 d ef f g af bf c6 ef3)
                               :for (new weighting :of '(9 5))))
                 (new chord
                      :of (new rotation :notes '(c2 d6 g5 f5 g4 af bf c5 c3 c3)
                               :for (new weighting :of '(4 9))))
                 (new chord
                      :of (new rotation :notes '(c3 d ef f g af2 bf3 c)
                               :for (new weighting :of '(7 5)))))))

(defparameter pat2
  (new cycle 
       :of (list (new chord 
                      :of (new palindrome :notes '(c3 d ef f  bf c6 ef3)
                               :for (new weighting :of '(9 7))))
                 (new chord
                      :of (new palindrome :notes '(c2 f5 g4 af bf d d c5 c3 c3)
                               :for (new weighting :of '(4 5))))
                 (new chord
                      :of (new palindrome :notes '(c3 d ef f g af2 bf3 c)
                               :for (new weighting :of '(3 5)))))))
                               
(defparameter motive
  (keynum '(e4 fs b cs5 d fs4 e b5 cs5 b4 fs d5 cs5 cs4 e4 fs cs4 e4 fs b cs5 d fs4 e cs5 d e b4 fs d5 cs5 cs4 cs3 cs3)))

(defparameter pp-pulse 1/24)

(defparameter pp-tempo 50)

(defun bpm->seconds (bpm)
  (/ 60.0 bpm))

(defun rhythm->seconds (rhy tempo)
  (* rhy 4.0 (bpm->seconds tempo)))
  
(defparameter pulses 0)

(defparameter chord (next progression))

(defun voice-1 (motive key-cycle-pop-tail amp channel time-offset key-offset extra-wait)
    (let* 
        (
            (key-cycle (new cycle :keynums (subseq motive 0 (- (length motive) key-cycle-pop-tail)) :repeat 32))
            (rate (rhythm->seconds pp-pulse pp-tempo))
            (wait-factors (new cycle :of '(1 1 1 1 2 1 1 1 1 2 1 1 1 1 4)))
        )
        (process 
            for k = (next key-cycle)
            until (eod? k)
            for progression-trigger = (rem (incf pulses) 21)
            for chord = (if (equal progression-trigger 0) 
                (next progression)
                chord)
            for note_ = (new midi :time (+ time-offset (now) )
                :keynum (keynum (+ key-offset k) :through chord)
                :duration (* (* extra-wait rate) (*  (next wait-factors)))
                :amplitude amp
                :channel channel)
            do (format t "Pulse: ~a ~a Chord: ~a Note: ~a~%" pulses progression-trigger (keynum chord) note_)
            output note_
        wait (* (pattern-value wait-factors) (* extra-wait rate)))
    )
)

(defun pphase (amp)
    (list 
        (voice-1 motive 0 amp 2   0 -24 4)
        (voice-1 motive 1 amp 1   1   0 3)
        (voice-1 motive 2 amp 0   2   5 2)
        (voice-1 motive 3 amp 1   3   8 1)
    )
)

(defparameter csound-seq (new seq :name "csound-seq"))

(events (pphase .75) csound-seq 1)

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

(defparameter output "dac")
(render-with-orc csound-seq all-in-one-orc :output output :channel-offset 1 :velocity-scale 127 :csd-filename "Triphase-Aeolus-3.csd" :options "--midi-key=4 --midi-velocity=5 --0dbfs=1 -m195 -+msg_color=0")
(quit)



