(require :asdf)
(asdf:load-system :nudruz)
(load "/home/mkg/csound-extended/nudruz/sources/all-in-one-orc.lisp")
(in-package :cm)

(set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc)

(defparameter CM9     (new mode :degrees   '(c  d  e     g     b  c)))
(defparameter Cm9     (new mode :degrees   '(c  d  ef    g     bf c)))
(defparameter C7s9f5  (new mode :degrees   '(c  ds e     gf    bf c)))

(defparameter progression (new cycle :of (list CM9 (transpose Cm9 2) CM9 (transpose Cm9 2) (transpose C7s9f5 7))))

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
                               
(defun play-pat (reps pat amp tempo transp offset channel_)
    (let 
        (
            (rhy (new cycle :of '(e s s e e s)))
            (chord (next progression))
        )
        (process 
            repeat reps
            for r = (rhythm (next rhy) tempo)
            each k in (next pat)
            output (new midi :time (+ (now) offset)
                :keynum (+ (keynum k :through chord) transp)
                :amplitude amp
                :channel channel_
                :duration (* r .975))
            when (eop? pat) set chord = (next progression)
            wait r
        )
    )
)

(defparameter csound-seq (new seq :name "csound-seq"))

(events (list (play-pat 300 pat1 .9 25 0 0 23) (play-pat 300 pat2 .8 24 5 0 57)) csound-seq 1)

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
;(seq-to-lilypond csound-seq "Triphase-Piano-2.ly" *piano-part* partids voices)
(seq-to-midifile csound-seq "Triphase-Piano-2.mid")

;(defparameter output "dac")
(defparameter output "Triphase-Piano-2.wav")
(render-with-orc csound-seq orc-vst :output output :channel-offset 0 :velocity-scale 90 :csd-filename "Triphase-Piano-2.csd")
(unless (equal output "dac")    
    (print "Post-processing...")
    (uiop:run-program '("python" "../post-process.py" "Triphase-Piano-1.wav") :output t)
)
(quit)



