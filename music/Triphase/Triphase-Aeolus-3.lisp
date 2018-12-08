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
                               
(defparameter trope
  (keynum '(e4 fs b cs5 d fs4 e cs5 b4 fs d5 cs5 cs4 cs4)))

(defparameter pp-pulse 1/24)

(defparameter pp-tempo 50)

(defun bpm->seconds (bpm)
  (/ 60.0 bpm))

(defun rhythm->seconds (rhy tempo)
  (* rhy 4.0 (bpm->seconds tempo)))

(defun piano1 (trope amp chan)
  (let* ((tlen (length trope))
         (cycs tlen)
         (rate (rhythm->seconds pp-pulse
                                pp-tempo)))
    (process repeat (* tlen cycs)
             for i from 0
             for x = (mod i tlen)
             for k = (nth x trope)
             output (new midi :time (now)
                         :keynum k
                         :duration (* rate .75)
                         :amplitude amp
                         :channel chan)
             wait rate)))

                               (defun pphase (trope amp)
  (list (piano1 trope amp 0)
        (piano1 trope amp 1)))

(defparameter csound-seq (new seq :name "csound-seq"))

;(events (list (play-pat 300 pat1 .9 25 0 0 23) (play-pat 300 pat2 .8 24 5 0 57)) csound-seq 1)
(events (pphase trope .75) csound-seq 1)

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
;(seq-to-lilypond csound-seq "Triphase-Piano-3.ly" *piano-part* partids voices)
(seq-to-midifile csound-seq "Triphase-Piano-3.mid")

(defparameter output "dac")
;(defparameter output "Triphase-Piano-3.wav")
(render-with-orc csound-seq all-in-one-orc :output output :channel-offset 1 :velocity-scale 1 :csd-filename "Triphase-Piano-3.csd" :options "--midi-key=4 --midi-velocity=5 --0dbfs=10000 -m195")
(quit)



