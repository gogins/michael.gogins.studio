(require :asdf)
(require :fomus)
(require :nudruz)
(in-package :cm)

(set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc)

(defparameter aeolus-orc #>qqq>sr = 48000
ksmps = 64
nchnls = 2 
0dbfs = 1

gi_aeolus aeolus_init "stops-0.3.0", "Aeolus", "waves", 0, 3

instr 1 
print p1, p2, p3, p4, p5
aeolus_note gi_aeolus, p1, p4, p5
endin

instr 2 
print p1, p2, p3, p4, p5
aeolus_note gi_aeolus, p1, p4, p5
endin

instr 3 
print p1, p2, p3, p4, p5
aeolus_note gi_aeolus, p1, p4, p5
endin

instr 4 
print p1, p2, p3, p4, p5
aeolus_note gi_aeolus, p1, p4, p5
endin

instr 5
print p1, p2, p3, p4, p5
aeolus_note gi_aeolus, p1, p4, p5
endin

instr 6
print p1, p2, p3, p4, p5
aeolus_note gi_aeolus, p1, p4, p5
endin

alwayson "aeolus_out"

; Send audio from the Aeolus to the output.
instr aeolus_out 
print p1, p2, p3
aeolus_preset gi_aeolus, 1, 1, ".aeolus-presets"
;aeolus_group_mode gi_aeolus, 0, 2
;aeolus_group_mode gi_aeolus, 1, 2
;aeolus_group_mode gi_aeolus, 2, 2
;aeolus_group_mode gi_aeolus, 3, 2
;aeolus_stop gi_aeolus, 20
;aeolus_stop gi_aeolus, 23
;aeolus_stop gi_aeolus, 33
;aeolus_stop gi_aeolus, 38
;aeolus_stop gi_aeolus, 41
;aeolus_stop gi_aeolus, 46
;aeolus_stop gi_aeolus, 51
;aeolus_stop gi_aeolus, 52
a_out[] init 2
a_out aeolus_out gi_aeolus
out a_out
endin                                
    qqq)
    
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

(events (pphase trope .75) csound-seq 11)

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
(render-with-orc csound-seq aeolus-orc :output output :channel-offset 1 :velocity-scale 1 :csd-filename "Triphase-Aeolus-3.csd" :options "--midi-key=4 --midi-velocity=5 --0dbfs=.1 -m195")
(quit)




