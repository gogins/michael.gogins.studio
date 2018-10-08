(require :asdf)
(require :cm2)
(require :nudruz)
(require :fomus)
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
aeolus_preset gi_aeolus, 1, 1, "/home/mkg/.aeolus-presets"
a_out[] init 2
a_out aeolus_out gi_aeolus
out a_out
endin                                
    qqq)
    
(defparameter csound-seq (new seq :name "csound-test"))
 
(events
 (let ((mypits (heapvec 100 10 50))
       (durs (strums 20 2.5 6 4 6)))
   (list
    (splay mypits (ferney '(1) '(4) durs))
    (splay (transp mypits 3) (ferney '(1) '(5) durs))
    (splay (transp mypits 21) (ferney '(1) '(9) durs))))
    csound-seq 1)
    
(defparameter *piano-part* 
  (new fomus:part
   :name "Piano"
   :instr :piano :partid 0))
(defparameter partids (make-hash-table))
(setf (gethash 0 partids) 0)
(defparameter voices (make-hash-table))
(setf (gethash 0 voices) (list 1 2 3 4))
(seq-to-lilypond csound-seq "tzplay.ly" *piano-part* partids voices)
;(render-with-orc csound-seq orc-text (seq-to-midifile csound-seq "Scratch.mid")

(defparameter output "Scratch.wav")
;(render-with-orc csound-seq aeolus-orc :output output :channel-offset 1 :velocity-scale 100)
(quit)



