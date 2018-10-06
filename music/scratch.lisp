(require "asdf")
(asdf:load-system :nudruz)
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
    
(defparameter csound-seq (new seq :name "csound-seq"))
 
(events
 (let ((mypits (heapvec 100 10 50))
       (durs (strums 20 2.5 6 4 6)))
   (list
    (splay mypits (ferney '(1) '(4) durs))
    (splay (transp mypits 3) (ferney '(1) '(5) durs))
    (splay (transp mypits 21) (ferney '(1) '(9) durs)))) 
 csound-seq '(0 2 7) :play 'nil)

(defparameter output "scratch.wav")
(render-with-orc csound-seq aeolus-orc :output output :channel-offset 1 :velocity-scale 100)
(quit)



