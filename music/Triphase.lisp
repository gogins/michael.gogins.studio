(require "asdf")
(asdf:load-system :nudruz)
(in-package :cm)

(defun piano-phase (trope pulse amp stay move)
  (let* ((len (length trope))
         (dur (- (* pulse 2) .01))
         (stop (* len len (+ stay move))))
    
    ;; stop is the number of notes to play. set to the
    ;; number notes in the trope times the number of times
    ;; the shifting happens to get back the first note
    ;; (also the length of the trope) times the number of
    ;; cycles of the trope perfomer 2 stays steady plus
    ;; the number of cycles the performer takes to move the
    ;; pattern ahead one sixteenth.
    
    ;; return two processes. the first keeps a regular beat
    ;; while the second plays the trope steadily for STAY
    ;; repetitions then moves one 16th ahead over MOVE
    ;; repetitions of the trope.
    
    (list
     (process with play = (new cycle :keynums trope)
              repeat stop
              output
              (new midi :time (now) :duration dur 
                   :keynum (next play) :amplitude amp)
              wait pulse)
     
     ;; phasing tempo is represented as a ratio P/N where P is
     ;; the time the phasing takes (counted in pulses) and N is
     ;; the number of notes to play in that time. so 16/16 means
     ;; play 16 notes in the time of 16 pulses and 15/16 means
     ;; to play 16 notes in the time of 15 pulses.  for piano
     ;; phase N is the length of the trope and P is one less.
     
     (process with play = (new cycle keynums trope)
              and tempo = (new cycle 
                            of
                            (list (new cycle :of 1
                                       :for (* len stay))
                                  (new cycle 
                                    :of (/ (1- (* len move))
                                          (* len move))
                                    :for (* len move))))
              repeat stop
              output
              (new midi :time (now) :duration dur
                   :keynum (next play) :amplitude amp)
              wait (* pulse (next tempo))))))


(defparameter pnotes  '(e4 fs4 b4 cs5 d5 fs4 g4 e4 cs5 b4 fs4 d5 a4 fs4 e4 e4 e3 e3))

(defparameter transpose 8)

(defparameter seq-II (new seq :name "seq-II"))
(defparameter seq-I (new seq :name "seq-I"))
(defparameter seq-P (new seq :name "seq-P"))
;        trope                pulse   amp  move stay
(events (piano-phase pnotes   (/ 1 4) .1   4    4) seq-I)
(events (piano-phase pnotes   (/ 1 2) .8   2    2) seq-II)
(events (piano-phase pnotes   (/ 1 1) .9   1    1) seq-P)
(map-objects (lambda (x) (+ x   0 transpose)) seq-I :slot! 'keynum)
(map-objects (lambda (x) (+ x -12 transpose)) seq-II :slot! 'keynum)
(map-objects (lambda (x) (+ x -24 transpose)) seq-P :slot! 'keynum)
(map-objects (lambda (x) (+ x   2)) seq-I :slot! 'channel)
(map-objects (lambda (x) (+ x   1)) seq-II :slot! 'channel)
(map-objects (lambda (x) (+ x   0)) seq-P :slot! 'channel)

(set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc)

(defparameter aeolus-orc #>qqq>sr = 48000
ksmps = 64
nchnls = 2 
0dbfs = 1

gi_aeolus aeolus_init "stops-0.3.0", "Aeolus", "waves", 0, 5

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
aeolus_preset gi_aeolus, 1, 1, "/home/mkg/michael.gogins.studio/music/stops-0.3.0/Aeolus/presets"
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
    
(defparameter csound-seq (new seq :name "csound-seq"))
(events (list seq-II seq-I seq-P ) csound-seq)
(defparameter output "Triphase-1.wav")
(render-with-orc csound-seq aeolus-orc :output output :channel-offset 1 :velocity-scale 100)
(unless (equal output "dac")    
    (print "Post-processing...")
    ;(uiop:run-program '("python" "post-process.py" "Triphase.wav") :output t)
)
(quit)


