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


(defparameter pnotes   '(e4 fs4 b4 cs5 d5 fs4 g4 e4 cs5 b4 fs4 d5 cs5 c4 cs4 cs4 cs3 cs3))

(defparameter seq-1 (new seq :name "seq-1"))
(defparameter seq-2 (new seq :name "seq-2"))
(defparameter seq-3 (new seq :name "seq-3"))
;        trope                pulse   amp  move stay
(events (piano-phase pnotes   (/ 1 4) .5   8    8) seq-1)
(events (piano-phase pnotes   (/ 1 2) .5   4    4) seq-2)
(events (piano-phase pnotes   (/ 1 1) .7   2    2) seq-3)
(map-objects (lambda (k) (+ k  12)) seq-1 :slot! 'keynum)
(map-objects (lambda (k) (+ k   0)) seq-2 :slot! 'keynum)
(map-objects (lambda (k) (+ k -12)) seq-3 :slot! 'keynum)
(map-objects (lambda (k) (+ k   0)) seq-1 :slot! 'channel)
(map-objects (lambda (k) (+ k   0)) seq-2 :slot! 'channel)
(map-objects (lambda (k) (+ k   1)) seq-3 :slot! 'channel)

(set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc)

(defparameter aeolus-orc #>qqq>sr = 48000
ksmps = 64
nchnls = 2 
0dbfs = 1

gi_aeolus aeolus_init "/home/mkg/stops-0.3.0", "Aeolus", "waves", 0, 3

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
aeolus_preset gi_aeolus, 1, 1, "~/.aeolus-presets"
a_out[] init 2
a_out aeolus_out gi_aeolus
out a_out
endin                                
    qqq)
    
(defparameter csound-seq (new seq :name "csound-seq"))
(events (list seq-1 seq-2 seq-3) csound-seq)
(render-with-orc csound-seq aeolus-orc :output "Triphase.wav" :channel-offset 1 :velocity-scale 100)
(uiop:run-program '("python" "./post-process.py" "Triphase.wav"))
(quit)



