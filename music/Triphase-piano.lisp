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


(set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc)

(defparameter aeolus-orc #>qqq>
sr = 48000
ksmps = 64
nchnls = 2

giPianoteq init 0

alwayson "PianoOut"

gk_PianoNote_midi_dynamic_range init 127
instr PianoNote
if p3 == -1 then
  p3 = 1000000
endif
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_PianoNote_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
i_homogeneity = p11
instances active p1
prints "PianoNotePt    i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", p1, p2, p3, p4, p5, p7, instances
i_pitch_correction = 44100 / sr
; prints "Pitch factor:   %9.4f\n", i_pitch_correction
vstnote giPianoteq, i_instrument, i_midi_key, i_midi_velocity, i_duration
endin


gk_Piano_level init 0
instr PianoOut
k_gain = ampdb(gk_Piano_level)
i_overall_amps = 80
i_normalization = ampdb(-i_overall_amps) * 2
i_amplitude = ampdb(80) * i_normalization
giPianoteq vstinit "/home/mkg/Pianoteq\ 6/amd64/Pianoteq\ 6.so", 0
; Should be "D4 Daily Practice".
vstprogset giPianoteq, 0
; Sustain off.
vstparamset giPianoteq, 0, 0
; Reverb off.
vstparamset giPianoteq, 72, 0
vstinfo giPianoteq
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
ainleft init 0.0
ainright init 0.0
aoutleft, aoutright vstaudiog giPianoteq, ainleft, ainright
; printks "vstaudiog:       %9.4f   %9.4f\n", 0.5, aoutleft, aoutright
#ifdef USE_SPATIALIZATION
a_signal = a_out_left + a_out_right
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, gk_Piano_front_to_back, gk_Piano_left_to_right, gk_Piano_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
a_out_left = aoutleft * k_gain * i_amplitude
a_out_right = aoutright * k_gain * i_amplitude
; printks "PianoOutPt     L %9.4f R %9.4f l %9.4f\\n", 0.5, a_out_left, a_out_right, gk_Piano_level
;outleta "outleft", a_out_left
;outleta "outright", a_out_right
#endif
prints "PianoOutPt     i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", p1, p2, p3, p4, p5, p7, active(p1)
outs a_out_left, a_out_right
endin

qqq)
    
;(defparameter pnotes  '(e4 fs4 b4 cs5 d5 fs4 g4 e4 cs5 b4 fs4 d5 a4 fs4 e4 e4 e3 e3))
;(defparameter pnotes  '(e4 fs4 b4 cs5 d5 fs4 d g4 e4 cs5 b4 fs4 d5 a4 fs4 e4 e4 e3 e3))
(defparameter pnotes  '(e4 fs4 b4 cs5 d5 fs4 a4 a4 cs5 b4 fs4 d5 a4 fs4 e4 e4 e3 e3))

(defparameter transpose 8)

(defparameter seq-II (new seq :name "seq-II"))
(defparameter seq-I  (new seq :name "seq-I"))
(defparameter seq-P  (new seq :name "seq-P"))
;        trope                pulse   amp  move stay
(events (piano-phase pnotes   (/ 1 4) .3   4    4) seq-I)
(events (piano-phase pnotes   (/ 1 2) .3   2    2) seq-II)
(events (piano-phase pnotes   (/ 1 1) .5   1    1) seq-P)
(map-objects (lambda (x) (+ x   0 transpose)) seq-I :slot! 'keynum)
(map-objects (lambda (x) (+ x -12 transpose)) seq-II :slot! 'keynum)
(map-objects (lambda (x) (+ x -24 transpose)) seq-P :slot! 'keynum)

(defparameter csound-seq (new seq :name "csound-seq"))
(events (list seq-II seq-I seq-P ) csound-seq 1)
(defparameter output "Triphase-piano.wav")
(render-with-orc csound-seq aeolus-orc :output output :channel-offset 1 :velocity-scale 100)
(unless (equal output "dac")    
    (print "Post-processing...")
    ;(uiop:run-program '("python" "post-process.py" "Triphase-piano.wav") :output t)
)
(quit)



