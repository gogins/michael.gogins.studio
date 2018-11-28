(require :asdf)
(require :nudruz)
(in-package :cm)

(set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc)

(defparameter aeolus-orc #>qqq>
sr = 44100
ksmps = 64
nchnls = 2

giPianoteq init 0

alwayson "PianoOut"

gk_Rhodes_level init 0
gi_Rhodes_sine ftgen 0, 0, 65536, 10, 1
gi_Rhodes_cosine ftgen 0, 0, 65536, 11, 1
gi_Rhodes_blank ftgen 0, 0, 65536, 10, 0 ; Blank wavetable for some Cook FM opcodes.
instr Rhodes
; Authors: Perry Cook, John ffitch, Michael Gogins
if p3 != -1 goto non_indefinite
    p3 = 10000000
non_indefinite:
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
k_space_front_to_back = p6
k_space_left_to_right = .5
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_overall_amps = 82
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Rhodes_level)
iindex = 4
icrossfade = 3
ivibedepth = 0.2
iviberate = 6
ifn1 = gi_Rhodes_sine
ifn2 = gi_Rhodes_cosine
ifn3 = gi_Rhodes_sine
ifn4 = gi_Rhodes_blank
ivibefn = gi_Rhodes_sine
a_signal fmrhode i_amplitude, i_frequency, iindex, icrossfade, ivibedepth, iviberate, ifn1, ifn2, ifn3, ifn4, ivibefn
i_attack = .002
i_sustain = p3
i_release = 0.01
xtratim i_attack + i_sustain + i_release
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_signal * a_declicking * k_gain
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
prints "Rhodes         i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p7, active(p1)
outs a_out_left, a_out_right
endin

gk_PianoNote_midi_dynamic_range init 127
instr 1,2,3,4
if p3 != -1 goto non_indefinite
    p3 = 10000000
non_indefinite:
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
giPianoteq vstinit "/home/mkg/pianoteq_linux_v630/Pianoteq\ 6/amd64/Pianoteq\ 6.so", 0
; Should be "D4 Daily Practice".
vstprogset giPianoteq, 0
; Sustain off.
vstparamset giPianoteq, 0, 0
; Reverb off.
;vstparamset giPianoteq, 72, 0
vstinfo giPianoteq
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
ainleft init 0.0
ainright init 0.0
aoutleft, aoutright vstaudio giPianoteq, ainleft, ainright
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
i_amplitude_adjustment = ampdbfs(-3) / 32767
fout "Triphase-Piano-2.wav", 18, a_out_left * i_amplitude_adjustment, a_out_right * i_amplitude_adjustment
endin

qqq)

(defparameter seq-II (new seq :name "seq-II"))
(defparameter seq-I  (new seq :name "seq-I"))
(defparameter seq-P  (new seq :name "seq-P"))
;                    trope    pulse   amp  move stay
;(events (piano-phase trope   (/ 1.75 4) .4   4    4) seq-I)
;(events (piano-phase trope   (/ 1.75 2) .4   2    2) seq-II)
;(events (piano-phase trope   (/ 1.75 1) .5   1    1) seq-P)
(map-objects (lambda (x) (+ x   0 transpose)) seq-I  :slot! 'keynum)
(map-objects (lambda (x) (+ x  -5 transpose)) seq-II :slot! 'keynum)
(map-objects (lambda (x) (+ x -24 transpose)) seq-P  :slot! 'keynum)
(map-objects (lambda (x) 1) seq-I  :slot! 'channel)
(map-objects (lambda (x) 2) seq-II :slot! 'channel)
(map-objects (lambda (x) 3) seq-P  :slot! 'channel)

(defparameter pat1
  (new cycle 
       :of (list (new chord 
                      :of (new rotation :notes '(c3 d ef f g af bf c6 ef3) 
                               :for (new weighting :of '(9 5))))
                 (new chord
                      :of (new rotation :notes '(c2 d6 g5 f5 g4 af bf c5 c3 c3) 
                               :for (new weighting :of '(4 9))))
                 (new chord
                      :of (new rotation :notes '(c3 d ef f g af bf c) 
                               :for (new weighting :of '(7 5)))))))

(defun play-pat (reps pat amp tempo)
  (let ((rhy (new cycle :of '(e s s e e s))))
    (process repeat reps
             for r = (rhythm (next rhy) tempo)
             each k in (next pat)
             output (new midi :time (now)
                         :keynum k
                         :amplitude amp
                         :duration (* r .975))
             wait r)))

(defparameter csound-seq (new seq :name "csound-seq"))


(events (play-pat 140 pat1 .3 225) csound-seq 1)


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

(defparameter output "dac")
;(defparameter output "Triphase-Piano-2.wav")
(render-with-orc csound-seq aeolus-orc :output output :channel-offset 1 :velocity-scale 100)
; (unless (equal output "dacx")    
    ; (print "Post-processing...")
    ; (uiop:run-program '("python" "../post-process.py" "Triphase-Piano-1.wav") :output t)
; )
; (quit)



