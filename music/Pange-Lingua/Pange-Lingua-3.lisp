(require :asdf)
(load "~/quicklisp/setup.lisp")
(ql:quickload "clmath")
(ql:quickload "cm2")
(ql:quickload :nudruz)
(in-package :cm)

(defparameter aeolus-csd #>qqq>
<CsoundSynthesizer>
<CsOptions>
-m195 --displays --midi-key=4 --midi-velocity=5 -oPange-Lingua-3.wav
</CsOptions>
<CsInstruments>
sr = 96000
ksmps = 128
nchnls = 2
0dbfs = 1

alwayson "aeolus_output"

gi_aeolus aeolus_init "stops-0.3.0", "Aeolus", "waves", 0, 15, 0

instr aeolus_note, 1, 2, 3, 4
prints "Aeolus note: i: %9.4f time: %9.4f duration: %9.4f key: %9.4f velocity: %9.4f\n", p1, p2, p3, p4, p5
aeolus_note gi_aeolus, p1, p4, p5
endin

instr aeolus_preset
prints "Aeolus preset: i: %9.4f time: %9.4f duration: %9.4f bank:%9.4f preset:%9.4f\n", p1, p2, p3, p4, p5
aeolus_preset gi_aeolus, p4, p5
endin

instr aeolus_output
aeolus_preset gi_aeolus, 0, 0
a_out[] init 2
a_out aeolus_out gi_aeolus
// Use reverbsc instead of the built-in Aeolus reverb.
i_reverb_feedback init .80
i_reverb_highpass init 9000
i_wet init .33334
i_dry init 1 - i_wet
a_left, a_right reverbsc a_out[0], a_out[1], i_reverb_feedback, i_reverb_highpass
outs a_out[0] * i_dry + a_left * i_wet, a_out[1] * i_dry + a_right * i_wet
endin

</CsInstruments>
<CsScore>
; Original tempo in the MIDI file -- seems too fast to me.
; t 0 119
; Use to correlate beats with seconds for finding sections in the score editor or soundfile sonogram view.
t 0 60
; Actual rendering tempo.
; t 0 103
</CsScore>
</CsoundSynthesizer>
qqq)

; PRESETS, INPUT DATA, ETC.
; LOAD FIRST

(defparameter pange-seq (import-events "data/gregorian-pange-lingua.mid" :meta-exclude t))
; (print pange-seq)
;(print (format nil "Duration of pange-seq: ~$~%" (duration-seconds pange-seq)))

(defparameter pange-lingua
   (midi-in "data/lu0957b.short.mid"))
(defparameter pange-pits (nth 0 pange-lingua))
(print pange-lingua)
(quit)

(defparameter pange-pits
    (not-flat (first pange-lingua)))

; E major home key
(defparameter emajor (transp-mode ionian 4))

;; FINAL VARIATIONS BELOW

; variation A = 'dots'

(defparameter dots-seq (new seq :name "dots"))

;; A PITCHES = each 4pt chord's registers are re-assigned randomly to '(3 4 5 5). 
;;    pitches shuffled & arpeggiated. 
;; A RHYTHM = motoric sixteenths (repeats become ties)
 
(events
 (play-sd
  (make-ties
   (flatten
    (shuffle-all
     (placereg (shuffle-all (nth 0 pange-lingua)) '(3 4 5 5)))))
  .25)
 dots-seq)
(print (format nil "Duration of dots-seq: ~$~%" (duration-seconds dots-seq)))

; variation B = 'hiccup'

;; B PITCHES
;; 59 chords from 'fairmid' are indexed by their place in the sequence 
;;         & selected (idx nos 0-58) in the following manner:
;;   1. 0-58 is broken up into 4 & 5 length sequences; each is repeated 3x.
;;   2. list flattened, then divided into sublists length 3-5
;;   3. each sublist is then re-ordered to maximize consecutive interval 2
;;         (preference for directly skipped chords)
;; chds selected from index list. direct repeats removed.
;; B RHYTHM
;; durations determined by 3rd gen of 'pleasantsrules' @ 16th-note

(defparameter hiccup-seq (new seq :name "hiccup"))

(events
 (splay
  (norpt
   (chooser
    (flatten
     (mapcar 
      (lambda (x) (car (reorder-by-melint x 2)))
      (butlast
       (make-poly
	(flatten
	 (repeater (make-poly (indices 59) '(4 5)) 3))
	(randvec 100 3 3)))))
    (not-flat
     (first pange-lingua)))
   #'list-eql)
  (ferney
   '(1)
   '(4)
   (transp (rwgen pleasantsrules '(1) 3) 1)))
 hiccup-seq)
(print (format nil "Duration of hiccup-seq: ~$~%" (duration-seconds hiccup-seq)))


; variation C = 'sync'

;; C PITCHES
;; hymn pits (incl passing tones): 
;;   1. chords alternate orig & reverse order
;;   2. list flatten & direct repeats removed
;;   3. sublists/chords generated by melodic minor 3rds (simple & compound)
;; C RHYTHMS
;; each duration = size of chord * 16th-note 

(defparameter sync-seq (new seq :name "sync"))

(events
 (let ((pits
	(gather-pits (lambda (a b) (eql 3 (mod12 (abs (- b a))))) 
		     (norpt
		      (flatten
		       (let ((dircyc (makecyc '(-1 1))))
			 (loop for fp in pange-lingua collect
			       (if (> (length fp) 1)
				   (case (next dircyc) 
				     (1 fp)
				     (-1 (reverse fp)))
				   fp))))))))
   (splay pits 
	  (durweight pits .25)))
sync-seq)
(print (format nil "Duration of sync-seq: ~$~%" (duration-seconds sync-seq)))

(defparameter seq-list (list pange-seq dots-seq hiccup-seq sync-seq))
(defparameter seq-times (list 0 70.0 (+ 70 142.5) (+ 196.96 142.5 79.75)))

(defparameter master-seq (new seq :name "master-seq"))

(events seq-list master-seq seq-times)
   
(render-with-csd master-seq aeolus-csd :velocity-scale 120 :csd-filename "Pange-Lingua-2.csd")
(print "Finished.")
(quit)

