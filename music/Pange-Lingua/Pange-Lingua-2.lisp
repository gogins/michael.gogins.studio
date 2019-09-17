(require :asdf)
(load "~/quicklisp/setup.lisp")
(ql:quickload "cm2")
(ql:quickload :nudruz)
(in-package :cm)


(defparameter pange-lingua
   (midi-in "data/lu0957b.mid"))
(print "pange-lingua:")
(print pange-lingua)
(defparameter pange-pits (nth 0 pange-lingua))

(defparameter aeolus-csd #>qqq>
<CsoundSynthesizer>
<CsOptions>
-m195 --displays --midi-key=4 --midi-velocity=5 -odac
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

(defparameter fairmid
    (midi-in "Fairfield-0.mid"))

;; 4-part texture (no nh tones)
(defparameter fairpits
    (mapcar #'safesort
	    '((64 64 52 56) (64 71 56 59) (68 76 52 56) (66 75 54 57) (63 73 54 57)
	      (64 71 56 59) (59 71 56 59) (63 69 54 59) (64 68 52 59) (64 66 45 61)
	      (63 69 47 59) (64 68 52 59) (64 64 52 56) (64 68 52 59) (64 70 49 61)
	      (63 71 56 59) (63 71 56 59) (68 73 52 56) (66 75 51 56) (64 76 49 56)
	      (64 76 49 61) (64 76 49 58) (66 75 51 59) (66 75 54 59) (73 54 58 66)
	      (63 71 47 59) (63 71 56 59) (66 69 61 61) (65 68 59 61) (66 73 57 61)
	      (65 71 56 61) (66 69 54 61) (61 69 54 57) (63 69 54 59) (64 68 52 59)
	      (59 66 51 59) (64 64 49 58) (63 71 47 59) (63 71 47 59) (64 73 59 59)
	      (66 75 57 59) (64 76 56 59) (64 68 52 59) (64 69 49 57) (68 71 47 62)
	      (69 73 45 61) (63 73 45 54) (64 71 44 56) (64 45 61 61) (64 68 47 59)
	      (63 66 47 57) (64 64 52 56))))

; includes passing tones
(defparameter altpits
    (not-flat (first fairmid)))
    
(defparameter pange-pits
    (not-flat (first pange-lingua)))

;; all triads (non-repeating)
(defparameter fairnorms
    '((4 8 11) (3 6 9) (4 8 11) (11 3 6) (4 8 11) (6 9 1) (9 1 3) (11 3 6)
      (4 8 11) (6 10 1) (8 11 3) (1 4 8) (0 3 7) (1 4 8) (6 10 1) (11 3 6)
      (6 10 1) (11 3 6) (8 11 3) (6 9 1) (1 5 8) (6 9 1) (1 5 8) (6 9 1) (11 3 6)
      (4 8 11) (11 3 6) (6 10 1) (11 3 6) (9 1 4) (11 3 6) (4 8 11) (9 1 4)
      (4 8 11) (9 1 4) (6 9 1) (4 8 11) (6 9 1) (4 8 11) (11 3 6) (4 8 11)))

(defparameter fairdurs
    (mapcar #'floor
	    '(1.0 1.0 1.0 1.0 1.0 3.0 1.0 1.0 1.0 1.0 1.0 3.0 1.0 1.0 1.0 1.0 1.0
	      1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 3.0 1.0 1.0 1.0 1.0 1.0 3.0 1.0
	      1.0 1.0 1.0 1.0 3.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
	      1.0 3.0)))

(defparameter fairphrases '(6 6 14 6 6 14))

;; first phrase durs Boulez-'demultiplied' as integer durations
(defparameter fair-demult
    (hits->ints
     '(1/8 1/8 1/8 1/8 1/8 3/8 1/8 1/8 1/8 1/8 1/8 3/8 1/8 1/8 1/8 1/8 1/8 3/8 1/8
       1/8 1/8 1/8 1/8 3/8 1/8 1/8 1/8 1/8 1/8 3/8 3/8 3/8 3/8 3/8 3/8 9/8)))

;; first phrase durs Boulez-'multiplied' as integers
(defparameter fair-mult
    (motive->durs
     (multiplied-motive
      (durs->motive
       (first (make-poly fairdurs fairphrases))))))

(defparameter fairlines
    (chds->lines fairpits))

(defparameter bass (nth 0 fairlines))
(defparameter tenor (nth 1 fairlines))
(defparameter alto (nth 2 fairlines))
(defparameter sopr (nth 3 fairlines))

; E major home key
(defparameter emajor (transp-mode ionian 4))

; randomized hymn durations
(defun theselens (factor)
  (loop for d in (hits->ints fairdurs) collect
	(floor (vary (* d factor) .3))))

;; FINAL VARIATIONS BELOW

; variation A = 'dots'

(defparameter dots-seq (new seq :name "dots"))

;; A PITCHES = each 4pt chord's registers are re-assigned randomly to '(3 4 5 5). 
;;    pitches shuffled & arpeggiated. 
;; A RHYTHM = motoric sixteenths (repeats become ties)

  ;; (events
;;  (play-sd
;;   (make-ties
;;    (flatten
;;     (shuffle-all
;;      (placereg (shuffle-all fairpits) '(3 4 5 5)))))
;;   .25)
;;  dots-seq)
;;
 
; MKG:
 
(events
 (play-sd
  (make-ties
   (flatten
    (shuffle-all
     (placereg (shuffle-all (nth 0 pange-lingua)) '(3 4 5 5)))))
  .25)
 dots-seq)

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

; variation D = 'lope'

;; D -- two lines
;; TOPLINE PITCHES ("wigpits") sopr line scale deg in E major: 
;;        'wigline' path +/- 2,3 between pitches
;; TOPLINE RHYTHM ("wigdurs") dwnbeat-type durations corresp to 'wigpits' (max 13) * 16th
;; BOTTOMLINE PITCHES ("mgpits") alternating bass & tenor pits
;; BOTTOMLINE RHYTHM ("mgdurs") = durations [1+remainder] of wigdurs (short/long) * 16th

(defparameter lope-seq (new seq :name "lope"))

(events
 (let* ((wigpits 
	 '((35 37) (39 41 43 40) (42 39) (41 38) (40 37)
	   (39 41 43 40 42 39 41 43 45 42) (39 36) (38 40) (37 34) (36) (38 35)
	   (37 39 36 38) (35) (37 39 36) (38 40 37) 39 (39 36 38) (40 42 44)
	   (41 38 40) 42 42 (42 39) 41 (41 43) (40 42)
	   (39 41 38 40 42 44 41 38 40 37) (39 41) (38 40) (37 34 36 38) (40 37)
	   (39 41) (38 40 37 39 41) 38 (38 40) (37 39) (36 38) (35 37)
	   (39 36 38 40 37 34 36 33 35 37) (39 36 38) (40 37 39) (41 43 45) 42
	   (37 39 36) (38 35 37) (39 41 38) 40 (40 37) (39 36 38) (35) (37 39)
	   (36 38) (35)))
	(wigdurs 
	 '((2 1) (1 1 1 1) (3 1) (3 1) (3 1) (1 1 1 1 1 1 1 1 1 1) (2 1) (3 1) (2 1)
	   (3) (3 1) (8 1 1 1) (4) (1 1 1) (2 1 1) 4 (2 1 1) (1 1 1) (2 1 1) 3 4
	   (2 1) 4 (3 1) (3 1) (3 1 1 1 1 1 1 1 1 1) (3 1) (3 1) (1 1 1 1) (3 1)
	   (2 1) (8 1 1 1 1) 3 (3 1) (3 1) (3 1) (2 1) (2 1 1 1 1 1 1 1 1 1) (2 1 1)
	   (2 1 1) (2 1 1) 4 (2 1 1) (2 1 1) (2 1 1) 4 (2 1) (2 1 1) (3) (2 1) (2 1)
	   (13)))
	(mgpits (loop for n to (- (length bass) 1) collect (nth n bass) collect (nth n tenor)))
	(mgdurs (flatten (mapcar (lambda (x) (list 1 (- (apply #'+ x) 1))) (not-flat wigdurs)))))
   (list
    (splay (play-mode (flatten wigpits) emajor) (transp (flatten wigdurs) .25 #'*))
    (splay mgpits (transp mgdurs .25 #'*))))
 lope-seq)

; variation E = 'threept'

;; E PITCHES =  'slonim' chds on sopr line, branched by 'tritone-func'. 
;;     converted from chords into lines (repeats become ties)
;; E RHYTHM
;;    -cyclic subdivs (4 4 3 4 3) of half-note
;;    -summed across by [# new pits in each chd + poisson randomness]

(defparameter sbranch3-1 (generic-branch #'tritone-func
		(slonim '(48 57) (norpt sopr))))

(defparameter threept-seq (new seq :name "threept"))

(defparameter sbranch3
    '(((48 57 64) (52 59 67)) ((52 55 71) (49 52 68)) ((49 56 76) (51 56 71))
      ((44 59 75)) ((40 56 73) (44 59 75)) ((39 56 71) (44 57 71) (41 56 69))
      ((38 53 69) (38 51 65) (38 51 59) (39 56 59)) ((39 47 68))
      ((39 46 66) (39 40 66) (36 39 64)) ((36 40 69)) ((36 41 68) (36 40 69))
      ((36 45 64) (36 44 65)) ((36 41 68))
      ((37 41 70) (37 46 71) (31 46 71) (28 43 71))
      ((40 43 71) (40 41 67) (37 40 65) (37 41 70)) ((41 46 73))
      ((42 46 75) (40 42 75) (36 40 75))
      ((36 45 76) (36 45 70) (30 45 70) (30 39 70)) ((30 46 75))
      ((29 46 73) (34 47 73) (34 47 67) (28 47 67)) ((28 43 71)) ((24 40 69))
      ((24 41 68) (25 40 68)) ((28 44 73) (32 47 75))
      ((27 44 71) (32 45 71) (29 44 69))
      ((26 41 69) (26 39 65) (26 39 59) (27 44 59)) ((27 35 68))
      ((27 34 66) (27 28 66) (24 27 64) (24 21 64)) ((24 33 64) (27 35 68))
      ((27 32 71) (25 28 68)) ((28 32 73)) ((32 35 75) (32 40 73))
      ((37 32 76) (37 32 76)) ((37 28 68)) ((37 30 69)) ((38 30 71) (37 30 69))
      ((42 33 73) (42 35 74)) ((38 30 71) (37 28 68)) ((37 32 64)) ((39 35 68))
      ((39 34 66) (39 28 66) (36 27 64) (36 21 64)) ((36 33 64))))

(events
 (playchds->lines
  (flatter sbranch3)
  (ferney '(2) '(4 4 3 4 3)
	  (transp
	   (map 'list #'+
		(poissonvec 1.5 (length (flatter sbranch3-1)))
		(take-poly
		 (smoothlist (flatter sbranch3))))
	   1)))
 threept-seq)

; variation F = 'stun'

;; F PITCHES
;; "sts7" = Steiner Triple System (designs.lisp), 7 elements, 3-vectors, intersection 1
;; "lopits" = for each sopr note, a sts7 tlen-length vector-list is randomly selected (@ heap), and 
;;                 trichords are played in mode determined by soprano pitch, then 
;;                 shuffled & 'stacked-up' from first pitch.
;; -- each vec in "lopits" is combined with tlens-repeated "sopr". then 4-7 pitches 
;;                 in each combo are chosen randomly to be repeated, and each is shuffled.   
;; F RHYTHM = motoric 32nds

(defparameter stun-seq (new seq :name "stun"))

(events
 (let* ((tlens (theselens 5))
	(lopits
	 (transp
	  (mapcar #'stack-up
		  (shuffle-all
		   (loop for n to (- (length sopr) 1) append
			 (play-mode
			  (chooser (heapvec (nth n tlens) 7) sts7)
			  (transp-mode emajor (mod12 (nth n sopr)))))))
	  36)))
   (splay
    (norpt
     (loop for x in 
	   (shuffle-all
	    (merge-slots (list 
			  (repeater sopr tlens)
			  lopits)))
	   append (chooser (heapvec (between 4 7) (length x)) x)))
    .125))
 stun-seq)

; variation G = 'jumpline'

;; G PITCHES
;; each 4pt chord in hymn ("fairpits") is "stacked-by" major 3rds, 
;;   and one of the 4 'best' stacks is 
;;   randomly selected. these chords are internally shuffled, 
;;   the list is flattened, and repeats are removed.
;; G RHYTHM
;; moves mostly in 16ths (steps move in 32nds)

(defparameter jumpline-seq (new seq :name "jumpline"))

(defparameter stakpits
    (mapcar (lambda (x) (pickl (subseq (stack-by x 4) 0 3))) 
	    fairpits))

(events
 (let ((pits
	(norpt
	 (flatten (shuffle-all stakpits)))))
   (splay pits 
	  (ornadurs (conjunct-fragments pits 2) .25)))
 jumpline-seq)

; variation H = 'bigchor'

;; H PITCHES = "sbranch2"
;; "sbranch2" = non-repeating sopr is doubled in descending 4ths 
;;    & branched with "stravrot-func"
;; H RHYTHM
;; "sbranch1" = non-repeating sopr slonim w/ (61 59 54) 
;;    & branched with "rgr-alldim1"
;; -- each chord's duration (* quarter) is determined by melint of 
;;    'avg-chdpit's in "sbranch1" & chords become tied lines.

(defparameter bigchor-seq (new seq :name "jumpline"))

(defparameter sbranch2-1
    (generic-branch #'stravrot-func
		    (mapcar (lambda (x) (transp '(0 -5 -10 -15) x)) (norpt sopr))))

(defparameter sbranch2
    '(((64 59 54 49) (66 59 56 49)) ((71 66 61 56) (71 64 61 54))
      ((76 71 66 61) (74 69 67 60)) ((75 70 65 60) (75 70 68 61))
      ((73 68 63 58) (73 68 66 59)) ((71 66 61 56) (71 66 64 57))
      ((69 64 59 54) (67 60 62 53)) ((68 63 58 53)) ((66 61 56 51))
      ((69 64 59 54) (67 60 62 53)) ((68 63 58 53) (66 61 59 52))
      ((64 59 54 49) (63 56 54 49) (63 56 58 53)) ((68 63 58 53) (67 60 58 53))
      ((70 65 60 55) (70 63 61 56)) ((71 66 61 56) (70 63 61 56))
      ((73 68 63 58) (72 65 63 58)) ((75 70 65 60) (75 68 66 61))
      ((76 71 66 61) (74 69 67 60)) ((75 70 65 60) (75 70 68 61))
      ((73 68 63 58) (73 68 66 59)) ((71 66 61 56) (71 66 64 57))
      ((69 64 59 54) (67 60 62 53)) ((68 63 58 53) (68 61 58 51))
      ((73 68 63 58) (73 68 66 59)) ((71 66 61 56) (71 66 64 57))
      ((69 64 59 54) (67 60 62 53)) ((68 63 58 53)) ((66 61 56 51) (66 61 59 52))
      ((64 59 54 49) (66 59 56 49)) ((71 66 61 56) (70 63 61 56))
      ((73 68 63 58) (72 65 63 58)) ((75 70 65 60) (75 68 66 61))
      ((76 71 66 61) (75 68 66 61) (75 68 70 65)) ((68 63 58 53) (68 61 59 54))
      ((69 64 59 54) (68 61 59 54)) ((71 66 61 56) (70 63 61 56))
      ((73 68 63 58) (73 68 66 59)) ((71 66 61 56) (71 64 61 54))
      ((64 59 54 49) (63 56 54 49) (63 56 58 53)) ((68 63 58 53))
      ((66 61 56 51) (66 61 59 52)) ((64 59 54 49))))

(defparameter sbranch1-1
    (generic-branch #'rgr-alldim1
		    (slonim '(61 59 54) (norpt sopr))))

(defparameter sbranch1
    '(((61 59 54 64) (61 57 54 64) (60 57 54 64) (64 57 54 59))
      ((64 57 54 71) (64 54 48 69) (64 54 49 69) (64 54 49 68) (64 54 49 71))
      ((61 59 54 76) (61 59 55 76) (61 59 56 76) (61 59 56 75))
      ((61 58 56 75) (61 58 54 75) (61 57 54 75) (61 56 54 75))
      ((63 56 54 73) (63 57 54 73) (63 57 54 71) (62 57 54 71) (62 57 53 71))
      ((62 57 52 71) (62 57 53 71) (62 57 54 71) (61 57 54 71) (64 57 54 71))
      ((64 59 54 69) (64 54 48 69) (64 54 49 69) (64 54 49 68) (63 54 49 68))
      ((61 63 54 68) (63 66 56 71) (62 66 56 71) (61 66 56 71))
      ((61 68 59 66) (62 68 59 66) (62 68 59 64) (62 67 59 64) (62 67 58 64)
       (62 67 57 64))
      ((62 64 55 69) (62 64 55 70) (62 65 55 70) (62 65 56 70) (61 65 56 70)
       (61 64 56 70) (61 63 56 70))
      ((61 63 58 68) (61 64 58 68) (61 64 59 68) (62 64 59 68) (62 64 59 67)
       (62 64 58 67))
      ((62 67 57 64) (62 67 58 64) (62 67 58 65) (62 68 58 65) (61 68 58 65)
       (61 68 58 64))
      ((61 63 58 68) (61 64 58 68) (61 65 58 68) (60 65 58 68) (63 65 58 68))
      ((63 65 56 70) (63 65 56 71) (63 66 56 71) (62 66 56 71))
      ((61 66 56 71) (61 64 56 71) (61 64 55 71) (61 64 54 71))
      ((64 66 59 73) (64 67 59 73) (64 68 59 73) (65 68 59 73) (65 68 59 75))
      ((65 68 58 75) (65 68 59 75) (66 68 59 75) (66 69 59 75) (66 69 59 74)
       (65 69 59 74) (64 69 59 74))
      ((62 69 59 76) (62 69 59 77) (60 69 62 77) (60 69 63 77) (60 68 63 77)
       (63 68 65 71) (63 68 65 70))
      ((65 68 58 75) (65 68 59 75) (65 68 59 73) (64 68 59 73) (64 67 59 73))
      ((64 66 59 73) (64 67 59 73) (64 68 59 73) (63 68 59 73) (66 68 59 73))
      ((61 66 56 71) (62 66 56 71) (62 64 56 71) (62 64 55 71) (62 64 55 70))
      ((62 64 55 69) (62 64 55 70) (62 65 55 70) (62 65 56 70) (61 65 56 70)
       (61 64 56 70) (61 63 56 70))
      ((61 63 58 68) (61 66 58 68) (61 66 59 68))
      ((66 68 59 73) (64 68 59 73) (64 67 59 73) (64 66 59 73))
      ((66 73 64 71) (67 73 64 71) (67 73 64 69) (67 72 64 69) (67 72 63 69))
      ((67 72 62 69) (67 72 63 69) (65 72 63 69) (65 72 63 68) (65 75 68 71))
      ((70 75 65 68) (71 75 65 68) (71 73 65 68) (71 73 64 68) (71 73 64 67))
      ((71 73 64 66) (69 73 64 66) (69 72 64 66) (71 66 64 69))
      ((71 78 69 64) (71 78 69 64))
      ((76 78 69 71) (76 78 69 72) (76 78 69 73) (76 78 70 73) (76 80 70 73))
      ((75 80 70 73) (75 78 70 73) (75 78 69 73) (75 78 68 73))
      ((73 80 66 75) (73 81 66 75) (73 81 66 76) (72 81 66 76))
      ((71 81 66 76) (71 80 66 76) (71 80 66 74) (71 80 66 73))
      ((71 85 66 68) (71 86 66 68) (71 86 66 69) (71 86 65 69))
      ((71 86 64 69) (71 86 65 69) (71 86 66 69) (71 85 66 69))
      ((69 88 66 71) (64 84 66 69) (64 85 66 69) (64 85 66 70) (64 85 68 70)
       (63 85 68 70))
      ((70 87 68 73) (70 88 68 73) (70 88 66 73) (69 88 66 73) (69 88 66 72))
      ((69 88 66 71) (69 88 66 71))
      ((71 90 69 64) (76 90 69 60) (76 90 69 61) (76 90 68 61) (75 90 68 61))
      ((73 87 66 68) (75 90 68 71) (74 90 68 71) (73 90 68 71))
      ((73 92 71 66) (74 92 71 66) (74 92 71 64) (74 91 71 64) (74 91 70 64))
      ((74 91 69 64))))

(events
 (playchds->lines
  (flatter sbranch2-1)
   (transp
    (mapcar #'abs
	    (melint
	     (mapcar #'avg-chdpit
		     (flatter sbranch1-1))))
    1))
 bigchor-seq)


; variation I = 'longrot2'
; ?

;; I -- two lines
;; PRELIMINARY VALUES
;; "pits" = 'stravrot' applied to the normal form of each hymn chd
;; "accpits" = "pits" w/corresponding soprano pitch removed
;; "tlens" = sum of corresponding members of (theselens 6) with lengths within "accpits"
;; "tsopr" = sopr with repeated notes @ ties
;; "tbass" = "accpits" placed in low reg with repeated notes @ ties
;; "bdurs" = long list of 16th notes
;; "basspits" = "tbass" pitches flattened & gathered into 2,3,4-note sublists.
;;                 each list is given a standard contour by size.
;; TOP LINE PITCH = hymn soprano as ties
;; TOP LINE RHYTHM = soprano summed across "tlens" (to correspond w/acc) across 16ths
;; BOTTOM LINE PITCH = "basspits" w/contours applied in low register
;; BOTTOM LINE RHYTHM = 'dnbeats' applied to each strav figure, so long note (w/sopr)
;;                        starts each 'tlen' figure

(defparameter out-seq (new seq :name "jumpline"))

(events
 (let* ((pits
    (mapcar #'stravrot (mapcar #'normal-form fairpits)))
	(accpits
    (map 'list #'set-difference pits (not-flat (mod12 sopr))))
	(tlens 
        ;(map 'list #'+ (theselens 6) (mapcar #'length accpits)))
        (map 'list #'+ (theselens 4) (mapcar #'length accpits)))
	(tsopr (make-ties sopr))
	;(tbass (make-ties (placereg (flatten accpits) 3)))
	;(bdurs (ferney '(1/2) (copylist '(2) 800)))
	(tbass (make-ties (placereg (flatten accpits) 3)))
	(bdurs (ferney '(1/3) (copylist '(2) 800)))
	(basspits
	 (flatten
	  (mapcar (lambda (x) (case (length x) 
				(4 (give-contour-to-mel '(3 1 2 0) x))
				(2 (give-contour-to-mel '(0 1) x))
				(3 (give-contour-to-mel '(2 1 0) x))
				(t x)))
		  (make-poly 
		   (first tbass)
		   (randvec 33 3 2))))))
	(list
	 (splay (transp (first tsopr) 0) (sum-across bdurs (sum-across tlens (second tsopr))))
	  (splay basspits
		 (sum-across
		  bdurs
		  (sum-across
		   (flatten
		    (map 'list #'dnbeats accpits tlens))
		   (second tbass))))))
   out-seq)
   
(render-with-csd lope-seq aeolus-csd :velocity-scale 120 :csd-filename "Pange-Lingua-2.csd")
(print "Finished.")
(quit)

