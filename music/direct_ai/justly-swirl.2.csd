<CsoundSynthesizer>
<CsLicense>
JUSTLY-SWIRL -- Pange Lingua; swirl inside the sounding notes

Author:     Michael Gogins
License:    Lesser GNU General Public License version 2.1
Copyright:  2026 Michael Gogins

Variant of justly.csd: same Mode III (Phrygian) Liber form, but every SwirlPad
note uses drone.csd's voice (GEN10 harmonics + Chebyshev distort) through the
same dry+0.8*reverbsc tank (feedback 0.9, cutoff 15000) at a slow tempo so
just intervals beat and swirl inside the notes.

    E 1/1 | F 16/15 | G 6/5 | A 4/3 | B 3/2 | C 8/5 | D 9/5

Outside the canon, accompaniment starts with the cantus and changes only on
cantus onsets: bass re-attacks every note; upper voices may tie. Cantus is
centered; accompaniment pans two voices per side.

The final Em hold lasts twice the earlier phrase holds (6 vs 3 beats). After
it completes, a four-voice canon enters at delays of 4/3 beat: each voice
begins on its tone of that final Em chord (E Phrygian); only the next
movement modulates up 4 semitones to just G-sharp Phrygian. A bridging
G-sharp hold then yields to section 3 back in E Phrygian, with quartal
voices under the cantus as the high voice (centered).
</CsLicense>
<CsOptions>
-odac -d -m160 -+msg_color=0
</CsOptions>
<CsInstruments>

sr = 48000
ksmps = 10
nchnls = 2
; Same full-scale convention as drone.csd so distort sees the same drive.
0dbfs = 20000

; SwirlPad / ZakianFlute (canon notes) -> DroneField.
connect "SwirlPad", "outleft",  "DroneField",   "inleft"
connect "SwirlPad", "outright", "DroneField",   "inright"
connect "ZakianFlute", "outleft",  "DroneField",   "inleft"
connect "ZakianFlute", "outright", "DroneField",   "inright"
connect "DroneField", "outleft",  "MasterOutput", "inleft"
connect "DroneField", "outright", "MasterOutput", "inright"

alwayson "DroneField"
alwayson "MasterOutput"

gk_DroneField_feedback chnexport "gk_DroneField_feedback", 3
gk_DroneField_wet chnexport "gk_DroneField_wet", 3
gk_DroneField_frequency_cutoff chnexport "gk_DroneField_frequency_cutoff", 3
gk_SwirlPad_level chnexport "gk_SwirlPad_level", 3
gk_ZakianFlute_midi_dynamic_range chnexport "gk_ZakianFlute_midi_dynamic_range", 3
gk_ZakianFlute_level chnexport "gk_ZakianFlute_level", 3
gk_ZakianFlute_pan chnexport "gk_ZakianFlute_pan", 3
gi_ZakianFLute_seed chnexport "gi_ZakianFLute_seed", 3
gi_ZakianFLute_space_left_to_front chnexport "gi_ZakianFLute_space_left_to_front", 3
gk_MasterOutput_level chnexport "gk_MasterOutput_level", 3
gS_MasterOutput_filename chnexport "gS_MasterOutput_filename", 3

; Level is dB relative to drone.csd's ampdb(velocity) drive (velocity ~60).
gk_SwirlPad_level init 0
; Canon flutes +4 dB (same boost previously on FMBell).
gk_ZakianFlute_midi_dynamic_range init 20
gk_ZakianFlute_level init 2
gk_ZakianFlute_pan init 0.5
gi_ZakianFLute_seed init 0.5
gi_ZakianFLute_space_left_to_front init 0.5
; Match drone.csd instr 3: reverbsc feedback 0.9, cutoff 15000, wet 0.8.
gk_DroneField_feedback init 0.9
gk_DroneField_wet init 0.8
gk_DroneField_frequency_cutoff init 15000
; Headroom for many overlapping voices (drone.csd uses only ~3 at once).
gk_MasterOutput_level init -12
gS_MasterOutput_filename init ""
; Slow enough that ~1.75-beat notes last ~9 s and holds ~15–30 s.
gi_Justly_tempo init 18
; Fixed tonic for the notional just Phrygian scale (E0 MIDI key class).
gi_Justly_tonic init 4
; Extra beats of overlap so just stacks ring into each other.
gi_Justly_legato init 1.5
; Cantus level boost in sections 1 and 3 (dB); canon voices stay unboosted.
gi_Justly_cantus_db init 3
; Extra beats so each canon note lasts through the next attack (legato).
gi_Justly_canon_legato init 1.5
; Beat where section 2 / flute canon must be silent (G# reverse-pivot hold).
gi_Justly_canon_end init 147

; Same tables as drone.csd f1 / f2.
gi_Swirl_harm ftgen 0, 0, 65536, 10, 3, 0, 0, 1, 0, 2
gi_Swirl_distort ftgen 0, 0, 65536, 13, 1, 1, 0, 3, 0, 2
; From patches/ZakianFlute.inc (Lee Zakian / Michael Gogins).
gi_ZakianFLute_f2  ftgen 0, 0, 16, -2, 40, 40, 80, 160, 320, 640, 1280, 2560, 5120, 10240, 10240
gi_ZakianFlute_f26 ftgen 0, 0, 65537, -10, 2000, 489, 74, 219, 125, 9, 33, 5, 5
gi_ZakianFlute_f27 ftgen 0, 0, 65537, -10, 2729, 1926, 346, 662, 537, 110, 61, 29, 7
gi_ZakianFlute_f28 ftgen 0, 0, 65537, -10, 2558, 2012, 390, 361, 534, 139, 53, 22, 10, 13, 10
gi_ZakianFlute_f29 ftgen 0, 0, 65537, -10, 12318, 8844, 1841, 1636, 256, 150, 60, 46, 11
gi_ZakianFlute_f30 ftgen 0, 0, 65537, -10, 1229, 16, 34, 57, 32
gi_ZakianFlute_f31 ftgen 0, 0, 65537, -10, 163, 31, 1, 50, 31
gi_ZakianFlute_f32 ftgen 0, 0, 65537, -10, 4128, 883, 354, 79, 59, 23
gi_ZakianFlute_f33 ftgen 0, 0, 65537, -10, 1924, 930, 251, 50, 25, 14
gi_ZakianFlute_f34 ftgen 0, 0, 65537, -10, 94, 6, 22, 8
gi_ZakianFlute_f35 ftgen 0, 0, 65537, -10, 2661, 87, 33, 18
gi_ZakianFlute_f36 ftgen 0, 0, 65537, -10, 174, 12
gi_ZakianFlute_f37 ftgen 0, 0, 65537, -10, 314, 13
gi_ZakianFlute_wtsin ftgen 0, 0, 65537, 10, 1

; Liber Mode III cantus: start (beats), dur, MIDI, velocity — 52 notes.
; Never use Csound macros in this piece; prefer UDOs (including recursion).
gi_Cantus_nnotes init 52
gi_Cantus ftgen 0, 0, -208, -2, \
    1.5, 2.50, 64, 63, \
    3.5, 1.85, 65, 61, \
    4.5, 1.85, 64, 60, \
    5.5, 1.85, 62, 59, \
    6.5, 1.85, 67, 62, \
    7.5, 1.85, 67, 61, \
    8.5, 1.85, 69, 62, \
    9.5, 1.85, 72, 63, \
    10.5, 3.00, 72, 63, \
    13, 1.75, 72, 62, \
    14, 1.75, 74, 63, \
    15, 1.75, 72, 61, \
    16, 1.75, 72, 60, \
    17, 1.75, 71, 61, \
    18, 1.75, 69, 60, \
    19, 1.75, 72, 62, \
    20, 1.75, 71, 61, \
    21, 1.75, 69, 60, \
    22, 3.00, 67, 62, \
    26, 1.75, 67, 61, \
    27, 1.75, 69, 62, \
    28, 1.75, 72, 63, \
    29, 1.75, 71, 62, \
    30, 1.75, 69, 60, \
    31, 1.75, 67, 61, \
    32, 1.75, 69, 62, \
    33, 3.00, 67, 62, \
    37, 1.75, 69, 62, \
    38, 1.75, 71, 62, \
    39, 1.75, 67, 60, \
    40, 1.75, 67, 61, \
    41, 1.75, 64, 59, \
    42, 1.75, 69, 62, \
    43, 1.75, 69, 61, \
    44, 3.00, 62, 60, \
    48, 1.75, 64, 61, \
    49, 1.75, 67, 62, \
    50, 1.75, 67, 61, \
    51, 1.75, 64, 60, \
    52, 1.75, 67, 62, \
    53, 1.75, 69, 62, \
    54, 1.75, 69, 61, \
    55, 3.00, 67, 63, \
    59, 1.75, 69, 62, \
    60, 1.75, 71, 62, \
    61, 1.75, 67, 61, \
    62, 1.75, 69, 62, \
    63, 1.75, 67, 60, \
    64, 1.75, 65, 61, \
    65, 1.75, 64, 60, \
    66, 1.75, 62, 59, \
    67, 6.00, 64, 64

; Same mix as drone.csd: dry + reverbsc(...) * 0.8.
instr DroneField
    aleftin init 0
    arightin init 0
    aleftin inleta "inleft"
    arightin inleta "inright"
    aleftout, arightout reverbsc aleftin, arightin, gk_DroneField_feedback, gk_DroneField_frequency_cutoff
    aleftoutmix = aleftin + aleftout * gk_DroneField_wet
    arightoutmix = arightin + arightout * gk_DroneField_wet
    outleta "outleft", aleftoutmix
    outleta "outright", arightoutmix
    prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", \
           nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

instr MasterOutput
    aleft inleta "inleft"
    aright inleta "inright"
    k_gain = ampdb(gk_MasterOutput_level)
    printks2 "Master gain: %f\n", k_gain
    a_output_left = aleft * k_gain
    a_output_right = aright * k_gain
    outs a_output_left, a_output_right
    ; fout in float; scale 0dbfs=20000 peak toward ~-3 dBFS float files.
    i_amplitude_adjustment = ampdbfs(-3) / 20000
    i_filename_length strlen gS_MasterOutput_filename
    if i_filename_length > 0 then
        prints sprintf("Output filename: %s\n", gS_MasterOutput_filename)
        fout gS_MasterOutput_filename, 18, aleft * i_amplitude_adjustment, aright * i_amplitude_adjustment
    endif
    prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", \
           nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

; From patches/ZakianFlute.inc -- canon-section cantus notes (Lee Zakian / M. Gogins).
instr ZakianFlute
    i_instrument = p1
    i_time = p2
    if p3 == -1 then
        i_duration = 1000
    else
        i_duration = p3
    endif
    i_midi_key = p4
    i_midi_velocity = p5
    k_space_front_to_back = p6
    if p7 == 0 then
        k_space_left_to_right = gi_ZakianFLute_space_left_to_front
    else
        k_space_left_to_right = p7
    endif
    k_space_bottom_to_top = p8
    i_phase = p9
    i_overall_amps = 65.2
    ; Patch was written for 0dbfs=1 and applies i_amplitude twice; keep that
    ; math, then scale once to this piece's 0dbfs at the output.
    i_normalization = ampdb(-i_overall_amps) / 2
    i_midi_dynamic_range = i(gk_ZakianFlute_midi_dynamic_range)
    i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
    i_amplitude = ampdb(i_midi_velocity) * i_normalization
    k_gain = ampdb(gk_ZakianFlute_level)
    iHz = cpsmidinn(i_midi_key)
    kHz = k(iHz)
    ip3 = 3
    ip4 init i_amplitude
    ip5 init iHz
    ip6 init 0.5
    ip7 init 0.08
    ip8 init 0.08
    ip9 init 5
    iampscale = ip4
    ifreq = ip5
    ivibdepth = abs(ip6 * ifreq / 100.0)
    iattack = ip7 * (1.1 - 0.2 * gi_ZakianFLute_seed)
    gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed * 105.947)
    idecay = ip8 * (1.1 - 0.2 * gi_ZakianFLute_seed)
    gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed * 105.947)
    ifiltcut tablei ip9, gi_ZakianFLute_f2
    iattack = (iattack < 6 / kr ? 6 / kr : iattack)
    idecay = (idecay < 6 / kr ? 6 / kr : idecay)
    isustain = i_duration - iattack - idecay
    isustain = (isustain < 5 / kr ? 5 / kr : isustain)
    iatt = iattack / 6
    isus = isustain / 4
    idec = idecay / 6
    iphase = gi_ZakianFLute_seed
    gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed * 105.947)
    kvibdepth linseg 0.1, 0.8 * ip3, 1, isustain, 1, 0.2 * ip3, 0.7
    kvibdepth = kvibdepth * ivibdepth
    kvibdepthr randi 0.1 * kvibdepth, 5, gi_ZakianFLute_seed
    gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed * 105.947)
    kvibdepth = kvibdepth + kvibdepthr
    ivibr1 = gi_ZakianFLute_seed
    gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed * 105.947)
    ivibr2 = gi_ZakianFLute_seed
    gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed * 105.947)
    if ip6 < 0 goto vibrato1
    kvibrate linseg 2.5 + ivibr1, isustain, 4.5 + ivibr2
    goto vibrato2
vibrato1:
    ivibr3 = gi_ZakianFLute_seed
    gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed * 105.947)
    kvibrate linseg 3.5 + ivibr1, 0.1, 4.5 + ivibr2, isustain - 0.1, 2.5 + ivibr3
vibrato2:
    kvibrater randi 0.1 * kvibrate, 5, gi_ZakianFLute_seed
    gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed * 105.947)
    kvibrate = kvibrate + kvibrater
    kvib oscili kvibdepth, kvibrate, gi_ZakianFlute_wtsin
    ifdev1 = -0.03 * gi_ZakianFLute_seed
    gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed * 105.947)
    ifdev2 = 0.003 * gi_ZakianFLute_seed
    gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed * 105.947)
    ifdev3 = -0.0015 * gi_ZakianFLute_seed
    gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed * 105.947)
    ifdev4 = 0.012 * gi_ZakianFLute_seed
    gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed * 105.947)
    kfreqr linseg ifdev1, iattack, ifdev2, isustain, ifdev3, idecay, ifdev4
    kfreq = kHz * (1 + kfreqr) + kvib
    if ifreq < 427.28 goto range1
    if ifreq < 608.22 goto range2
    if ifreq < 1013.7 goto range3
    goto range4
range1:
    kamp1 linseg 0, iatt, 0.002, iatt, 0.045, iatt, 0.146, iatt, \
        0.272, iatt, 0.072, iatt, 0.043, isus, 0.230, isus, 0.000, isus, \
        0.118, isus, 0.923, idec, 1.191, idec, 0.794, idec, 0.418, idec, \
        0.172, idec, 0.053, idec, 0
    kamp2 linseg 0, iatt, 0.009, iatt, 0.022, iatt, -0.049, iatt, \
        -0.120, iatt, 0.297, iatt, 1.890, isus, 1.543, isus, 0.000, isus, \
        0.546, isus, 0.690, idec, -0.318, idec, -0.326, idec, -0.116, idec, \
        -0.035, idec, -0.020, idec, 0
    kamp3 linseg 0, iatt, 0.005, iatt, -0.026, iatt, 0.023, iatt, \
        0.133, iatt, 0.060, iatt, -1.245, isus, -0.760, isus, 1.000, isus, \
        0.360, isus, -0.526, idec, 0.165, idec, 0.184, idec, 0.060, idec, \
        0.010, idec, 0.013, idec, 0
    iwt1 = gi_ZakianFlute_f26
    iwt2 = gi_ZakianFlute_f27
    iwt3 = gi_ZakianFlute_f28
    inorm = 3949
    goto flute_end
range2:
    kamp1 linseg 0, iatt, 0.000, iatt, -0.005, iatt, 0.000, iatt, \
        0.030, iatt, 0.198, iatt, 0.664, isus, 1.451, isus, 1.782, isus, \
        1.316, isus, 0.817, idec, 0.284, idec, 0.171, idec, 0.082, idec, \
        0.037, idec, 0.012, idec, 0
    kamp2 linseg 0, iatt, 0.000, iatt, 0.320, iatt, 0.882, iatt, \
        1.863, iatt, 4.175, iatt, 4.355, isus, -5.329, isus, -8.303, isus, \
        -1.480, isus, -0.472, idec, 1.819, idec, -0.135, idec, -0.082, idec, \
        -0.170, idec, -0.065, idec, 0
    kamp3 linseg 0, iatt, 1.000, iatt, 0.520, iatt, -0.303, iatt, \
        0.059, iatt, -4.103, iatt, -6.784, isus, 7.006, isus, 11, isus, \
        12.495, isus, -0.562, idec, -4.946, idec, -0.587, idec, 0.440, idec, \
        0.174, idec, -0.027, idec, 0
    iwt1 = gi_ZakianFlute_f29
    iwt2 = gi_ZakianFlute_f30
    iwt3 = gi_ZakianFlute_f31
    inorm = 27668.2
    goto flute_end
range3:
    kamp1 linseg 0, iatt, 0.005, iatt, 0.000, iatt, -0.082, iatt, \
        0.36, iatt, 0.581, iatt, 0.416, isus, 1.073, isus, 0.000, isus, \
        0.356, isus, 0.86, idec, 0.532, idec, 0.162, idec, 0.076, idec, 0.064, \
        idec, 0.031, idec, 0
    kamp2 linseg 0, iatt, -0.005, iatt, 0.000, iatt, 0.205, iatt, \
        -0.284, iatt, -0.208, iatt, 0.326, isus, -0.401, isus, 1.540, isus, \
        0.589, isus, -0.486, idec, -0.016, idec, 0.141, idec, 0.105, idec, \
        -0.003, idec, -0.023, idec, 0
    kamp3 linseg 0, iatt, 0.722, iatt, 1.500, iatt, 3.697, iatt, \
        0.080, iatt, -2.327, iatt, -0.684, isus, -2.638, isus, 0.000, isus, \
        1.347, isus, 0.485, idec, -0.419, idec, -0.700, idec, -0.278, idec, \
        0.167, idec, -0.059, idec, 0
    iwt1 = gi_ZakianFlute_f32
    iwt2 = gi_ZakianFlute_f33
    iwt3 = gi_ZakianFlute_f34
    inorm = 3775
    goto flute_end
range4:
    kamp1 linseg 0, iatt, 0.000, iatt, 0.000, iatt, 0.211, iatt, \
        0.526, iatt, 0.989, iatt, 1.216, isus, 1.727, isus, 1.881, isus, \
        1.462, isus, 1.28, idec, 0.75, idec, 0.34, idec, 0.154, idec, 0.122, \
        idec, 0.028, idec, 0
    kamp2 linseg 0, iatt, 0.500, iatt, 0.000, iatt, 0.181, iatt, \
        0.859, iatt, -0.205, iatt, -0.430, isus, -0.725, isus, -0.544, isus, \
        -0.436, isus, -0.109, idec, -0.03, idec, -0.022, idec, -0.046, idec, \
        -0.071, idec, -0.019, idec, 0
    kamp3 linseg 0, iatt, 0.000, iatt, 1.000, iatt, 0.426, iatt, \
        0.222, iatt, 0.175, iatt, -0.153, isus, 0.355, isus, 0.175, isus, \
        0.16, isus, -0.246, idec, -0.045, idec, -0.072, idec, 0.057, idec, \
        -0.024, idec, 0.002, idec, 0
    iwt1 = gi_ZakianFlute_f35
    iwt2 = gi_ZakianFlute_f36
    iwt3 = gi_ZakianFlute_f37
    inorm = 4909.05
    goto flute_end
flute_end:
    kampr1 randi 0.02 * kamp1, 10, gi_ZakianFLute_seed
    gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed * 105.947)
    kamp1 = kamp1 + kampr1
    kampr2 randi 0.02 * kamp2, 10, gi_ZakianFLute_seed
    gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed * 105.947)
    kamp2 = kamp2 + kampr2
    kampr3 randi 0.02 * kamp3, 10, gi_ZakianFLute_seed
    gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed * 105.947)
    kamp3 = kamp3 + kampr3
    awt1 poscil kamp1, kfreq, iwt1, iphase
    awt2 poscil kamp2, kfreq, iwt2, iphase
    awt3 poscil kamp3, kfreq, iwt3, iphase
    asig = awt1 + awt2 + awt3
    asig = asig * (iampscale / inorm)
    kcut linseg 0, iattack, ifiltcut, isustain, ifiltcut, idecay, 0
    afilt tone asig, kcut
    a_signal balance afilt, asig
    i_attack = 0.002
    i_release = 0.01
    i_declick_attack = i_attack
    i_declick_release = i_declick_attack * 2
    xtratim 1
    a_declicking_envelope cossegr 0, i_declick_attack, 1, i_duration, 1, i_declick_release, 0
    a_filtered_envelope tonex a_declicking_envelope, 40, 4
    a_signal = a_signal * i_amplitude * a_filtered_envelope * k_gain
    a_signal *= 0.7 * 20000
    a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
    outleta "outleft", a_out_left
    outleta "outright", a_out_right
    prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", \
           nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

; Same voice as drone.csd instr 1: GEN10 harmonics, then distort(..., 0.125, f2).
instr SwirlPad
    i_midi_key = p4
    ivelocity = p5
    ipan = p7
    i_frequency = cpsmidinn(i_midi_key)
    iamp = ampdb(ivelocity) * ampdb(i(gk_SwirlPad_level))
    ; Keep blooms soft, but short enough that section 3 reaches full level
    ; before the canon/reverb wash has died away.
    iattack = 5
    idecay = 8
    if p3 < 12 then
        iattack = p3 * 0.4
        idecay = p3 * 0.4
    endif
    if iattack < 2 then
        iattack = 2
    endif
    if idecay < 3 then
        idecay = 3
    endif
    p3 = p3 + (iattack + idecay) / 2.0
    isustain = p3 - (iattack + idecay)
    if isustain < 0.05 then
        isustain = 0.05
    endif
    kenvelope transeg 0.0, iattack, 0.0, iamp, isustain, 0.0, iamp, idecay, 0.0, 0.0
    asignal poscil3 kenvelope, i_frequency, gi_Swirl_harm
    asignal distort asignal, 0.125, gi_Swirl_distort
    aleft, aright pan2 asignal, ipan
    outleta "outleft", aleft
    outleta "outright", aright
    prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", \
           nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

; Map an ET MIDI spelling onto just Phrygian on itonic (MIDI class of tonic).
opcode PhrygianKey, i, ii
    imid, itonic xin
    ipc = (imid - itonic) % 12
    ioct = int((imid - itonic) / 12)
    iroot = itonic + 12 * ioct
    iratio = -1
    if ipc == 0 then
        iratio = 1 / 1
    elseif ipc == 1 then
        iratio = 16 / 15
    elseif ipc == 3 then
        iratio = 6 / 5
    elseif ipc == 5 then
        iratio = 4 / 3
    elseif ipc == 7 then
        iratio = 3 / 2
    elseif ipc == 8 then
        iratio = 8 / 5
    elseif ipc == 10 then
        iratio = 9 / 5
    endif
    if iratio > 0 then
        ikey = iroot + 12 * log(iratio) / log(2)
    else
        ikey = imid
    endif
    xout ikey
endop

; p4 start (beats), p5 dur (beats), p6 ET MIDI spelling, p7 vel, p8 pan,
; p9 attack (unused by SwirlPad; kept for score compat), p10 tonic MIDI class,
; p11 optional extra dB (cantus in sections 1 and 3),
; p12 > 0 => ZakianFlute (canon-section notes); else SwirlPad
instr JustTone
    istart = p4 * 60 / gi_Justly_tempo
    iduration = p5 * 60 / gi_Justly_tempo
    imid = p6
    ivelocity = p7
    ipan = p8
    if p10 <= 0 then
        itonic = gi_Justly_tonic
    else
        itonic = p10
    endif
    ; ampdb(vel): +p11 dB is exactly +p11 on the velocity number.
    if p11 != 0 then
        ivelocity = ivelocity + p11
    endif
    ikey PhrygianKey imid, itonic
    if p12 > 0 then
        event_i "i", "ZakianFlute", istart, iduration, ikey, ivelocity, 0, ipan, 0, 0
    else
        event_i "i", "SwirlPad", istart, iduration, ikey, ivelocity, 0, ipan, 0, 0
    endif
endin

; Cantus note: p4 start, p5 dur, p6 MIDI, p7 vel, p8 pan, p9 tonic (0 => gi_Justly_tonic)
instr Melody
    ist = p4
    idu = p5 + gi_Justly_legato
    imid = p6
    ivel = p7
    ipan = p8
    if p9 <= 0 then
        itonic = gi_Justly_tonic
    else
        itonic = p9
    endif
    schedule "JustTone", 0, 1, ist, idu, imid, ivel, ipan, 0, itonic, gi_Justly_cantus_db
endin

; Recursively schedule cantus notes from gi_Cantus (start, dur, midi, vel).
; ilegato = extra beats of overlap. idb = optional level boost.
; If itonic_alt > 0: note 0 uses ixpose/itonic; notes 1.. use ixpose_alt/itonic_alt.
; iflute > 0: JustTone fires ZakianFlute (canon section); else SwirlPad.
opcode ScheduleCantus, 0, iiiiiiiiiiiii
    indx, ientry, ibase, ixpose, ipan, itonic, ivscale, iattack, ilegato, idb, ixpose_alt, itonic_alt, iflute xin
    if indx >= gi_Cantus_nnotes igoto ScheduleCantus_done
    ist tab_i indx * 4 + 0, gi_Cantus
    idu tab_i indx * 4 + 1, gi_Cantus
    imid tab_i indx * 4 + 2, gi_Cantus
    ivel tab_i indx * 4 + 3, gi_Cantus
    ix = ixpose
    iton = itonic
    if itonic_alt > 0 && indx > 0 then
        ix = ixpose_alt
        iton = itonic_alt
    endif
    inote_st = ientry + (ist - ibase)
    inote_du = idu + ilegato
    ; Flute is section 2 only: do not let staggered/legato notes spill into section 3.
    if iflute > 0 then
        if inote_st >= gi_Justly_canon_end igoto ScheduleCantus_next
        if inote_st + inote_du > gi_Justly_canon_end then
            inote_du = gi_Justly_canon_end - inote_st
        endif
    endif
    schedule "JustTone", 0, 1, inote_st, inote_du, imid + ix, ivel * ivscale, ipan, iattack, iton, idb, iflute
ScheduleCantus_next:
    ScheduleCantus indx + 1, ientry, ibase, ixpose, ipan, itonic, ivscale, iattack, ilegato, idb, ixpose_alt, itonic_alt, iflute
ScheduleCantus_done:
endop

; One canon / section-3 cantus voice from gi_Cantus.
; p4 = entry beat, p5 = MIDI transpose for the first note (E-Phrygian spellings),
; p6 = pan, p7 = tonic MIDI class for notes after a pivot (or all notes if no pivot),
; p8 = velocity scale, p9 = attack, p10 = legato,
; p11 = optional extra dB, p12 = post-pivot MIDI transpose (0 => no pivot; all notes use p5/p7)
; Canon section (p12 != 0): each new cantus note is ZakianFlute. Section 3 stays SwirlPad.
instr CanonVoice
    i_entry = p4
    i_xpose = p5
    i_pan = p6
    i_tonic = p7
    i_vscale = p8
    if p9 <= 0 then
        iattack = 0
    else
        iattack = p9
    endif
    if p10 <= 0 then
        ilegato = gi_Justly_canon_legato
    else
        ilegato = p10
    endif
    idb = p11
    i_xpose_alt = p12
    ; Section 3 is by entry beat (do not trust p12==0: score carry can
    ; inherit the prior canon voice's xpose_alt and wrongly select flute).
    if i_entry >= gi_Justly_canon_end then
        ScheduleCantus 0, i_entry, 1.5, i_xpose, i_pan, i_tonic, i_vscale, iattack, ilegato, idb, 0, 0, 0
    elseif i_xpose_alt != 0 then
        ; Canon section: ZakianFlute on every new note; pivot then G# Phrygian.
        ScheduleCantus 0, i_entry, 1.5, i_xpose, i_pan, gi_Justly_tonic, i_vscale, iattack, ilegato, idb, i_xpose_alt, i_tonic, 1
    else
        ScheduleCantus 0, i_entry, 1.5, i_xpose, i_pan, i_tonic, i_vscale, iattack, ilegato, idb, 0, 0, 0
    endif
endin

; Accompaniment pitch for section 1 (wide tertian) or 3 (quartal): ivox 0..3.
; isect = 1 or 3. Changes only on cantus indices; upper voices may repeat (ties).
opcode HarmPitch, i, iii
    indx, ivox, isect xin
    if isect == 3 igoto HarmPitch_quartal
    ; ---- Section 1: Em / C / Am / Em / Dm / F / G / Em ----
    if indx <= 1 then
        i0 = 28
        i1 = 35
        i2 = 43
        i3 = 52
    elseif indx <= 6 then
        i0 = 28
        i1 = 35
        i2 = 43
        i3 = 50
    elseif indx <= 13 then
        i0 = 24
        i1 = 31
        i2 = 40
        i3 = 47
    elseif indx <= 18 then
        i0 = 21
        i1 = 28
        i2 = 36
        i3 = 43
    elseif indx <= 26 then
        i0 = 28
        i1 = 35
        i2 = 43
        i3 = 50
    elseif indx <= 34 then
        i0 = 26
        i1 = 33
        i2 = 41
        i3 = 48
    elseif indx <= 38 then
        i0 = 29
        i1 = 36
        i2 = 45
        i3 = 52
    elseif indx <= 42 then
        i0 = 19
        i1 = 26
        i2 = 35
        i3 = 41
    else
        i0 = 28
        i1 = 35
        i2 = 43
        i3 = 50
    endif
    igoto HarmPitch_pick
HarmPitch_quartal:
    ; ---- Section 3: stacked fourths in the same register as section 1 ----
    if indx <= 8 then
        i0 = 28
        i1 = 33
        i2 = 38
        i3 = 43
    elseif indx <= 18 then
        i0 = 29
        i1 = 35
        i2 = 40
        i3 = 45
    elseif indx <= 26 then
        i0 = 31
        i1 = 36
        i2 = 41
        i3 = 47
    elseif indx <= 34 then
        i0 = 33
        i1 = 38
        i2 = 43
        i3 = 48
    elseif indx <= 42 then
        i0 = 26
        i1 = 31
        i2 = 36
        i3 = 41
    else
        i0 = 28
        i1 = 33
        i2 = 38
        i3 = 43
    endif
HarmPitch_pick:
    if ivox == 0 then
        iout = i0
    elseif ivox == 1 then
        iout = i1
    elseif ivox == 2 then
        iout = i2
    else
        iout = i3
    endif
    xout iout
endop

; Bass: one attack per cantus note (never tied). Always into the drone field.
opcode ScheduleAccompBass, 0, iiiii
    indx, ientry, ibase, isect, itonic xin
    if indx >= gi_Cantus_nnotes igoto ScheduleAccompBass_done
    ist tab_i indx * 4 + 0, gi_Cantus
    idu tab_i indx * 4 + 1, gi_Cantus
    ivel tab_i indx * 4 + 3, gi_Cantus
    imid HarmPitch indx, 0, isect
    schedule "JustTone", 0, 1, ientry + (ist - ibase), idu + gi_Justly_legato, imid, ivel * 0.88, 0.12, 0, itonic
    ScheduleAccompBass indx + 1, ientry, ibase, isect, itonic
ScheduleAccompBass_done:
endop

; Upper voice: tied across equal HarmPitch values; changes only with cantus.
opcode ScheduleAccompTied, 0, iiiiii
    indx, ientry, ibase, ivox, isect, itonic xin
    if indx >= gi_Cantus_nnotes igoto ScheduleAccompTied_done
    ipit HarmPitch indx, ivox, isect
    iend = indx
ScheduleAccompTied_scan:
    if iend + 1 >= gi_Cantus_nnotes igoto ScheduleAccompTied_fire
    inext HarmPitch iend + 1, ivox, isect
    if inext != ipit igoto ScheduleAccompTied_fire
    iend = iend + 1
    igoto ScheduleAccompTied_scan
ScheduleAccompTied_fire:
    ist tab_i indx * 4 + 0, gi_Cantus
    i_last_st tab_i iend * 4 + 0, gi_Cantus
    i_last_du tab_i iend * 4 + 1, gi_Cantus
    idu = (i_last_st + i_last_du) - ist + gi_Justly_legato
    ivel tab_i indx * 4 + 3, gi_Cantus
    if ivox == 1 then
        ipan = 0.34
        ivelscl = 0.82
    elseif ivox == 2 then
        ipan = 0.66
        ivelscl = 0.80
    else
        ipan = 0.88
        ivelscl = 0.78
    endif
    schedule "JustTone", 0, 1, ientry + (ist - ibase), idu, ipit, ivel * ivelscl, ipan, 0, itonic
    ScheduleAccompTied iend + 1, ientry, ibase, ivox, isect, itonic
ScheduleAccompTied_done:
endop

; p4 = cantus entry beat, p5 = section (1 or 3), p6 = tonic (0 => E)
instr AccompWithCantus
    i_entry = p4
    i_sect = p5
    if p6 <= 0 then
        itonic = gi_Justly_tonic
    else
        itonic = p6
    endif
    ScheduleAccompBass 0, i_entry, 1.5, i_sect, itonic
    ScheduleAccompTied 0, i_entry, 1.5, 1, i_sect, itonic
    ScheduleAccompTied 0, i_entry, 1.5, 2, i_sect, itonic
    ScheduleAccompTied 0, i_entry, 1.5, 3, i_sect, itonic
endin

</CsInstruments>
<CsScore>

; Match gi_Justly_tempo so score end covers the full audio (plus release/reverb).
t 0 18

; ------------------------------------------------------------------
; Section 1: accomp starts with cantus and moves only on cantus onsets.
; All voices are SwirlPad notes; swirl happens in the sounding intervals.
; Pans: bass 0.12 | bari 0.34 | (cantus 0.50) | tenor 0.66 | alto 0.88
; ------------------------------------------------------------------
i "AccompWithCantus" 0 1  1.5  1  4

; ------------------------------------------------------------------
; Cantus -- Liber Mode III at E4; center of the stage (0.50)
; ------------------------------------------------------------------
; Phrase 1 -- Pange lingua gloriosi: E~ F E D | G G A C C~
i "Melody"  0 1   1.5  2.50  64  63  0.50
i "Melody"  0 1   3.5  1.85  65  61  0.50
i "Melody"  0 1   4.5  1.85  64  60  0.50
i "Melody"  0 1   5.5  1.85  62  59  0.50
i "Melody"  0 1   6.5  1.85  67  62  0.50
i "Melody"  0 1   7.5  1.85  67  61  0.50
i "Melody"  0 1   8.5  1.85  69  62  0.50
i "Melody"  0 1   9.5  1.85  72  63  0.50
i "Melody"  0 1  10.5  3.00  72  63  0.50

; Phrase 2 -- Corporis mysterium: C D C C B A | C B A G~
i "Melody"  0 1  13   1.75  72  62  0.50
i "Melody"  0 1  14   1.75  74  63  0.50
i "Melody"  0 1  15   1.75  72  61  0.50
i "Melody"  0 1  16   1.75  72  60  0.50
i "Melody"  0 1  17   1.75  71  61  0.50
i "Melody"  0 1  18   1.75  69  60  0.50
i "Melody"  0 1  19   1.75  72  62  0.50
i "Melody"  0 1  20   1.75  71  61  0.50
i "Melody"  0 1  21   1.75  69  60  0.50
i "Melody"  0 1  22   3.00  67  62  0.50

; Phrase 3 -- Sanguinisque pretiosi: G A C B A G | A G~
i "Melody"  0 1  26   1.75  67  61  0.50
i "Melody"  0 1  27   1.75  69  62  0.50
i "Melody"  0 1  28   1.75  72  63  0.50
i "Melody"  0 1  29   1.75  71  62  0.50
i "Melody"  0 1  30   1.75  69  60  0.50
i "Melody"  0 1  31   1.75  67  61  0.50
i "Melody"  0 1  32   1.75  69  62  0.50
i "Melody"  0 1  33   3.00  67  62  0.50

; Phrase 4 -- Quem in mundi pretium: A B G G E | A A D~
i "Melody"  0 1  37   1.75  69  62  0.50
i "Melody"  0 1  38   1.75  71  62  0.50
i "Melody"  0 1  39   1.75  67  60  0.50
i "Melody"  0 1  40   1.75  67  61  0.50
i "Melody"  0 1  41   1.75  64  59  0.50
i "Melody"  0 1  42   1.75  69  62  0.50
i "Melody"  0 1  43   1.75  69  61  0.50
i "Melody"  0 1  44   3.00  62  60  0.50

; Phrase 5 -- Fructus ventris generosi: E G G E | G A A G~
i "Melody"  0 1  48   1.75  64  61  0.50
i "Melody"  0 1  49   1.75  67  62  0.50
i "Melody"  0 1  50   1.75  67  61  0.50
i "Melody"  0 1  51   1.75  64  60  0.50
i "Melody"  0 1  52   1.75  67  62  0.50
i "Melody"  0 1  53   1.75  69  62  0.50
i "Melody"  0 1  54   1.75  69  61  0.50
i "Melody"  0 1  55   3.00  67  63  0.50

; Phrase 6 -- Rex effudit gentium: A B G A G F E D | E~~
i "Melody"  0 1  59   1.75  69  62  0.50
i "Melody"  0 1  60   1.75  71  62  0.50
i "Melody"  0 1  61   1.75  67  61  0.50
i "Melody"  0 1  62   1.75  69  62  0.50
i "Melody"  0 1  63   1.75  67  60  0.50
i "Melody"  0 1  64   1.75  65  61  0.50
i "Melody"  0 1  65   1.75  64  60  0.50
i "Melody"  0 1  66   1.75  62  59  0.50
; Final E hold: 6 beats = 2x earlier phrase holds (3); ends beat 73
i "Melody"  0 1  67   6.00  64  64  0.50

; ------------------------------------------------------------------
; Canon begins only after the final hold completes (beat 73).
; Each voice starts on its final-Em chord tone (E2 B2 G3 D4, E Phrygian);
; from the second note, +4 semitones to G# Phrygian (tonic class 8).
; ------------------------------------------------------------------
; Em pedal until the first voice's second note (~beat 75)
i "JustTone" 0 1  73    2.2  28  42  0.12  0  4
i "JustTone" 0 1  73.2  2.0  35  40  0.34  0  4
i "JustTone" 0 1  73.4  1.8  43  38  0.66  0  4
i "JustTone" 0 1  73.6  1.6  50  36  0.88  0  4
; G# pedal for the rest of the canon (~through beat 146)
i "JustTone" 0 1  75.2  71.8  32  42  0.12  0  8
i "JustTone" 0 1  75.4  71.6  39  40  0.34  0  8
i "JustTone" 0 1  75.6  71.4  47  38  0.66  0  8
i "JustTone" 0 1  75.8  71.2  54  36  0.88  0  8

; p5 = Em-chord entry xpose; p12 = G#-Phrygian continuation xpose
i "CanonVoice" 0 1  73      -24  0.12  8  1.00  0  0  0  -20
i "CanonVoice" 0 1  74.3333 -17  0.34  8  1.00  0  0  0  -13
i "CanonVoice" 0 1  75.6667  -9  0.66  8  1.00  0  0  0   -5
i "CanonVoice" 0 1  77        -2  0.88  8  1.00  0  0  0    2

; ------------------------------------------------------------------
; Reverse pivot into section 3: hold still in G# Phrygian, then E Phrygian.
; ------------------------------------------------------------------
i "Melody"   0 1  147  6  68  62  0.50  8
i "JustTone" 0 1  147  6  44  54  0.12  0  8
i "JustTone" 0 1  147  6  49  52  0.34  0  8
i "JustTone" 0 1  147  6  54  50  0.66  0  8
i "JustTone" 0 1  147  6  59  50  0.88  0  8

; Section 3: return to E Phrygian (p12 must be explicit 0 — score carry
; would otherwise inherit the last canon voice's G# xpose_alt and mute the cantus).
i "CanonVoice" 0 1  153  0  0.50  4  1.05  0  1.5  3  0
i "AccompWithCantus" 0 1  153  3  4

; Tail for long release + reverb after last cantus (~beat 220)
e 280

</CsScore>
</CsoundSynthesizer>
