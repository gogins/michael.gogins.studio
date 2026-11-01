<CsoundSynthesizer>
<CsLicense>
JUSTLY -- Pange Lingua in just 7th and 9th harmony

Author:     Michael Gogins
License:    Lesser GNU General Public License version 2.1
Copyright:  2026 Michael Gogins

The Mode III (Phrygian) cantus of Pange Lingua Gloriosi -- the Liber / Spanish
melody beginning E E F E D G G A C -- is set with just seventh and ninth
harmony. All pitches (cantus and accompaniment) are just relative to one
notional E Phrygian scale -- not retuned per chord:

    E 1/1 | F 16/15 | G 6/5 | A 4/3 | B 3/2 | C 8/5 | D 9/5

Outside the canon, accompaniment starts with the cantus and changes only on
cantus onsets: bass re-attacks every note; upper voices may tie. Cantus is
centered; accompaniment pans two voices per side.

The final Em hold lasts twice the earlier phrase holds (6 vs 3 beats). After
it completes, a four-voice canon of the cantus enters at delays of 4/3 beat
(one entry per chord voice register), modulating to just B Phrygian. After
another 6-beat hold, the piece returns to E Phrygian: four moving quartal
voices under the cantus as the high voice (centered).
</CsLicense>
<CsOptions>
-odac -d -m160 -+msg_color=0
</CsOptions>
<CsInstruments>

sr = 48000
ksmps = 32
nchnls = 2
0dbfs = 1

; SmoothPad kept in the orchestra but not played; ChebyshevPoly is the sounding instrument.
connect "SmoothPad", "outleft",  "ReverbSC",     "inleft"
connect "SmoothPad", "outright", "ReverbSC",     "inright"
connect "ChebyshevPoly", "outleft",  "ReverbSC",     "inleft"
connect "ChebyshevPoly", "outright", "ReverbSC",     "inright"
connect "ReverbSC",  "outleft",  "MasterOutput", "inleft"
connect "ReverbSC",  "outright", "MasterOutput", "inright"

alwayson "ReverbSC"
alwayson "MasterOutput"

gk_SmoothPad_level chnexport "gk_SmoothPad_level", 3
gk_SmoothPad_midi_dynamic_range chnexport "gk_SmoothPad_midi_dynamic_range", 3
gk_SmoothPad_space_left_to_right chnexport "gk_SmoothPad_space_left_to_right", 3
gk_ChebyshevPoly_level chnexport "gk_ChebyshevPoly_level", 3
gk_ChebyshevPoly_Coefficient1 chnexport "gk_ChebyshevPoly_Coefficient1", 3
gk_ChebyshevPoly_Coefficient2 chnexport "gk_ChebyshevPoly_Coefficient2", 3
gk_ChebyshevPoly_Coefficient3 chnexport "gk_ChebyshevPoly_Coefficient3", 3
gk_ChebyshevPoly_Coefficient4 chnexport "gk_ChebyshevPoly_Coefficient4", 3
gk_ChebyshevPoly_Coefficient5 chnexport "gk_ChebyshevPoly_Coefficient5", 3
gk_ChebyshevPoly_Coefficient6 chnexport "gk_ChebyshevPoly_Coefficient6", 3
gk_ChebyshevPoly_Coefficient7 chnexport "gk_ChebyshevPoly_Coefficient7", 3
gk_ChebyshevPoly_Coefficient8 chnexport "gk_ChebyshevPoly_Coefficient8", 3
gk_ChebyshevPoly_Coefficient9 chnexport "gk_ChebyshevPoly_Coefficient9", 3
gk_ChebyshevPoly_Coefficient10 chnexport "gk_ChebyshevPoly_Coefficient10", 3
gi_ChebyshevPoly_attack chnexport "gi_ChebyshevPoly_attack", 3
gi_ChebyshevPoly_release chnexport "gi_ChebyshevPoly_release", 3
gk_ChebyshevPoly_midi_dynamic_range chnexport "gk_ChebyshevPoly_midi_dynamic_range", 3
gk_ReverbSC_feedback chnexport "gk_ReverbSC_feedback", 3
gk_ReverbSC_wet chnexport "gk_ReverbSC_wet", 3
gi_ReverbSC_delay_modulation chnexport "gi_ReverbSC_delay_modulation", 3
gk_ReverbSC_frequency_cutoff chnexport "gk_ReverbSC_frequency_cutoff", 3
gk_MasterOutput_level chnexport "gk_MasterOutput_level", 3
gS_MasterOutput_filename chnexport "gS_MasterOutput_filename", 3

gk_SmoothPad_level init -12
gk_SmoothPad_midi_dynamic_range init 20
gk_SmoothPad_space_left_to_right init 0.5
gk_ChebyshevPoly_level init -28
gk_ChebyshevPoly_Coefficient1 init .5
gk_ChebyshevPoly_Coefficient2 init 1
gk_ChebyshevPoly_Coefficient3 init .2
gk_ChebyshevPoly_Coefficient4 init .1
gk_ChebyshevPoly_Coefficient5 init .01
gk_ChebyshevPoly_Coefficient6 init 0
gk_ChebyshevPoly_Coefficient7 init 0
gk_ChebyshevPoly_Coefficient8 init .1
gk_ChebyshevPoly_Coefficient9 init .01
gk_ChebyshevPoly_Coefficient10 init .05
gi_ChebyshevPoly_attack init 0.003
gi_ChebyshevPoly_release init 3.0
gk_ChebyshevPoly_midi_dynamic_range init 20
gk_ReverbSC_feedback init 0.72
gk_ReverbSC_wet init 0.28
gi_ReverbSC_delay_modulation init 0.004
gk_ReverbSC_frequency_cutoff init 9000
gk_MasterOutput_level init 30
gS_MasterOutput_filename init ""
gi_Justly_tempo init 60
; Fixed tonic for the notional just Phrygian scale (E0 MIDI key class).
gi_Justly_tonic init 4
; Extra duration (beats) for legato overlap in sections 1 and 3 (not canon).
gi_Justly_legato init 0.75

gi_SmoothPad_sine ftgen 0, 0, 65537, 10, 1
gi_ChebyshevPoly_sine ftgen 0, 0, 65537, 10, 1

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

instr ReverbSC
    gk_ReverbSC_dry = 1.0 - gk_ReverbSC_wet
    aleftin init 0
    arightin init 0
    aleftout init 0
    arightout init 0
    aleftin inleta "inleft"
    arightin inleta "inright"
    aleftout, arightout reverbsc aleftin, arightin, gk_ReverbSC_feedback, gk_ReverbSC_frequency_cutoff, sr, gi_ReverbSC_delay_modulation
    aleftoutmix = aleftin * gk_ReverbSC_dry + aleftout * gk_ReverbSC_wet
    arightoutmix = arightin * gk_ReverbSC_dry + arightout * gk_ReverbSC_wet
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
    aleft butterlp aleft, 18000
    aright butterlp aright, 18000
    a_output_left = aleft * k_gain
    a_output_right = aright * k_gain
    outs a_output_left, a_output_right
    i_amplitude_adjustment = ampdbfs(-3) / 32767
    i_filename_length strlen gS_MasterOutput_filename
    if i_filename_length > 0 then
        prints sprintf("Output filename: %s\n", gS_MasterOutput_filename)
        fout gS_MasterOutput_filename, 18, aleft * i_amplitude_adjustment, aright * i_amplitude_adjustment
    endif
    prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", \
           nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

; Kept for reference / possible return; not scheduled by JustTone.
; Near-sine / flute pad: fundamental plus a bare hint of 2nd and 3rd,
; very slow cosine attack, long release, soft lowpass.
instr SmoothPad
    i_instrument = p1
    i_time = p2
    i_duration = p3
    i_midi_key = p4
    i_midi_dynamic_range = i(gk_SmoothPad_midi_dynamic_range)
    i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
    if p7 == 0 then
        k_space_left_to_right = gk_SmoothPad_space_left_to_right
    else
        k_space_left_to_right = p7
    endif
    i_frequency = cpsmidinn(i_midi_key)
    i_level_correction = 96
    i_normalization = ampdb(-i_level_correction) / 2
    i_amplitude = ampdb(i_midi_velocity) * i_normalization
    k_gain = ampdb(gk_SmoothPad_level)

    a1 poscil 1.00, i_frequency, gi_SmoothPad_sine
    a2 poscil 0.07, i_frequency * 2.003, gi_SmoothPad_sine
    a3 poscil 0.025, i_frequency * 3.01, gi_SmoothPad_sine
    a_signal = a1 + a2 + a3
    a_signal butterlp a_signal, 2800

    ; Bare hint of attack: long raised-cosine fade in/out.
    ; p6 > 0 selects a custom attack (cantus uses a shorter one so notes speak).
    if p6 > 0 then
        i_attack = p6
    else
        i_attack = 0.85
    endif
    if i_attack > i_duration * 0.45 then
        i_attack = i_duration * 0.45
    endif
    if i_attack < 0.18 then
        i_attack = 0.18
    endif
    i_release = 1.6
    xtratim i_release
    a_envelope cossegr 0, i_attack, 1, i_duration, 1, i_release, 0
    a_envelope tonex a_envelope, 8, 4
    a_signal = a_signal * i_amplitude * a_envelope * k_gain

    a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
    outleta "outleft", a_out_left
    outleta "outright", a_out_right
    prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", \
           nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

; From patches/ChebyshevPoly.inc (Hans Mikelson / Michael Gogins) -- sounding instrument.
instr ChebyshevPoly
    i_instrument = p1
    i_time = p2
    i_sustain = p3
    i_midi_key = p4
    i_midi_dynamic_range = i(gk_ChebyshevPoly_midi_dynamic_range)
    i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
    k_space_front_to_back = p6
    k_space_left_to_right = p7
    k_space_bottom_to_top = p8
    i_phase = p9
    i_frequency = cpsmidinn(i_midi_key)
    i_level_correction = 68
    i_normalization = ampdb(-i_level_correction) / 2
    i_amplitude = ampdb(i_midi_velocity) * i_normalization
    ; Soften higher pitches (~0.275 dB per semitone above E3).
    if i_midi_key > 52 then
        i_amplitude = i_amplitude * ampdb(-(i_midi_key - 52) * 0.275)
    endif
    k_gain = ampdb(gk_ChebyshevPoly_level)
    ; Soft attack; long exponential release after note-off (not scaled to p3).
    iattack = 0.35
    idecay = gi_ChebyshevPoly_release
    if p3 > 0 && p3 / 4.0 < iattack then
        iattack = p3 / 4.0
    endif
    if iattack < 0.05 then
        iattack = 0.05
    endif
    if idecay < 0.5 then
        idecay = 0.5
    endif
    xtratim idecay + 0.05
    ; expsegr cannot reach absolute 0; floor keeps the curve valid.
    aenvelope expsegr 0.001, iattack, i_amplitude, i_sustain, i_amplitude, idecay, 0.001
    a_signal poscil3 1, i_frequency, gi_ChebyshevPoly_sine
    a_signal chebyshevpoly a_signal, 0, gk_ChebyshevPoly_Coefficient1, gk_ChebyshevPoly_Coefficient2, gk_ChebyshevPoly_Coefficient3, gk_ChebyshevPoly_Coefficient4, gk_ChebyshevPoly_Coefficient5, gk_ChebyshevPoly_Coefficient6, gk_ChebyshevPoly_Coefficient7, gk_ChebyshevPoly_Coefficient8, gk_ChebyshevPoly_Coefficient9, gk_ChebyshevPoly_Coefficient10
    a_signal *= aenvelope
    ; Attack-only click guard; release is carried by aenvelope.
    a_attack linseg 0, gi_ChebyshevPoly_attack, 1, 1, 1
    a_signal = a_signal * i_amplitude * a_attack * k_gain
    a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
    outleta "outleft", a_out_left
    outleta "outright", a_out_right
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
; p9 attack (0=pad default), p10 tonic MIDI class (<0 => gi_Justly_tonic)
instr JustTone
    istart = p4 * 60 / gi_Justly_tempo
    iduration = p5 * 60 / gi_Justly_tempo
    imid = p6
    ivelocity = p7
    ipan = p8
    iattack = p9
    if p10 <= 0 then
        itonic = gi_Justly_tonic
    else
        itonic = p10
    endif
    ikey PhrygianKey imid, itonic
    ; ChebyshevPoly: p4 key, p5 vel, p6 front/back, p7 pan, p8 elevation, p9 phase
    event_i "i", "ChebyshevPoly", istart, iduration, ikey, ivelocity, 0, ipan, 0, 0
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
    iattack = 0.32
    schedule "JustTone", 0, 1, ist, idu, imid, ivel, ipan, iattack, itonic
endin

; Recursively schedule cantus notes from gi_Cantus (start, dur, midi, vel).
; ilegato = extra beats of overlap (0 for canon stretto).
opcode ScheduleCantus, 0, iiiiiiiii
    indx, ientry, ibase, ixpose, ipan, itonic, ivscale, iattack, ilegato xin
    if indx >= gi_Cantus_nnotes igoto ScheduleCantus_done
    ist tab_i indx * 4 + 0, gi_Cantus
    idu tab_i indx * 4 + 1, gi_Cantus
    imid tab_i indx * 4 + 2, gi_Cantus
    ivel tab_i indx * 4 + 3, gi_Cantus
    schedule "JustTone", 0, 1, ientry + (ist - ibase), idu + ilegato, imid + ixpose, ivel * ivscale, ipan, iattack, itonic
    ScheduleCantus indx + 1, ientry, ibase, ixpose, ipan, itonic, ivscale, iattack, ilegato
ScheduleCantus_done:
endop

; One canon voice: full Liber cantus transposed and just-tuned to p7 tonic.
; p4 = entry beat, p5 = MIDI transpose (added to E-Phrygian spellings),
; p6 = pan, p7 = tonic MIDI class, p8 = velocity scale, p9 = attack, p10 = legato
instr CanonVoice
    i_entry = p4
    i_xpose = p5
    i_pan = p6
    i_tonic = p7
    i_vscale = p8
    if p9 <= 0 then
        iattack = 0.14
    else
        iattack = p9
    endif
    if p10 <= 0 then
        ilegato = 0
    else
        ilegato = p10
    endif
    ScheduleCantus 0, i_entry, 1.5, i_xpose, i_pan, i_tonic, i_vscale, iattack, ilegato
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
    ; ---- Section 3: stacked fourths under cantus ----
    if indx <= 8 then
        i0 = 40
        i1 = 45
        i2 = 50
        i3 = 55
    elseif indx <= 18 then
        i0 = 41
        i1 = 47
        i2 = 52
        i3 = 57
    elseif indx <= 26 then
        i0 = 43
        i1 = 48
        i2 = 53
        i3 = 59
    elseif indx <= 34 then
        i0 = 33
        i1 = 38
        i2 = 43
        i3 = 48
    elseif indx <= 42 then
        i0 = 38
        i1 = 43
        i2 = 48
        i3 = 53
    else
        i0 = 40
        i1 = 45
        i2 = 50
        i3 = 55
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

; Bass: one attack per cantus note (never tied).
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

t 0 60

; ------------------------------------------------------------------
; Section 1: accomp starts with cantus and moves only on cantus onsets.
; Bass re-attacks every cantus note; bari/ten/alto may tie across notes.
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
; Entries every 4/3 beat on final-chord tones E2 B2 G3 D4; just B Phrygian.
; Soft B-Phrygian pedal under the canon.
; ------------------------------------------------------------------
; B pedal under the canon (through last entry's cantus, ~beat 146)
i "JustTone" 0 1  73  74  23  42  0.12  0  11
i "JustTone" 0 1  73.2 74  30  40  0.34  0  11
i "JustTone" 0 1  73.4 74  38  38  0.66  0  11
i "JustTone" 0 1  73.6 74  45  36  0.88  0  11

i "CanonVoice" 0 1  73      -24  0.12  11  1.00  0.14
i "CanonVoice" 0 1  74.3333 -17  0.34  11  1.00  0.14
i "CanonVoice" 0 1  75.6667  -9  0.66  11  1.00  0.14
i "CanonVoice" 0 1  77        -2  0.88  11  1.00  0.14

; ------------------------------------------------------------------
; Post-canon double hold, then section 3: quartal under cantus.
; Accomp starts with cantus, moves only on cantus onsets; bass untied.
; ------------------------------------------------------------------
; Hold (6 beats) with cantus E -- bass re-attacks once for this hold note
i "Melody"   0 1  147  6  64  62  0.50
i "JustTone" 0 1  147  6  40  54  0.12
i "JustTone" 0 1  147  6  45  52  0.34
i "JustTone" 0 1  147  6  50  50  0.66
i "JustTone" 0 1  147  6  55  50  0.88

; Cantus + quartal accomp (tied upper voices, bass every cantus note)
i "CanonVoice" 0 1  153  0  0.50  4  1.05  0.32  0.75
i "AccompWithCantus" 0 1  153  3  4

e 235

</CsScore>
</CsoundSynthesizer>
