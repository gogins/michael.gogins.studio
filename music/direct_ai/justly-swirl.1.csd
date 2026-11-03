<CsoundSynthesizer>
<CsLicense>
JUSTLY-SWIRL -- Pange Lingua; swirl inside the sounding notes

Author:     Michael Gogins
License:    Lesser GNU General Public License version 2.1
Copyright:  2026 Michael Gogins

Variant of justly.csd: same Mode III (Phrygian) Liber form, but every note of
cantus, accompaniment, and canon uses a bandlimited drone-like voice (same
partial recipe as drone.csd's GEN10, no waveshaping aliasing) through
dry+0.8*reverbsc at a slow tempo so just intervals beat and swirl without
foldover.

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

; All sounding notes: SwirlPad -> DroneField (drone.csd voice + reverb).
connect "SwirlPad", "outleft",  "DroneField",   "inleft"
connect "SwirlPad", "outright", "DroneField",   "inright"
connect "DroneField", "outleft",  "MasterOutput", "inleft"
connect "DroneField", "outright", "MasterOutput", "inright"

alwayson "DroneField"
alwayson "MasterOutput"

gk_DroneField_feedback chnexport "gk_DroneField_feedback", 3
gk_DroneField_wet chnexport "gk_DroneField_wet", 3
gk_DroneField_frequency_cutoff chnexport "gk_DroneField_frequency_cutoff", 3
gk_SwirlPad_level chnexport "gk_SwirlPad_level", 3
gk_MasterOutput_level chnexport "gk_MasterOutput_level", 3
gS_MasterOutput_filename chnexport "gS_MasterOutput_filename", 3

; Level is dB relative to drone.csd's ampdb(velocity) drive (velocity ~60).
gk_SwirlPad_level init 0
; Wet field for swirl, but darker tank so reverbsc does not ring metallic.
gk_DroneField_feedback init 0.85
gk_DroneField_wet init 0.72
gk_DroneField_frequency_cutoff init 5500
; Headroom for many overlapping voices (drone.csd uses only ~3 at once).
gk_MasterOutput_level init -12
gS_MasterOutput_filename init ""
; Slow enough that ~1.75-beat notes last ~9 s and holds ~15–30 s.
gi_Justly_tempo init 12
; Fixed tonic for the notional just Phrygian scale (E0 MIDI key class).
gi_Justly_tonic init 4
; Extra beats of overlap so just stacks ring into each other.
gi_Justly_legato init 1.5
; Cantus level boost in sections 1 and 3 (dB); canon voices stay unboosted.
gi_Justly_cantus_db init 3
; Light overlap inside canon voices so notes bloom into each other.
gi_Justly_canon_legato init 0.75

; Sine for bandlimited additive partials (drone.csd GEN10 weights, no distort).
gi_Swirl_sine ftgen 0, 0, 65536, 10, 1
; Safety limit: keep every partial below this fraction of Nyquist.
gi_Swirl_nyquist_guard init 0.45

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

; Dry + wet*gk_DroneField_wet; mild crossfeed for swirl without comb sheen.
instr DroneField
    aleftin init 0
    arightin init 0
    aleftin inleta "inleft"
    arightin inleta "inright"
    aL = aleftin
    aR = arightin + aleftin * 0.25
    aleftout, arightout reverbsc aL, aR, gk_DroneField_feedback, gk_DroneField_frequency_cutoff
    ; Extra damp on the wet only (tank cutoff already low; this softens the return).
    aleftout butterlp aleftout, 4500
    arightout butterlp arightout, 4500
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

; Drone-like voice without waveshape aliasing: bandlimited additive partials
; matching drone.csd f1 weights (1:3, 4:1, 6:2), plus a few extra odd partials
; only while n*f0 stays under gi_Swirl_nyquist_guard * sr. Swirl comes from
; just beating + wet reverb, not from folded ultrasonics.
instr SwirlPad
    i_midi_key = p4
    ivelocity = p5
    ipan = p7
    i_frequency = cpsmidinn(i_midi_key)
    iamp = ampdb(ivelocity) * ampdb(i(gk_SwirlPad_level))
    iattack = 12
    idecay = 12
    if p3 < 24 then
        iattack = p3 * 0.45
        idecay = p3 * 0.45
    endif
    if iattack < 4 then
        iattack = 4
    endif
    if idecay < 4 then
        idecay = 4
    endif
    p3 = p3 + (iattack + idecay) / 2.0
    isustain = p3 - (iattack + idecay)
    if isustain < 0.05 then
        isustain = 0.05
    endif
    kenvelope transeg 0.0, iattack, 0.0, iamp, isustain, 0.0, iamp, idecay, 0.0, 0.0
    ilimit = sr * gi_Swirl_nyquist_guard
    ; Softer top than drone.csd GEN10 (was 6th@2): chrome lived in 4–6.
    inorm = 3 + 0.85 + 0.5 + 0.35 + 0.12
    asignal = poscil3(kenvelope * (3 / inorm), i_frequency, gi_Swirl_sine)
    if i_frequency * 2 < ilimit then
        asignal += poscil3(kenvelope * (0.85 / inorm), i_frequency * 2, gi_Swirl_sine)
    endif
    if i_frequency * 3 < ilimit then
        asignal += poscil3(kenvelope * (0.5 / inorm), i_frequency * 3, gi_Swirl_sine)
    endif
    if i_frequency * 4 < ilimit then
        asignal += poscil3(kenvelope * (0.35 / inorm), i_frequency * 4, gi_Swirl_sine)
    endif
    if i_frequency * 5 < ilimit then
        asignal += poscil3(kenvelope * (0.12 / inorm), i_frequency * 5, gi_Swirl_sine)
    endif
    ; No 6th partial — that was the main steely edge on high notes.
    asignal butterlp asignal, 3800
    asignal pareq asignal, 2600, ampdb(2.5), 0.9, 0
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
; p11 optional extra dB (cantus in sections 1 and 3)
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
    event_i "i", "SwirlPad", istart, iduration, ikey, ivelocity, 0, ipan, 0, 0
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
; ilegato = extra beats of overlap (0 for canon stretto). idb = optional level boost.
; If itonic_alt > 0: note 0 uses ixpose/itonic; notes 1.. use ixpose_alt/itonic_alt.
opcode ScheduleCantus, 0, iiiiiiiiiiii
    indx, ientry, ibase, ixpose, ipan, itonic, ivscale, iattack, ilegato, idb, ixpose_alt, itonic_alt xin
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
    schedule "JustTone", 0, 1, ientry + (ist - ibase), idu + ilegato, imid + ix, ivel * ivscale, ipan, iattack, iton, idb
    ScheduleCantus indx + 1, ientry, ibase, ixpose, ipan, itonic, ivscale, iattack, ilegato, idb, ixpose_alt, itonic_alt
ScheduleCantus_done:
endop

; One canon / section-3 cantus voice from gi_Cantus.
; p4 = entry beat, p5 = MIDI transpose for the first note (E-Phrygian spellings),
; p6 = pan, p7 = tonic MIDI class for notes after a pivot (or all notes if no pivot),
; p8 = velocity scale, p9 = attack, p10 = legato,
; p11 = optional extra dB, p12 = post-pivot MIDI transpose (0 => no pivot; all notes use p5/p7)
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
    if i_xpose_alt == 0 then
        ; No pivot (section 3): every note uses p5 transpose and p7 tonic.
        ScheduleCantus 0, i_entry, 1.5, i_xpose, i_pan, i_tonic, i_vscale, iattack, ilegato, idb, 0, 0
    else
        ; Canon pivot: first note on final Em chord (E Phrygian); rest in G# Phrygian.
        ScheduleCantus 0, i_entry, 1.5, i_xpose, i_pan, gi_Justly_tonic, i_vscale, iattack, ilegato, idb, i_xpose_alt, i_tonic
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
t 0 12

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

; Section 3: return to E Phrygian
i "CanonVoice" 0 1  153  0  0.50  4  1.05  0  1.5  3
i "AccompWithCantus" 0 1  153  3  4

; Tail for long release + reverb after last cantus (~beat 220)
e 280

</CsScore>
</CsoundSynthesizer>
