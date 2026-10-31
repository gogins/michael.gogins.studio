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

Cantus and accompaniment share one smooth near-sine pad. Four accompaniment
voices move independently with close voice leading; cantus is centered,
accompaniment pans two voices per side.
</CsLicense>
<CsOptions>
-odac -d -m160 -+msg_color=0
</CsOptions>
<CsInstruments>

sr = 48000
ksmps = 32
nchnls = 2
0dbfs = 1

connect "SmoothPad", "outleft",  "ReverbSC",     "inleft"
connect "SmoothPad", "outright", "ReverbSC",     "inright"
connect "ReverbSC",  "outleft",  "MasterOutput", "inleft"
connect "ReverbSC",  "outright", "MasterOutput", "inright"

alwayson "ReverbSC"
alwayson "MasterOutput"

gk_SmoothPad_level chnexport "gk_SmoothPad_level", 3
gk_SmoothPad_midi_dynamic_range chnexport "gk_SmoothPad_midi_dynamic_range", 3
gk_SmoothPad_space_left_to_right chnexport "gk_SmoothPad_space_left_to_right", 3
gk_ReverbSC_feedback chnexport "gk_ReverbSC_feedback", 3
gk_ReverbSC_wet chnexport "gk_ReverbSC_wet", 3
gi_ReverbSC_delay_modulation chnexport "gi_ReverbSC_delay_modulation", 3
gk_ReverbSC_frequency_cutoff chnexport "gk_ReverbSC_frequency_cutoff", 3
gk_MasterOutput_level chnexport "gk_MasterOutput_level", 3
gS_MasterOutput_filename chnexport "gS_MasterOutput_filename", 3

gk_SmoothPad_level init -12
gk_SmoothPad_midi_dynamic_range init 20
gk_SmoothPad_space_left_to_right init 0.5
gk_ReverbSC_feedback init 0.94
gk_ReverbSC_wet init 0.42
gi_ReverbSC_delay_modulation init 0.009
gk_ReverbSC_frequency_cutoff init 15000
gk_MasterOutput_level init 29
gS_MasterOutput_filename init ""
gi_Justly_tempo init 60
; Fixed tonic for the notional just Phrygian scale (E0 MIDI key class).
gi_Justly_tonic init 4

gi_SmoothPad_sine ftgen 0, 0, 65537, 10, 1

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

; Map an ET MIDI spelling onto just E Phrygian (fixed tonic, all octaves).
opcode PhrygianKey, i, i
    imid xin
    ipc = (imid - gi_Justly_tonic) % 12
    ioct = int((imid - gi_Justly_tonic) / 12)
    iroot = gi_Justly_tonic + 12 * ioct
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

; p4 start (beats), p5 dur (beats), p6 ET MIDI spelling, p7 vel, p8 pan, p9 attack (0=pad default)
instr JustTone
    istart = p4 * 60 / gi_Justly_tempo
    iduration = p5 * 60 / gi_Justly_tempo
    imid = p6
    ivelocity = p7
    ipan = p8
    iattack = p9
    ikey PhrygianKey imid
    event_i "i", "SmoothPad", istart, iduration, ikey, ivelocity, iattack, ipan
endin

; Cantus: Mode III MIDI spelling through the same fixed E Phrygian map.
; Slightly quicker attack than the accompaniment so syllables speak.
; p4 start, p5 dur, p6 MIDI key, p7 velocity, p8 pan
instr Melody
    ist = p4
    idu = p5
    imid = p6
    ivel = p7
    ipan = p8
    iattack = 0.32
    schedule "JustTone", 0, 1, ist, idu, imid, ivel, ipan, iattack
endin

</CsInstruments>
<CsScore>

t 0 60

; ------------------------------------------------------------------
; Five-part texture: cantus (center, unchanged at E4) + four accomp voices.
; Wide open spacing (~7–9 semitones between adjacent chord voices), bass
; an octave-plus below the old E3; close voice-leading between changes.
; Pans: bass 0.12 | bari 0.34 | (cantus 0.50) | tenor 0.66 | alto 0.88
; Open frames (low→high): Em E1 B1 G2 D3 | C C1 G1 E2 B2 | Am A0 E1 C2 G2
; ------------------------------------------------------------------

; Bass -- roots, low; far left 0.12
; E1 C1 C1 A0 E1 D1 F1 G0 C1 E1
i "JustTone" 0 1   0   9  28  56  0.12
i "JustTone" 0 1   8.5 6  24  54  0.12
i "JustTone" 0 1  13  14  24  54  0.12
i "JustTone" 0 1  26   6  21  54  0.12
i "JustTone" 0 1  31   7  28  55  0.12
i "JustTone" 0 1  37  12  26  54  0.12
i "JustTone" 0 1  48   6  29  54  0.12
i "JustTone" 0 1  53   7  19  55  0.12
i "JustTone" 0 1  59   6  24  54  0.12
i "JustTone" 0 1  64   9  28  56  0.12

; Baritone -- ~fifth above bass; mid-left 0.34
; B1 B1 G1 G1 E1 B1 A1 C2 D1 G1 B1
i "JustTone" 0 1   0.25  5.5  35  52  0.34
i "JustTone" 0 1   5.5    4  35  52  0.34
i "JustTone" 0 1   9     5.5  31  52  0.34
i "JustTone" 0 1  13.5  13.5  31  51  0.34
i "JustTone" 0 1  26.5   5.5  28  51  0.34
i "JustTone" 0 1  31.5   6.5  35  51  0.34
i "JustTone" 0 1  37.5  11.5  33  51  0.34
i "JustTone" 0 1  48.5   5.5  36  51  0.34
i "JustTone" 0 1  53.5   6.5  26  51  0.34
i "JustTone" 0 1  59.5   5.5  31  51  0.34
i "JustTone" 0 1  64.5   8.5  35  51  0.34

; Tenor -- ~octave above bari / wide middle; mid-right 0.66
; G2 G2 E2 E2 C2 G2 F2 A2 B1 E2 G2
i "JustTone" 0 1   0.5   5.5  43  50  0.66
i "JustTone" 0 1   5.75  3.5  43  50  0.66
i "JustTone" 0 1   9     5.5  40  50  0.66
i "JustTone" 0 1  13.75 13.5  40  49  0.66
i "JustTone" 0 1  26.75  5.5  36  49  0.66
i "JustTone" 0 1  31.75  6.5  43  49  0.66
i "JustTone" 0 1  37.75 11.5  41  49  0.66
i "JustTone" 0 1  48.75  5.5  45  49  0.66
i "JustTone" 0 1  53.75  6.5  35  49  0.66
i "JustTone" 0 1  59.75  5.5  40  49  0.66
i "JustTone" 0 1  64.75  8.5  43  49  0.66

; Alto -- top of open chord, still below cantus E4; far right 0.88
; Opening: E3 (wide) then step to D3 after Phrygian F
; E3 D3 B2 B2 G2 D3 C3 E3 F2 B2 D3
i "JustTone" 0 1   0.75  4.5  52  46  0.88
i "JustTone" 0 1   5    4.5  50  48  0.88
i "JustTone" 0 1   9    5.5  47  48  0.88
i "JustTone" 0 1  14   13   47  47  0.88
i "JustTone" 0 1  27    5   43  47  0.88
i "JustTone" 0 1  32    6   50  47  0.88
i "JustTone" 0 1  38   11   48  47  0.88
i "JustTone" 0 1  49    5   52  47  0.88
i "JustTone" 0 1  54    6   41  47  0.88
i "JustTone" 0 1  60    5   47  47  0.88
i "JustTone" 0 1  65    8   50  47  0.88

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
i "Melody"  0 1  67   3.50  64  64  0.50

e 80

</CsScore>
</CsoundSynthesizer>
