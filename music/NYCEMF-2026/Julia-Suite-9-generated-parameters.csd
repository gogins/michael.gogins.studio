        <CsoundSynthesizer>
          <CsOptions>
          -d -m162 -odac
          </CsOptions>
          
<CsInstruments>

; The following global variables were defined by cloud-5 and are available to 
; use in the rest of the orchestra for controlling the performance mode, 
; duration, and fadeout time of the piece.

gi_cloud5_performance_mode init 1
gi_cloud5_duration init 2000000
gi_cloud5_fadeout init 6
gS_cloud5_soundfile_name init "Julia-Suite-9.wav"


          sr = 48000
          ksmps = 128
          nchnls = 2
          0dbfs = 3
          
          ; Ensure the same random stream for each rendering.
          ; rand, randh, randi, rnd(x) and birnd(x) are not affected by seed.
          
          ;seed  81814
          ;seed  818145
          seed  88818145
          
          connect "Blower", "outleft", "ReverbSC", "inleft"
          connect "Blower", "outright", "ReverbSC", "inright"
          connect "Bower", "outleft", "ReverbSC", "inleft"
          connect "Bower", "outright", "ReverbSC", "inright"
          connect "Buzzer", "outleft", "ReverbSC", "inleft"
          connect "Buzzer", "outright", "ReverbSC", "inright"
          connect "Droner", "outleft", "ReverbSC", "inleft"
          connect "Droner", "outright", "ReverbSC", "inright"
          connect "FMWaterBell", "outleft", "ReverbSC", "inleft"
          connect "FMWaterBell", "outright", "ReverbSC", "inright"
          ; Phaser is the one that actually "buzzes" in this piece.
          connect "Phaser", "outleft", "ReverbSC", "inleft"
          connect "Phaser", "outright", "ReverbSC", "inright"
          connect "Sweeper", "outleft", "ReverbSC", "inleft"
          connect "Sweeper", "outright", "ReverbSC", "inright"
          connect "Shiner", "outleft", "ReverbSC", "inleft"
          connect "Shiner", "outright", "ReverbSC", "inright"
          connect "ZakianFlute", "outleft", "ReverbSC", "inleft"
          connect "ZakianFlute", "outright", "ReverbSC", "inright"
          connect "FilteredSines", "outleft", "ReverbSC", "inleft"
          connect "FilteredSines", "outright", "ReverbSC", "inright"
          connect "Guitar", "outleft", "ReverbSC", "inleft"
          connect "Guitar", "outleft", "ReverbSC", "inleft"
          connect "Harpsichord", "outleft", "ReverbSC", "inleft"
          connect "Harpsichord", "outright", "ReverbSC", "inright"
          connect "Kung4", "outleft", "ReverbSC", "inleft"
          connect "Kung4", "outright", "ReverbSC", "inright"
          connect "Plucked", "outleft", "ReverbSC", "inleft"
          connect "Plucked", "outright", "ReverbSC", "inright"
          connect "SeidelHarmOsc", "outleft", "ReverbSC", "inleft"
          connect "SeidelHarmOsc", "outright", "ReverbSC", "inright"
          connect "TubularBell", "outleft", "ReverbSC", "inleft"
          connect "TubularBell", "outright", "ReverbSC", "inright"
          connect "YiString", "outleft", "ReverbSC", "inleft"
          connect "YiString", "outright", "ReverbSC", "inright"
          connect "VcvOut", "outleft", "ReverbSC", "inleft"
          connect "VcvOut", "outright", "ReverbSC", "inright"
          connect "Xing", "outleft", "ReverbSC", "inleft"
          connect "Xing", "outright", "ReverbSC", "inright"
        
          connect "ReverbSC", "outleft", "MasterOutput", "inleft"
          connect "ReverbSC", "outright", "MasterOutput", "inright"
          
          alwayson "ReverbSC"
          alwayson "MasterOutput"
          
gk_Duration_factor init 0.8682696259761612 ; Updated from: gk_Duration_factor init 4.5
          
          prealloc 1, 50
          prealloc 2, 50
          prealloc 3, 50
          prealloc 4, 50
          prealloc 5, 50
          prealloc 6, 50
          prealloc 7, 50
          prealloc 8, 20
          prealloc 9, 20

          //////////////////////////////////////////////
          // Original by Steven Yi.
          // Adapted by Michael Gogins.
          //////////////////////////////////////////////
          gk_FMWaterBell_level chnexport "gk_FMWaterBell_level", 3 ; 0
          gi_FMWaterBell_attack chnexport "gi_FMWaterBell_attack", 3 ; 0.002
          gi_FMWaterBell_release chnexport "gi_FMWaterBell_release", 3 ; 0.01
          gi_FMWaterBell_sustain chnexport "gi_FMWaterBell_sustain", 3 ; 20
          gi_FMWaterBell_sustain_level chnexport "gi_FMWaterBell_sustain_level", 3 ; .1
          gk_FMWaterBell_index chnexport "gk_FMWaterBell_index", 3 ; .5
          gk_FMWaterBell_crossfade chnexport "gk_FMWaterBell_crossfade", 3 ; .5
          gk_FMWaterBell_vibrato_depth chnexport "gk_FMWaterBell_vibrato_depth", 3 ; 0.05
          gk_FMWaterBell_vibrato_rate chnexport "gk_FMWaterBell_vibrato_rate", 3 ; 6
          gk_FMWaterBell_midi_dynamic_range chnexport "gk_FMWaterBell_midi_dynamic_range", 3 ; 20
gk_FMWaterBell_level init 0 ; Updated from: gk_FMWaterBell_level init 0
gi_FMWaterBell_attack init 0.002 ; Updated from: gi_FMWaterBell_attack init 0.002
gi_FMWaterBell_release init 0.01 ; Updated from: gi_FMWaterBell_release init 0.01
gi_FMWaterBell_sustain init 20 ; Updated from: gi_FMWaterBell_sustain init 20
gi_FMWaterBell_sustain_level init 0 ; Updated from: gi_FMWaterBell_sustain_level init .1
gk_FMWaterBell_index init 0.5 ; Updated from: gk_FMWaterBell_index init .5
gk_FMWaterBell_crossfade init 0.5 ; Updated from: gk_FMWaterBell_crossfade init .5
gk_FMWaterBell_vibrato_depth init 0.05 ; Updated from: gk_FMWaterBell_vibrato_depth init 0.05
gk_FMWaterBell_vibrato_rate init 6 ; Updated from: gk_FMWaterBell_vibrato_rate init 6
          gk_FMWaterBell_midi_dynamic_range init 20
          gk_FMWaterBell_space_left_to_right chnexport "gk_FMWaterBell_space_left_to_right", 3
          gk_FMWaterBell_space_left_to_right init .5
          gi_FMWaterBell_cosine ftgen 0, 0, 65537, 11, 1
          instr FMWaterBell
          i_instrument = p1
          i_time = p2
          i_duration = p3
          ; One of the envelopes in this instrument should be releasing, and use this:
          i_sustain = 1000
          xtratim gi_FMWaterBell_attack + gi_FMWaterBell_release
          i_midi_key = p4
          i_midi_dynamic_range = i(gk_FMWaterBell_midi_dynamic_range)
          i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
          k_space_front_to_back = p6
          if p7 == 0 then
          k_space_left_to_right = gk_FMWaterBell_space_left_to_right
          else
          k_space_left_to_right = p7
          endif
          k_space_bottom_to_top = p8
          i_phase = p9
          i_frequency = cpsmidinn(i_midi_key)
          ; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
          i_level_correction = 80
          i_normalization = ampdb(-i_level_correction) / 2
          i_amplitude = ampdb(i_midi_velocity) * i_normalization * 1.6
          k_gain = ampdb(gk_FMWaterBell_level)
          i_releasing_attack = 3 / min(i_frequency, 256)
          i_releasing_release = .01
          a_signal fmbell	1, i_frequency, gk_FMWaterBell_index, gk_FMWaterBell_crossfade, gk_FMWaterBell_vibrato_depth, gk_FMWaterBell_vibrato_rate, gi_FMWaterBell_cosine, gi_FMWaterBell_cosine, gi_FMWaterBell_cosine, gi_FMWaterBell_cosine, gi_FMWaterBell_cosine ;, gi_FMWaterBell_sustain
          a_envelope transeg 0, gi_FMWaterBell_attack, 6,  1, gi_FMWaterBell_sustain, -6,  0
          a_declicking cossegr 0, i_releasing_attack, 1, gi_FMWaterBell_sustain - 1, 1, i_releasing_release, 0
          ;;;a_signal = a_signal * i_amplitude * a_envelope * a_declicking * k_gain
          a_signal = a_signal * i_amplitude * a_envelope * a_declicking * k_gain
          a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
          outleta "outleft", a_out_left
          outleta "outright", a_out_right
          prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
          ;printks "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", 1, nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_out_left)), dbamp(rms(a_out_right))
          endin
          
          gk_Phaser_attack chnexport "gk_Phaser_attack", 3 ;  .125
          gk_Phaser_release chnexport "gk_Phaser_release", 3 ;  .125
          gk_Phaser_ratio1 chnexport "gk_Phaser_ratio1", 3 ;  1
          gk_Phaser_ratio2 chnexport "gk_Phaser_ratio2", 3 ;  1/5
          gk_Phaser_index1 chnexport "gk_Phaser_index1", 3 ;  1.01
          gk_Phaser_index2 chnexport "gk_Phaser_index2", 3 ;  .103
          gk_Phaser_level chnexport "gk_Phaser_level", 3 ;  0.5
          gk_Phaser_midi_dynamic_range chnexport "gk_Phaser_midi_dynamic_range", 3 ;  20
          gk_Phaser_attack init .125
          gk_Phaser_release init .125
gk_Phaser_ratio1 init 2 ; Updated from: gk_Phaser_ratio1 init 1
gk_Phaser_ratio2 init 3 ; Updated from: gk_Phaser_ratio2 init 1/5
gk_Phaser_index1 init 1.0266159695817492 ; Updated from: gk_Phaser_index1 init 1.01
gk_Phaser_index2 init 0.533347035248176 ; Updated from: gk_Phaser_index2 init .103
gk_Phaser_level init 0 ; Updated from: gk_Phaser_level init 0.5
          gk_Phaser_midi_dynamic_range init 20
          gi_Phaser_sine ftgen 0,0,65537,10,1
          instr Phaser
          i_instrument = p1
          i_time = p2
          i_duration = p3
          i_midi_key = p4
          i_midi_dynamic_range = i(gk_Phaser_midi_dynamic_range)
          i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
          k_space_front_to_back = p6
          k_space_left_to_right = p7
          k_space_bottom_to_top = p8
          i_phase = p9
          i_frequency = cpsmidinn(i_midi_key)
          ; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
          i_level_correction = 81
          i_normalization = ampdb(-i_level_correction) / 2
          i_amplitude = ampdb(i_midi_velocity) * i_normalization
          k_gain = ampdb(gk_Phaser_level)
          i_attack = i(gk_Phaser_attack)
          i_release = i(gk_Phaser_release)
          i_sustain = 1000
          xtratim i_attack + i_release
          a_envelope transegr 0.0, i_attack / 2.0, 1.5, i_amplitude / 2.0, i_attack / 2.0, -1.5, i_amplitude, i_sustain, 0.0, i_amplitude, i_release / 2.0, 1.5, i_amplitude / 2.0, i_release / 2.0, -1.5, 0
          a1,a2 crosspm gk_Phaser_ratio1, gk_Phaser_ratio2, gk_Phaser_index1, gk_Phaser_index2, i_frequency, gi_Phaser_sine, gi_Phaser_sine
          a_signal = (a1 + a2) * k_gain * a_envelope
          prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
          a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
          outleta "outleft", a_out_left
          outleta "outright", a_out_right
          ;printks "Phaser         i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", 1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(aleft)), dbamp(rms(aright))
          endin
          
          gk_Droner_partial1 chnexport "gk_Droner_partial1", 3
          gk_Droner_partial2 chnexport "gk_Droner_partial2", 3
          gk_Droner_partial3 chnexport "gk_Droner_partial3", 3
          gk_Droner_partial4 chnexport "gk_Droner_partial4", 3
          gk_Droner_partial5 chnexport "gk_Droner_partial5", 3
          gk_Droner_partial6 chnexport "gk_Droner_partial6", 3
          gk_Droner_partial7 chnexport "gk_Droner_partial7", 3
          gk_Droner_partial8 chnexport "gk_Droner_partial8", 3
          gk_Droner_partial9 chnexport "gk_Droner_partial9", 3
          gk_Droner_partial10 chnexport "gk_Droner_partial10", 3
          gk_Droner_level chnexport "gk_Droner_level", 3
          gi_Droner_waveform chnexport "gi_Droner_waveform", 3
gk_Droner_partial1 init 0.12105641763436441 ; Updated from: gk_Droner_partial1 init .5
gk_Droner_partial2 init 0.22628712362552666 ; Updated from: gk_Droner_partial2 init .05
gk_Droner_partial3 init 0.6406330284657281 ; Updated from: gk_Droner_partial3 init .1
gk_Droner_partial4 init 0.11447949850991676 ; Updated from: gk_Droner_partial4 init .2
gk_Droner_partial5 init 0 ; Updated from: gk_Droner_partial5 init .1
          gk_Droner_partial6 init 0
          gk_Droner_partial7 init 0
          gk_Droner_partial8 init 0
          gk_Droner_partial9 init 0
          gk_Droner_partial10 init 0
gk_Droner_level init 0 ; Updated from: gk_Droner_level init 0
          gi_Droner_waveform init 0
          gk_Droner_space_left_to_right chnexport "gk_Droner_space_left_to_right", 3
          gk_Droner_space_left_to_right init .5
          gi_Droner_sine ftgen 0, 0, 65537, 10, 1, 0, .02
          instr Droner
          i_instrument = p1
          i_time = p2
          ; Make indefinite notes last no longer than the physical decay.
          i_physical_decay = 200000
          if p3 == -1 then
          i_duration = i_physical_decay
          else
          i_duration = p3
          endif
          i_midi_key = p4
          i_midi_velocity = p5
          k_space_front_to_back = p6
          if p7 ==0 then
          k_space_left_to_right = gk_Droner_space_left_to_right
          else
          k_space_left_to_right = p7
          endif
          k_space_bottom_to_top = p8
          i_phase = p9
          i_frequency = cpsmidinn(i_midi_key)
          ; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
          i_overall_amps = 19
          i_normalization = ampdb(-i_overall_amps) / 2
          i_amplitude = ampdb(i_midi_velocity) * i_normalization
          k_gain = ampdb(gk_Droner_level)
          k1 = gk_Droner_partial1
          k2 = gk_Droner_partial2
          k3 = gk_Droner_partial3
          k4 = gk_Droner_partial4
          k5 = gk_Droner_partial5
          k6 = gk_Droner_partial6
          k7 = gk_Droner_partial7
          k8 = gk_Droner_partial8
          k9 = gk_Droner_partial9
          k10 = gk_Droner_partial10
          iwaveform = gi_Droner_waveform
          iattack = .5
          idecay = .5
          isustain = p3
          aenvelope transegr 0.0, iattack / 2.0, 1.5, 1 / 2.0, iattack / 2.0, -1.5, 1, isustain, 0.0, 1, idecay / 2.0, 1.5, 1 / 2.0, idecay / 2.0, -1.5, 0
          ihertz = cpsmidinn(i_midi_key)
          if iwaveform == 0 goto i_waveform_0
          if iwaveform == 1 goto i_waveform_1
          if iwaveform == 2 goto i_waveform_2
          i_waveform_0:
          asignal poscil3 1, ihertz, gi_Droner_sine
          goto i_waveform_endif
          i_waveform_1:
          asignal vco2 1, ihertz, 8 ; integrated saw
          goto i_waveform_endif
          i_waveform_2:
          asignal vco2 1, ihertz, 12 ; triangle
          i_waveform_endif:
          a_signal chebyshevpoly asignal, 0, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10
          ;adeclicking linsegr 0, .004, 1, p3 - .014, 1, .1, 0
          ;a_signal = asignal * adeclicking
          ;
          ; The de-clicking envelope must have attack and release segments that damp 
          ; artifacts in the signal. The duration of these segments depends on 
          ; the behavior of the instrument, and may vary as a function of frequency.
          i_declick_attack = .005
          i_declick_release = .01
          ; The end of the note must be extended _past_ the end of the release segment.
          xtratim 1
          a_declicking_envelope cossegr 0, i_declick_attack, 1,  i_duration, 1,  i_declick_release, 0
          ; The envelope of the instrument is the product of the physical envelope times 
          ; the declicking envelope. 
          a_envelope = aenvelope * a_declicking_envelope
          ; That envelope is then low-pass filtered to remove most discontinuities.
          a_filtered_envelope tonex a_envelope, 40, 4
          a_signal = a_signal * i_amplitude * a_filtered_envelope * k_gain *.001
          prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
          a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
          outleta "outleft", a_out_left
          outleta "outright", a_out_right
          endin
            
          gk_Sweeper_midi_dynamic_range chnexport "gk_Sweeper_midi_dynamic_range", 3 ;  127
          gk_Sweeper_attack chnexport "gk_Sweeper_attack", 3 ;  .125
          gk_Sweeper_release chnexport "gk_Sweeper_release", 3 ;  .25
          gk_Sweeper_britel chnexport "gk_Sweeper_britel", 3 ;  0.1
          gk_Sweeper_briteh chnexport "gk_Sweeper_briteh", 3 ;  2.9
          gk_Sweeper_britels chnexport "gk_Sweeper_britels", 3 ;  2
          gk_Sweeper_britehs chnexport "gk_Sweeper_britehs", 3 ;  1
          gk_Sweeper_level chnexport "gk_Sweeper_level", 3 ;  0
          gk_Sweeper_midi_dynamic_range init 20
          gk_Sweeper_attack init .125
          gk_Sweeper_release init .25
gk_Sweeper_britel init 0.22114890555955194 ; Updated from: gk_Sweeper_britel init .01
gk_Sweeper_briteh init 2.4309937313739596 ; Updated from: gk_Sweeper_briteh init 5
gk_Sweeper_britels init 0.3526872880485048 ; Updated from: gk_Sweeper_britels init .5
gk_Sweeper_britehs init 0.7473024355153632 ; Updated from: gk_Sweeper_britehs init 1.75
gk_Sweeper_level init 0 ; Updated from: gk_Sweeper_level init 0
          gk_Sweeper_space_left_to_right chnexport "gk_Sweeper_space_left_to_right", 3
          gk_Sweeper_space_left_to_right init .5
          gi_Sweeper_sine ftgen 0, 0, 65537, 10, 1
          gi_Sweeper_octfn ftgen 0, 0, 65537, -19, 1, 0.5, 270, 0.5
          instr Sweeper
          //////////////////////////////////////////////
          // Original by Iain McCurdy.
          // Adapted by Michael Gogins.
          //////////////////////////////////////////////
          i_instrument = p1
          i_time = p2
          i_duration = p3
          i_midi_key = p4
          i_midi_dynamic_range = i(gk_Sweeper_midi_dynamic_range)
          i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
          k_space_front_to_back = p6
          if p7 ==0 then
          k_space_left_to_right = gk_Sweeper_space_left_to_right
          else
          k_space_left_to_right = p7
          endif
          k_space_bottom_to_top = p8
          i_phase = p9
          i_frequency = cpsmidinn(i_midi_key)
          ; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
          i_level_correction = 34.2
          i_normalization = ampdb(-i_level_correction) / 2
          i_amplitude = ampdb(i_midi_velocity) * i_normalization
          k_gain = ampdb(gk_Sweeper_level)
          iattack = i(gk_Sweeper_attack)
          irelease = i(gk_Sweeper_release)
          isustain = p3
          kenvelope transegr 0.0, iattack / 2.0, 1.5, i_amplitude / 2.0, iattack / 2.0, -1.5, i_amplitude, isustain, 0.0, i_amplitude, irelease / 2.0, 1.5, i_amplitude / 2.0, irelease / 2.0, -1.5, 0
          ihertz = i_frequency
          icps = ihertz
          kamp expseg 0.001,0.02,0.2,p3-0.01,0.001
          ktonemoddep jspline 0.01,0.05,0.2
          ktonemodrte jspline 6,0.1,0.2
          ktone poscil3 ktonemoddep, ktonemodrte, gi_Sweeper_sine
          ; kres rspline krangeMin, krangeMax, kcpsMin, kcpsMax
          kbrite rspline gk_Sweeper_britel, gk_Sweeper_briteh, gk_Sweeper_britels, gk_Sweeper_britehs
          ibasfreq init icps
          ioctcnt init 3
          iphs init 0
          a1 hsboscil kenvelope, ktone, kbrite, ibasfreq, gi_Sweeper_sine, gi_Sweeper_octfn, ioctcnt, iphs
          amod poscil3 0.25, ibasfreq*(1/3), gi_Sweeper_sine
          arm = a1*amod
          kmix expseg 0.001, 0.01, rnd(1), rnd(3)+0.3, 0.001
          kmix=.25
          a1 ntrpol a1, arm, kmix
          kpanrte jspline 5, 0.05, 0.1
          kpandep jspline 0.9, 0.2, 0.4
          kpan poscil3 kpandep, kpanrte, gi_Sweeper_sine
          ;a1,a2 pan2 a1, kpan
          a1,a2 pan2 a1, k_space_left_to_right
          aleft delay a1, rnd(0.1)
          aright delay a2, rnd(0.11)
          a_signal = (aleft + aright)
          ; As with most software instruments that are modeled on an impulse exciting a 
          ; resonator, there should be two envelopes. The "physical" envelope must have a 
          ; fixed decay ending at zero.
          i_declick_minimum = .003
          i_attack = .001 / i_frequency + i_declick_minimum
          i_declick_attack = i_attack
          i_declick_release = i_declick_minimum * 2
          ; The end of the note must be extended _past_ the end of the release segment.
          xtratim 1
          a_declicking_envelope cossegr 0, i_declick_attack, 1,  i_duration, 1,  i_declick_release, 0
          ; The envelope of the instrument is the product of the physical envelope times 
          ; the declicking envelope. 
          a_envelope = a_declicking_envelope
          ; That envelope is then low-pass filtered to remove most discontinuities.
          a_filtered_envelope tonex a_envelope, 40, 4
          a_signal = a_signal * i_amplitude * a_filtered_envelope * k_gain *.001
          prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
          a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
          outleta "outleft", a_out_left
          outleta "outright", a_out_right
          outs a_out_left, a_out_right
          endin
            
          gk_Buzzer_attack chnexport "gk_Buzzer_attack", 3
          gk_Buzzer_release chnexport "gk_Buzzer_release", 3
          gk_Buzzer_harmonics chnexport "gk_Buzzer_harmonics", 3
          gk_Buzzer_level chnexport "gk_Buzzer_level", 3
          gk_Buzzer_midi_dynamic_range chnexport "gk_Buzzer_midi_dynamic_range", 3
          gk_Buzzer_attack init .125
          gk_Buzzer_release init .25
gk_Buzzer_harmonics init 1.3688212927756656 ; Updated from: gk_Buzzer_harmonics init 8
gk_Buzzer_level init 0 ; Updated from: gk_Buzzer_level init 0
          gk_Buzzer_midi_dynamic_range init 20
          gi_Buzzer_sine ftgen 0, 0, 65537, 10, 1
          instr Buzzer
          i_instrument = p1
          i_time = p2
          i_duration = p3
          i_midi_key = p4
          i_midi_dynamic_range = i(gk_Buzzer_midi_dynamic_range)
          i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
          k_space_front_to_back = p6
          k_space_left_to_right = p7
          k_space_bottom_to_top = p8
          i_phase = p9
          i_frequency = cpsmidinn(i_midi_key)
          ; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
          i_level_correction = 75
          i_normalization = ampdb(-i_level_correction) / 2
          i_amplitude = ampdb(i_midi_velocity) * i_normalization
          k_gain = ampdb(gk_Buzzer_level)
          i_attack = i(gk_Buzzer_attack)
          i_release = i(gk_Buzzer_release)
          i_sustain = p3
          xtratim 1
          a_envelope transegr 0.0, i_attack / 2.0, 1.5, i_amplitude / 2.0, i_attack / 2.0, -1.5, i_amplitude, i_sustain, 0.0, i_amplitude, i_release / 2.0, 1.5, i_amplitude / 2.0, i_release / 2.0, -1.5, 0
          a_signal buzz a_envelope, i_frequency, gk_Buzzer_harmonics, gi_Buzzer_sine
          a_signal = a_signal * k_gain
          prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
          a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
          outleta "outleft", a_out_left
          outleta "outright", a_out_right
          ;printks "Buzzer         i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", 1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_out_left)), dbamp(rms(a_out_right))
          endin
            
        gk_Shiner_midi_dynamic_range chnexport "gk_Shiner_midi_dynamic_range", 3 ;  127
        gk_Shiner_attack chnexport "gk_Shiner_attack", 3 ;  .0125
        gk_Shiner_release chnexport "gk_Shiner_release", 3 ;  .0125
        gk_Shiner_level chnexport "gk_Shiner_level", 3 ;  0.5
        gk_Shiner_midi_dynamic_range init 20
gk_Shiner_attack init 0.125 ; Updated from: gk_Shiner_attack init .0125
gk_Shiner_release init 0.5 ; Updated from: gk_Shiner_release init .0125
gk_Shiner_level init 0 ; Updated from: gk_Shiner_level init -23
        gk_Shiner_front_to_back chnexport "gk_Shiner_front_to_back", 3 ;  0
        gk_Shiner_left_to_right chnexport "gk_Shiner_left_to_right", 3 ;  0.5
        gk_Shiner_bottom_to_top chnexport "gk_Shiner_bottom_to_top", 3 ;  0
        gk_Shiner_front_to_back init 0
        gk_Shiner_left_to_right init 0.5
        gk_Shiner_bottom_to_top init 0
        instr Shiner
        i_instrument = p1
        i_time = p2
        ; Make indefinite notes last no longer than the physical decay.
        i_physical_decay = 20
        if p3 == -1 then
        i_duration = i_physical_decay
        else
        i_duration = p3
        endif
        i_midi_key = p4
        i_midi_dynamic_range = i(gk_Shiner_midi_dynamic_range)
        i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
        k_space_front_to_back = p6
        if p7 ==0 then
        k_space_left_to_right = gk_Shiner_left_to_right
        else
        k_space_left_to_right = p7
        endif
        k_space_bottom_to_top = p8
        i_phase = p9
        i_frequency = cpsmidinn(i_midi_key)
        ; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
        i_level_correction = 42.5
        i_normalization = ampdb(-i_level_correction) / 2
        i_amplitude = ampdb(i_midi_velocity) * i_normalization
        k_gain = ampdb(gk_Shiner_level)
        iattack = i(gk_Shiner_attack)
        idecay = i(gk_Shiner_release)
        isustain = i_duration
        a_physical_envelope transeg 0.0, iattack / 2.0, 1.5, i_amplitude / 2.0, iattack / 2.0, -1.5, i_amplitude, isustain, 0.0, i_amplitude, idecay / 2.0, 1.5, i_amplitude / 2.0, idecay / 2.0, -1.5, 0
        ihertz = cpsmidinn(i_midi_key)
        gk_Harmonics = 1 * 20
        a_signal vco2 4, ihertz, 12
        kgain = ampdb(gk_Shiner_level) * .5
        ; The end of the note must be extended _past_ the end of the release segment.
        xtratim 1
        a_declicking_envelope cossegr 0, .005, 1,  i_duration, 1,  .01, 0
        ; The envelope of the instrument is the product of the physical envelope times 
        ; the declicking envelope. 
        a_envelope = a_physical_envelope * a_declicking_envelope
        ; That envelope is then low-pass filtered to remove most discontinuities.
        a_filtered_envelope tonex a_envelope, 40, 4
        a_signal = a_signal * i_amplitude * a_filtered_envelope * k_gain *.001
        prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
        a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
        outleta "outleft", a_out_left
        outleta "outright", a_out_right
        endin
        
        gk_Blower_grainDensity chnexport "gk_Blower_grainDensity", 3
        gk_Blower_grainDuration chnexport "gk_Blower_grainDuration", 3
        gk_Blower_grainAmplitudeRange chnexport "gk_Blower_grainAmplitudeRange", 3
        gk_Blower_grainFrequencyRange chnexport "gk_Blower_grainFrequencyRange", 3
        gk_Blower_attack chnexport "gk_Blower_attack", 3
        gk_Blower_release chnexport "gk_Blower_release", 3
        gk_Blower_level chnexport "gk_Blower_level", 3
        gk_Blower_midi_dynamic_range chnexport "gk_Blower_midi_dynamic_range", 3
gk_Blower_grainDensity init 245.730140787175 ; Updated from: gk_Blower_grainDensity init 40
gk_Blower_grainDuration init 0.03750899188161545 ; Updated from: gk_Blower_grainDuration init 0.2
gk_Blower_grainAmplitudeRange init 119.4532935977803 ; Updated from: gk_Blower_grainAmplitudeRange init 100
gk_Blower_grainFrequencyRange init 50.251772685232766 ; Updated from: gk_Blower_grainFrequencyRange init 3
gk_Blower_attack init 1.5 ; Updated from: gk_Blower_attack init 1.5
gk_Blower_release init 2 ; Updated from: gk_Blower_release init 2
gk_Blower_level init 0 ; Updated from: gk_Blower_level init 0
        gk_Blower_midi_dynamic_range init 20
        gk_Blower_space_left_to_right chnexport "gk_Blower_space_left_to_right", 3
        gk_Blower_space_left_to_right init .5
        gi_Blower_grtab ftgen 0, 0, 65537, 10, 1, .3, .1, 0, .2, .02, 0, .1, .04
        gi_Blower_wintab ftgen 0, 0, 65537, 10, 1, 0, .5, 0, .33, 0, .25, 0, .2, 0, .167
        instr Blower
        //////////////////////////////////////////////
        // Original by Hans Mikelson.
        // Adapted by Michael Gogins.
        //////////////////////////////////////////////
        i_instrument = p1
        i_time = p2
        i_duration = p3
        i_midi_key = p4
        i_midi_dynamic_range = i(gk_Blower_midi_dynamic_range)
        i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
        k_space_front_to_back = p6
        if p7 ==0 then
        k_space_left_to_right = gk_Blower_space_left_to_right
        else
        k_space_left_to_right = p7
        endif
        k_space_bottom_to_top = p8
        i_phase = p9
        i_frequency = cpsmidinn(i_midi_key)
        ; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
        i_level_correction = 123
        i_normalization = ampdb(-i_level_correction) / 2
        i_amplitude = ampdb(i_midi_velocity) * i_normalization
        k_gain = ampdb(gk_Blower_level)
        iHz = i_frequency
        ihertz = iHz
        ip4 = i_amplitude
        ip5 = iHz
        ip6 = gi_Blower_grtab
        ip7 = gi_Blower_wintab
        ip8 = 0.033
        ip8 = .002
        ip9 = 150
        ip9 = 100
        ip10 = 1.6
        ip10 = 3
        idur = p3
        iamp = i_amplitude ; p4
        ifqc = iHz ; cpspch(p5)
        igrtab = ip6
        iwintab = ip7
        ifrng = ip8
        idens = ip9
        ifade = ip10
        igdur = 0.2
        iattack = i(gk_Blower_attack)
        i_sustain = p3
        idecay = i(gk_Blower_release)
        xtratim iattack + idecay
        kenvelope transegr 0.0, iattack / 2.0, 1.5, .5, iattack / 2.0, -1.5, 1, i_sustain, 0.0, 1, idecay / 2.0, 1.5, .5, idecay / 2.0, -1.5, 0
        ; Amp Fqc Dense AmpOff PitchOff GrDur GrTable WinTable MaxGrDur
        // Maybe frequency range should really be pitch range?
        aoutl grain ip4, ifqc, gk_Blower_grainDensity, gk_Blower_grainAmplitudeRange, gk_Blower_grainFrequencyRange, gk_Blower_grainDuration, igrtab, iwintab, 5
        aoutr grain ip4, ifqc, gk_Blower_grainDensity, gk_Blower_grainAmplitudeRange, gk_Blower_grainFrequencyRange, gk_Blower_grainDuration, igrtab, iwintab, 5
        a_signal = aoutl + aoutr
        i_attack = .002
        i_release = 0.01
        ; xtratim i_attack + i_release
        a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
        ; print iattack, idecay
        a_signal = a_signal * i_amplitude * k_gain * kenvelope
        prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
        a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
        outleta "outleft", a_out_left
        outleta "outright", a_out_right
        ;printks "Blower         i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", 1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_out_left)), dbamp(rms(a_out_right))
        endin
          
          gk_ZakianFlute_midi_dynamic_range chnexport "gk_ZakianFlute_midi_dynamic_range", 3 ;  20
          gk_ZakianFlute_level chnexport "gk_ZakianFlute_level", 3 ;  0
          gk_ZakianFlute_pan chnexport "gk_ZakianFlute_pan", 3 ;  .5
          gi_ZakianFLute_seed chnexport "gi_ZakianFLute_seed", 3 ;  .5
          gi_ZakianFLute_space_left_to_front chnexport "gi_ZakianFLute_space_left_to_front", 3 ;  .5
          gk_ZakianFlute_midi_dynamic_range init 20
gk_ZakianFlute_level init 0 ; Updated from: gk_ZakianFlute_level init 0
          gk_ZakianFlute_pan init .5
          gi_ZakianFLute_seed init .5
          gi_ZakianFLute_space_left_to_front init .5
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
          instr ZakianFlute
          ; Author: Lee Zakian
          ; Adapted by: Michael Gogins
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
          i_normalization = ampdb(-i_overall_amps) / 2
          i_midi_dynamic_range = i(gk_ZakianFlute_midi_dynamic_range)
          i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
          i_amplitude = ampdb(i_midi_velocity) * i_normalization
          k_gain = ampdb(gk_ZakianFlute_level)
          ;;;xtratim iattack + irelease
          iHz = cpsmidinn(i_midi_key)
          kHz = k(iHz)
          // Bug?
          aenvelope transeg 1.0, 20.0, -10.0, 0.05
          ///aenvelope transegr 1.0, 20.0, -10.0, 0.05
          ip3 = 3;;; (p3 < 3.0 ? p3 : 3.0)
          ; parameters
          ; p4 overall amplitude scaling factor
          ip4 init i_amplitude
          ; p5 pitch in Hertz (normal pitch range: C4-C7)
          ip5 init iHz
          ; p6 percent vibrato depth, recommended values in range [-1., +1.]
          ip6 init 0.5
          ; 0.0 -> no vibrato
          ; +1. -> 1% vibrato depth, where vibrato rate increases slightly
          ; -1. -> 1% vibrato depth, where vibrato rate decreases slightly
          ; p7 attack time in seconds
          ; recommended value: .12 for slurred notes, .06 for tongued notes
          ; (.03 for short notes)
          ip7 init .08
          ; p8 decay time in seconds
          ; recommended value: .1 (.05 for short notes)
          ip8 init .08
          ; p9 overall brightness / filter cutoff factor
          ; 1 -> least bright / minimum filter cutoff frequency (40 Hz)
          ; 9 -> brightest / maximum filter cutoff frequency (10,240Hz)
          ip9 init 5
          ; initial variables
          iampscale = ip4 ; overall amplitude scaling factor
          ifreq = ip5 ; pitch in Hertz
          ivibdepth = abs(ip6*ifreq/100.0) ; vibrato depth relative to fundamental frequency
          iattack = ip7 * (1.1 - .2*gi_ZakianFLute_seed) ; attack time with up to +-10% random deviation
          gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947) ; reset gi_ZakianFLute_seed
          idecay = ip8 * (1.1 - .2*gi_ZakianFLute_seed) ; decay time with up to +-10% random deviation
          gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
          ifiltcut tablei ip9, gi_ZakianFLute_f2 ; lowpass filter cutoff frequency
          iattack = (iattack < 6/kr ? 6/kr : iattack) ; minimal attack length
          idecay = (idecay < 6/kr ? 6/kr : idecay) ; minimal decay length
          isustain = i_duration - iattack - idecay
          ;;;p3 = (isustain < 5/kr ? iattack+idecay+5/kr : i_duration) ; minimal sustain length
          isustain = (isustain < 5/kr ? 5/kr : isustain)
          iatt = iattack/6
          isus = isustain/4
          idec = idecay/6
          iphase = gi_ZakianFLute_seed ; use same phase for all wavetables
          gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
          ; vibrato block
          ; kvibdepth linseg .1, .8*p3, 1, .2*p3, .7
          kvibdepth linseg .1, .8*ip3, 1, isustain, 1, .2*ip3, .7
          kvibdepth = kvibdepth* ivibdepth ; vibrato depth
          kvibdepthr randi .1*kvibdepth, 5, gi_ZakianFLute_seed ; up to 10% vibrato depth variation
          gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
          kvibdepth = kvibdepth + kvibdepthr
          ivibr1 = gi_ZakianFLute_seed ; vibrato rate
          gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
          ivibr2 = gi_ZakianFLute_seed
          gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
          if ip6 < 0 goto vibrato1
          kvibrate linseg 2.5+ivibr1, isustain, 4.5+ivibr2 ; if p6 positive vibrato gets faster
          goto vibrato2
          vibrato1:
          ivibr3 = gi_ZakianFLute_seed
          gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
          kvibrate linseg 3.5+ivibr1, .1, 4.5+ivibr2,isustain-.1, 2.5+ivibr3 ; if p6 negative vibrato gets slower
          vibrato2:
          kvibrater randi .1*kvibrate, 5, gi_ZakianFLute_seed ; up to 10% vibrato rate variation
          gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
          kvibrate = kvibrate + kvibrater
          kvib oscili kvibdepth, kvibrate, gi_ZakianFlute_wtsin
          ifdev1 = -.03 * gi_ZakianFLute_seed ; frequency deviation
          gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
          ifdev2 = .003 * gi_ZakianFLute_seed
          gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
          ifdev3 = -.0015 * gi_ZakianFLute_seed
          gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
          ifdev4 = .012 * gi_ZakianFLute_seed
          gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
          kfreqr linseg ifdev1, iattack, ifdev2, isustain, ifdev3, idecay, ifdev4
          kfreq = kHz * (1 + kfreqr) + kvib
          if ifreq < 427.28 goto range1 ; (cpspch(8.08) + cpspch(8.09))/2
          if ifreq < 608.22 goto range2 ; (cpspch(9.02) + cpspch(9.03))/2
          if ifreq < 1013.7 goto range3 ; (cpspch(9.11) + cpspch(10.00))/2
          goto range4
          ; wavetable amplitude envelopes
          range1: ; for low range tones
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
          iwt1 = gi_ZakianFlute_f26 ; wavetable numbers
          iwt2 = gi_ZakianFlute_f27
          iwt3 = gi_ZakianFlute_f28
          inorm = 3949
          goto end
          range2: ; for low mid-range tones
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
          goto end
          range3: ; for high mid-range tones
          kamp1 linseg 0, iatt, 0.005, iatt, 0.000, iatt, -0.082, iatt, \
          0.36, iatt, 0.581, iatt, 0.416, isus, 1.073, isus, 0.000, isus, \
          0.356, isus, .86, idec, 0.532, idec, 0.162, idec, 0.076, idec, 0.064, \
          idec, 0.031, idec, 0
          kamp2 linseg 0, iatt, -0.005, iatt, 0.000, iatt, 0.205, iatt, \
          -0.284, iatt, -0.208, iatt, 0.326, isus, -0.401, isus, 1.540, isus, \
          0.589, isus, -0.486, idec, -0.016, idec, 0.141, idec, 0.105, idec, \
          -0.003, idec, -0.023, idec, 0
          kamp3 linseg 0, iatt, 0.722, iatt, 1.500, iatt, 3.697, iatt, \
          0.080, iatt, -2.327, iatt, -0.684, isus, -2.638, isus, 0.000, isus, \
          1.347, isus, 0.485, idec, -0.419, idec, -.700, idec, -0.278, idec, \
          0.167, idec, -0.059, idec, 0
          iwt1 = gi_ZakianFlute_f32
          iwt2 = gi_ZakianFlute_f33
          iwt3 = gi_ZakianFlute_f34
          inorm = 3775
          goto end
          range4: ; for high range tones
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
          goto end
          end:
          kampr1 randi .02*kamp1, 10, gi_ZakianFLute_seed ; up to 2% wavetable amplitude variation
          gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
          kamp1 = kamp1 + kampr1
          kampr2 randi .02*kamp2, 10, gi_ZakianFLute_seed ; up to 2% wavetable amplitude variation
          gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
          kamp2 = kamp2 + kampr2
          kampr3 randi .02*kamp3, 10, gi_ZakianFLute_seed ; up to 2% wavetable amplitude variation
          gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
          kamp3 = kamp3 + kampr3
          awt1 poscil kamp1, kfreq, iwt1, iphase ; wavetable lookup
          awt2 poscil kamp2, kfreq, iwt2, iphase
          awt3 poscil kamp3, kfreq, iwt3, iphase
          asig = awt1 + awt2 + awt3
          asig = asig*(iampscale/inorm)
          kcut linseg 0, iattack, ifiltcut, isustain, ifiltcut, idecay, 0 ; lowpass filter for brightness control
          afilt tone asig, kcut
          a_signal balance afilt, asig
          i_attack = .002
          i_sustain = i_duration
          i_release = 0.01
          i_declick_attack = i_attack
          i_declick_release = i_declick_attack * 2
          ; The end of the note must be extended _past_ the end of the release segment.
          xtratim 1
          a_declicking_envelope cossegr 0, i_declick_attack, 1,  i_duration, 1,  i_declick_release, 0
          ; That envelope is then low-pass filtered to remove most discontinuities.
          a_filtered_envelope tonex a_declicking_envelope, 40, 4
          a_signal = a_signal * i_amplitude * a_filtered_envelope * k_gain 
          prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
          a_signal *= .7
          a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
          outleta "outleft", a_out_left
          outleta "outright", a_out_right
          endin
            
        
          gk_Bower_midi_dynamic_range chnexport "gk_Bower_midi_dynamic_range", 3
          gk_Bower_attack chnexport "gk_Bower_attack", 3
          gk_Bower_release chnexport "gk_Bower_release", 3
          gk_Bower_level chnexport "gk_Bower_level", 3
          gk_Bower_pressure chnexport "gk_Bower_pressure", 3
          gk_Bower_space_left_to_right chnexport "gk_Bower_space_left_to_right", 3
          gk_Bower_midi_dynamic_range init 20
          gk_Bower_attack init .125
          gk_Bower_release init .125
gk_Bower_level init 0 ; Updated from: gk_Bower_level init 0
gk_Bower_pressure init 4.715856540951598 ; Updated from: gk_Bower_pressure init 0.25
          gk_Bower_space_left_to_right init 0.75
          gi_Bower_sine ftgen 0,0,65537,10,1
          instr Bower
          i_instrument = p1
          i_time = p2
          i_duration = p3
          i_midi_key = p4
          i_midi_dynamic_range = i(gk_Bower_midi_dynamic_range)
          i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
          k_space_front_to_back = p6
          if p7 == 0 then
          k_space_left_to_right = gk_Bower_space_left_to_right
          else
          k_space_left_to_right = p7
          endif
          k_space_bottom_to_top = p8
          k_space_bottom_to_top = p8
          i_phase = p9
          i_frequency = cpsmidinn(i_midi_key)
          ; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
          i_level_correction = 66
          i_normalization = ampdb(-i_level_correction) / 2
          i_amplitude = ampdb(i_midi_velocity) * i_normalization
          k_gain = ampdb(gk_Bower_level)
          iattack = i(gk_Bower_attack)
          idecay = i(gk_Bower_release)
          isustain = p3
          iamp = i_amplitude
          xtratim iattack + idecay
          kenvelope transegr 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
          ihertz = cpsmidinn(i_midi_key)
          kamp = kenvelope
          kfreq = ihertz
          kpres = 0.25
          krat rspline 0.006,0.988,1,4
          kvibf = 4.5
          kvibamp = 0
          iminfreq = i(kfreq) / 2
          aSig wgbow kamp,kfreq,gk_Bower_pressure,krat,kvibf,kvibamp,gi_Bower_sine,iminfreq
          a_signal = aSig * kenvelope * k_gain
          prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
          a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
          outleta "outleft", a_out_left
          outleta "outright", a_out_right
          ;printks "Blower         i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", 1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_out_left)), dbamp(rms(a_out_right))
          endin
            
          gk_FilteredSines_level chnexport "gk_FilteredSines_level", 3
        gi_FilteredSines_attack chnexport "gi_FilteredSines_attack", 3
        gi_FilteredSines_release chnexport "gi_FilteredSines_release", 3
        
gk_FilteredSines_level init 0 ; Updated from: gk_FilteredSines_level init 0
        gi_FilteredSines_attack init 1
          gi_FilteredSines_release init 1
        
        gi_FilteredSines_bergeman ftgen 0, 0, 65537, 10, .28, 1, .74, .66, .78, .48, .05, .33, 0.12, .08, .01, .54, 0.19, .08, .05, 0.16, .01, 0.11, .3, .02, 0.2 ; Bergeman f1
        
        instr FilteredSines
        ; Author: Michael Bergeman
        ; Modified by: Michael Gogins
        xtratim gi_FilteredSines_attack + gi_FilteredSines_release
        i_instrument = p1
        i_time = p2
        i_duration = p3
        i_midi_key = p4
        i_midi_velocity = p5
        k_space_front_to_back = p6
        k_space_left_to_right = p7
        k_space_bottom_to_top = p8
        i_phase = p9
        i_overall_amps = 166
        i_normalization = ampdb(-i_overall_amps) / 2
        i_amplitude = ampdb(i_midi_velocity) * i_normalization
        i_frequency = cpsmidinn(i_midi_key)
        k_gain = ampdb(gk_FilteredSines_level)
        kHz = k(i_frequency)
        koctave = octcps(kHz)
        iattack init gi_FilteredSines_attack
        isustain init p3
        irelease init gi_FilteredSines_release
        idb = 1.5
        ip5 = gi_FilteredSines_bergeman
        ip3 = 5.0
        ip6 = 0.9
        ip7 = 1.4
        kp8 = cpsoct(koctave - .01)
        kp9 = cpsoct(koctave + .01)
        isc = idb * .333
        k1 linseg 40, ip3, 800, p3, 800, 0.06, 0.0
        k2 linseg 440, ip3, 220, p3, 220, 0.06, 0.0
        k3 linseg 0.0, ip6, 800, ip7, 200.0, p3, 200, 0.06, 0.0
        k4 linseg 800, ip3, 40, p3, 40, 0.06, 0.0
        k5 linseg 220, ip3, 440, p3, 440, 0.06, 0.0
        k6 linseg isc, ip6, p3, ip7, p3, 0.06, 0.0
        k7 linseg 0.0, ip6, 1, ip7, .3, p3, .1, 0.06, 0.0
        a5 poscil k3, kp8, ip5
        a6 poscil k3, kp8 * 0.999, ip5
        a7 poscil k3, kp8 * 1.001, ip5
        a1 = a5 + a6 + a7
        a8 poscil k6, kp9, ip5
        a9 poscil k6, kp9 * 0.999, ip5
        a10 poscil k6, kp9 * 1.001, ip5
        a11 = a8 + a9 + a10
        a2 butterbp a1, k1, 40
        a3 butterbp a2, k5, k2 * 0.8
        a4 balance a3, a1
        a12 butterbp a11, k4, 40
        a13 butterbp a12, k2, k5 * 0.8
        a14 balance a13, a11
        a15 reverb2 a4, 5, 0.3
        a16 reverb2 a4, 4, 0.2
        a17 = (a15 + a4) * k7
        a18 = (a16 + a4) * k7
        a_signal = (a17 + a18)
        i_attack = .002
        i_sustain = p3
        i_release = 0.01
        xtratim (i_attack + i_release)
        a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
        a_signal = a_signal * i_amplitude * a_declicking * k_gain * 1.88
        
        a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
        outleta "outleft", a_out_left
        outleta "outright", a_out_right
          prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
          endin
          
          gk_Guitar_midi_dynamic_range chnexport "gk_Guitar_midi_dynamic_range", 3 ; 127
        gk_Guitar_midi_dynamic_range init 30
        gk_Guitar_level chnexport "gk_Guitar_level", 3
gk_Guitar_level init 0 ; Updated from: gk_Guitar_level init 0
        gk_Guitar_space_left_to_right chnexport "gk_Guitar_space_left_to_right", 3
        gk_Guitar_space_left_to_right init .5
        
        instr Guitar
        ; Michael Gogins
        ; Simple emulation of a Spanish guitar.
        ; Considerably cleaned up after close listening and systematic testing. 
        ; But I think the plain `pluck` opcode is inherently a bit noisy. The 
        ; waveform is just jagged at first.
        i_instrument = p1
        i_time = p2
        ; Make indefinite notes last no longer than the physical decay.
        i_physical_decay = 20
        if p3 == -1 then
        i_duration = i_physical_decay
        else
        i_duration = p3
        endif
        i_midi_key = p4
        i_midi_dynamic_range = i(gk_Guitar_midi_dynamic_range)
        i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
        k_space_front_to_back = p6
        if p7 ==0 then
        k_space_left_to_right = gk_Guitar_space_left_to_right
        else
        k_space_left_to_right = p7
        endif
        k_space_bottom_to_top = p8
        i_phase = p9
        i_frequency = cpsmidinn(i_midi_key)
        ; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
        i_level_correction = 73
        i_normalization = ampdb(-i_level_correction) / 2
        i_amplitude = ampdb(i_midi_velocity) * i_normalization
        k_gain = ampdb(gk_Guitar_level)
        i_frequency2 = i_frequency
        a_signal pluck 1.0, i_frequency, i_frequency2, 0, 6
        a_top_body reson a_signal, 110, 80
        a_bottom_body reson a_signal, 220, 100
        a_whole_body reson a_signal, 440, 80
        a_signal = (.6 * a_top_body + .8 * a_bottom_body + .6 * a_whole_body + .4 * a_signal) 
        ; For testing envelopes with a simple signal that lacks confounding artifacts.
        ; a_signal oscils .1, i_frequency2, 0
        ; For testing envelopes with a DC signal (shows only the envelope).
        ; a_signal = .25
        
        ; As with most software instruments that are modeled on an impulse exciting a 
        ; resonator, there should be two envelopes. The "physical" envelope must have a 
        ; fixed decay ending at zero.
        i_declick_minimum = .003
        i_attack = .001 / i_frequency + i_declick_minimum
        i_exponent = 7
        a_physical_envelope transeg 0,   i_attack, i_exponent,  1,   i_physical_decay, -i_exponent,  0
        ; The de-clicking envelope must have attack and release segments that damp 
        ; artifacts in the signal. The duration of these segments depends on 
        ; the behavior of the instrument, and may vary as a function of frequency.
        i_declick_attack = i_attack
        i_declick_release = i_declick_minimum * 2
        ; The end of the note must be extended _past_ the end of the release segment.
        xtratim 1
        a_declicking_envelope cossegr 0, i_declick_attack, 1,  i_duration, 1,  i_declick_release, 0
        ; The envelope of the instrument is the product of the physical envelope times 
        ; the declicking envelope. 
        a_envelope = a_physical_envelope * a_declicking_envelope
        ; That envelope is then low-pass filtered to remove most discontinuities.
        a_filtered_envelope tonex a_envelope, 40, 4
        a_signal = a_signal * i_amplitude * a_filtered_envelope * k_gain *.001
        
        a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
        outleta "outleft", a_out_left
        outleta "outright", a_out_right
        prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
        endin
        
        gk_Harpsichord_level chnexport "gk_Harpsichord_level", 3
        gk_Harpsichord_pick chnexport "gk_Harpsichord_pick", 3
        gk_Harpsichord_reflection chnexport "gk_Harpsichord_reflection", 3
        gk_Harpsichord_pluck chnexport "gk_Harpsichord_pluck", 3
        gk_Harpsichord_midi_dynamic_range chnexport "gk_Harpsichord_midi_dynamic_range", 3
        gk_Harpsichord_space_left_to_right chnexport "gk_Harpsichord_space_left_to_right", 3
        
        gk_Harpsichord_level init 0
        gk_Harpsichord_pick init .075
        gk_Harpsichord_reflection init .5
        gk_Harpsichord_pluck init .75
        gk_Harpsichord_midi_dynamic_range init 20
        gk_Harpsichord_space_left_to_right init .5
        
        gi_Harpsichord_harptable ftgen 0, 0, 65537, 7, -1, 1024, 1, 1024, -1
        
        instr 1000
        i_instrument = p1
        i_time = p2
        ; Make indefinite notes last no longer than the physical decay.
        i_physical_decay = 40
        if p3 == -1 then
        i_duration = i_physical_decay
        else
        i_duration = p3
        endif
        i_midi_key = p4
        i_midi_dynamic_range = i(gk_Harpsichord_midi_dynamic_range)
        i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
        k_space_front_to_back = p6
        if p7 == 0 then
        k_space_left_to_right = gk_Harpsichord_space_left_to_right
        else
        k_space_left_to_right = p7
        endif
        k_space_bottom_to_top = p8
        i_phase = p9
        i_frequency = cpsmidinn(i_midi_key)
        ; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
        i_level_correction = 66
        i_normalization = ampdb(-i_level_correction) / 2
        i_amplitude = ampdb(i_midi_velocity) * i_normalization
        k_gain = ampdb(gk_Harpsichord_level)
        iHz = cpsmidinn(i_midi_key)
        kHz = k(iHz)
        a_physical_envelope transeg 1.0, i_physical_decay, -25.0, 0.0
        apluck pluck i_amplitude * k_gain, kHz, iHz, 0, 1
        aharp poscil a_physical_envelope, kHz, gi_Harpsichord_harptable
        aharp2 balance apluck, aharp
        a_signal	= (apluck + aharp2)
        i_attack = .0005
        i_sustain = p3
        i_release = 0.01
        ; The end of the note must be extended _past_ the end of the release segment.
        xtratim 1
        i_declick_attack init .0008
        i_declick_release init .01
        a_declicking_envelope cossegr 0, i_declick_attack, 1,  i_duration, 1,  i_declick_release, 0
        a_envelope = a_declicking_envelope
        a_filtered_envelope tonex a_envelope, 40, 4
        a_signal = a_signal * i_amplitude * a_filtered_envelope * k_gain 
        
        a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
        outleta "outleft", a_out_left
        outleta "outright", a_out_right
        ;printks "Harpsichord      %9.4f   %9.4f\n", 0.5, a_out_left, a_out_right
        prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
        endin
         
        gi_Kung4_detune_cents chnexport "gk_Kung4_detune_cents", 3
        gi_Kung4_detune_cents init 8
        gk_Kung4_level chnexport "gk_Kung4_level", 3
gk_Kung4_level init 0 ; Updated from: gk_Kung4_level init 0
        gk_Kung4_midi_dynamic_range chnexport "gk_Kung4_midi_dynamic_range", 3
        gk_Kung4_midi_dynamic_range init 30
        gk_Kung4_space_left_to_right chnexport "gk_Kung4_space_left_to_right", 3
        gk_Kung4_space_left_to_right init .5
        gi_Kung4_modulation_ratio_start chnexport "gi_Kung4_modulation_ratio_start", 3
        gi_Kung4_modulation_ratio_start init 1.7
        gi_Kung4_modulation_ratio_end chnexport "gi_Kung4_modulation_ratio_end", 3
        gi_Kung4_modulation_ratio_end init .5
        
        gi_Kung4_sine ftgen 0, 0, 65537, 10, 1
        gi_Kung4_cosine ftgen 0, 0, 65537, 11, 1
        gi_Kung4_ln ftgen 0, 9, 65537, -12, 20.0  ;unscaled ln(I(x)) from 0 to 20.0
        /**
         * This is yet another attempt to adapt this beautiful sound for use in 
         * wider contexts.
         */
        instr Kung4
        i_instrument = p1
        i_time = p2
        ; Make indefinite notes last no longer than the physical decay.
        i_instrument_duration = 999999
        if p3 == -1 then
        i_duration = i_instrument_duration
        else
        i_duration = p3
        endif
        ;print i_duration
        i_midi_key = p4
        i_midi_dynamic_range = i(gk_Kung4_midi_dynamic_range)
        i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
        k_space_front_to_back = p6
        if p7 ==0 then
        k_space_left_to_right = gk_Kung4_space_left_to_right
        else
        k_space_left_to_right = p7
        endif
        k_space_bottom_to_top = p8
        i_phase = p9
        i_frequency = cpsmidinn(i_midi_key)
        ; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
        i_level_correction = 130 + 8 + 6
        i_normalization = ampdb(-i_level_correction) / 2
        i_amplitude = ampdb(i_midi_velocity) * i_normalization
        k_gain = ampdb(gk_Kung4_level)
        ishift = gi_Kung4_detune_cents/12000
        ipch = cpsmidinn(i_midi_key)
        ioct = octmidinn(i_midi_key)
        kvib poscil 1/120, ipch/50, gi_Kung4_sine
        ; The instrument envelope should observe limits.
        if (i_duration > 10) then
        i_instrument_attack = 10 / 3
        elseif (i_duration < 1) then
        i_instrument_attack = 1 / 3
        else
        i_instrument_attack = i_duration / 3
        endif
        ;print i_instrument_attack
        ;i_instrument_duration -= (2 * i_instrument_attack)
        aadsr linsegr 0, i_instrument_attack, 1.0, i_duration, 1.0, i_instrument_attack, 0 ;ADSR envelope
        amodi linseg 0, i_instrument_attack, 5, i_duration, 3, i_instrument_attack, 0 ;ADSR envelope for I
        amodr linseg gi_Kung4_modulation_ratio_start, i_duration, gi_Kung4_modulation_ratio_end ;r moves from p6->p7 in p3 sec.
        a1 = amodi*(amodr-1/amodr)/2
        a1ndx = abs(a1*2/20) ;a1*2 is normalized from 0-1.
        a2 = amodi*(amodr+1/amodr)/2
        a3 tablei a1ndx, gi_Kung4_ln, 1 ;lookup tbl in f3, normal index
        ao1 poscil a1, ipch, gi_Kung4_cosine ;cosine
        a4 = exp(-0.5*a3+ao1)
        ao2 poscil a2*ipch, ipch, gi_Kung4_cosine ;cosine
        a_out_left poscil 1000*aadsr*a4, ao2+cpsoct(ioct+ishift), gi_Kung4_sine ;fnl outleft
        a_out_right poscil 1000*aadsr*a4, ao2+cpsoct(ioct-ishift), gi_Kung4_sine ;fnl outright
        i_declick_minimum = .003
        i_attack = .001 / i_frequency + i_declick_minimum
        i_exponent = 7
        a_physical_envelope = aadsr ;  transeg 0,   i_attack, i_exponent,  1,   i_duration, -i_exponent,  0
        ; The de-clicking envelope must have attack and release segments that damp 
        ; artifacts in the signal. The duration of these segments depends on 
        ; the behavior of the instrument, and may vary as a function of frequency.
        i_declick_attack = i_attack
        i_declick_release = i_declick_minimum * 2
        ; The end of the note must be extended _past_ the end of the release segment.
        xtratim 5
        ;print i_declick_attack
        ;print i_declick_release
        ;print i_duration
        a_declicking_envelope cossegr 0, i_declick_attack, 1,  i_duration, 1,  i_declick_release, 0
        ; The envelope of the instrument is the product of the physical envelope times 
        ; the declicking envelope. 
        a_envelope = a_physical_envelope * a_declicking_envelope
        ; That envelope is then low-pass filtered to remove most discontinuities.
        a_filtered_envelope tonex a_envelope, 40, 4
        a_out_left = a_out_left * i_amplitude * a_filtered_envelope * k_gain 
        a_out_right = a_out_right * i_amplitude * a_filtered_envelope * k_gain 
        
        outleta "outleft", a_out_left 
        outleta "outright", a_out_right
        prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
        endin
        gk_Plucked_midi_dynamic_range chnexport "gk_Plucked_midi_dynamic_range", 3
        gk_Plucked_midi_dynamic_range init 30
        gk_Plucked_space_left_to_right chnexport "gk_Plucked_space_left_to_right", 3
        gk_Plucked_space_left_to_right init .5
        gk_Plucked_level chnexport "gk_Plucked_level", 3
gk_Plucked_level init 0 ; Updated from: gk_Plucked_level init 0
        
        gi_Plucked_sine ftgen 0, 0, 65537, 10, 1
        
        instr Plucked
        ; Author: Michael Gogins
        i_instrument = p1
        i_time = p2
        ; Make indefinite notes last no longer than the physical decay.
        i_physical_decay = 20
        if p3 == -1 then
        i_duration = i_physical_decay
        else
        i_duration = p3
        endif
        i_midi_key = p4
        i_midi_dynamic_range = i(gk_Plucked_midi_dynamic_range)
        i_midi_velocity = p5 ;* i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
        i_midi_velocity ampmidid i_midi_velocity, i_midi_dynamic_range
        k_space_front_to_back = p6
        if p7 == 0 then
        k_space_left_to_right = gk_Plucked_space_left_to_right
        else
        k_space_left_to_right = p7
        endif
        k_space_bottom_to_top = p8
        i_phase = p9
        i_detune_cents = 1.5
        i_detune = i_detune_cents / 100
        i_frequency1 = cpsmidinn(i_midi_key - i_detune)
        i_frequency2 = cpsmidinn(i_midi_key)
        i_frequency3 = cpsmidinn(i_midi_key + i_detune)
        ; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
        i_overall_amps = 0
        i_normalization = ampdb(-(i_overall_amps)) / 2
        i_amplitude = ampdb(i_midi_velocity) * i_normalization
        k_gain = ampdb(gk_Plucked_level)
        asignal1 wgpluck2 0.1, 1.0, i_frequency1, 0.25, 0.222
        asignal2 wgpluck2 0.1, 1.0, i_frequency2, 0.20, 0.223
        asignal3 wgpluck2 0.1, 1.0, i_frequency3, 0.23, 0.225
        a_signal = (asignal1 + asignal2 + asignal3)
        ; As with most instruments that are based upon an impulse delivered to a 
        ; resonator, there are two envelopes, one for the physical decay with a 
        ; fixed release ending at zero, and one with a release segment to remove 
        ; clicks from the attack and release.
        ;
        ; As with most software instruments that are modeled on an impulse exciting a 
        ; resonator, there should be two envelopes. The "physical" envelope must have a 
        ; fixed decay ending at zero.
        i_declick_minimum = .001
        i_attack = .001 / i_frequency2 + i_declick_minimum
        i_exponent = 7
        a_physical_envelope transeg 0,   i_attack, i_exponent,  1,   i_physical_decay, -i_exponent,  0
        ; The de-clicking envelope must have attack and release segments that damp 
        ; artifacts in the signal. The duration of these segments depends on 
        ; the behavior of the instrument, and may vary as a function of frequency.
        i_declick_attack = i_attack
        i_declick_release = i_declick_minimum * 2
        ; The end of the note must be extended _past_ the end of the release segment.
        xtratim 1
        a_declicking_envelope cossegr 0, i_declick_attack, 1,  i_duration, 1,  i_declick_release, 0
        ; The envelope of the instrument is the product of the physical envelope times 
        ; the declicking envelope. 
        a_envelope = a_physical_envelope * a_declicking_envelope
        ; That envelope is then low-pass filtered to remove most discontinuities.
        a_filtered_envelope tonex a_envelope, 40, 4
        a_signal = a_signal * i_amplitude * a_filtered_envelope * k_gain *.05
        
        a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
        outleta "outleft", a_out_left
        outleta "outright", a_out_right
        prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
        endin
        gi_SeidelHarmOsc_tabsz init 2^16
        
        gi_SeidelHarmOsc_Sin     ftgen 0, 0, gi_SeidelHarmOsc_tabsz, 9, 1,1,0
        gi_SeidelHarmOsc_Tri     ftgen 0, 0, gi_SeidelHarmOsc_tabsz, 9, 1,1,0,  3,0.333,180,  5,0.2,0,  7,0.143,180, 9,0.111,0, 11,0.091,180, 13,0.077,0, 15,0.067,180, 17,0.059,0, 19,0.053,180, 21,0.048,0, 23,0.043,180, 25,0.04,0, 27,0.037,180, 29,0.034,0, 31,0.032,180
        gi_SeidelHarmOsc_Saw     ftgen 0, 0, gi_SeidelHarmOsc_tabsz, 7, 0, gi_SeidelHarmOsc_tabsz/2, 1, 0, -1, gi_SeidelHarmOsc_tabsz/2, 0
        gi_SeidelHarmOsc_Square  ftgen 0, 0, gi_SeidelHarmOsc_tabsz, 7, 1, gi_SeidelHarmOsc_tabsz/2, 1, 0, -1, gi_SeidelHarmOsc_tabsz/2, -1
        gi_SeidelHarmOsc_Prime   ftgen 0, 0, gi_SeidelHarmOsc_tabsz, 9, 1,1,0,  2,0.5,0,  3,0.3333,0,  5,0.2,0,   7,0.143,0,  11,0.0909,0,  13,0.077,0,   17,0.0588,0,  19,0.0526,0, 23,0.0435,0, 27,0.037,0, 31,0.032,180
        gi_SeidelHarmOsc_Fib     ftgen 0, 0, gi_SeidelHarmOsc_tabsz, 9, 1,1,0,  2,0.5,0,  3,0.3333,0,  5,0.2,0,   8,0.125,0,  13,0.0769,0,  21,0.0476,0,  34,0.0294,0 ;,  55,0.0182,0,  89,0.0112,0, 144,0.0069,0
        
        gi_SeidelHarmOsc_NumTables = 5
        gi_SeidelHarmOsc_List ftgen 1000, 0, gi_SeidelHarmOsc_NumTables, -2, gi_SeidelHarmOsc_Tri, gi_SeidelHarmOsc_Saw, gi_SeidelHarmOsc_Square, gi_SeidelHarmOsc_Prime, gi_SeidelHarmOsc_Fib, gi_SeidelHarmOsc_Sin
        gi_SeidelHarmOsc_Morf ftgen 1001, 0, gi_SeidelHarmOsc_tabsz, 10, 1
        
        gi_SeidelHarmOsc_lforabsz init 2^13
        gi_SeidelHarmOsc_LfoTri ftgen 0, 0, gi_SeidelHarmOsc_lforabsz, 7, 0, gi_SeidelHarmOsc_lforabsz/4, 1, gi_SeidelHarmOsc_lforabsz/2, -1, gi_SeidelHarmOsc_lforabsz/4, 0
        
        gk_SeidelHarmOsc_level chnexport "gk_SeidelHarmOsc_level", 3 ;  0
        gi_SeidelHarmOsc_attack chnexport "gi_SeidelHarmOsc_attack", 3 ;  0.003
        gi_SeidelHarmOsc_petals chnexport "gi_SeidelHarmOsc_petals", 3 ;  2.99
        gi_SeidelHarmOsc_release chnexport "gi_SeidelHarmOsc_release", 3 ;  0.01
        gk_SeidelHarmOsc_midi_dynamic_range chnexport "gk_SeidelHarmOsc_midi_dynamic_range", 3 ;  20
        
        gk_SeidelHarmOsc_level init 0
        gi_SeidelHarmOsc_attack init 0.003
        gi_SeidelHarmOsc_petals init 2.99
        gi_SeidelHarmOsc_release init 0.01
        gk_SeidelHarmOsc_midi_dynamic_range init 20
        
        gk_SeidelHarmOsc_P1 chnexport "gk_SeidelHarmOsc_P1", 3
        gk_SeidelHarmOsc_IN1CON chnexport "gk_SeidelHarmOsc_IN1CON", 3
        gk_SeidelHarmOsc_IN1C1 chnexport "gk_SeidelHarmOsc_IN1C1", 3
        
        gk_SeidelHarmOsc_P1 init 0.1
        gk_SeidelHarmOsc_IN1CON init 0.1
        gk_SeidelHarmOsc_IN1C1 init 1
        
        gk_SeidelHarmOsc_P2 chnexport "gk_SeidelHarmOsc_P2", 3
        gk_SeidelHarmOsc_IN2C1 chnexport "gk_SeidelHarmOsc_IN2C1", 3
        gk_SeidelHarmOsc_IN2CON chnexport "gk_SeidelHarmOsc_IN2CON", 3
        
        gk_SeidelHarmOsc_P2 init 0.1
        gk_SeidelHarmOsc_IN2C1 init 1
        gk_SeidelHarmOsc_IN2CON init 0.1
        
        gk_SeidelHarmOsc_P3 chnexport "gk_SeidelHarmOsc_P3", 3
        gk_SeidelHarmOsc_IN3C1 chnexport "gk_SeidelHarmOsc_IN3C1", 3
        gk_SeidelHarmOsc_IN3CON chnexport "gk_SeidelHarmOsc_IN3CON", 3
        
        gk_SeidelHarmOsc_P3 init 0.1
        gk_SeidelHarmOsc_IN3C1 init 1
        gk_SeidelHarmOsc_IN3CON init 0.1
        
        gk_SeidelHarmOsc_P4 chnexport "gk_SeidelHarmOsc_P4", 3
        gk_SeidelHarmOsc_IN4C1 chnexport "gk_SeidelHarmOsc_IN4C1", 3
        gk_SeidelHarmOsc_IN4CON chnexport "gk_SeidelHarmOsc_IN4CON", 3
        
        gk_SeidelHarmOsc_P4 init 0.1
        gk_SeidelHarmOsc_IN4C1 init 1
        gk_SeidelHarmOsc_IN4CON init 0.1
        
        gi_SeidelHarmOsc_pitch_bend_table ftgen 0, 0, 1024, -7, 1, 1024, 1 
        gi_SeidelHarmOsc_sine ftgen 0, 0, 65537, 10, 1
        
        instr SeidelHarmOsc
        i_instrument = p1
        i_time = p2
        i_sustain = p3
        xtratim gi_SeidelHarmOsc_attack + gi_SeidelHarmOsc_release
        i_midi_key = p4
        i_midi_dynamic_range = i(gk_SeidelHarmOsc_midi_dynamic_range)
        i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
        // Spatial location is specified in Ambisonic coordinates.
        k_space_front_to_back = p6
        // AKA stereo pan.
        k_space_left_to_right = p7
        k_space_bottom_to_top = p8
        i_phase = p9
        i_frequency = cpsmidinn(i_midi_key)
        // Adjust the following value until "overall amps" at the end of performance is about -6 dB.
        i_level_correction = 87
        i_normalization = ampdb(-i_level_correction) / 2
        i_amplitude = ampdb(i_midi_velocity) * i_normalization
        k_gain = ampdb(gk_SeidelHarmOsc_level)
        ; prints("p1=%f, p2=%f, p3=%f, p4=%f\n", p1, p2, p3, p4)
        ;;;kfreq chnget sprintf("FREQ%d", p4)
        kfreq init i_frequency
        
        kin1con init 0
        kin2con init 0
        kin3con init 0
        kin4con init 0
        
        koff  init 0.001
        koff1 init 0.001
        koff2 init 2 * 0.001
        koff3 init 3 * 0.001
        koff4 init 5 * 0.001
        koffa = scale2(gk_SeidelHarmOsc_P1, 0.001, 0.1, -10, 10)
        if (gk_SeidelHarmOsc_IN1CON == 1) then
        koffb = scale2(gk_SeidelHarmOsc_IN1C1, 0.001, 0.1, 0, 10)
        koff = koffa + koffb
        else
        koff = koffa
        endif
        koff1 = koff
        koff2 = 2 * koff
        koff3 = 3 * koff
        koff4 = 5 * koff
        
        itbl = gi_SeidelHarmOsc_Morf
        kndx init 0
        kndxa = scale2(gk_SeidelHarmOsc_P2, 0, gi_SeidelHarmOsc_NumTables-1.01, -10, 10)
        if (gk_SeidelHarmOsc_IN2CON == 1) then
        kndxb = scale2(gk_SeidelHarmOsc_IN2C1, 0, gi_SeidelHarmOsc_NumTables-1.01, 0, 10)
        kndx = kndxa + kndxb
        else
        kndx = kndxa
        endif
        ftmorf kndx, gi_SeidelHarmOsc_List, gi_SeidelHarmOsc_Morf
        
        kamp init 0.8/9
        
        ; a1 oscil3 kamp, kfreq, itbl
        a2 oscil3 kamp, kfreq+koff1, itbl
        a3 oscil3 kamp, kfreq+koff2, itbl
        a4 oscil3 kamp, kfreq+koff3, itbl
        a5 oscil3 kamp, kfreq+koff4, itbl
        a6 oscil3 kamp, kfreq-koff1, itbl
        a7 oscil3 kamp, kfreq-koff2, itbl
        a8 oscil3 kamp, kfreq-koff3, itbl
        a9 oscil3 kamp, kfreq-koff4, itbl
        
        kdst init 0
        kdsta = scale2(gk_SeidelHarmOsc_P3, 0, 10, -10, 10)
        if (gk_SeidelHarmOsc_IN3CON == 1) then
        kdstb = scale2(gk_SeidelHarmOsc_IN3C1, 0, 10, 0, 10)
        kdst = kdsta + kdstb
        else
        kdst = kdsta
        endif
        
        aL = a2+a4+a6+a8
        aR = a3+a5+a7+a9
        aoutL = aL + distort1(aL, kdst, 0.1, 0, 0)
        aoutR = aR + distort1(aR, kdst, 0.1, 0, 0)
        
        kpana = scale2(gk_SeidelHarmOsc_P4, 0, 7, -10, 10)
        if (gk_SeidelHarmOsc_IN4CON == 1) then
        kpanb = scale2(gk_SeidelHarmOsc_IN4C1, 0, 5, 0, 10)
        kpan = kpana + kpanb
        else
        kpan = kpana
        endif
        if (kpan == 0) then
        kext = 0
        else
        kext = 0.1
        endif
        klfoL oscili 0.49, kpan-kext, gi_SeidelHarmOsc_LfoTri, 90
        klfoR oscili 0.49, kpan+kext, gi_SeidelHarmOsc_LfoTri, 270
        klfoL += 0.5
        klfoR += 0.5
        aoutL1, aoutR1 pan2 aoutL, klfoL
        aoutL2, aoutR2 pan2 aoutR, klfoR
        aoutL = aoutL1+aoutL2
        aoutR = aoutR1+aoutR2
        
        aenv madsr 0.07,0,1,0.7
        a_out_left = aoutL * aenv * k_gain * i_amplitude
        a_out_right = aoutR * aenv * k_gain * i_amplitude
        ;;; outs aoutL*aenv, aoutR*aenv
        ;;; a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
        outleta "outleft", a_out_left
        outleta "outright", a_out_right
        ;printks "WGPluck      %9.4f   %9.4f\n", 0.5, a_out_left, a_out_right
        prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
        endin
        
        gk_TubularBell_level chnexport "gk_TubularBell_level", 3
        gi_TubularBell_crossfade chnexport "gi_TubularBell_crossfade", 3
        gi_TubularBell_vibrato_depth chnexport "gi_TubularBell_vibrato_depth", 3
        gi_TubularBell_vibrato_rate chnexport "gi_TubularBell_vibrato_rate", 3
        
gk_TubularBell_level init 0 ; Updated from: gk_TubularBell_level init 0
        gi_TubularBell_crossfade init 2
        gi_TubularBell_vibrato_depth init .1
        gi_TubularBell_vibrato_rate init 5
        gk_TubularBell_midi_dynamic_range chnexport "gk_TubularBell_midi_dynamic_range", 3 ; 127
        gk_TubularBell_midi_dynamic_range init 30
        
        gk_TubularBell_space_left_to_right chnexport "gk_TubularBell_space_left_to_right", 3
        gk_TubularBell_space_left_to_right init .5
        
        gi_TubularBell_isine ftgen 0, 0, 65537, 10, 1
        gi_TubularBell_icosine ftgen 0, 0, 65537, 11, 1
        gi_TubularBell_icook3 ftgen 0, 0, 65537, 10, 1, .4, 0.2, 0.1, 0.1, .05
        
        instr TubularBell
        ; Authors: Perry Cook, John ffitch, Michael Gogins
        i_instrument = p1
        i_time = p2
        i_physical_decay = 5
        if p3 == -1 then
        i_duration = i_physical_decay
        else
        i_duration = p3
        endif
        i_midi_key = p4
        i_midi_velocity = p5
        k_space_front_to_back = p6
        if p7 ==0 then
        k_space_left_to_right = gk_TubularBell_space_left_to_right
        else
        k_space_left_to_right = p7
        endif
        k_space_bottom_to_top = p8
        i_phase = p9
        i_frequency = cpsmidinn(i_midi_key)
        ; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
        i_overall_amps = 12.5
        i_normalization = ampdb(-i_overall_amps) / 2
        i_midi_dynamic_range = i(gk_TubularBell_midi_dynamic_range)
        i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
        i_amplitude = ampdb(i_midi_velocity) * i_normalization
        k_gain = ampdb(gk_TubularBell_level)
        iattack = 0.002
        isustain = p3
        irelease = 0.05
        iindex = 1
        ifn1 = gi_TubularBell_isine
        ifn2 = gi_TubularBell_icook3
        ifn3 = gi_TubularBell_isine
        ifn4 = gi_TubularBell_isine
        ivibefn = gi_TubularBell_icosine
        a_signal fmbell 1, i_frequency, iindex, gi_TubularBell_crossfade, gi_TubularBell_vibrato_depth, gi_TubularBell_vibrato_rate, ifn1, ifn2, ifn3, ifn4, ivibefn
        i_attack = .002
        i_sustain = i_duration
        i_release = 0.01
        ; As with most software instruments that are modeled on an impulse exciting a 
        ; resonator, there should be two envelopes. The "physical" envelope must have a 
        ; fixed decay ending at zero.
        i_declick_minimum = .001
        i_attack = .001 / i_frequency + i_declick_minimum
        i_exponent = 7
        a_physical_envelope transeg 0,   i_attack, i_exponent,  1,   i_physical_decay, -i_exponent,  0
        ; The de-clicking envelope must have attack and release segments that damp 
        ; artifacts in the signal. The duration of these segments depends on 
        ; the behavior of the instrument, and may vary as a function of frequency.
        i_declick_attack = .004
        i_declick_release = i_declick_minimum * 2
        ; The end of the note must be extended _past_ the end of the release segment.
        xtratim 1
        a_declicking_envelope cossegr 0, i_declick_attack, 1,  i_duration, 1,  i_declick_release, 0
        ; The envelope of the instrument is the product of the physical envelope times 
        ; the declicking envelope. 
        a_envelope = a_physical_envelope * a_declicking_envelope
        ; That envelope is then low-pass filtered to remove most discontinuities.
        a_filtered_envelope tonex a_envelope, 120, 4
        a_signal = a_signal * i_amplitude * a_filtered_envelope * k_gain *.001
        
        a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
        outleta "outleft", a_out_left
        outleta "outright", a_out_right
        prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
        endin
        
        gk_YiString_midi_dynamic_range chnexport "gk_YiString_midi_dynamic_range", 3 ;  127
        gk_YiString_level chnexport "gk_YiString_level", 3 ;  0
        gk_YiString_reverb_send chnexport "gk_YiString_reverb_send", 3 ;  .5
        gk_YiString_chorus_send chnexport "gk_YiString_chorus_send", 3 ;  .5
        gi_YiString_overlap chnexport "gi_YiString_overlap", 3 ;  .1
        
        gk_YiString_midi_dynamic_range init 20
gk_YiString_level init 0 ; Updated from: gk_YiString_level init 0
        gk_YiString_reverb_send init .5
        gk_YiString_chorus_send init .5
        gi_YiString_overlap init .1
        
        gk_YiString_space_left_to_right chnexport "gk_YiString_space_left_to_right", 3
        gk_YiString_space_left_to_right init .5
        
        instr YiString
        //////////////////////////////////////////////
        // Original by Steven Yi.
        // Adapted by Michael Gogins.
        //////////////////////////////////////////////
        i_instrument = p1
        i_time = p2
        ; Make indefinite notes last no longer than the physical decay.
        i_physical_duration = 20000
        if p3 == -1 then
        i_duration = i_physical_duration
        else
        i_duration = p3
        endif
        i_midi_key = p4
        i_midi_dynamic_range = i(gk_YiString_midi_dynamic_range)
        i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
        k_space_front_to_back = p6
        k_space_left_to_right = p7
        k_space_bottom_to_top = p8
        i_phase = p9
        i_frequency = cpsmidinn(i_midi_key)
        ; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
        i_level_correction = 40
        i_normalization = ampdb(-i_level_correction) / 2
        i_amplitude = ampdb(i_midi_velocity) * i_normalization
        k_gain = ampdb(gk_YiString_level)
        iattack = gi_YiString_overlap
        isustain = i_duration
        idecay = gi_YiString_overlap
        xtratim 1
        a_physical_envelope transeg 0.0, iattack / 2.0, 1.5, i_amplitude / 2.0, iattack / 2.0, -1.5, i_amplitude, isustain, 0.0, i_amplitude, idecay / 2.0, 1.5, i_amplitude / 2.0, idecay / 2.0, -1.5, 0
        ;ampenv = madsr:a(1, 0.1, 0.95, 0.5)
        a_signal = vco2(1, i_frequency)
        a_signal = moogladder(a_signal, 6000, 0.1)
        
        ; The de-clicking envelope must have attack and release segments that damp 
        ; artifacts in the signal. The duration of these segments depends on 
        ; the behavior of the instrument, and may vary as a function of frequency.
        i_declick_attack = iattack
        i_declick_release = i_declick_attack * 2
        ; The end of the note must be extended _past_ the end of the release segment.
        xtratim 1
        a_declicking_envelope cossegr 0, i_declick_attack, 1,  i_duration, 1,  i_declick_release, 0
        ; The envelope of the instrument is the product of the physical envelope times 
        ; the declicking envelope. 
        a_envelope = a_physical_envelope * a_declicking_envelope
        ; That envelope is then low-pass filtered to remove most discontinuities.
        a_filtered_envelope tonex a_envelope, 40, 4
        a_signal = a_signal * i_amplitude * a_filtered_envelope * k_gain *.001
        
        a_signal_reverb = a_signal * gk_YiString_reverb_send
        a_signal_chorus = a_signal * gk_YiString_chorus_send
        a_out_left, a_out_right pan2 a_signal_reverb, p7
        outleta "outleft", a_out_left
        outleta "outright",  a_out_right
        a_out_left, a_out_right pan2 a_signal_chorus, p7
        outleta "chorusleft", a_out_left 
        outleta "chorusright", a_out_right 
        ;printks "YiString         %9.4f  %9.4f\n", 0.5, a_out_left, a_out_right
        prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
        endin
        gk_Xing_level chnexport "gk_Xing_level", 3
        
gk_Xing_level init 0 ; Updated from: gk_Xing_level init 0
        
        gi_Xing_isine ftgen 0, 0, 65537, 10, 1
        
        instr Xing
        ; Author: Andrew Horner
        i_instrument = p1
        i_time = p2
        i_duration = p3
        i_midi_key = p4
        i_midi_velocity = p5
        k_space_front_to_back = p6
        k_space_left_to_right = p7
        k_space_bottom_to_top = p8
        i_phase = p9
        i_overall_amps = 75
        i_normalization = ampdb(-i_overall_amps) / 2
        i_amplitude = ampdb(i_midi_velocity) * i_normalization
        i_frequency = cpsmidinn(i_midi_key)
        k_gain = ampdb(gk_Xing_level)
        iinstrument = p1
        istarttime = p2
        ioctave = p4
        idur = p3
        kfreq = k(i_frequency)
        iamp = 1
        inorm = 32310
        aamp1 linseg 0,.001,5200,.001,800,.001,3000,.0025,1100,.002,2800,.0015,1500,.001,2100,.011,1600,.03,1400,.95,700,1,320,1,180,1,90,1,40,1,20,1,12,1,6,1,3,1,0,1,0
        adevamp1 linseg 0, .05, .3, idur - .05, 0
        adev1 poscil adevamp1, 6.7, gi_Xing_isine, .8
        amp1 = aamp1 * (1 + adev1)
        aamp2 linseg 0,.0009,22000,.0005,7300,.0009,11000,.0004,5500,.0006,15000,.0004,5500,.0008,2200,.055,7300,.02,8500,.38,5000,.5,300,.5,73,.5,5.,5,0,1,1
        adevamp2 linseg 0,.12,.5,idur-.12,0
        adev2 poscil adevamp2, 10.5, gi_Xing_isine, 0
        amp2 = aamp2 * (1 + adev2)
        aamp3 linseg 0,.001,3000,.001,1000,.0017,12000,.0013,3700,.001,12500,.0018,3000,.0012,1200,.001,1400,.0017,6000,.0023,200,.001,3000,.001,1200,.0015,8000,.001,1800,.0015,6000,.08,1200,.2,200,.2,40,.2,10,.4,0,1,0
        adevamp3 linseg 0, .02, .8, idur - .02, 0
        adev3 poscil adevamp3, 70, gi_Xing_isine ,0
        amp3 = aamp3 * (1 + adev3)
        awt1 poscil amp1, i_frequency, gi_Xing_isine
        awt2 poscil amp2, 2.7 * i_frequency, gi_Xing_isine
        awt3 poscil amp3, 4.95 * i_frequency, gi_Xing_isine
        asig = awt1 + awt2 + awt3
        arel linenr 1,0, idur, .06
        a_signal = asig * arel * (iamp / inorm)
        i_attack = .002
        i_sustain = p3
        i_release = 0.01
        xtratim i_attack + i_release
        a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
        a_signal = a_signal * i_amplitude * a_declicking * k_gain
        
        a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
        outleta "outleft", a_out_left
        outleta "outright", a_out_right
        prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
        endin
        
        

          gk_ReverbSC_feedback chnexport "gk_ReverbSC_feedback", 3
          gk_ReverbSC_wet chnexport "gk_ReverbSC_wet", 3
          gi_ReverbSC_delay_modulation chnexport "gi_ReverbSC_delay_modulation", 3
          gk_ReverbSC_frequency_cutoff chnexport "gk_ReverbSC_frequency_cutoff", 3
          
gk_ReverbSC_feedback init 0.85 ; Updated from: gk_ReverbSC_feedback init 0.875
gk_ReverbSC_wet init 0.5 ; Updated from: gk_ReverbSC_wet init 0.5
          gi_ReverbSC_delay_modulation init 0.0075
gk_ReverbSC_frequency_cutoff init 9000 ; Updated from: gk_ReverbSC_frequency_cutoff init 15000
          
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
          prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
          endin
      
          
          gk_MasterOutput_level chnexport "gk_MasterOutput_level", 3 ; 0
          gS_MasterOutput_filename chnexport "gS_MasterOutput_filename", 3 ; ""
          
          gk_MasterOutput_level init 0
          gS_MasterOutput_filename init ""
        
          chn_k "gk_MasterOutput_output_level_left", 3
          chn_k "gk_MasterOutput_output_level_right", 3
        
          instr MasterOutput
          aleft inleta "inleft"
          aright inleta "inright"
          k_gain = ampdb(gk_MasterOutput_level)
          printks2 "Master gain: %f\n", k_gain
          iamp init 1
          aleft butterlp aleft, 18000
          aright butterlp aright, 18000
          a_out_left = aleft * k_gain
          a_out_right = aright * k_gain
          outs a_out_left, a_out_right
          ; We want something that will play on my phone.
          i_amplitude_adjustment = ampdbfs(-3) / 32767
          i_filename_length strlen gS_MasterOutput_filename
          ;if i_filename_length > 0 then
          ;prints sprintf("Output filename: %s\n", gS_MasterOutput_filename)
          ;fout gS_MasterOutput_filename, 18, a_out_left * i_amplitude_adjustment, a_out_right * i_amplitude_adjustment
          ;endif
          prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
          gk_MasterOutput_output_level_left = dbfsamp(rms(a_out_left))
          gk_MasterOutput_output_level_right = dbfsamp(rms(a_out_right))
          chnset gk_MasterOutput_output_level_left, "gk_MasterOutput_output_level_left"
          chnset gk_MasterOutput_output_level_right, "gk_MasterOutput_output_level_right"
          ;;; printks "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", 1, nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1), gk_MasterOutput_output_level_left, gk_MasterOutput_output_level_right
          endin
          
          </CsInstruments>
          <CsScore>
          f 0 [ 7 * 60 + 45]
          
i 1 0 0.3 61 87 0 0 0 0 4095 1 
i 4 0 0.3 64 89 0 0 0 0 4095 1 
i 4 0 0.3 80 91 0 0 0 0 4095 1 
i 5 0 0.3 37 92 0 0 0 0 4095 1 
i 5 0 0.3 38 90 0 0 0 0 4095 1 
i 6 0 0.3 54 93 0 0 0 0 4095 1 
i 1 0.3 0.3 59 89 0 0 0 0 4095 1 
i 2 0.3 0.3 52 99 0 0 0 0 4095 1 
i 2 0.3 0.3 57 90 0 0 0 0 4095 1 
i 3 0.3 0.3 54 91 0 0 0 0 4095 1 
i 5 0.3 0.3 52 93 0 0 0 0 4095 1 
i 6 0.3 0.3 61 100 0 0 0 0 4095 1 
i 1 0.6 0.9 50 95 0 0 0 0 4095 1 
i 1 0.6 0.3 54 85 0 0 0 0 4095 1 
i 1 0.6 0.6 71 86 0 0 0 0 4095 1 
i 2 0.6 0.3 55 88 0 0 0 0 4095 1 
i 2 0.6 0.3 74 83 0 0 0 0 4095 1 
i 4 0.6 0.3 85 98 0 0 0 0 4095 1 
i 1 0.9 0.3 74 90 0 0 0 0 4095 1 
i 1 0.9 0.3 76 97 0 0 0 0 4095 1 
i 4 0.9 0.3 62 88 0 0 0 0 4095 1 
i 6 0.9 0.3 69 88 0 0 0 0 4095 1 
i 1 1.2 0.3 79 96 0 0 0 0 4095 1 
i 1 1.2 0.3 81 89 0 0 0 0 4095 1 
i 2 1.2 0.3 73 98 0 0 0 0 4095 1 
i 4 1.2 0.3 71 90 0 0 0 0 4095 1 
i 6 1.2 0.3 73 91 0 0 0 0 4095 1 
i 2 1.5 0.3 81 87 0 0 0 0 4095 1 
i 3 1.5 0.3 50 86 0 0 0 0 4095 1 
i 4 1.5 0.3 67 85 0 0 0 0 4095 1 
i 4 1.5 0.3 74 86 0 0 0 0 4095 1 
i 4 1.5 0.3 79 85 0 0 0 0 4095 1 
i 1 1.8 0.3 78 89 0 0 0 0 4095 1 
i 2 1.8 0.3 71 91 0 0 0 0 4095 1 
i 2 1.8 0.6 76 97 0 0 0 0 4095 1 
i 5 1.8 0.3 55 88 0 0 0 0 4095 1 
i 6 1.8 0.3 71 89 0 0 0 0 4095 1 
i 1 2.1 0.3 76 92 0 0 0 0 4095 1 
i 3 2.1 0.3 50 88 0 0 0 0 4095 1 
i 3 2.1 0.3 69 97 0 0 0 0 4095 1 
i 3 2.1 0.3 73 95 0 0 0 0 4095 1 
i 4 2.1 0.3 55 88 0 0 0 0 4095 1 
i 2 2.4 0.3 69 92 0 0 0 0 4095 1 
i 3 2.4 0.3 76 84 0 0 0 0 4095 1 
i 5 2.4 0.9 62 99 0 0 0 0 4095 1 
i 6 2.4 0.3 71 95 0 0 0 0 4095 1 
i 2 2.7 0.3 71 93 0 0 0 0 4095 1 
i 2 2.7 0.3 74 92 0 0 0 0 4095 1 
i 4 2.7 0.3 54 89 0 0 0 0 4095 1 
i 5 2.7 0.3 52 93 0 0 0 0 4095 1 
i 6 2.7 0.3 74 91 0 0 0 0 4095 1 
i 2 3 0.6 69 99 0 0 0 0 4095 1 
i 2 3 0.3 73 96 0 0 0 0 4095 1 
i 3 3 0.3 66 91 0 0 0 0 4095 1 
i 3 3 0.3 71 88 0 0 0 0 4095 1 
i 4 3 0.3 49 87 0 0 0 0 4095 1 
i 2 3.3 0.3 66 91 0 0 0 0 4095 1 
i 3 3.3 0.3 76 91 0 0 0 0 4095 1 
i 5 3.3 0.3 57 94 0 0 0 0 4095 1 
i 6 3.3 0.3 62 89 0 0 0 0 4095 1 
i 1 3.6 0.3 80 91 0 0 0 0 4095 1 
i 1 3.6 0.3 85 87 0 0 0 0 4095 1 
i 2 3.6 0.3 62 86 0 0 0 0 4095 1 
i 2 3.6 0.3 74 86 0 0 0 0 4095 1 
i 3 3.6 0.3 66 96 0 0 0 0 4095 1 
i 3 3.6 0.3 71 89 0 0 0 0 4095 1 
i 1 3.9 0.3 71 87 0 0 0 0 4095 1 
i 4 3.9 0.6 52 83 0 0 0 0 4095 1 
i 4 3.9 0.3 78 88 0 0 0 0 4095 1 
i 5 3.9 0.3 62 98 0 0 0 0 4095 1 
i 1 4.2 0.3 73 87 0 0 0 0 4095 1 
i 2 4.2 0.3 69 98 0 0 0 0 4095 1 
i 4 4.2 0.3 66 88 0 0 0 0 4095 1 
i 5 4.2 0.6 57 85 0 0 0 0 4095 1 
i 6 4.2 0.3 76 96 0 0 0 0 4095 1 
i 3 4.5 0.3 61 87 0 0 0 0 4095 1 
i 4 4.5 0.3 47 93 0 0 0 0 4095 1 
i 5 4.5 0.3 54 87 0 0 0 0 4095 1 
i 6 4.5 0.6 75 86 0 0 0 0 4095 1 
i 3 4.8 0.3 59 82 0 0 0 0 4095 1 
i 5 4.8 0.3 47 97 0 0 0 0 4095 1 
i 5 4.8 0.9 51 86 0 0 0 0 4095 1 
i 6 4.8 0.6 71 99 0 0 0 0 4095 1 
i 1 5.1 0.3 69 83 0 0 0 0 4095 1 
i 2 5.1 0.3 66 87 0 0 0 0 4095 1 
i 3 5.1 0.3 54 93 0 0 0 0 4095 1 
i 3 5.1 0.3 75 87 0 0 0 0 4095 1 
i 1 5.4 0.3 61 84 0 0 0 0 4095 1 
i 2 5.4 0.3 63 82 0 0 0 0 4095 1 
i 5 5.4 0.3 52 90 0 0 0 0 4095 1 
i 6 5.4 0.3 66 90 0 0 0 0 4095 1 
i 6 5.4 0.3 68 90 0 0 0 0 4095 1 
i 1 5.7 0.3 71 93 0 0 0 0 4095 1 
i 2 5.7 0.3 59 89 0 0 0 0 4095 1 
i 4 5.7 0.3 52 84 0 0 0 0 4095 1 
i 5 5.7 0.3 47 86 0 0 0 0 4095 1 
i 5 5.7 0.3 61 85 0 0 0 0 4095 1 
i 6 5.7 0.3 49 83 0 0 0 0 4095 1 
i 1 6 0.3 76 87 0 0 0 0 4095 1 
i 1 6 0.3 80 87 0 0 0 0 4095 1 
i 4 6 0.3 59 84 0 0 0 0 4095 1 
i 5 6 0.3 66 84 0 0 0 0 4095 1 
i 5 6 0.3 71 83 0 0 0 0 4095 1 
i 6 6 0.9 73 91 0 0 0 0 4095 1 
i 3 6.3 0.3 68 95 0 0 0 0 4095 1 
i 3 6.3 0.3 74 90 0 0 0 0 4095 1 
i 6 6.3 0.3 71 90 0 0 0 0 4095 1 
i 1 6.6 0.3 54 86 0 0 0 0 4095 1 
i 1 6.6 0.3 56 92 0 0 0 0 4095 1 
i 1 6.6 0.3 61 92 0 0 0 0 4095 1 
i 2 6.6 0.3 59 90 0 0 0 0 4095 1 
i 5 6.6 0.3 69 88 0 0 0 0 4095 1 
i 1 6.9 0.6 59 80 0 0 0 0 4095 1 
i 2 6.9 0.3 64 81 0 0 0 0 4095 1 
i 3 6.9 0.3 73 81 0 0 0 0 4095 1 
i 4 6.9 0.3 59 85 0 0 0 0 4095 1 
i 4 6.9 0.3 62 91 0 0 0 0 4095 1 
i 1 7.2 0.3 57 82 0 0 0 0 4095 1 
i 2 7.2 0.3 66 91 0 0 0 0 4095 1 
i 4 7.2 0.3 61 84 0 0 0 0 4095 1 
i 6 7.2 0.3 73 84 0 0 0 0 4095 1 
i 2 7.5 0.3 71 87 0 0 0 0 4095 1 
i 2 7.5 0.6 73 85 0 0 0 0 4095 1 
i 3 7.5 0.3 74 91 0 0 0 0 4095 1 
i 3 7.5 0.3 76 92 0 0 0 0 4095 1 
i 5 7.5 0.3 71 86 0 0 0 0 4095 1 
i 5 7.5 0.3 76 85 0 0 0 0 4095 1 
i 1 7.8 0.3 56 83 0 0 0 0 4095 1 
i 5 7.8 0.3 57 91 0 0 0 0 4095 1 
i 5 7.8 0.3 61 84 0 0 0 0 4095 1 
i 6 7.8 0.3 54 100 0 0 0 0 4095 1 
i 1 8.1 0.3 61 95 0 0 0 0 4095 1 
i 1 8.1 0.3 76 89 0 0 0 0 4095 1 
i 3 8.1 0.6 74 85 0 0 0 0 4095 1 
i 4 8.1 0.3 57 91 0 0 0 0 4095 1 
i 5 8.1 0.3 47 86 0 0 0 0 4095 1 
i 5 8.1 0.6 50 84 0 0 0 0 4095 1 
i 2 8.4 0.3 74 83 0 0 0 0 4095 1 
i 1 8.7 0.3 59 88 0 0 0 0 4095 1 
i 2 8.7 0.6 59 84 0 0 0 0 4095 1 
i 4 8.7 0.3 74 89 0 0 0 0 4095 1 
i 1 9 0.3 62 89 0 0 0 0 4095 1 
i 3 9 0.3 74 85 0 0 0 0 4095 1 
i 4 9 0.3 59 86 0 0 0 0 4095 1 
i 6 9 0.3 74 87 0 0 0 0 4095 1 
i 1 9.3 0.3 73 89 0 0 0 0 4095 1 
i 2 9.3 0.3 57 92 0 0 0 0 4095 1 
i 4 9.3 0.3 57 97 0 0 0 0 4095 1 
i 5 9.3 0.3 49 87 0 0 0 0 4095 1 
i 1 9.6 0.3 56 86 0 0 0 0 4095 1 
i 2 9.6 0.3 56 89 0 0 0 0 4095 1 
i 2 9.6 0.3 76 97 0 0 0 0 4095 1 
i 3 9.6 0.3 69 85 0 0 0 0 4095 1 
i 4 9.6 0.3 62 85 0 0 0 0 4095 1 
i 6 9.6 0.3 64 93 0 0 0 0 4095 1 
i 1 9.9 0.3 61 84 0 0 0 0 4095 1 
i 3 9.9 0.3 64 83 0 0 0 0 4095 1 
i 3 9.9 0.3 76 99 0 0 0 0 4095 1 
i 4 9.9 0.3 66 83 0 0 0 0 4095 1 
i 5 9.9 0.3 71 87 0 0 0 0 4095 1 
i 5 9.9 0.3 74 94 0 0 0 0 4095 1 
i 1 10.2 0.3 56 95 0 0 0 0 4095 1 
i 1 10.2 0.3 66 84 0 0 0 0 4095 1 
i 3 10.2 0.3 80 85 0 0 0 0 4095 1 
i 3 10.2 0.3 83 87 0 0 0 0 4095 1 
i 4 10.2 0.3 76 89 0 0 0 0 4095 1 
i 5 10.2 0.6 69 84 0 0 0 0 4095 1 
i 1 10.5 0.3 64 87 0 0 0 0 4095 1 
i 3 10.5 0.3 81 85 0 0 0 0 4095 1 
i 5 10.5 0.3 52 90 0 0 0 0 4095 1 
i 5 10.5 0.6 56 92 0 0 0 0 4095 1 
i 6 10.5 0.3 62 93 0 0 0 0 4095 1 
i 1 10.8 0.3 57 86 0 0 0 0 4095 1 
i 1 10.8 0.3 62 87 0 0 0 0 4095 1 
i 2 10.8 0.3 47 96 0 0 0 0 4095 1 
i 4 10.8 0.3 71 86 0 0 0 0 4095 1 
i 6 10.8 0.6 56 90 0 0 0 0 4095 1 
i 2 11.1 0.3 52 89 0 0 0 0 4095 1 
i 4 11.1 0.3 56 86 0 0 0 0 4095 1 
i 5 11.1 0.3 61 95 0 0 0 0 4095 1 
i 5 11.1 0.3 63 89 0 0 0 0 4095 1 
i 6 11.1 0.3 51 85 0 0 0 0 4095 1 
i 3 11.4 0.6 59 85 0 0 0 0 4095 1 
i 3 11.4 0.3 61 94 0 0 0 0 4095 1 
i 4 11.4 0.3 51 86 0 0 0 0 4095 1 
i 4 11.4 0.6 52 83 0 0 0 0 4095 1 
i 5 11.4 0.3 71 84 0 0 0 0 4095 1 
i 5 11.4 0.3 75 86 0 0 0 0 4095 1 
i 1 11.7 0.3 61 87 0 0 0 0 4095 1 
i 3 11.7 0.3 64 85 0 0 0 0 4095 1 
i 4 11.7 0.3 54 88 0 0 0 0 4095 1 
i 6 11.7 0.3 57 87 0 0 0 0 4095 1 
i 1 12 0.3 80 83 0 0 0 0 4095 1 
i 4 12 0.3 51 84 0 0 0 0 4095 1 
i 5 12 0.3 57 84 0 0 0 0 4095 1 
i 5 12 0.3 59 85 0 0 0 0 4095 1 
i 6 12 0.6 56 88 0 0 0 0 4095 1 
i 6 12 0.3 61 99 0 0 0 0 4095 1 
i 1 12.3 0.3 83 91 0 0 0 0 4095 1 
i 2 12.3 0.3 59 92 0 0 0 0 4095 1 
i 4 12.3 0.3 56 90 0 0 0 0 4095 1 
i 5 12.3 0.3 61 86 0 0 0 0 4095 1 
i 6 12.3 0.3 73 87 0 0 0 0 4095 1 
i 1 12.6 0.3 81 95 0 0 0 0 4095 1 
i 2 12.6 0.6 57 90 0 0 0 0 4095 1 
i 3 12.6 0.3 56 98 0 0 0 0 4095 1 
i 3 12.6 0.3 61 89 0 0 0 0 4095 1 
i 3 12.6 0.3 62 85 0 0 0 0 4095 1 
i 4 12.6 0.3 50 88 0 0 0 0 4095 1 
i 2 12.9 0.3 61 94 0 0 0 0 4095 1 
i 4 12.9 0.6 52 85 0 0 0 0 4095 1 
i 4 12.9 0.3 56 87 0 0 0 0 4095 1 
i 6 12.9 0.3 62 88 0 0 0 0 4095 1 
i 6 12.9 0.9 68 94 0 0 0 0 4095 1 
i 1 13.2 0.9 80 94 0 0 0 0 4095 1 
i 2 13.2 0.3 62 93 0 0 0 0 4095 1 
i 3 13.2 0.3 52 91 0 0 0 0 4095 1 
i 5 13.2 0.3 59 89 0 0 0 0 4095 1 
i 1 13.5 0.6 73 100 0 0 0 0 4095 1 
i 2 13.5 0.3 64 87 0 0 0 0 4095 1 
i 5 13.5 0.3 49 87 0 0 0 0 4095 1 
i 6 13.5 0.3 66 92 0 0 0 0 4095 1 
i 2 13.8 0.6 68 93 0 0 0 0 4095 1 
i 3 13.8 0.3 49 97 0 0 0 0 4095 1 
i 6 13.8 0.3 69 90 0 0 0 0 4095 1 
i 3 14.1 0.3 54 94 0 0 0 0 4095 1 
i 4 14.1 0.3 57 88 0 0 0 0 4095 1 
i 4 14.1 0.3 81 91 0 0 0 0 4095 1 
i 5 14.1 0.3 52 96 0 0 0 0 4095 1 
i 5 14.1 0.3 54 89 0 0 0 0 4095 1 
i 4 14.4 0.3 71 92 0 0 0 0 4095 1 
i 4 14.4 0.3 75 91 0 0 0 0 4095 1 
i 5 14.4 0.3 49 91 0 0 0 0 4095 1 
i 5 14.4 0.3 51 89 0 0 0 0 4095 1 
i 6 14.4 0.3 68 89 0 0 0 0 4095 1 
i 6 14.4 0.3 69 89 0 0 0 0 4095 1 
i 1 14.7 0.3 73 83 0 0 0 0 4095 1 
i 1 14.7 0.3 76 89 0 0 0 0 4095 1 
i 2 14.7 0.3 73 89 0 0 0 0 4095 1 
i 3 14.7 0.3 49 93 0 0 0 0 4095 1 
i 4 14.7 0.3 69 87 0 0 0 0 4095 1 
i 5 14.7 0.6 68 88 0 0 0 0 4095 1 
i 2 15 0.6 68 88 0 0 0 0 4095 1 
i 4 15 0.3 73 98 0 0 0 0 4095 1 
i 4 15 0.3 76 94 0 0 0 0 4095 1 
i 4 15 0.3 80 97 0 0 0 0 4095 1 
i 6 15 0.3 68 93 0 0 0 0 4095 1 
i 3 15.3 0.3 59 85 0 0 0 0 4095 1 
i 4 15.3 0.3 85 87 0 0 0 0 4095 1 
i 5 15.3 0.6 69 89 0 0 0 0 4095 1 
i 2 15.6 0.3 71 86 0 0 0 0 4095 1 
i 3 15.6 0.3 56 95 0 0 0 0 4095 1 
i 5 15.6 0.3 64 86 0 0 0 0 4095 1 
i 5 15.6 0.3 73 89 0 0 0 0 4095 1 
i 6 15.6 0.3 59 91 0 0 0 0 4095 1 
i 1 15.9 0.3 68 85 0 0 0 0 4095 1 
i 1 15.9 0.3 71 96 0 0 0 0 4095 1 
i 2 15.9 0.3 76 92 0 0 0 0 4095 1 
i 3 15.9 0.3 73 95 0 0 0 0 4095 1 
i 6 15.9 0.3 57 87 0 0 0 0 4095 1 
i 6 15.9 0.3 80 97 0 0 0 0 4095 1 
i 2 16.2 0.9 68 87 0 0 0 0 4095 1 
i 2 16.2 0.3 69 84 0 0 0 0 4095 1 
i 4 16.2 0.6 42 92 0 0 0 0 4095 1 
i 4 16.2 0.3 45 99 0 0 0 0 4095 1 
i 4 16.2 0.3 59 84 0 0 0 0 4095 1 
i 5 16.2 0.3 69 91 0 0 0 0 4095 1 
i 3 16.5 0.6 69 85 0 0 0 0 4095 1 
i 4 16.5 0.3 64 89 0 0 0 0 4095 1 
i 4 16.5 0.3 68 84 0 0 0 0 4095 1 
i 6 16.5 0.3 80 95 0 0 0 0 4095 1 
i 2 16.8 0.6 73 87 0 0 0 0 4095 1 
i 5 16.8 0.3 61 87 0 0 0 0 4095 1 
i 5 16.8 0.3 64 87 0 0 0 0 4095 1 
i 5 16.8 0.3 81 87 0 0 0 0 4095 1 
i 2 17.1 0.3 74 85 0 0 0 0 4095 1 
i 3 17.1 0.3 57 94 0 0 0 0 4095 1 
i 3 17.1 0.3 61 83 0 0 0 0 4095 1 
i 5 17.1 0.3 68 96 0 0 0 0 4095 1 
i 5 17.1 0.3 71 83 0 0 0 0 4095 1 
i 2 17.4 0.3 76 89 0 0 0 0 4095 1 
i 2 17.4 0.3 78 90 0 0 0 0 4095 1 
i 3 17.4 0.3 80 87 0 0 0 0 4095 1 
i 4 17.4 0.3 57 84 0 0 0 0 4095 1 
i 6 17.4 0.3 80 85 0 0 0 0 4095 1 
i 6 17.4 0.3 81 84 0 0 0 0 4095 1 
i 1 17.7 0.3 73 87 0 0 0 0 4095 1 
i 2 17.7 0.3 45 86 0 0 0 0 4095 1 
i 3 17.7 0.3 76 86 0 0 0 0 4095 1 
i 6 17.7 0.3 45 91 0 0 0 0 4095 1 
i 6 17.7 0.6 49 90 0 0 0 0 4095 1 
i 3 18 0.3 71 95 0 0 0 0 4095 1 
i 5 18 0.3 71 88 0 0 0 0 4095 1 
i 6 18 1.2 61 85 0 0 0 0 4095 1 
i 1 18.3 0.9 73 89 0 0 0 0 4095 1 
i 2 18.3 0.3 45 90 0 0 0 0 4095 1 
i 2 18.3 0.9 61 92 0 0 0 0 4095 1 
i 4 18.3 0.3 57 98 0 0 0 0 4095 1 
i 5 18.3 0.3 57 90 0 0 0 0 4095 1 
i 3 18.6 0.3 69 90 0 0 0 0 4095 1 
i 3 18.9 0.3 45 94 0 0 0 0 4095 1 
i 3 18.9 0.3 49 99 0 0 0 0 4095 1 
i 5 18.9 0.3 61 93 0 0 0 0 4095 1 
i 1 19.2 0.3 52 89 0 0 0 0 4095 1 
i 1 19.2 0.3 57 92 0 0 0 0 4095 1 
i 2 19.2 0.3 66 87 0 0 0 0 4095 1 
i 2 19.2 0.3 69 90 0 0 0 0 4095 1 
i 3 19.2 0.3 50 86 0 0 0 0 4095 1 
i 4 19.2 0.3 54 82 0 0 0 0 4095 1 
i 2 19.5 0.3 71 85 0 0 0 0 4095 1 
i 3 19.5 0.3 44 92 0 0 0 0 4095 1 
i 4 19.5 0.3 47 97 0 0 0 0 4095 1 
i 5 19.5 0.3 61 92 0 0 0 0 4095 1 
i 5 19.5 0.3 78 85 0 0 0 0 4095 1 
i 6 19.5 0.3 57 92 0 0 0 0 4095 1 
i 1 19.8 0.3 54 87 0 0 0 0 4095 1 
i 2 19.8 0.6 73 88 0 0 0 0 4095 1 
i 4 19.8 0.3 42 85 0 0 0 0 4095 1 
i 4 19.8 0.3 64 87 0 0 0 0 4095 1 
i 6 19.8 0.3 52 85 0 0 0 0 4095 1 
i 1 20.1 0.3 69 87 0 0 0 0 4095 1 
i 2 20.1 0.3 83 85 0 0 0 0 4095 1 
i 4 20.1 0.3 59 90 0 0 0 0 4095 1 
i 6 20.1 0.3 81 91 0 0 0 0 4095 1 
i 1 20.4 0.3 47 97 0 0 0 0 4095 1 
i 2 20.4 0.3 78 95 0 0 0 0 4095 1 
i 3 20.4 0.3 56 89 0 0 0 0 4095 1 
i 5 20.4 0.3 83 88 0 0 0 0 4095 1 
i 6 20.4 0.6 54 92 0 0 0 0 4095 1 
i 6 20.4 0.3 59 97 0 0 0 0 4095 1 
i 2 20.7 0.3 80 85 0 0 0 0 4095 1 
i 3 20.7 0.3 57 86 0 0 0 0 4095 1 
i 3 20.7 0.3 62 94 0 0 0 0 4095 1 
i 4 20.7 0.3 78 83 0 0 0 0 4095 1 
i 6 20.7 0.3 64 89 0 0 0 0 4095 1 
i 6 20.7 0.3 83 83 0 0 0 0 4095 1 
i 1 21 0.3 44 85 0 0 0 0 4095 1 
i 1 21 0.9 49 88 0 0 0 0 4095 1 
i 2 21 0.3 73 93 0 0 0 0 4095 1 
i 5 21 0.3 85 89 0 0 0 0 4095 1 
i 5 21 0.3 90 82 0 0 0 0 4095 1 
i 6 21 0.3 52 83 0 0 0 0 4095 1 
i 1 21.3 0.3 45 82 0 0 0 0 4095 1 
i 1 21.3 0.3 54 84 0 0 0 0 4095 1 
i 1 21.3 0.3 55 86 0 0 0 0 4095 1 
i 1 21.3 0.3 78 88 0 0 0 0 4095 1 
i 4 21.3 0.3 47 89 0 0 0 0 4095 1 
i 1 21.6 0.3 66 89 0 0 0 0 4095 1 
i 3 21.6 0.3 64 87 0 0 0 0 4095 1 
i 4 21.6 0.3 68 95 0 0 0 0 4095 1 
i 5 21.6 0.3 88 95 0 0 0 0 4095 1 
i 6 21.6 0.3 56 91 0 0 0 0 4095 1 
i 1 21.9 0.3 68 89 0 0 0 0 4095 1 
i 2 21.9 0.3 68 95 0 0 0 0 4095 1 
i 4 21.9 0.3 66 95 0 0 0 0 4095 1 
i 5 21.9 0.3 66 90 0 0 0 0 4095 1 
i 5 21.9 0.6 71 88 0 0 0 0 4095 1 
i 1 22.2 0.3 64 90 0 0 0 0 4095 1 
i 3 22.2 0.3 81 90 0 0 0 0 4095 1 
i 4 22.2 0.3 45 95 0 0 0 0 4095 1 
i 4 22.2 0.3 85 99 0 0 0 0 4095 1 
i 5 22.2 0.3 73 89 0 0 0 0 4095 1 
i 1 22.5 0.3 68 86 0 0 0 0 4095 1 
i 2 22.5 0.3 49 82 0 0 0 0 4095 1 
i 2 22.5 0.3 52 83 0 0 0 0 4095 1 
i 4 22.5 0.3 47 83 0 0 0 0 4095 1 
i 5 22.5 0.3 59 90 0 0 0 0 4095 1 
i 6 22.5 0.3 61 96 0 0 0 0 4095 1 
i 3 22.8 0.3 78 89 0 0 0 0 4095 1 
i 3 22.8 0.3 80 95 0 0 0 0 4095 1 
i 3 22.8 0.3 83 90 0 0 0 0 4095 1 
i 4 22.8 0.3 62 92 0 0 0 0 4095 1 
i 5 22.8 0.3 61 89 0 0 0 0 4095 1 
i 6 22.8 0.3 66 89 0 0 0 0 4095 1 
i 2 23.1 0.3 44 87 0 0 0 0 4095 1 
i 3 23.1 0.3 88 85 0 0 0 0 4095 1 
i 5 23.1 0.3 56 97 0 0 0 0 4095 1 
i 6 23.1 0.3 62 81 0 0 0 0 4095 1 
i 6 23.1 0.6 68 91 0 0 0 0 4095 1 
i 2 23.4 0.3 56 89 0 0 0 0 4095 1 
i 4 23.4 0.3 56 97 0 0 0 0 4095 1 
i 5 23.4 0.3 68 98 0 0 0 0 4095 1 
i 6 23.4 0.3 71 88 0 0 0 0 4095 1 
i 6 23.4 0.3 73 90 0 0 0 0 4095 1 
i 1 23.7 0.3 67 85 0 0 0 0 4095 1 
i 1 23.7 0.3 83 90 0 0 0 0 4095 1 
i 2 23.7 0.3 52 92 0 0 0 0 4095 1 
i 2 23.7 0.3 55 97 0 0 0 0 4095 1 
i 2 23.7 0.3 69 84 0 0 0 0 4095 1 
i 3 23.7 0.3 85 99 0 0 0 0 4095 1 
i 1 24 0.3 56 88 0 0 0 0 4095 1 
i 1 24 0.3 59 90 0 0 0 0 4095 1 
i 1 24 0.3 71 88 0 0 0 0 4095 1 
i 2 24 0.6 68 87 0 0 0 0 4095 1 
i 5 24 0.3 68 99 0 0 0 0 4095 1 
i 2 24.3 0.3 66 87 0 0 0 0 4095 1 
i 2 24.3 0.3 85 89 0 0 0 0 4095 1 
i 4 24.3 0.3 57 93 0 0 0 0 4095 1 
i 5 24.3 0.3 71 87 0 0 0 0 4095 1 
i 6 24.3 0.3 68 89 0 0 0 0 4095 1 
i 1 24.6 0.3 74 89 0 0 0 0 4095 1 
i 2 24.6 0.3 83 90 0 0 0 0 4095 1 
i 3 24.6 0.3 81 87 0 0 0 0 4095 1 
i 3 24.6 0.3 83 86 0 0 0 0 4095 1 
i 5 24.6 0.3 59 91 0 0 0 0 4095 1 
i 1 24.9 0.3 78 89 0 0 0 0 4095 1 
i 1 24.9 0.3 80 83 0 0 0 0 4095 1 
i 2 24.9 0.3 80 85 0 0 0 0 4095 1 
i 3 24.9 0.3 80 85 0 0 0 0 4095 1 
i 6 24.9 0.3 66 83 0 0 0 0 4095 1 
i 6 24.9 0.3 74 88 0 0 0 0 4095 1 
i 1 25.2 0.6 44 86 0 0 0 0 4095 1 
i 1 25.2 0.3 47 97 0 0 0 0 4095 1 
i 1 25.2 0.3 81 84 0 0 0 0 4095 1 
i 4 25.2 0.3 73 86 0 0 0 0 4095 1 
i 6 25.2 0.3 73 92 0 0 0 0 4095 1 
i 2 25.5 0.3 57 92 0 0 0 0 4095 1 
i 5 25.5 0.6 57 92 0 0 0 0 4095 1 
i 5 25.5 0.3 62 93 0 0 0 0 4095 1 
i 6 25.5 0.6 69 93 0 0 0 0 4095 1 
i 6 25.5 0.3 71 90 0 0 0 0 4095 1 
i 1 25.8 0.3 73 88 0 0 0 0 4095 1 
i 1 25.8 0.3 76 92 0 0 0 0 4095 1 
i 3 25.8 0.3 54 97 0 0 0 0 4095 1 
i 6 25.8 0.3 64 87 0 0 0 0 4095 1 
i 6 25.8 0.6 80 87 0 0 0 0 4095 1 
i 2 26.1 0.6 52 94 0 0 0 0 4095 1 
i 3 26.1 0.3 59 89 0 0 0 0 4095 1 
i 5 26.1 0.3 62 89 0 0 0 0 4095 1 
i 6 26.1 0.3 85 99 0 0 0 0 4095 1 
i 3 26.4 0.3 61 83 0 0 0 0 4095 1 
i 3 26.4 0.3 62 92 0 0 0 0 4095 1 
i 4 26.4 0.3 57 83 0 0 0 0 4095 1 
i 5 26.4 0.3 54 92 0 0 0 0 4095 1 
i 6 26.4 0.3 76 84 0 0 0 0 4095 1 
i 1 26.7 0.6 42 93 0 0 0 0 4095 1 
i 1 26.7 0.3 45 88 0 0 0 0 4095 1 
i 3 26.7 0.3 56 100 0 0 0 0 4095 1 
i 4 26.7 0.3 62 89 0 0 0 0 4095 1 
i 6 26.7 0.3 71 86 0 0 0 0 4095 1 
i 1 27 0.9 59 91 0 0 0 0 4095 1 
i 2 27 0.3 83 91 0 0 0 0 4095 1 
i 3 27 0.3 57 91 0 0 0 0 4095 1 
i 3 27 0.3 59 89 0 0 0 0 4095 1 
i 6 27 0.3 69 88 0 0 0 0 4095 1 
i 3 27.3 0.3 40 97 0 0 0 0 4095 1 
i 3 27.3 0.3 45 88 0 0 0 0 4095 1 
i 3 27.3 0.3 74 93 0 0 0 0 4095 1 
i 5 27.3 0.9 59 89 0 0 0 0 4095 1 
i 4 27.6 0.3 62 89 0 0 0 0 4095 1 
i 4 27.6 0.3 74 86 0 0 0 0 4095 1 
i 1 27.9 0.9 57 87 0 0 0 0 4095 1 
i 2 27.9 0.9 81 94 0 0 0 0 4095 1 
i 3 27.9 0.6 57 93 0 0 0 0 4095 1 
i 6 27.9 0.6 73 86 0 0 0 0 4095 1 
i 4 28.2 0.3 73 88 0 0 0 0 4095 1 
i 5 28.2 0.3 73 89 0 0 0 0 4095 1 
i 1 28.5 0.3 74 87 0 0 0 0 4095 1 
i 1 28.5 0.3 86 98 0 0 0 0 4095 1 
i 5 28.5 0.3 59 85 0 0 0 0 4095 1 
i 1 28.8 0.3 85 99 0 0 0 0 4095 1 
i 1 28.8 0.3 90 99 0 0 0 0 4095 1 
i 2 28.8 0.3 85 88 0 0 0 0 4095 1 
i 4 28.8 0.3 71 95 0 0 0 0 4095 1 
i 5 28.8 0.3 85 89 0 0 0 0 4095 1 
i 6 28.8 0.3 78 94 0 0 0 0 4095 1 
i 1 29.1 0.3 57 86 0 0 0 0 4095 1 
i 2 29.1 0.3 83 94 0 0 0 0 4095 1 
i 3 29.1 0.3 80 90 0 0 0 0 4095 1 
i 4 29.1 0.3 66 85 0 0 0 0 4095 1 
i 5 29.1 0.3 69 86 0 0 0 0 4095 1 
i 6 29.1 0.3 73 89 0 0 0 0 4095 1 
i 1 29.4 0.3 52 92 0 0 0 0 4095 1 
i 1 29.4 0.3 56 97 0 0 0 0 4095 1 
i 2 29.4 0.3 85 91 0 0 0 0 4095 1 
i 4 29.4 0.3 64 89 0 0 0 0 4095 1 
i 5 29.4 0.3 71 88 0 0 0 0 4095 1 
i 5 29.4 0.3 85 97 0 0 0 0 4095 1 
i 3 29.7 0.3 85 89 0 0 0 0 4095 1 
i 4 29.7 0.3 59 92 0 0 0 0 4095 1 
i 5 29.7 0.3 52 90 0 0 0 0 4095 1 
i 5 29.7 0.3 54 97 0 0 0 0 4095 1 
i 6 29.7 1.2 59 87 0 0 0 0 4095 1 
i 6 29.7 0.3 64 87 0 0 0 0 4095 1 
i 2 30 0.3 83 86 0 0 0 0 4095 1 
i 3 30 0.3 71 86 0 0 0 0 4095 1 
i 4 30 0.3 61 85 0 0 0 0 4095 1 
i 6 30 0.3 61 88 0 0 0 0 4095 1 
i 1 30.3 0.3 61 85 0 0 0 0 4095 1 
i 1 30.3 0.3 81 89 0 0 0 0 4095 1 
i 2 30.3 0.3 74 89 0 0 0 0 4095 1 
i 3 30.3 0.3 76 85 0 0 0 0 4095 1 
i 4 30.3 0.6 66 91 0 0 0 0 4095 1 
i 1 30.6 0.6 78 90 0 0 0 0 4095 1 
i 2 30.6 0.3 76 90 0 0 0 0 4095 1 
i 6 30.6 0.3 83 89 0 0 0 0 4095 1 
i 2 30.9 0.3 66 90 0 0 0 0 4095 1 
i 2 30.9 0.3 71 85 0 0 0 0 4095 1 
i 4 30.9 0.3 62 84 0 0 0 0 4095 1 
i 4 30.9 0.3 73 85 0 0 0 0 4095 1 
i 5 30.9 0.3 54 97 0 0 0 0 4095 1 
i 1 31.2 0.3 76 93 0 0 0 0 4095 1 
i 2 31.2 0.3 69 90 0 0 0 0 4095 1 
i 2 31.2 0.3 73 87 0 0 0 0 4095 1 
i 5 31.2 0.3 59 85 0 0 0 0 4095 1 
i 6 31.2 0.3 64 87 0 0 0 0 4095 1 
i 6 31.2 0.3 66 84 0 0 0 0 4095 1 
i 2 31.5 0.3 68 95 0 0 0 0 4095 1 
i 2 31.5 0.6 71 87 0 0 0 0 4095 1 
i 3 31.5 0.3 61 90 0 0 0 0 4095 1 
i 3 31.5 0.3 66 95 0 0 0 0 4095 1 
i 4 31.5 0.6 76 87 0 0 0 0 4095 1 
i 6 31.5 0.3 59 88 0 0 0 0 4095 1 
i 1 31.8 0.3 78 95 0 0 0 0 4095 1 
i 3 31.8 0.3 64 98 0 0 0 0 4095 1 
i 4 31.8 1.5 71 99 0 0 0 0 4095 1 
i 5 31.8 0.3 61 93 0 0 0 0 4095 1 
i 2 32.1 0.3 76 90 0 0 0 0 4095 1 
i 2 32.1 0.3 78 94 0 0 0 0 4095 1 
i 5 32.1 0.3 64 96 0 0 0 0 4095 1 
i 6 32.1 0.6 64 90 0 0 0 0 4095 1 
i 3 32.4 0.6 76 88 0 0 0 0 4095 1 
i 4 32.4 0.3 66 90 0 0 0 0 4095 1 
i 6 32.4 0.3 68 91 0 0 0 0 4095 1 
i 2 32.7 0.3 76 99 0 0 0 0 4095 1 
i 3 32.7 0.3 78 96 0 0 0 0 4095 1 
i 5 32.7 0.3 74 89 0 0 0 0 4095 1 
i 1 33 0.3 76 92 0 0 0 0 4095 1 
i 2 33 0.3 66 96 0 0 0 0 4095 1 
i 2 33 0.3 68 93 0 0 0 0 4095 1 
i 3 33 0.3 81 89 0 0 0 0 4095 1 
i 4 33 0.3 73 87 0 0 0 0 4095 1 
i 1 33.3 0.3 81 88 0 0 0 0 4095 1 
i 1 33.3 0.3 85 86 0 0 0 0 4095 1 
i 3 33.3 0.3 68 97 0 0 0 0 4095 1 
i 3 33.3 0.3 73 86 0 0 0 0 4095 1 
i 3 33.3 0.3 76 87 0 0 0 0 4095 1 
i 4 33.3 0.3 59 88 0 0 0 0 4095 1 
i 2 33.6 0.3 66 90 0 0 0 0 4095 1 
i 2 33.6 0.3 71 90 0 0 0 0 4095 1 
i 6 33.6 0.3 64 88 0 0 0 0 4095 1 
i 6 33.6 0.3 76 92 0 0 0 0 4095 1 
i 6 33.6 0.3 83 96 0 0 0 0 4095 1 
i 1 33.9 0.3 61 88 0 0 0 0 4095 1 
i 1 33.9 0.3 66 89 0 0 0 0 4095 1 
i 2 33.9 0.3 76 87 0 0 0 0 4095 1 
i 3 33.9 0.3 71 95 0 0 0 0 4095 1 
i 4 33.9 0.3 64 95 0 0 0 0 4095 1 
i 6 33.9 0.3 86 87 0 0 0 0 4095 1 
i 1 34.2 0.3 64 85 0 0 0 0 4095 1 
i 1 34.2 0.6 85 85 0 0 0 0 4095 1 
i 2 34.2 0.3 71 85 0 0 0 0 4095 1 
i 4 34.2 0.3 76 85 0 0 0 0 4095 1 
i 6 34.2 0.3 57 92 0 0 0 0 4095 1 
i 6 34.2 0.3 83 89 0 0 0 0 4095 1 
i 2 34.5 0.3 76 87 0 0 0 0 4095 1 
i 4 34.5 0.3 74 87 0 0 0 0 4095 1 
i 5 34.5 0.3 64 89 0 0 0 0 4095 1 
i 5 34.5 0.3 69 90 0 0 0 0 4095 1 
i 6 34.5 0.3 78 89 0 0 0 0 4095 1 
i 1 34.8 0.3 76 95 0 0 0 0 4095 1 
i 1 34.8 0.3 80 98 0 0 0 0 4095 1 
i 4 34.8 0.3 69 91 0 0 0 0 4095 1 
i 5 34.8 0.3 61 95 0 0 0 0 4095 1 
i 6 34.8 0.3 68 94 0 0 0 0 4095 1 
i 6 34.8 0.3 74 91 0 0 0 0 4095 1 
i 1 35.1 0.3 54 91 0 0 0 0 4095 1 
i 2 35.1 0.3 76 91 0 0 0 0 4095 1 
i 4 35.1 0.3 61 88 0 0 0 0 4095 1 
i 6 35.1 0.3 69 95 0 0 0 0 4095 1 
i 6 35.1 0.3 73 88 0 0 0 0 4095 1 
i 6 35.1 0.3 76 88 0 0 0 0 4095 1 
i 1 35.4 0.3 49 90 0 0 0 0 4095 1 
i 3 35.4 0.3 76 92 0 0 0 0 4095 1 
i 4 35.4 0.3 73 94 0 0 0 0 4095 1 
i 4 35.4 0.3 83 87 0 0 0 0 4095 1 
i 5 35.4 0.3 49 89 0 0 0 0 4095 1 
i 5 35.4 0.3 52 89 0 0 0 0 4095 1 
i 1 35.7 0.3 45 90 0 0 0 0 4095 1 
i 3 35.7 0.3 73 91 0 0 0 0 4095 1 
i 4 35.7 0.3 76 95 0 0 0 0 4095 1 
i 4 35.7 0.3 81 93 0 0 0 0 4095 1 
i 5 35.7 0.3 54 95 0 0 0 0 4095 1 
i 5 35.7 0.3 56 95 0 0 0 0 4095 1 
i 2 36 0.3 54 90 0 0 0 0 4095 1 
i 2 36 0.3 57 86 0 0 0 0 4095 1 
i 2 36 0.3 59 87 0 0 0 0 4095 1 
i 2 36 0.3 74 87 0 0 0 0 4095 1 
i 3 36 0.3 74 92 0 0 0 0 4095 1 
i 3 36 0.3 76 92 0 0 0 0 4095 1 
i 2 36.3 0.3 61 84 0 0 0 0 4095 1 
i 2 36.3 0.3 62 86 0 0 0 0 4095 1 
i 3 36.3 0.3 68 86 0 0 0 0 4095 1 
i 3 36.3 0.3 69 85 0 0 0 0 4095 1 
i 4 36.3 0.6 76 84 0 0 0 0 4095 1 
i 4 36.3 0.6 78 87 0 0 0 0 4095 1 
i 2 36.6 0.6 78 99 0 0 0 0 4095 1 
i 3 36.6 0.3 74 91 0 0 0 0 4095 1 
i 5 36.6 0.3 49 86 0 0 0 0 4095 1 
i 6 36.6 0.3 76 91 0 0 0 0 4095 1 
i 1 36.9 0.6 54 89 0 0 0 0 4095 1 
i 2 36.9 0.3 52 91 0 0 0 0 4095 1 
i 4 36.9 0.6 59 92 0 0 0 0 4095 1 
i 5 36.9 0.3 57 87 0 0 0 0 4095 1 
i 6 36.9 0.3 80 88 0 0 0 0 4095 1 
i 1 37.2 0.3 74 97 0 0 0 0 4095 1 
i 2 37.2 0.3 59 94 0 0 0 0 4095 1 
i 6 37.2 0.3 71 92 0 0 0 0 4095 1 
i 4 37.5 0.3 57 95 0 0 0 0 4095 1 
i 4 37.5 0.3 61 91 0 0 0 0 4095 1 
i 4 37.5 0.3 73 92 0 0 0 0 4095 1 
i 5 37.5 0.3 57 97 0 0 0 0 4095 1 
i 5 37.5 0.9 61 90 0 0 0 0 4095 1 
i 6 37.5 0.3 73 95 0 0 0 0 4095 1 
i 1 37.8 0.6 71 96 0 0 0 0 4095 1 
i 2 37.8 0.3 59 89 0 0 0 0 4095 1 
i 2 37.8 0.3 62 89 0 0 0 0 4095 1 
i 4 37.8 0.3 74 89 0 0 0 0 4095 1 
i 3 38.1 0.3 73 85 0 0 0 0 4095 1 
i 4 38.1 0.3 59 84 0 0 0 0 4095 1 
i 6 38.1 0.3 73 87 0 0 0 0 4095 1 
i 1 38.4 0.3 73 90 0 0 0 0 4095 1 
i 1 38.4 0.3 74 96 0 0 0 0 4095 1 
i 3 38.4 0.6 76 85 0 0 0 0 4095 1 
i 4 38.4 0.3 76 96 0 0 0 0 4095 1 
i 5 38.4 0.3 59 86 0 0 0 0 4095 1 
i 1 38.7 0.3 79 93 0 0 0 0 4095 1 
i 2 38.7 0.3 76 94 0 0 0 0 4095 1 
i 2 38.7 0.3 81 89 0 0 0 0 4095 1 
i 4 38.7 0.3 81 96 0 0 0 0 4095 1 
i 5 38.7 0.3 64 86 0 0 0 0 4095 1 
i 1 39 0.3 59 91 0 0 0 0 4095 1 
i 1 39 0.3 61 95 0 0 0 0 4095 1 
i 2 39 0.3 49 92 0 0 0 0 4095 1 
i 2 39 0.3 52 89 0 0 0 0 4095 1 
i 3 39 0.3 74 89 0 0 0 0 4095 1 
i 6 39 0.3 71 93 0 0 0 0 4095 1 
i 1 39.3 0.3 54 83 0 0 0 0 4095 1 
i 2 39.3 0.3 45 82 0 0 0 0 4095 1 
i 3 39.3 0.3 71 88 0 0 0 0 4095 1 
i 3 39.3 0.6 73 85 0 0 0 0 4095 1 
i 6 39.3 0.6 74 85 0 0 0 0 4095 1 
i 6 39.3 0.6 76 82 0 0 0 0 4095 1 
i 2 39.6 0.3 47 94 0 0 0 0 4095 1 
i 2 39.6 0.3 49 92 0 0 0 0 4095 1 
i 1 39.9 0.3 49 91 0 0 0 0 4095 1 
i 1 39.9 0.3 64 94 0 0 0 0 4095 1 
i 3 39.9 0.3 66 89 0 0 0 0 4095 1 
i 4 39.9 0.3 59 96 0 0 0 0 4095 1 
i 5 39.9 0.3 66 87 0 0 0 0 4095 1 
i 2 40.2 0.3 52 100 0 0 0 0 4095 1 
i 3 40.2 0.3 69 91 0 0 0 0 4095 1 
i 3 40.2 0.3 81 91 0 0 0 0 4095 1 
i 4 40.2 0.3 61 95 0 0 0 0 4095 1 
i 5 40.2 0.3 79 90 0 0 0 0 4095 1 
i 6 40.2 0.3 66 97 0 0 0 0 4095 1 
i 1 40.5 0.3 61 87 0 0 0 0 4095 1 
i 2 40.5 0.9 57 88 0 0 0 0 4095 1 
i 2 40.5 0.3 62 88 0 0 0 0 4095 1 
i 3 40.5 0.3 61 92 0 0 0 0 4095 1 
i 5 40.5 0.3 76 96 0 0 0 0 4095 1 
i 6 40.5 0.3 54 86 0 0 0 0 4095 1 
i 1 40.8 0.3 81 87 0 0 0 0 4095 1 
i 3 40.8 0.3 59 84 0 0 0 0 4095 1 
i 4 40.8 0.3 54 85 0 0 0 0 4095 1 
i 4 40.8 0.3 59 97 0 0 0 0 4095 1 
i 5 40.8 0.3 73 84 0 0 0 0 4095 1 
i 1 41.1 0.3 79 84 0 0 0 0 4095 1 
i 3 41.1 0.3 54 83 0 0 0 0 4095 1 
i 5 41.1 0.3 71 84 0 0 0 0 4095 1 
i 6 41.1 0.3 57 85 0 0 0 0 4095 1 
i 1 41.4 0.6 61 93 0 0 0 0 4095 1 
i 2 41.4 0.3 76 88 0 0 0 0 4095 1 
i 3 41.4 0.3 57 91 0 0 0 0 4095 1 
i 3 41.4 0.3 69 83 0 0 0 0 4095 1 
i 4 41.4 0.3 59 86 0 0 0 0 4095 1 
i 6 41.4 0.6 59 84 0 0 0 0 4095 1 
i 2 41.7 0.3 78 91 0 0 0 0 4095 1 
i 2 41.7 0.3 81 96 0 0 0 0 4095 1 
i 3 41.7 0.3 64 95 0 0 0 0 4095 1 
i 6 41.7 0.3 55 90 0 0 0 0 4095 1 
i 1 42 0.3 78 91 0 0 0 0 4095 1 
i 2 42 0.3 66 92 0 0 0 0 4095 1 
i 3 42 0.3 66 97 0 0 0 0 4095 1 
i 3 42 0.3 69 93 0 0 0 0 4095 1 
i 4 42 0.3 61 97 0 0 0 0 4095 1 
i 5 42 0.3 71 92 0 0 0 0 4095 1 
i 1 42.3 0.6 73 91 0 0 0 0 4095 1 
i 3 42.3 0.3 61 98 0 0 0 0 4095 1 
i 4 42.3 0.3 59 88 0 0 0 0 4095 1 
i 4 42.3 0.3 64 88 0 0 0 0 4095 1 
i 5 42.3 0.3 73 93 0 0 0 0 4095 1 
i 6 42.3 0.3 56 86 0 0 0 0 4095 1 
i 1 42.6 0.3 76 95 0 0 0 0 4095 1 
i 1 42.6 0.3 78 81 0 0 0 0 4095 1 
i 2 42.6 0.3 64 82 0 0 0 0 4095 1 
i 4 42.6 0.3 69 81 0 0 0 0 4095 1 
i 6 42.6 0.3 59 87 0 0 0 0 4095 1 
i 1 42.9 0.6 61 94 0 0 0 0 4095 1 
i 1 42.9 0.3 79 92 0 0 0 0 4095 1 
i 5 42.9 0.3 55 96 0 0 0 0 4095 1 
i 5 42.9 0.3 57 91 0 0 0 0 4095 1 
i 5 42.9 0.3 59 88 0 0 0 0 4095 1 
i 1 43.2 0.3 78 86 0 0 0 0 4095 1 
i 3 43.2 0.3 56 87 0 0 0 0 4095 1 
i 4 43.2 0.3 69 91 0 0 0 0 4095 1 
i 4 43.2 0.3 73 88 0 0 0 0 4095 1 
i 6 43.2 0.3 64 88 0 0 0 0 4095 1 
i 2 43.5 0.3 59 95 0 0 0 0 4095 1 
i 4 43.5 0.3 68 83 0 0 0 0 4095 1 
i 5 43.5 0.6 59 84 0 0 0 0 4095 1 
i 5 43.5 0.3 81 83 0 0 0 0 4095 1 
i 6 43.5 0.3 52 85 0 0 0 0 4095 1 
i 6 43.5 0.3 57 85 0 0 0 0 4095 1 
i 1 43.8 0.3 73 92 0 0 0 0 4095 1 
i 2 43.8 0.3 56 100 0 0 0 0 4095 1 
i 4 43.8 0.3 64 92 0 0 0 0 4095 1 
i 5 43.8 0.3 54 94 0 0 0 0 4095 1 
i 5 43.8 0.3 66 88 0 0 0 0 4095 1 
i 1 44.1 0.3 78 90 0 0 0 0 4095 1 
i 1 44.1 0.6 80 100 0 0 0 0 4095 1 
i 3 44.1 0.3 61 96 0 0 0 0 4095 1 
i 5 44.1 0.3 68 86 0 0 0 0 4095 1 
i 6 44.1 0.3 47 92 0 0 0 0 4095 1 
i 6 44.1 0.3 71 85 0 0 0 0 4095 1 
i 3 44.4 0.3 71 91 0 0 0 0 4095 1 
i 3 44.4 0.3 74 88 0 0 0 0 4095 1 
i 5 44.4 0.3 66 94 0 0 0 0 4095 1 
i 6 44.4 0.3 73 91 0 0 0 0 4095 1 
i 6 44.4 0.6 76 93 0 0 0 0 4095 1 
i 1 44.7 0.3 76 95 0 0 0 0 4095 1 
i 2 44.7 0.3 59 90 0 0 0 0 4095 1 
i 2 44.7 0.3 81 92 0 0 0 0 4095 1 
i 3 44.7 0.3 76 92 0 0 0 0 4095 1 
i 6 44.7 0.3 61 92 0 0 0 0 4095 1 
i 1 45 0.3 80 87 0 0 0 0 4095 1 
i 2 45 0.3 61 97 0 0 0 0 4095 1 
i 3 45 0.6 69 97 0 0 0 0 4095 1 
i 3 45 0.3 73 93 0 0 0 0 4095 1 
i 5 45 0.3 61 89 0 0 0 0 4095 1 
i 5 45 0.3 80 85 0 0 0 0 4095 1 
i 4 45.3 0.3 59 99 0 0 0 0 4095 1 
i 6 45.3 0.3 61 88 0 0 0 0 4095 1 
i 6 45.3 0.3 66 91 0 0 0 0 4095 1 
i 6 45.3 0.3 68 88 0 0 0 0 4095 1 
i 1 45.6 0.3 63 88 0 0 0 0 4095 1 
i 1 45.6 0.3 68 92 0 0 0 0 4095 1 
i 5 45.6 0.3 76 90 0 0 0 0 4095 1 
i 5 45.6 0.3 80 91 0 0 0 0 4095 1 
i 6 45.6 0.3 63 93 0 0 0 0 4095 1 
i 1 45.9 0.3 64 89 0 0 0 0 4095 1 
i 1 45.9 0.3 76 88 0 0 0 0 4095 1 
i 3 45.9 0.3 52 91 0 0 0 0 4095 1 
i 4 45.9 0.3 59 89 0 0 0 0 4095 1 
i 5 45.9 0.3 71 97 0 0 0 0 4095 1 
i 6 45.9 0.3 73 93 0 0 0 0 4095 1 
i 2 46.2 0.3 56 95 0 0 0 0 4095 1 
i 3 46.2 0.6 68 87 0 0 0 0 4095 1 
i 4 46.2 0.3 80 93 0 0 0 0 4095 1 
i 6 46.2 0.6 63 88 0 0 0 0 4095 1 
i 6 46.2 0.3 66 87 0 0 0 0 4095 1 
i 6 46.2 0.3 69 87 0 0 0 0 4095 1 
i 1 46.5 0.3 71 90 0 0 0 0 4095 1 
i 4 46.5 0.3 78 89 0 0 0 0 4095 1 
i 5 46.5 0.3 71 97 0 0 0 0 4095 1 
i 6 46.5 0.3 68 87 0 0 0 0 4095 1 
i 1 46.8 1.2 69 83 0 0 0 0 4095 1 
i 3 46.8 0.6 69 85 0 0 0 0 4095 1 
i 6 46.8 0.3 57 93 0 0 0 0 4095 1 
i 6 46.8 0.3 73 98 0 0 0 0 4095 1 
i 1 47.1 0.3 57 88 0 0 0 0 4095 1 
i 1 47.1 0.3 73 87 0 0 0 0 4095 1 
i 2 47.1 0.9 73 86 0 0 0 0 4095 1 
i 4 47.1 0.9 78 85 0 0 0 0 4095 1 
i 6 47.4 0.3 73 96 0 0 0 0 4095 1 
i 3 47.7 0.3 69 96 0 0 0 0 4095 1 
i 5 47.7 0.3 69 87 0 0 0 0 4095 1 
i 2 48 0.6 69 98 0 0 0 0 4095 1 
i 2 48 0.3 71 91 0 0 0 0 4095 1 
i 3 48 0.3 61 90 0 0 0 0 4095 1 
i 4 48 0.3 61 95 0 0 0 0 4095 1 
i 4 48 0.3 66 99 0 0 0 0 4095 1 
i 6 48 0.3 68 92 0 0 0 0 4095 1 
i 1 48.3 0.3 49 93 0 0 0 0 4095 1 
i 1 48.3 0.6 59 93 0 0 0 0 4095 1 
i 3 48.3 0.3 54 91 0 0 0 0 4095 1 
i 4 48.3 0.3 68 95 0 0 0 0 4095 1 
i 5 48.6 0.3 64 91 0 0 0 0 4095 1 
i 5 48.6 0.3 69 85 0 0 0 0 4095 1 
i 6 48.6 0.3 61 87 0 0 0 0 4095 1 
i 6 48.6 0.6 66 97 0 0 0 0 4095 1 
i 6 48.6 0.6 68 98 0 0 0 0 4095 1 
i 1 48.9 0.3 50 95 0 0 0 0 4095 1 
i 1 48.9 0.3 68 90 0 0 0 0 4095 1 
i 2 48.9 0.3 66 87 0 0 0 0 4095 1 
i 4 48.9 0.3 56 87 0 0 0 0 4095 1 
i 1 49.2 0.3 71 91 0 0 0 0 4095 1 
i 1 49.2 0.3 74 88 0 0 0 0 4095 1 
i 2 49.2 0.3 69 86 0 0 0 0 4095 1 
i 3 49.2 0.3 52 93 0 0 0 0 4095 1 
i 4 49.2 0.3 73 87 0 0 0 0 4095 1 
i 5 49.2 0.3 68 88 0 0 0 0 4095 1 
i 1 49.5 0.3 76 88 0 0 0 0 4095 1 
i 2 49.5 0.3 73 95 0 0 0 0 4095 1 
i 5 49.5 0.3 47 91 0 0 0 0 4095 1 
i 5 49.5 0.3 49 97 0 0 0 0 4095 1 
i 6 49.5 0.3 68 87 0 0 0 0 4095 1 
i 1 49.8 0.3 80 93 0 0 0 0 4095 1 
i 2 49.8 0.3 52 95 0 0 0 0 4095 1 
i 2 49.8 0.3 56 89 0 0 0 0 4095 1 
i 2 49.8 0.3 57 86 0 0 0 0 4095 1 
i 4 49.8 0.3 68 92 0 0 0 0 4095 1 
i 1 50.1 0.3 83 86 0 0 0 0 4095 1 
i 3 50.1 0.3 56 86 0 0 0 0 4095 1 
i 5 50.1 0.3 52 89 0 0 0 0 4095 1 
i 5 50.1 0.3 66 89 0 0 0 0 4095 1 
i 6 50.1 0.3 64 85 0 0 0 0 4095 1 
i 1 50.4 0.3 50 96 0 0 0 0 4095 1 
i 2 50.4 0.3 49 95 0 0 0 0 4095 1 
i 2 50.4 0.6 52 90 0 0 0 0 4095 1 
i 4 50.4 0.3 54 94 0 0 0 0 4095 1 
i 5 50.4 0.3 68 99 0 0 0 0 4095 1 
i 6 50.4 0.6 61 97 0 0 0 0 4095 1 
i 2 50.7 0.3 44 96 0 0 0 0 4095 1 
i 2 50.7 0.3 64 89 0 0 0 0 4095 1 
i 3 50.7 0.3 56 86 0 0 0 0 4095 1 
i 4 50.7 0.3 52 97 0 0 0 0 4095 1 
i 4 50.7 0.3 56 85 0 0 0 0 4095 1 
i 1 51 0.3 69 90 0 0 0 0 4095 1 
i 2 51 0.3 61 91 0 0 0 0 4095 1 
i 2 51 0.3 76 87 0 0 0 0 4095 1 
i 5 51 0.3 56 88 0 0 0 0 4095 1 
i 6 51 0.3 56 95 0 0 0 0 4095 1 
i 6 51 0.3 59 95 0 0 0 0 4095 1 
i 1 51.3 0.3 66 94 0 0 0 0 4095 1 
i 1 51.3 0.3 71 92 0 0 0 0 4095 1 
i 2 51.3 0.6 73 90 0 0 0 0 4095 1 
i 3 51.3 0.6 71 88 0 0 0 0 4095 1 
i 4 51.3 0.3 61 90 0 0 0 0 4095 1 
i 5 51.3 0.3 64 95 0 0 0 0 4095 1 
i 1 51.6 0.3 54 87 0 0 0 0 4095 1 
i 3 51.6 0.3 76 89 0 0 0 0 4095 1 
i 4 51.6 0.3 66 87 0 0 0 0 4095 1 
i 6 51.6 0.3 69 88 0 0 0 0 4095 1 
i 2 51.9 0.3 67 92 0 0 0 0 4095 1 
i 3 51.9 0.3 81 96 0 0 0 0 4095 1 
i 4 51.9 0.3 64 91 0 0 0 0 4095 1 
i 4 51.9 0.6 69 92 0 0 0 0 4095 1 
i 5 51.9 0.3 49 88 0 0 0 0 4095 1 
i 5 51.9 0.3 54 95 0 0 0 0 4095 1 
i 1 52.2 0.3 50 87 0 0 0 0 4095 1 
i 2 52.2 0.3 54 93 0 0 0 0 4095 1 
i 2 52.2 0.3 55 91 0 0 0 0 4095 1 
i 4 52.2 0.3 74 90 0 0 0 0 4095 1 
i 4 52.2 0.3 76 87 0 0 0 0 4095 1 
i 1 52.5 0.6 54 90 0 0 0 0 4095 1 
i 1 52.5 0.3 57 87 0 0 0 0 4095 1 
i 2 52.5 0.3 52 93 0 0 0 0 4095 1 
i 3 52.5 0.3 81 97 0 0 0 0 4095 1 
i 3 52.5 0.3 85 94 0 0 0 0 4095 1 
i 2 52.8 0.3 76 91 0 0 0 0 4095 1 
i 4 52.8 0.3 45 88 0 0 0 0 4095 1 
i 6 52.8 0.3 73 96 0 0 0 0 4095 1 
i 6 52.8 0.3 78 87 0 0 0 0 4095 1 
i 6 52.8 0.3 81 97 0 0 0 0 4095 1 
i 1 53.1 0.3 49 94 0 0 0 0 4095 1 
i 1 53.1 0.3 52 96 0 0 0 0 4095 1 
i 2 53.1 0.3 81 92 0 0 0 0 4095 1 
i 2 53.1 0.3 83 90 0 0 0 0 4095 1 
i 3 53.1 0.3 86 85 0 0 0 0 4095 1 
i 3 53.1 0.3 88 83 0 0 0 0 4095 1 
i 1 53.4 0.3 55 97 0 0 0 0 4095 1 
i 3 53.4 0.3 85 91 0 0 0 0 4095 1 
i 3 53.4 0.3 91 93 0 0 0 0 4095 1 
i 4 53.4 0.3 54 94 0 0 0 0 4095 1 
i 4 53.4 0.3 57 92 0 0 0 0 4095 1 
i 5 53.4 0.3 59 94 0 0 0 0 4095 1 
i 2 53.7 0.3 54 91 0 0 0 0 4095 1 
i 3 53.7 0.6 71 90 0 0 0 0 4095 1 
i 3 53.7 0.3 73 90 0 0 0 0 4095 1 
i 4 53.7 0.3 56 91 0 0 0 0 4095 1 
i 6 53.7 0.3 83 90 0 0 0 0 4095 1 
i 6 53.7 0.3 88 93 0 0 0 0 4095 1 
i 1 54 0.3 56 85 0 0 0 0 4095 1 
i 1 54 0.3 66 91 0 0 0 0 4095 1 
i 2 54 0.3 49 88 0 0 0 0 4095 1 
i 4 54 0.3 57 99 0 0 0 0 4095 1 
i 4 54 0.3 59 89 0 0 0 0 4095 1 
i 1 54.3 0.3 54 83 0 0 0 0 4095 1 
i 2 54.3 0.3 45 85 0 0 0 0 4095 1 
i 4 54.3 0.3 56 90 0 0 0 0 4095 1 
i 5 54.3 0.6 54 85 0 0 0 0 4095 1 
i 6 54.3 0.3 80 88 0 0 0 0 4095 1 
i 6 54.3 0.3 86 83 0 0 0 0 4095 1 
i 2 54.6 0.3 52 84 0 0 0 0 4095 1 
i 2 54.6 0.3 66 84 0 0 0 0 4095 1 
i 3 54.6 0.3 71 84 0 0 0 0 4095 1 
i 6 54.6 0.3 85 88 0 0 0 0 4095 1 
i 6 54.6 0.3 90 88 0 0 0 0 4095 1 
i 1 54.9 0.3 54 86 0 0 0 0 4095 1 
i 1 54.9 0.3 57 86 0 0 0 0 4095 1 
i 3 54.9 0.3 66 91 0 0 0 0 4095 1 
i 4 54.9 0.3 56 85 0 0 0 0 4095 1 
i 5 54.9 0.9 59 90 0 0 0 0 4095 1 
i 6 54.9 0.3 64 93 0 0 0 0 4095 1 
i 2 55.2 0.3 66 93 0 0 0 0 4095 1 
i 2 55.2 0.3 71 88 0 0 0 0 4095 1 
i 5 55.2 0.3 56 88 0 0 0 0 4095 1 
i 6 55.2 0.3 56 95 0 0 0 0 4095 1 
i 6 55.2 0.3 61 91 0 0 0 0 4095 1 
i 1 55.5 0.3 49 89 0 0 0 0 4095 1 
i 1 55.5 0.3 50 87 0 0 0 0 4095 1 
i 6 55.5 0.3 52 96 0 0 0 0 4095 1 
i 1 55.8 0.9 47 87 0 0 0 0 4095 1 
i 2 55.8 0.3 61 86 0 0 0 0 4095 1 
i 2 55.8 0.3 64 94 0 0 0 0 4095 1 
i 2 55.8 0.3 66 86 0 0 0 0 4095 1 
i 6 55.8 0.3 47 90 0 0 0 0 4095 1 
i 5 56.1 0.3 49 87 0 0 0 0 4095 1 
i 5 56.1 0.3 81 86 0 0 0 0 4095 1 
i 6 56.1 0.3 54 88 0 0 0 0 4095 1 
i 6 56.1 0.3 66 91 0 0 0 0 4095 1 
i 3 56.4 0.9 64 91 0 0 0 0 4095 1 
i 4 56.4 0.6 61 83 0 0 0 0 4095 1 
i 5 56.4 0.3 80 85 0 0 0 0 4095 1 
i 6 56.4 0.3 64 98 0 0 0 0 4095 1 
i 2 56.7 0.3 61 88 0 0 0 0 4095 1 
i 5 56.7 0.3 81 93 0 0 0 0 4095 1 
i 6 56.7 0.6 45 89 0 0 0 0 4095 1 
i 1 57 0.3 45 89 0 0 0 0 4095 1 
i 1 57 0.6 49 89 0 0 0 0 4095 1 
i 6 57 0.6 61 89 0 0 0 0 4095 1 
i 4 57.3 0.9 61 86 0 0 0 0 4095 1 
i 5 57.3 0.3 81 93 0 0 0 0 4095 1 
i 1 57.6 0.3 44 87 0 0 0 0 4095 1 
i 2 57.6 0.3 61 97 0 0 0 0 4095 1 
i 2 57.6 0.3 64 87 0 0 0 0 4095 1 
i 3 57.6 0.3 66 97 0 0 0 0 4095 1 
i 4 57.6 0.3 66 86 0 0 0 0 4095 1 
i 2 57.9 0.3 56 91 0 0 0 0 4095 1 
i 5 57.9 0.3 52 88 0 0 0 0 4095 1 
i 5 57.9 0.3 54 97 0 0 0 0 4095 1 
i 5 57.9 0.3 56 91 0 0 0 0 4095 1 
i 5 57.9 0.3 66 97 0 0 0 0 4095 1 
i 1 58.2 0.3 47 99 0 0 0 0 4095 1 
i 2 58.2 0.3 54 90 0 0 0 0 4095 1 
i 4 58.2 0.3 45 88 0 0 0 0 4095 1 
i 4 58.2 0.6 49 93 0 0 0 0 4095 1 
i 5 58.2 0.3 71 98 0 0 0 0 4095 1 
i 5 58.2 0.3 81 87 0 0 0 0 4095 1 
i 1 58.5 0.3 52 93 0 0 0 0 4095 1 
i 2 58.5 0.6 50 87 0 0 0 0 4095 1 
i 2 58.5 0.3 76 87 0 0 0 0 4095 1 
i 3 58.5 0.3 71 91 0 0 0 0 4095 1 
i 3 58.5 0.3 76 93 0 0 0 0 4095 1 
i 1 58.8 0.3 45 94 0 0 0 0 4095 1 
i 1 58.8 0.3 47 96 0 0 0 0 4095 1 
i 1 58.8 0.3 62 95 0 0 0 0 4095 1 
i 5 58.8 0.3 85 91 0 0 0 0 4095 1 
i 1 59.1 0.3 50 91 0 0 0 0 4095 1 
i 2 59.1 0.3 71 85 0 0 0 0 4095 1 
i 3 59.1 0.3 62 87 0 0 0 0 4095 1 
i 5 59.1 0.3 86 88 0 0 0 0 4095 1 
i 6 59.1 0.3 57 90 0 0 0 0 4095 1 
i 6 59.1 0.3 85 85 0 0 0 0 4095 1 
i 1 59.4 0.6 49 88 0 0 0 0 4095 1 
i 2 59.4 0.3 73 87 0 0 0 0 4095 1 
i 3 59.4 0.3 66 85 0 0 0 0 4095 1 
i 3 59.4 0.3 83 99 0 0 0 0 4095 1 
i 5 59.4 0.3 73 99 0 0 0 0 4095 1 
i 5 59.4 0.3 76 86 0 0 0 0 4095 1 
i 1 59.7 0.3 47 89 0 0 0 0 4095 1 
i 1 59.7 0.3 61 90 0 0 0 0 4095 1 
i 5 59.7 0.3 68 84 0 0 0 0 4095 1 
i 6 59.7 0.3 73 89 0 0 0 0 4095 1 
i 2 60 0.3 73 85 0 0 0 0 4095 1 
i 3 60 0.3 47 84 0 0 0 0 4095 1 
i 3 60 0.3 52 85 0 0 0 0 4095 1 
i 4 60 0.3 45 83 0 0 0 0 4095 1 
i 4 60 0.3 49 95 0 0 0 0 4095 1 
i 5 60 0.3 73 96 0 0 0 0 4095 1 
i 1 60.3 0.3 64 96 0 0 0 0 4095 1 
i 1 60.3 0.3 68 95 0 0 0 0 4095 1 
i 1 60.3 0.3 69 96 0 0 0 0 4095 1 
i 2 60.3 0.3 61 97 0 0 0 0 4095 1 
i 4 60.3 0.6 66 97 0 0 0 0 4095 1 
i 6 60.3 0.3 73 87 0 0 0 0 4095 1 
i 4 60.6 0.6 64 93 0 0 0 0 4095 1 
i 4 60.6 0.3 69 90 0 0 0 0 4095 1 
i 4 60.6 0.3 83 98 0 0 0 0 4095 1 
i 6 60.6 0.3 61 97 0 0 0 0 4095 1 
i 6 60.6 0.3 62 88 0 0 0 0 4095 1 
i 2 60.9 0.3 54 85 0 0 0 0 4095 1 
i 2 60.9 0.3 56 89 0 0 0 0 4095 1 
i 4 60.9 0.3 85 89 0 0 0 0 4095 1 
i 5 60.9 0.3 62 90 0 0 0 0 4095 1 
i 5 60.9 0.3 83 85 0 0 0 0 4095 1 
i 1 61.2 0.3 73 88 0 0 0 0 4095 1 
i 1 61.2 0.6 78 86 0 0 0 0 4095 1 
i 1 61.2 0.3 81 86 0 0 0 0 4095 1 
i 3 61.2 0.3 50 84 0 0 0 0 4095 1 
i 4 61.2 0.3 88 87 0 0 0 0 4095 1 
i 6 61.2 0.3 61 86 0 0 0 0 4095 1 
i 1 61.5 0.3 61 94 0 0 0 0 4095 1 
i 4 61.5 0.3 85 93 0 0 0 0 4095 1 
i 5 61.5 0.3 64 95 0 0 0 0 4095 1 
i 5 61.5 0.3 68 96 0 0 0 0 4095 1 
i 6 61.5 0.3 50 98 0 0 0 0 4095 1 
i 2 61.8 0.3 54 96 0 0 0 0 4095 1 
i 2 61.8 0.3 56 93 0 0 0 0 4095 1 
i 3 61.8 0.3 49 95 0 0 0 0 4095 1 
i 6 61.8 0.3 47 90 0 0 0 0 4095 1 
i 6 61.8 0.9 57 94 0 0 0 0 4095 1 
i 6 61.8 0.3 59 91 0 0 0 0 4095 1 
i 1 62.1 0.3 81 89 0 0 0 0 4095 1 
i 3 62.1 0.3 52 97 0 0 0 0 4095 1 
i 4 62.1 0.3 90 97 0 0 0 0 4095 1 
i 5 62.1 0.3 68 94 0 0 0 0 4095 1 
i 5 62.1 0.3 76 88 0 0 0 0 4095 1 
i 2 62.4 0.3 49 86 0 0 0 0 4095 1 
i 2 62.4 0.3 81 95 0 0 0 0 4095 1 
i 3 62.4 0.3 56 95 0 0 0 0 4095 1 
i 5 62.4 0.3 56 100 0 0 0 0 4095 1 
i 6 62.4 0.3 61 91 0 0 0 0 4095 1 
i 1 62.7 0.3 73 95 0 0 0 0 4095 1 
i 1 62.7 0.3 76 92 0 0 0 0 4095 1 
i 2 62.7 0.3 76 92 0 0 0 0 4095 1 
i 4 62.7 0.3 88 96 0 0 0 0 4095 1 
i 5 62.7 0.3 55 88 0 0 0 0 4095 1 
i 5 62.7 0.3 59 93 0 0 0 0 4095 1 
i 2 63 0.3 78 87 0 0 0 0 4095 1 
i 2 63 0.3 79 87 0 0 0 0 4095 1 
i 2 63 0.3 81 86 0 0 0 0 4095 1 
i 2 63 0.3 83 89 0 0 0 0 4095 1 
i 4 63 0.3 73 87 0 0 0 0 4095 1 
i 6 63 0.3 62 91 0 0 0 0 4095 1 
i 1 63.3 0.3 76 88 0 0 0 0 4095 1 
i 1 63.3 0.3 78 92 0 0 0 0 4095 1 
i 1 63.3 0.3 79 91 0 0 0 0 4095 1 
i 1 63.3 0.3 81 95 0 0 0 0 4095 1 
i 2 63.3 0.3 49 88 0 0 0 0 4095 1 
i 2 63.3 0.3 55 89 0 0 0 0 4095 1 
i 1 63.6 0.3 85 87 0 0 0 0 4095 1 
i 4 63.6 0.3 76 93 0 0 0 0 4095 1 
i 5 63.6 0.3 54 99 0 0 0 0 4095 1 
i 5 63.6 0.3 56 93 0 0 0 0 4095 1 
i 6 63.6 0.3 56 88 0 0 0 0 4095 1 
i 6 63.6 0.3 59 87 0 0 0 0 4095 1 
i 1 63.9 0.3 81 85 0 0 0 0 4095 1 
i 3 63.9 0.3 61 86 0 0 0 0 4095 1 
i 4 63.9 0.3 78 86 0 0 0 0 4095 1 
i 5 63.9 0.3 52 87 0 0 0 0 4095 1 
i 5 63.9 0.6 76 87 0 0 0 0 4095 1 
i 6 63.9 0.6 52 88 0 0 0 0 4095 1 
i 1 64.2 0.3 61 92 0 0 0 0 4095 1 
i 1 64.2 0.3 85 88 0 0 0 0 4095 1 
i 2 64.2 0.3 57 93 0 0 0 0 4095 1 
i 2 64.2 0.3 64 90 0 0 0 0 4095 1 
i 1 64.5 0.3 43 88 0 0 0 0 4095 1 
i 1 64.5 0.3 45 95 0 0 0 0 4095 1 
i 1 64.5 0.3 47 88 0 0 0 0 4095 1 
i 2 64.5 0.3 55 97 0 0 0 0 4095 1 
i 2 64.5 0.3 61 87 0 0 0 0 4095 1 
i 6 64.5 0.3 69 93 0 0 0 0 4095 1 
i 1 64.8 0.3 40 92 0 0 0 0 4095 1 
i 1 64.8 0.3 64 91 0 0 0 0 4095 1 
i 4 64.8 0.3 79 85 0 0 0 0 4095 1 
i 4 64.8 0.6 81 85 0 0 0 0 4095 1 
i 5 64.8 0.3 64 91 0 0 0 0 4095 1 
i 5 64.8 0.3 67 87 0 0 0 0 4095 1 
i 1 65.1 0.6 52 97 0 0 0 0 4095 1 
i 1 65.1 0.3 54 86 0 0 0 0 4095 1 
i 2 65.1 0.3 57 97 0 0 0 0 4095 1 
i 4 65.1 0.3 52 87 0 0 0 0 4095 1 
i 5 65.1 0.3 61 86 0 0 0 0 4095 1 
i 2 65.4 0.3 52 94 0 0 0 0 4095 1 
i 2 65.4 0.3 61 89 0 0 0 0 4095 1 
i 4 65.4 0.3 54 89 0 0 0 0 4095 1 
i 5 65.4 0.6 54 86 0 0 0 0 4095 1 
i 5 65.4 0.3 57 90 0 0 0 0 4095 1 
i 1 65.7 0.9 49 85 0 0 0 0 4095 1 
i 2 65.7 0.3 83 86 0 0 0 0 4095 1 
i 5 65.7 0.3 52 86 0 0 0 0 4095 1 
i 5 65.7 0.3 59 85 0 0 0 0 4095 1 
i 6 65.7 0.3 59 90 0 0 0 0 4095 1 
i 2 66 0.9 69 95 0 0 0 0 4095 1 
i 3 66 0.3 61 93 0 0 0 0 4095 1 
i 5 66 1.2 57 91 0 0 0 0 4095 1 
i 6 66 0.6 45 92 0 0 0 0 4095 1 
i 5 66.3 0.3 61 89 0 0 0 0 4095 1 
i 4 66.6 0.3 54 86 0 0 0 0 4095 1 
i 6 66.6 0.6 49 86 0 0 0 0 4095 1 
i 1 66.9 0.3 49 94 0 0 0 0 4095 1 
i 4 66.9 0.3 61 99 0 0 0 0 4095 1 
i 2 67.2 0.3 59 84 0 0 0 0 4095 1 
i 2 67.2 0.3 61 87 0 0 0 0 4095 1 
i 2 67.2 0.3 64 86 0 0 0 0 4095 1 
i 4 67.2 0.3 80 88 0 0 0 0 4095 1 
i 6 67.2 0.3 54 98 0 0 0 0 4095 1 
i 6 67.2 0.6 61 85 0 0 0 0 4095 1 
i 3 67.5 0.3 42 94 0 0 0 0 4095 1 
i 3 67.5 0.3 47 94 0 0 0 0 4095 1 
i 3 67.5 0.3 64 93 0 0 0 0 4095 1 
i 5 67.5 0.3 59 100 0 0 0 0 4095 1 
i 5 67.5 0.3 68 95 0 0 0 0 4095 1 
i 1 67.8 0.3 62 93 0 0 0 0 4095 1 
i 2 67.8 0.9 66 99 0 0 0 0 4095 1 
i 3 67.8 0.3 69 91 0 0 0 0 4095 1 
i 5 67.8 0.3 61 99 0 0 0 0 4095 1 
i 5 67.8 0.3 66 90 0 0 0 0 4095 1 
i 6 67.8 0.3 64 91 0 0 0 0 4095 1 
i 1 68.1 0.6 61 95 0 0 0 0 4095 1 
i 2 68.1 0.3 68 93 0 0 0 0 4095 1 
i 4 68.1 0.3 76 90 0 0 0 0 4095 1 
i 4 68.1 0.3 78 100 0 0 0 0 4095 1 
i 5 68.1 0.6 59 92 0 0 0 0 4095 1 
i 2 68.4 0.3 69 93 0 0 0 0 4095 1 
i 3 68.4 0.3 64 90 0 0 0 0 4095 1 
i 3 68.4 0.3 68 91 0 0 0 0 4095 1 
i 2 68.7 0.3 59 99 0 0 0 0 4095 1 
i 2 68.7 0.3 64 87 0 0 0 0 4095 1 
i 3 68.7 0.3 71 87 0 0 0 0 4095 1 
i 4 68.7 0.3 61 93 0 0 0 0 4095 1 
i 6 68.7 0.6 61 84 0 0 0 0 4095 1 
i 6 68.7 0.3 62 85 0 0 0 0 4095 1 
i 1 69 0.3 59 92 0 0 0 0 4095 1 
i 1 69 0.3 76 91 0 0 0 0 4095 1 
i 2 69 0.3 68 90 0 0 0 0 4095 1 
i 4 69 0.3 66 92 0 0 0 0 4095 1 
i 5 69 0.3 50 90 0 0 0 0 4095 1 
i 5 69 0.3 54 94 0 0 0 0 4095 1 
i 1 69.3 0.3 71 88 0 0 0 0 4095 1 
i 2 69.3 0.3 66 100 0 0 0 0 4095 1 
i 2 69.3 0.3 76 97 0 0 0 0 4095 1 
i 4 69.3 0.3 61 89 0 0 0 0 4095 1 
i 6 69.3 0.3 47 86 0 0 0 0 4095 1 
i 6 69.3 0.6 52 96 0 0 0 0 4095 1 
i 2 69.6 0.3 56 89 0 0 0 0 4095 1 
i 2 69.6 0.3 57 92 0 0 0 0 4095 1 
i 2 69.6 0.3 59 92 0 0 0 0 4095 1 
i 5 69.6 0.3 52 87 0 0 0 0 4095 1 
i 5 69.6 0.9 59 93 0 0 0 0 4095 1 
i 2 69.9 0.3 52 97 0 0 0 0 4095 1 
i 4 69.9 0.3 83 98 0 0 0 0 4095 1 
i 5 69.9 0.3 61 92 0 0 0 0 4095 1 
i 5 69.9 0.3 63 98 0 0 0 0 4095 1 
i 5 69.9 0.3 75 95 0 0 0 0 4095 1 
i 1 70.2 0.6 69 93 0 0 0 0 4095 1 
i 5 70.2 0.3 73 98 0 0 0 0 4095 1 
i 6 70.2 0.3 47 94 0 0 0 0 4095 1 
i 6 70.2 0.3 51 90 0 0 0 0 4095 1 
i 6 70.2 0.3 59 89 0 0 0 0 4095 1 
i 3 70.5 0.3 78 93 0 0 0 0 4095 1 
i 4 70.5 0.6 63 90 0 0 0 0 4095 1 
i 6 70.5 0.3 61 89 0 0 0 0 4095 1 
i 6 70.5 0.3 63 90 0 0 0 0 4095 1 
i 1 70.8 0.9 63 93 0 0 0 0 4095 1 
i 5 70.8 0.3 44 96 0 0 0 0 4095 1 
i 5 70.8 0.6 45 90 0 0 0 0 4095 1 
i 6 70.8 0.3 75 93 0 0 0 0 4095 1 
i 6 70.8 0.3 78 95 0 0 0 0 4095 1 
i 3 71.1 0.3 83 98 0 0 0 0 4095 1 
i 4 71.1 0.3 61 86 0 0 0 0 4095 1 
i 4 71.1 0.3 78 93 0 0 0 0 4095 1 
i 6 71.1 0.3 73 87 0 0 0 0 4095 1 
i 2 71.4 0.3 75 92 0 0 0 0 4095 1 
i 5 71.4 0.3 49 99 0 0 0 0 4095 1 
i 5 71.4 0.3 78 95 0 0 0 0 4095 1 
i 6 71.4 0.3 51 95 0 0 0 0 4095 1 
i 6 71.4 0.3 59 92 0 0 0 0 4095 1 
i 1 71.7 0.3 69 90 0 0 0 0 4095 1 
i 1 71.7 0.6 76 85 0 0 0 0 4095 1 
i 2 71.7 0.3 78 86 0 0 0 0 4095 1 
i 3 71.7 0.3 83 86 0 0 0 0 4095 1 
i 4 71.7 0.3 80 87 0 0 0 0 4095 1 
i 5 71.7 0.3 83 94 0 0 0 0 4095 1 
i 1 72 0.3 73 87 0 0 0 0 4095 1 
i 1 72 0.3 78 97 0 0 0 0 4095 1 
i 3 72 0.6 88 94 0 0 0 0 4095 1 
i 5 72 0.3 85 89 0 0 0 0 4095 1 
i 6 72 0.9 61 88 0 0 0 0 4095 1 
i 1 72.3 0.3 52 88 0 0 0 0 4095 1 
i 2 72.3 0.3 61 87 0 0 0 0 4095 1 
i 3 72.3 0.3 73 91 0 0 0 0 4095 1 
i 4 72.3 0.3 85 91 0 0 0 0 4095 1 
i 1 72.6 0.3 51 93 0 0 0 0 4095 1 
i 4 72.6 0.3 63 98 0 0 0 0 4095 1 
i 6 72.6 0.3 59 92 0 0 0 0 4095 1 
i 6 72.6 0.6 66 88 0 0 0 0 4095 1 
i 1 72.9 0.3 49 88 0 0 0 0 4095 1 
i 1 72.9 0.3 52 87 0 0 0 0 4095 1 
i 2 72.9 0.3 59 87 0 0 0 0 4095 1 
i 3 72.9 0.3 73 88 0 0 0 0 4095 1 
i 3 72.9 0.3 76 89 0 0 0 0 4095 1 
i 1 73.2 0.3 47 90 0 0 0 0 4095 1 
i 2 73.2 0.3 71 95 0 0 0 0 4095 1 
i 4 73.2 0.3 61 89 0 0 0 0 4095 1 
i 6 73.2 0.3 56 91 0 0 0 0 4095 1 
i 6 73.2 0.3 61 99 0 0 0 0 4095 1 
i 6 73.2 0.3 71 93 0 0 0 0 4095 1 
i 1 73.5 0.3 51 92 0 0 0 0 4095 1 
i 2 73.5 0.3 73 91 0 0 0 0 4095 1 
i 3 73.5 0.3 78 94 0 0 0 0 4095 1 
i 4 73.5 0.6 71 89 0 0 0 0 4095 1 
i 5 73.5 0.3 80 91 0 0 0 0 4095 1 
i 6 73.5 0.3 69 88 0 0 0 0 4095 1 
i 4 73.8 0.3 68 93 0 0 0 0 4095 1 
i 5 73.8 0.3 66 96 0 0 0 0 4095 1 
i 5 73.8 0.9 69 87 0 0 0 0 4095 1 
i 6 73.8 0.3 61 99 0 0 0 0 4095 1 
i 6 73.8 0.3 66 94 0 0 0 0 4095 1 
i 1 74.1 0.3 45 92 0 0 0 0 4095 1 
i 2 74.1 0.3 71 91 0 0 0 0 4095 1 
i 3 74.1 0.3 80 91 0 0 0 0 4095 1 
i 4 74.1 0.3 64 90 0 0 0 0 4095 1 
i 5 74.1 0.3 73 93 0 0 0 0 4095 1 
i 2 74.4 0.3 69 87 0 0 0 0 4095 1 
i 3 74.4 0.3 83 95 0 0 0 0 4095 1 
i 3 74.4 0.3 88 83 0 0 0 0 4095 1 
i 4 74.4 0.3 63 82 0 0 0 0 4095 1 
i 6 74.4 0.3 69 84 0 0 0 0 4095 1 
i 1 74.7 0.3 66 89 0 0 0 0 4095 1 
i 3 74.7 0.3 93 87 0 0 0 0 4095 1 
i 5 74.7 0.3 66 92 0 0 0 0 4095 1 
i 6 74.7 0.3 73 87 0 0 0 0 4095 1 
i 6 74.7 0.3 75 90 0 0 0 0 4095 1 
i 1 75 0.3 63 88 0 0 0 0 4095 1 
i 1 75 0.3 69 86 0 0 0 0 4095 1 
i 2 75 0.3 71 85 0 0 0 0 4095 1 
i 3 75 0.3 78 89 0 0 0 0 4095 1 
i 5 75 0.3 69 85 0 0 0 0 4095 1 
i 6 75 0.3 71 94 0 0 0 0 4095 1 
i 2 75.3 0.3 47 84 0 0 0 0 4095 1 
i 2 75.3 0.3 49 90 0 0 0 0 4095 1 
i 4 75.3 0.9 71 86 0 0 0 0 4095 1 
i 5 75.3 0.6 68 97 0 0 0 0 4095 1 
i 6 75.3 0.3 49 86 0 0 0 0 4095 1 
i 6 75.3 0.3 51 88 0 0 0 0 4095 1 
i 2 75.6 0.3 85 83 0 0 0 0 4095 1 
i 5 75.6 0.6 59 86 0 0 0 0 4095 1 
i 1 75.9 0.6 64 97 0 0 0 0 4095 1 
i 2 75.9 0.3 83 98 0 0 0 0 4095 1 
i 6 75.9 0.3 49 90 0 0 0 0 4095 1 
i 6 75.9 0.3 61 93 0 0 0 0 4095 1 
i 3 76.2 0.3 76 89 0 0 0 0 4095 1 
i 6 76.2 0.6 59 90 0 0 0 0 4095 1 
i 1 76.5 0.3 65 99 0 0 0 0 4095 1 
i 2 76.5 0.3 83 94 0 0 0 0 4095 1 
i 3 76.5 0.6 77 87 0 0 0 0 4095 1 
i 4 76.5 0.3 71 99 0 0 0 0 4095 1 
i 1 76.8 0.3 69 87 0 0 0 0 4095 1 
i 3 76.8 0.3 69 92 0 0 0 0 4095 1 
i 3 76.8 0.6 73 89 0 0 0 0 4095 1 
i 3 76.8 0.3 85 93 0 0 0 0 4095 1 
i 5 76.8 0.3 52 95 0 0 0 0 4095 1 
i 5 76.8 0.3 56 87 0 0 0 0 4095 1 
i 5 77.1 0.3 49 97 0 0 0 0 4095 1 
i 5 77.1 0.3 51 89 0 0 0 0 4095 1 
i 5 77.1 0.9 54 89 0 0 0 0 4095 1 
i 6 77.1 0.3 57 92 0 0 0 0 4095 1 
i 6 77.1 0.3 78 86 0 0 0 0 4095 1 
i 1 77.4 0.3 64 93 0 0 0 0 4095 1 
i 2 77.4 0.3 57 90 0 0 0 0 4095 1 
i 3 77.4 0.3 80 94 0 0 0 0 4095 1 
i 6 77.4 0.3 76 99 0 0 0 0 4095 1 
i 1 77.7 0.3 59 87 0 0 0 0 4095 1 
i 2 77.7 0.3 80 97 0 0 0 0 4095 1 
i 3 77.7 0.3 74 96 0 0 0 0 4095 1 
i 6 77.7 0.6 81 88 0 0 0 0 4095 1 
i 1 78 0.3 57 95 0 0 0 0 4095 1 
i 1 78 0.3 62 93 0 0 0 0 4095 1 
i 3 78 0.3 69 99 0 0 0 0 4095 1 
i 5 78 0.3 57 91 0 0 0 0 4095 1 
i 6 78 0.3 78 92 0 0 0 0 4095 1 
i 1 78.3 0.3 59 96 0 0 0 0 4095 1 
i 2 78.3 0.3 66 99 0 0 0 0 4095 1 
i 2 78.3 0.3 73 89 0 0 0 0 4095 1 
i 2 78.3 0.3 80 91 0 0 0 0 4095 1 
i 3 78.3 0.3 64 89 0 0 0 0 4095 1 
i 5 78.3 0.3 54 90 0 0 0 0 4095 1 
i 1 78.6 0.6 54 96 0 0 0 0 4095 1 
i 2 78.6 0.3 83 96 0 0 0 0 4095 1 
i 3 78.6 0.3 76 90 0 0 0 0 4095 1 
i 4 78.6 0.3 69 94 0 0 0 0 4095 1 
i 4 78.6 0.3 78 93 0 0 0 0 4095 1 
i 6 78.6 0.3 47 89 0 0 0 0 4095 1 
i 1 78.9 0.3 52 89 0 0 0 0 4095 1 
i 1 78.9 0.3 66 87 0 0 0 0 4095 1 
i 2 78.9 0.3 88 90 0 0 0 0 4095 1 
i 3 78.9 0.3 81 90 0 0 0 0 4095 1 
i 6 78.9 0.3 57 88 0 0 0 0 4095 1 
i 2 79.2 0.3 85 88 0 0 0 0 4095 1 
i 4 79.2 0.3 54 92 0 0 0 0 4095 1 
i 5 79.2 0.3 66 93 0 0 0 0 4095 1 
i 5 79.2 0.3 71 88 0 0 0 0 4095 1 
i 6 79.2 0.3 52 99 0 0 0 0 4095 1 
i 6 79.2 0.3 78 89 0 0 0 0 4095 1 
i 1 79.5 0.3 68 85 0 0 0 0 4095 1 
i 2 79.5 0.3 54 93 0 0 0 0 4095 1 
i 3 79.5 0.6 83 85 0 0 0 0 4095 1 
i 5 79.5 0.3 73 85 0 0 0 0 4095 1 
i 5 79.5 0.6 80 90 0 0 0 0 4095 1 
i 6 79.5 0.3 74 95 0 0 0 0 4095 1 
i 1 79.8 0.3 71 88 0 0 0 0 4095 1 
i 3 79.8 0.3 85 87 0 0 0 0 4095 1 
i 4 79.8 0.3 54 88 0 0 0 0 4095 1 
i 5 79.8 0.6 81 89 0 0 0 0 4095 1 
i 2 80.1 0.3 54 96 0 0 0 0 4095 1 
i 3 80.1 0.6 88 91 0 0 0 0 4095 1 
i 3 80.1 0.3 90 94 0 0 0 0 4095 1 
i 4 80.1 0.3 59 91 0 0 0 0 4095 1 
i 6 80.1 0.3 66 93 0 0 0 0 4095 1 
i 6 80.1 0.3 69 94 0 0 0 0 4095 1 
i 1 80.4 0.3 81 88 0 0 0 0 4095 1 
i 2 80.4 0.3 76 86 0 0 0 0 4095 1 
i 3 80.4 0.3 86 88 0 0 0 0 4095 1 
i 4 80.4 0.3 47 96 0 0 0 0 4095 1 
i 6 80.4 0.3 80 86 0 0 0 0 4095 1 
i 1 80.7 0.3 74 92 0 0 0 0 4095 1 
i 1 80.7 0.3 78 86 0 0 0 0 4095 1 
i 2 80.7 0.3 73 91 0 0 0 0 4095 1 
i 3 80.7 0.3 73 88 0 0 0 0 4095 1 
i 3 80.7 0.3 76 98 0 0 0 0 4095 1 
i 5 80.7 0.3 68 88 0 0 0 0 4095 1 
i 1 81 0.3 68 90 0 0 0 0 4095 1 
i 2 81 0.3 71 89 0 0 0 0 4095 1 
i 3 81 0.3 66 91 0 0 0 0 4095 1 
i 3 81 0.3 71 92 0 0 0 0 4095 1 
i 5 81 0.3 66 92 0 0 0 0 4095 1 
i 6 81 0.3 76 95 0 0 0 0 4095 1 
i 1 81.3 0.3 62 99 0 0 0 0 4095 1 
i 2 81.3 0.3 66 91 0 0 0 0 4095 1 
i 2 81.3 0.3 83 93 0 0 0 0 4095 1 
i 4 81.3 0.3 44 99 0 0 0 0 4095 1 
i 4 81.3 0.3 49 89 0 0 0 0 4095 1 
i 5 81.3 0.3 68 91 0 0 0 0 4095 1 
i 2 81.6 0.3 85 88 0 0 0 0 4095 1 
i 3 81.6 0.3 64 89 0 0 0 0 4095 1 
i 4 81.6 0.3 50 94 0 0 0 0 4095 1 
i 5 81.6 0.3 64 90 0 0 0 0 4095 1 
i 6 81.6 0.3 78 95 0 0 0 0 4095 1 
i 6 81.6 0.3 81 89 0 0 0 0 4095 1 
i 3 81.9 0.3 66 92 0 0 0 0 4095 1 
i 4 81.9 0.3 54 93 0 0 0 0 4095 1 
i 4 81.9 0.3 59 96 0 0 0 0 4095 1 
i 5 81.9 0.6 63 93 0 0 0 0 4095 1 
i 6 81.9 0.3 75 99 0 0 0 0 4095 1 
i 6 81.9 0.3 80 95 0 0 0 0 4095 1 
i 1 82.2 0.3 54 91 0 0 0 0 4095 1 
i 1 82.2 0.3 59 85 0 0 0 0 4095 1 
i 3 82.2 0.3 61 87 0 0 0 0 4095 1 
i 5 82.2 0.3 52 86 0 0 0 0 4095 1 
i 6 82.2 0.3 73 96 0 0 0 0 4095 1 
i 1 82.5 0.3 52 87 0 0 0 0 4095 1 
i 1 82.5 0.3 56 85 0 0 0 0 4095 1 
i 2 82.5 0.3 73 93 0 0 0 0 4095 1 
i 2 82.5 0.3 78 91 0 0 0 0 4095 1 
i 3 82.5 0.3 83 87 0 0 0 0 4095 1 
i 5 82.5 0.3 57 85 0 0 0 0 4095 1 
i 2 82.8 0.3 52 91 0 0 0 0 4095 1 
i 3 82.8 0.3 73 87 0 0 0 0 4095 1 
i 3 82.8 0.6 78 87 0 0 0 0 4095 1 
i 4 82.8 0.3 54 92 0 0 0 0 4095 1 
i 5 82.8 0.3 66 87 0 0 0 0 4095 1 
i 6 82.8 0.3 64 96 0 0 0 0 4095 1 
i 1 83.1 0.9 54 93 0 0 0 0 4095 1 
i 1 83.1 0.3 59 87 0 0 0 0 4095 1 
i 6 83.1 0.3 61 86 0 0 0 0 4095 1 
i 6 83.1 0.9 69 93 0 0 0 0 4095 1 
i 2 83.4 0.3 49 90 0 0 0 0 4095 1 
i 4 83.4 0.3 50 88 0 0 0 0 4095 1 
i 5 83.4 0.3 64 87 0 0 0 0 4095 1 
i 6 83.4 0.3 73 85 0 0 0 0 4095 1 
i 3 83.7 0.3 64 88 0 0 0 0 4095 1 
i 4 83.7 0.3 49 87 0 0 0 0 4095 1 
i 5 83.7 0.3 83 92 0 0 0 0 4095 1 
i 6 83.7 0.3 74 96 0 0 0 0 4095 1 
i 1 84 0.3 59 88 0 0 0 0 4095 1 
i 2 84 0.3 47 88 0 0 0 0 4095 1 
i 2 84 0.3 52 90 0 0 0 0 4095 1 
i 3 84 0.3 66 97 0 0 0 0 4095 1 
i 5 84 0.9 78 88 0 0 0 0 4095 1 
i 6 84 0.3 68 87 0 0 0 0 4095 1 
i 1 84.3 0.3 64 90 0 0 0 0 4095 1 
i 2 84.3 0.3 83 91 0 0 0 0 4095 1 
i 4 84.3 0.3 40 90 0 0 0 0 4095 1 
i 4 84.3 0.6 45 89 0 0 0 0 4095 1 
i 1 84.6 0.3 83 88 0 0 0 0 4095 1 
i 2 84.6 0.3 81 92 0 0 0 0 4095 1 
i 5 84.6 0.3 56 95 0 0 0 0 4095 1 
i 6 84.6 1.5 66 91 0 0 0 0 4095 1 
i 2 84.9 0.3 49 100 0 0 0 0 4095 1 
i 2 84.9 0.3 80 89 0 0 0 0 4095 1 
i 3 84.9 1.2 66 96 0 0 0 0 4095 1 
i 4 84.9 0.3 42 87 0 0 0 0 4095 1 
i 4 84.9 0.3 44 93 0 0 0 0 4095 1 
i 1 85.2 0.9 81 92 0 0 0 0 4095 1 
i 2 85.2 0.6 81 84 0 0 0 0 4095 1 
i 5 85.2 0.3 57 85 0 0 0 0 4095 1 
i 4 85.8 0.3 45 87 0 0 0 0 4095 1 
i 4 85.8 0.3 49 89 0 0 0 0 4095 1 
i 2 86.1 0.3 81 100 0 0 0 0 4095 1 
i 3 86.1 0.3 73 92 0 0 0 0 4095 1 
i 2 86.4 0.3 76 91 0 0 0 0 4095 1 
i 3 86.4 0.3 69 94 0 0 0 0 4095 1 
i 4 86.4 0.3 81 89 0 0 0 0 4095 1 
i 5 86.4 0.3 57 94 0 0 0 0 4095 1 
i 6 86.4 0.3 59 85 0 0 0 0 4095 1 
i 6 86.4 0.3 62 91 0 0 0 0 4095 1 
i 2 86.7 0.3 54 93 0 0 0 0 4095 1 
i 3 86.7 0.3 71 84 0 0 0 0 4095 1 
i 5 86.7 0.3 62 99 0 0 0 0 4095 1 
i 5 86.7 0.3 69 93 0 0 0 0 4095 1 
i 6 86.7 0.3 54 99 0 0 0 0 4095 1 
i 6 86.7 0.6 56 84 0 0 0 0 4095 1 
i 1 87 0.3 86 97 0 0 0 0 4095 1 
i 2 87 0.3 76 89 0 0 0 0 4095 1 
i 3 87 0.3 66 100 0 0 0 0 4095 1 
i 5 87 0.3 74 95 0 0 0 0 4095 1 
i 6 87 0.3 59 90 0 0 0 0 4095 1 
i 1 87.3 0.3 78 91 0 0 0 0 4095 1 
i 2 87.3 0.3 69 91 0 0 0 0 4095 1 
i 4 87.3 0.3 57 92 0 0 0 0 4095 1 
i 4 87.3 0.3 59 93 0 0 0 0 4095 1 
i 5 87.3 0.9 73 97 0 0 0 0 4095 1 
i 1 87.6 0.3 64 88 0 0 0 0 4095 1 
i 2 87.6 0.3 64 88 0 0 0 0 4095 1 
i 2 87.6 0.3 66 87 0 0 0 0 4095 1 
i 5 87.6 0.3 71 93 0 0 0 0 4095 1 
i 1 87.9 0.3 66 89 0 0 0 0 4095 1 
i 1 87.9 0.3 69 86 0 0 0 0 4095 1 
i 2 87.9 0.3 71 90 0 0 0 0 4095 1 
i 6 87.9 0.3 59 87 0 0 0 0 4095 1 
i 6 87.9 0.3 66 86 0 0 0 0 4095 1 
i 2 88.2 0.3 73 93 0 0 0 0 4095 1 
i 2 88.2 0.3 76 87 0 0 0 0 4095 1 
i 3 88.2 0.3 64 98 0 0 0 0 4095 1 
i 4 88.2 0.3 59 88 0 0 0 0 4095 1 
i 5 88.2 0.3 66 93 0 0 0 0 4095 1 
i 5 88.2 0.6 69 90 0 0 0 0 4095 1 
i 1 88.5 0.3 73 89 0 0 0 0 4095 1 
i 3 88.5 0.3 66 94 0 0 0 0 4095 1 
i 4 88.5 0.3 64 91 0 0 0 0 4095 1 
i 5 88.5 0.3 80 91 0 0 0 0 4095 1 
i 2 88.8 0.3 64 100 0 0 0 0 4095 1 
i 2 88.8 0.3 69 97 0 0 0 0 4095 1 
i 5 88.8 0.3 85 95 0 0 0 0 4095 1 
i 6 88.8 0.3 64 89 0 0 0 0 4095 1 
i 6 88.8 0.3 68 94 0 0 0 0 4095 1 
i 6 88.8 0.3 69 93 0 0 0 0 4095 1 
i 1 89.1 0.3 71 89 0 0 0 0 4095 1 
i 1 89.1 0.3 76 99 0 0 0 0 4095 1 
i 2 89.1 0.3 54 87 0 0 0 0 4095 1 
i 2 89.1 0.3 59 90 0 0 0 0 4095 1 
i 4 89.1 0.6 66 88 0 0 0 0 4095 1 
i 1 89.4 0.3 56 88 0 0 0 0 4095 1 
i 1 89.4 0.3 57 91 0 0 0 0 4095 1 
i 1 89.4 0.3 66 93 0 0 0 0 4095 1 
i 4 89.4 0.3 61 89 0 0 0 0 4095 1 
i 6 89.4 0.3 62 88 0 0 0 0 4095 1 
i 1 89.7 0.3 68 92 0 0 0 0 4095 1 
i 2 89.7 0.3 64 90 0 0 0 0 4095 1 
i 5 89.7 0.3 83 90 0 0 0 0 4095 1 
i 6 89.7 0.3 64 89 0 0 0 0 4095 1 
i 6 89.7 0.3 66 90 0 0 0 0 4095 1 
i 2 90 0.3 61 87 0 0 0 0 4095 1 
i 3 90 0.3 61 88 0 0 0 0 4095 1 
i 4 90 0.3 61 97 0 0 0 0 4095 1 
i 4 90 0.3 64 95 0 0 0 0 4095 1 
i 5 90 0.3 80 92 0 0 0 0 4095 1 
i 6 90 0.3 71 99 0 0 0 0 4095 1 
i 1 90.3 0.3 61 91 0 0 0 0 4095 1 
i 1 90.3 0.3 66 87 0 0 0 0 4095 1 
i 1 90.3 0.3 85 87 0 0 0 0 4095 1 
i 3 90.3 0.3 66 96 0 0 0 0 4095 1 
i 4 90.3 0.3 81 90 0 0 0 0 4095 1 
i 1 90.6 0.3 83 97 0 0 0 0 4095 1 
i 2 90.6 0.3 61 94 0 0 0 0 4095 1 
i 4 90.6 0.3 54 94 0 0 0 0 4095 1 
i 5 90.6 0.3 78 94 0 0 0 0 4095 1 
i 6 90.6 0.3 73 93 0 0 0 0 4095 1 
i 2 90.9 0.3 59 98 0 0 0 0 4095 1 
i 3 90.9 0.3 64 93 0 0 0 0 4095 1 
i 3 90.9 0.3 68 94 0 0 0 0 4095 1 
i 4 90.9 0.3 64 93 0 0 0 0 4095 1 
i 6 90.9 0.6 47 93 0 0 0 0 4095 1 
i 6 90.9 0.6 49 90 0 0 0 0 4095 1 
i 1 91.2 0.3 76 87 0 0 0 0 4095 1 
i 3 91.2 0.3 56 97 0 0 0 0 4095 1 
i 4 91.2 0.6 47 95 0 0 0 0 4095 1 
i 4 91.2 0.3 49 87 0 0 0 0 4095 1 
i 2 91.5 0.3 64 95 0 0 0 0 4095 1 
i 2 91.5 0.3 66 99 0 0 0 0 4095 1 
i 3 91.5 0.3 59 91 0 0 0 0 4095 1 
i 3 91.5 0.6 61 94 0 0 0 0 4095 1 
i 6 91.5 0.3 50 95 0 0 0 0 4095 1 
i 1 91.8 0.3 45 91 0 0 0 0 4095 1 
i 5 91.8 0.3 78 97 0 0 0 0 4095 1 
i 5 91.8 0.3 83 93 0 0 0 0 4095 1 
i 5 91.8 0.3 86 94 0 0 0 0 4095 1 
i 5 91.8 0.3 88 90 0 0 0 0 4095 1 
i 1 92.1 0.3 42 91 0 0 0 0 4095 1 
i 1 92.1 0.3 47 91 0 0 0 0 4095 1 
i 2 92.1 0.3 61 95 0 0 0 0 4095 1 
i 3 92.1 0.3 56 97 0 0 0 0 4095 1 
i 4 92.1 0.3 49 93 0 0 0 0 4095 1 
i 6 92.1 0.3 54 91 0 0 0 0 4095 1 
i 1 92.4 0.3 44 92 0 0 0 0 4095 1 
i 2 92.4 0.6 59 92 0 0 0 0 4095 1 
i 3 92.4 0.3 50 90 0 0 0 0 4095 1 
i 4 92.4 0.3 52 93 0 0 0 0 4095 1 
i 4 92.4 0.3 56 93 0 0 0 0 4095 1 
i 5 92.4 0.3 47 90 0 0 0 0 4095 1 
i 1 92.7 0.3 40 98 0 0 0 0 4095 1 
i 1 92.7 0.3 42 95 0 0 0 0 4095 1 
i 4 92.7 0.3 61 91 0 0 0 0 4095 1 
i 4 92.7 0.3 62 91 0 0 0 0 4095 1 
i 6 92.7 0.3 52 95 0 0 0 0 4095 1 
i 1 93 0.3 45 94 0 0 0 0 4095 1 
i 2 93 0.3 56 89 0 0 0 0 4095 1 
i 3 93 0.3 47 91 0 0 0 0 4095 1 
i 5 93 0.6 49 91 0 0 0 0 4095 1 
i 5 93 0.3 52 94 0 0 0 0 4095 1 
i 6 93 0.3 47 90 0 0 0 0 4095 1 
i 3 93.3 0.3 45 93 0 0 0 0 4095 1 
i 3 93.3 0.3 59 96 0 0 0 0 4095 1 
i 4 93.3 0.3 62 91 0 0 0 0 4095 1 
i 4 93.3 0.3 64 91 0 0 0 0 4095 1 
i 5 93.3 0.3 56 91 0 0 0 0 4095 1 
i 1 93.6 0.6 42 96 0 0 0 0 4095 1 
i 2 93.6 0.3 52 93 0 0 0 0 4095 1 
i 3 93.6 0.3 61 99 0 0 0 0 4095 1 
i 4 93.6 0.3 78 95 0 0 0 0 4095 1 
i 5 93.6 0.3 54 93 0 0 0 0 4095 1 
i 6 93.6 0.3 44 95 0 0 0 0 4095 1 
i 1 93.9 0.6 50 87 0 0 0 0 4095 1 
i 5 93.9 0.3 57 97 0 0 0 0 4095 1 
i 5 93.9 0.3 71 98 0 0 0 0 4095 1 
i 5 93.9 0.3 78 93 0 0 0 0 4095 1 
i 6 93.9 0.3 40 88 0 0 0 0 4095 1 
i 1 94.2 0.3 45 89 0 0 0 0 4095 1 
i 3 94.2 0.3 57 94 0 0 0 0 4095 1 
i 3 94.2 0.3 59 89 0 0 0 0 4095 1 
i 3 94.2 0.3 78 85 0 0 0 0 4095 1 
i 6 94.2 0.6 52 87 0 0 0 0 4095 1 
i 1 94.5 0.3 47 93 0 0 0 0 4095 1 
i 1 94.5 0.6 52 88 0 0 0 0 4095 1 
i 2 94.5 0.6 50 91 0 0 0 0 4095 1 
i 6 94.5 0.3 57 90 0 0 0 0 4095 1 
i 1 94.8 0.6 50 84 0 0 0 0 4095 1 
i 4 94.8 1.2 78 89 0 0 0 0 4095 1 
i 3 95.1 0.3 78 94 0 0 0 0 4095 1 
i 5 95.1 0.9 78 91 0 0 0 0 4095 1 
i 6 95.1 0.3 57 87 0 0 0 0 4095 1 
i 1 95.4 0.6 47 84 0 0 0 0 4095 1 
i 2 95.4 0.6 50 87 0 0 0 0 4095 1 
i 2 95.7 0.3 62 89 0 0 0 0 4095 1 
i 3 95.7 0.3 78 84 0 0 0 0 4095 1 
i 1 96 0.3 50 87 0 0 0 0 4095 1 
i 3 96 0.3 52 97 0 0 0 0 4095 1 
i 3 96 0.3 54 85 0 0 0 0 4095 1 
i 3 96 0.3 56 90 0 0 0 0 4095 1 
i 3 96 0.3 59 89 0 0 0 0 4095 1 
i 4 96 0.3 69 97 0 0 0 0 4095 1 
i 2 96.3 0.3 66 84 0 0 0 0 4095 1 
i 3 96.3 0.3 61 84 0 0 0 0 4095 1 
i 4 96.3 0.3 64 100 0 0 0 0 4095 1 
i 4 96.3 0.6 71 89 0 0 0 0 4095 1 
i 5 96.3 0.3 50 87 0 0 0 0 4095 1 
i 5 96.3 0.3 56 90 0 0 0 0 4095 1 
i 1 96.6 0.3 47 88 0 0 0 0 4095 1 
i 3 96.6 0.3 56 89 0 0 0 0 4095 1 
i 4 96.6 0.3 68 85 0 0 0 0 4095 1 
i 5 96.6 0.6 54 93 0 0 0 0 4095 1 
i 6 96.6 0.3 57 86 0 0 0 0 4095 1 
i 2 96.9 0.3 71 89 0 0 0 0 4095 1 
i 4 96.9 0.3 47 95 0 0 0 0 4095 1 
i 5 96.9 0.3 61 91 0 0 0 0 4095 1 
i 6 96.9 0.6 56 89 0 0 0 0 4095 1 
i 6 96.9 0.3 59 90 0 0 0 0 4095 1 
i 1 97.2 0.9 47 89 0 0 0 0 4095 1 
i 2 97.2 0.3 76 87 0 0 0 0 4095 1 
i 4 97.2 0.6 42 89 0 0 0 0 4095 1 
i 5 97.2 0.3 59 95 0 0 0 0 4095 1 
i 6 97.2 0.3 54 88 0 0 0 0 4095 1 
i 1 97.5 0.6 49 93 0 0 0 0 4095 1 
i 3 97.5 0.3 56 90 0 0 0 0 4095 1 
i 5 97.5 0.9 61 87 0 0 0 0 4095 1 
i 5 97.5 0.3 64 89 0 0 0 0 4095 1 
i 1 97.8 0.3 85 95 0 0 0 0 4095 1 
i 6 97.8 0.3 50 94 0 0 0 0 4095 1 
i 1 98.1 0.3 83 85 0 0 0 0 4095 1 
i 2 98.1 0.6 74 85 0 0 0 0 4095 1 
i 4 98.1 0.3 44 91 0 0 0 0 4095 1 
i 5 98.1 0.3 78 90 0 0 0 0 4095 1 
i 6 98.1 0.3 44 99 0 0 0 0 4095 1 
i 1 98.4 0.3 78 91 0 0 0 0 4095 1 
i 2 98.4 0.3 76 88 0 0 0 0 4095 1 
i 6 98.4 0.3 49 85 0 0 0 0 4095 1 
i 6 98.4 0.3 52 86 0 0 0 0 4095 1 
i 1 98.7 0.3 80 93 0 0 0 0 4095 1 
i 2 98.7 0.6 71 89 0 0 0 0 4095 1 
i 3 98.7 0.3 47 93 0 0 0 0 4095 1 
i 3 98.7 0.3 71 91 0 0 0 0 4095 1 
i 4 98.7 0.3 49 93 0 0 0 0 4095 1 
i 6 98.7 0.3 45 89 0 0 0 0 4095 1 
i 1 99 0.3 56 97 0 0 0 0 4095 1 
i 2 99 0.3 47 95 0 0 0 0 4095 1 
i 2 99 0.3 49 98 0 0 0 0 4095 1 
i 5 99 0.6 56 93 0 0 0 0 4095 1 
i 6 99 0.3 40 94 0 0 0 0 4095 1 
i 1 99.3 0.3 44 87 0 0 0 0 4095 1 
i 1 99.3 0.3 47 91 0 0 0 0 4095 1 
i 4 99.3 0.3 50 89 0 0 0 0 4095 1 
i 5 99.3 0.6 54 93 0 0 0 0 4095 1 
i 1 99.6 0.3 76 87 0 0 0 0 4095 1 
i 2 99.6 0.9 45 89 0 0 0 0 4095 1 
i 2 99.6 0.3 47 86 0 0 0 0 4095 1 
i 5 99.6 0.3 52 86 0 0 0 0 4095 1 
i 5 99.6 0.3 83 95 0 0 0 0 4095 1 
i 2 99.9 0.9 42 95 0 0 0 0 4095 1 
i 2 99.9 0.3 49 92 0 0 0 0 4095 1 
i 4 99.9 0.3 68 100 0 0 0 0 4095 1 
i 5 99.9 0.3 86 90 0 0 0 0 4095 1 
i 6 99.9 0.3 42 89 0 0 0 0 4095 1 
i 3 100.2 0.3 69 98 0 0 0 0 4095 1 
i 4 100.2 0.3 66 88 0 0 0 0 4095 1 
i 5 100.2 0.3 88 90 0 0 0 0 4095 1 
i 6 100.2 0.6 50 90 0 0 0 0 4095 1 
i 1 100.5 0.3 78 97 0 0 0 0 4095 1 
i 3 100.5 0.6 73 89 0 0 0 0 4095 1 
i 4 100.5 0.3 49 96 0 0 0 0 4095 1 
i 5 100.5 0.3 54 90 0 0 0 0 4095 1 
i 1 100.8 0.3 66 89 0 0 0 0 4095 1 
i 1 100.8 0.3 68 90 0 0 0 0 4095 1 
i 3 100.8 0.3 68 90 0 0 0 0 4095 1 
i 6 100.8 0.6 45 93 0 0 0 0 4095 1 
i 6 100.8 0.3 47 94 0 0 0 0 4095 1 
i 2 101.1 0.6 61 87 0 0 0 0 4095 1 
i 3 101.1 0.3 63 89 0 0 0 0 4095 1 
i 5 101.1 0.3 54 91 0 0 0 0 4095 1 
i 5 101.1 0.3 56 88 0 0 0 0 4095 1 
i 6 101.1 0.3 85 99 0 0 0 0 4095 1 
i 2 101.4 0.3 56 99 0 0 0 0 4095 1 
i 3 101.4 0.3 61 97 0 0 0 0 4095 1 
i 4 101.4 0.3 50 94 0 0 0 0 4095 1 
i 6 101.4 0.3 88 99 0 0 0 0 4095 1 
i 6 101.4 0.3 92 91 0 0 0 0 4095 1 
i 1 101.7 0.3 66 87 0 0 0 0 4095 1 
i 3 101.7 0.3 66 91 0 0 0 0 4095 1 
i 4 101.7 0.3 44 89 0 0 0 0 4095 1 
i 5 101.7 0.3 59 88 0 0 0 0 4095 1 
i 5 101.7 0.3 61 89 0 0 0 0 4095 1 
i 6 101.7 0.6 95 94 0 0 0 0 4095 1 
i 1 102 0.3 49 97 0 0 0 0 4095 1 
i 1 102 0.3 54 97 0 0 0 0 4095 1 
i 4 102 0.6 37 91 0 0 0 0 4095 1 
i 4 102 0.3 42 89 0 0 0 0 4095 1 
i 5 102 0.3 66 89 0 0 0 0 4095 1 
i 1 102.3 0.6 52 92 0 0 0 0 4095 1 
i 2 102.3 0.3 56 90 0 0 0 0 4095 1 
i 3 102.3 0.3 61 91 0 0 0 0 4095 1 
i 3 102.3 0.3 66 90 0 0 0 0 4095 1 
i 4 102.3 0.3 62 91 0 0 0 0 4095 1 
i 1 102.6 0.6 49 95 0 0 0 0 4095 1 
i 2 102.6 0.3 54 90 0 0 0 0 4095 1 
i 5 102.6 0.3 64 97 0 0 0 0 4095 1 
i 6 102.6 0.3 78 92 0 0 0 0 4095 1 
i 6 102.6 0.3 81 91 0 0 0 0 4095 1 
i 1 102.9 0.3 50 92 0 0 0 0 4095 1 
i 2 102.9 0.3 56 88 0 0 0 0 4095 1 
i 5 102.9 0.3 61 98 0 0 0 0 4095 1 
i 6 102.9 0.6 83 91 0 0 0 0 4095 1 
i 6 102.9 0.3 85 92 0 0 0 0 4095 1 
i 1 103.2 0.6 54 93 0 0 0 0 4095 1 
i 4 103.2 0.6 49 94 0 0 0 0 4095 1 
i 4 103.2 0.3 50 99 0 0 0 0 4095 1 
i 6 103.2 0.3 47 95 0 0 0 0 4095 1 
i 2 103.5 0.3 54 91 0 0 0 0 4095 1 
i 5 103.5 0.3 68 89 0 0 0 0 4095 1 
i 6 103.5 0.6 61 91 0 0 0 0 4095 1 
i 1 103.8 0.3 49 99 0 0 0 0 4095 1 
i 4 103.8 0.3 42 96 0 0 0 0 4095 1 
i 4 103.8 0.3 44 92 0 0 0 0 4095 1 
i 6 103.8 0.3 59 95 0 0 0 0 4095 1 
i 6 103.8 0.3 73 91 0 0 0 0 4095 1 
i 1 104.1 0.3 44 95 0 0 0 0 4095 1 
i 1 104.1 0.3 68 91 0 0 0 0 4095 1 
i 2 104.1 0.3 59 92 0 0 0 0 4095 1 
i 3 104.1 0.9 69 94 0 0 0 0 4095 1 
i 4 104.1 0.3 40 93 0 0 0 0 4095 1 
i 4 104.1 0.9 78 95 0 0 0 0 4095 1 
i 5 104.4 0.3 69 93 0 0 0 0 4095 1 
i 5 104.4 0.3 73 95 0 0 0 0 4095 1 
i 1 104.7 0.9 69 89 0 0 0 0 4095 1 
i 2 104.7 0.3 59 88 0 0 0 0 4095 1 
i 2 105 0.3 57 96 0 0 0 0 4095 1 
i 2 105 0.3 73 90 0 0 0 0 4095 1 
i 4 105 0.3 69 96 0 0 0 0 4095 1 
i 5 105 0.6 69 90 0 0 0 0 4095 1 
i 6 105 0.6 73 88 0 0 0 0 4095 1 
i 3 105.3 0.3 69 95 0 0 0 0 4095 1 
i 5 105.3 0.3 73 89 0 0 0 0 4095 1 
i 1 105.6 0.6 68 91 0 0 0 0 4095 1 
i 2 105.6 0.3 69 97 0 0 0 0 4095 1 
i 3 105.6 0.3 64 93 0 0 0 0 4095 1 
i 4 105.6 0.3 73 95 0 0 0 0 4095 1 
i 6 105.6 0.3 69 93 0 0 0 0 4095 1 
i 6 105.6 0.3 71 100 0 0 0 0 4095 1 
i 1 105.9 0.3 69 94 0 0 0 0 4095 1 
i 2 105.9 0.3 68 99 0 0 0 0 4095 1 
i 2 105.9 0.3 80 93 0 0 0 0 4095 1 
i 4 105.9 0.3 66 96 0 0 0 0 4095 1 
i 6 105.9 0.6 73 97 0 0 0 0 4095 1 
i 1 106.2 0.6 61 95 0 0 0 0 4095 1 
i 2 106.2 0.3 74 91 0 0 0 0 4095 1 
i 4 106.2 0.3 56 92 0 0 0 0 4095 1 
i 4 106.2 0.3 68 93 0 0 0 0 4095 1 
i 1 106.5 0.3 73 93 0 0 0 0 4095 1 
i 3 106.5 0.3 61 94 0 0 0 0 4095 1 
i 3 106.5 0.3 62 91 0 0 0 0 4095 1 
i 4 106.5 0.3 64 91 0 0 0 0 4095 1 
i 6 106.5 0.3 68 97 0 0 0 0 4095 1 
i 1 106.8 0.3 66 91 0 0 0 0 4095 1 
i 1 106.8 0.3 68 98 0 0 0 0 4095 1 
i 2 106.8 0.3 78 89 0 0 0 0 4095 1 
i 4 106.8 0.3 66 90 0 0 0 0 4095 1 
i 5 106.8 0.3 76 90 0 0 0 0 4095 1 
i 5 106.8 0.3 78 91 0 0 0 0 4095 1 
i 1 107.1 0.3 64 90 0 0 0 0 4095 1 
i 2 107.1 0.3 47 90 0 0 0 0 4095 1 
i 2 107.1 0.3 50 93 0 0 0 0 4095 1 
i 3 107.1 0.3 59 96 0 0 0 0 4095 1 
i 4 107.1 0.3 61 96 0 0 0 0 4095 1 
i 6 107.1 0.6 73 95 0 0 0 0 4095 1 
i 1 107.4 0.3 68 94 0 0 0 0 4095 1 
i 5 107.4 0.3 64 89 0 0 0 0 4095 1 
i 5 107.4 0.3 66 100 0 0 0 0 4095 1 
i 6 107.4 0.3 62 97 0 0 0 0 4095 1 
i 6 107.4 0.3 68 95 0 0 0 0 4095 1 
i 1 107.7 0.6 49 87 0 0 0 0 4095 1 
i 2 107.7 0.3 47 90 0 0 0 0 4095 1 
i 2 107.7 0.3 66 87 0 0 0 0 4095 1 
i 3 107.7 0.3 56 91 0 0 0 0 4095 1 
i 4 107.7 0.3 56 93 0 0 0 0 4095 1 
i 6 107.7 0.3 56 89 0 0 0 0 4095 1 
i 1 108 0.3 52 92 0 0 0 0 4095 1 
i 6 108 0.3 57 87 0 0 0 0 4095 1 
i 6 108 0.3 59 88 0 0 0 0 4095 1 
i 6 108 0.3 64 87 0 0 0 0 4095 1 
i 6 108 0.9 80 92 0 0 0 0 4095 1 
i 1 108.3 0.3 66 91 0 0 0 0 4095 1 
i 2 108.3 0.3 69 95 0 0 0 0 4095 1 
i 2 108.3 0.3 71 93 0 0 0 0 4095 1 
i 3 108.3 0.3 54 100 0 0 0 0 4095 1 
i 6 108.3 0.3 83 96 0 0 0 0 4095 1 
i 1 108.6 0.3 62 96 0 0 0 0 4095 1 
i 4 108.6 0.3 52 97 0 0 0 0 4095 1 
i 5 108.6 0.3 62 92 0 0 0 0 4095 1 
i 6 108.6 0.3 76 93 0 0 0 0 4095 1 
i 2 108.9 0.3 76 100 0 0 0 0 4095 1 
i 2 108.9 0.3 81 94 0 0 0 0 4095 1 
i 2 108.9 0.3 85 93 0 0 0 0 4095 1 
i 2 108.9 0.3 88 91 0 0 0 0 4095 1 
i 3 108.9 0.3 80 95 0 0 0 0 4095 1 
i 1 109.2 0.3 68 90 0 0 0 0 4095 1 
i 2 109.2 0.3 69 89 0 0 0 0 4095 1 
i 2 109.2 0.3 71 94 0 0 0 0 4095 1 
i 3 109.2 0.3 76 96 0 0 0 0 4095 1 
i 5 109.2 0.6 57 95 0 0 0 0 4095 1 
i 6 109.2 0.6 83 92 0 0 0 0 4095 1 
i 2 109.5 0.3 54 88 0 0 0 0 4095 1 
i 3 109.5 0.3 80 99 0 0 0 0 4095 1 
i 5 109.5 0.3 68 94 0 0 0 0 4095 1 
i 6 109.5 0.3 85 97 0 0 0 0 4095 1 
i 1 109.8 0.3 61 96 0 0 0 0 4095 1 
i 1 109.8 0.3 64 92 0 0 0 0 4095 1 
i 4 109.8 0.3 57 98 0 0 0 0 4095 1 
i 6 109.8 0.3 73 90 0 0 0 0 4095 1 
i 6 109.8 0.3 78 89 0 0 0 0 4095 1 
i 2 110.1 0.3 59 86 0 0 0 0 4095 1 
i 3 110.1 0.6 85 89 0 0 0 0 4095 1 
i 4 110.1 0.3 62 95 0 0 0 0 4095 1 
i 5 110.1 0.3 64 94 0 0 0 0 4095 1 
i 6 110.1 0.3 68 97 0 0 0 0 4095 1 
i 6 110.1 0.3 71 91 0 0 0 0 4095 1 
i 1 110.4 0.3 76 89 0 0 0 0 4095 1 
i 2 110.4 0.3 80 90 0 0 0 0 4095 1 
i 4 110.4 0.3 64 88 0 0 0 0 4095 1 
i 5 110.4 0.3 59 96 0 0 0 0 4095 1 
i 6 110.4 0.3 66 87 0 0 0 0 4095 1 
i 1 110.7 0.3 80 88 0 0 0 0 4095 1 
i 2 110.7 0.3 83 98 0 0 0 0 4095 1 
i 3 110.7 0.3 86 94 0 0 0 0 4095 1 
i 3 110.7 0.3 90 92 0 0 0 0 4095 1 
i 4 110.7 0.3 73 91 0 0 0 0 4095 1 
i 4 110.7 0.3 76 93 0 0 0 0 4095 1 
i 2 111 0.3 81 98 0 0 0 0 4095 1 
i 3 111 0.3 71 96 0 0 0 0 4095 1 
i 4 111 0.3 61 97 0 0 0 0 4095 1 
i 4 111 0.3 64 100 0 0 0 0 4095 1 
i 5 111 0.3 61 93 0 0 0 0 4095 1 
i 6 111 0.3 71 92 0 0 0 0 4095 1 
i 1 111.3 0.3 59 90 0 0 0 0 4095 1 
i 1 111.3 0.6 61 99 0 0 0 0 4095 1 
i 1 111.3 0.3 62 92 0 0 0 0 4095 1 
i 1 111.3 0.3 76 98 0 0 0 0 4095 1 
i 3 111.3 0.3 69 96 0 0 0 0 4095 1 
i 5 111.3 0.3 55 92 0 0 0 0 4095 1 
i 2 111.6 0.6 78 89 0 0 0 0 4095 1 
i 3 111.6 0.6 73 97 0 0 0 0 4095 1 
i 4 111.6 0.3 80 89 0 0 0 0 4095 1 
i 5 111.6 0.3 52 92 0 0 0 0 4095 1 
i 6 111.6 0.6 76 96 0 0 0 0 4095 1 
i 1 111.9 0.3 81 94 0 0 0 0 4095 1 
i 3 111.9 0.3 71 97 0 0 0 0 4095 1 
i 6 111.9 0.6 78 91 0 0 0 0 4095 1 
i 1 112.2 0.3 85 93 0 0 0 0 4095 1 
i 2 112.2 0.9 59 95 0 0 0 0 4095 1 
i 3 112.2 0.3 61 96 0 0 0 0 4095 1 
i 4 112.2 0.3 80 91 0 0 0 0 4095 1 
i 5 112.2 0.3 52 90 0 0 0 0 4095 1 
i 1 112.5 0.3 69 91 0 0 0 0 4095 1 
i 1 112.5 0.3 71 90 0 0 0 0 4095 1 
i 3 112.5 0.6 64 90 0 0 0 0 4095 1 
i 3 112.5 0.3 66 93 0 0 0 0 4095 1 
i 5 112.5 0.3 47 88 0 0 0 0 4095 1 
i 2 112.8 0.3 52 96 0 0 0 0 4095 1 
i 4 112.8 0.3 80 91 0 0 0 0 4095 1 
i 4 112.8 0.6 85 90 0 0 0 0 4095 1 
i 5 112.8 0.3 80 93 0 0 0 0 4095 1 
i 2 113.1 0.3 85 97 0 0 0 0 4095 1 
i 3 113.1 0.3 80 92 0 0 0 0 4095 1 
i 4 113.1 0.3 83 92 0 0 0 0 4095 1 
i 5 113.1 0.3 76 91 0 0 0 0 4095 1 
i 6 113.1 0.6 80 91 0 0 0 0 4095 1 
i 1 113.4 0.3 71 91 0 0 0 0 4095 1 
i 2 113.4 0.6 81 89 0 0 0 0 4095 1 
i 4 113.4 0.3 78 96 0 0 0 0 4095 1 
i 5 113.4 0.3 69 91 0 0 0 0 4095 1 
i 5 113.4 0.3 71 94 0 0 0 0 4095 1 
i 1 113.7 0.3 76 93 0 0 0 0 4095 1 
i 1 113.7 0.3 78 98 0 0 0 0 4095 1 
i 1 113.7 0.3 85 95 0 0 0 0 4095 1 
i 3 113.7 0.3 54 96 0 0 0 0 4095 1 
i 5 113.7 0.3 49 94 0 0 0 0 4095 1 
i 2 114 0.9 57 90 0 0 0 0 4095 1 
i 3 114 0.3 61 90 0 0 0 0 4095 1 
i 4 114 0.3 78 93 0 0 0 0 4095 1 
i 6 114 0.9 81 94 0 0 0 0 4095 1 
i 5 114.3 0.3 45 91 0 0 0 0 4095 1 
i 5 114.3 0.3 49 87 0 0 0 0 4095 1 
i 5 114.3 0.3 85 89 0 0 0 0 4095 1 
i 1 114.6 0.6 85 91 0 0 0 0 4095 1 
i 5 114.6 0.3 81 93 0 0 0 0 4095 1 
i 2 114.9 0.3 56 92 0 0 0 0 4095 1 
i 4 114.9 0.3 77 93 0 0 0 0 4095 1 
i 5 114.9 0.3 71 92 0 0 0 0 4095 1 
i 6 114.9 0.6 80 92 0 0 0 0 4095 1 
i 1 115.2 0.3 88 90 0 0 0 0 4095 1 
i 2 115.2 0.3 52 94 0 0 0 0 4095 1 
i 3 115.2 0.3 56 93 0 0 0 0 4095 1 
i 3 115.2 0.6 61 93 0 0 0 0 4095 1 
i 5 115.2 0.3 69 91 0 0 0 0 4095 1 
i 1 115.5 0.6 73 89 0 0 0 0 4095 1 
i 1 115.5 0.3 74 89 0 0 0 0 4095 1 
i 1 115.5 0.3 76 89 0 0 0 0 4095 1 
i 6 115.5 0.3 85 98 0 0 0 0 4095 1 
i 1 115.8 0.3 69 89 0 0 0 0 4095 1 
i 2 115.8 0.6 50 89 0 0 0 0 4095 1 
i 3 115.8 0.3 64 98 0 0 0 0 4095 1 
i 6 115.8 0.3 61 93 0 0 0 0 4095 1 
i 6 115.8 0.3 66 90 0 0 0 0 4095 1 
i 1 116.1 0.3 52 92 0 0 0 0 4095 1 
i 1 116.1 0.3 57 91 0 0 0 0 4095 1 
i 1 116.1 0.6 64 92 0 0 0 0 4095 1 
i 2 116.1 0.3 47 89 0 0 0 0 4095 1 
i 5 116.1 0.9 64 91 0 0 0 0 4095 1 
i 2 116.4 0.3 76 90 0 0 0 0 4095 1 
i 4 116.4 0.3 73 90 0 0 0 0 4095 1 
i 6 116.4 0.3 61 95 0 0 0 0 4095 1 
i 6 116.4 0.3 62 92 0 0 0 0 4095 1 
i 6 116.4 0.3 81 90 0 0 0 0 4095 1 
i 1 116.7 0.3 59 93 0 0 0 0 4095 1 
i 2 116.7 0.3 52 88 0 0 0 0 4095 1 
i 3 116.7 0.3 61 94 0 0 0 0 4095 1 
i 3 116.7 0.6 85 90 0 0 0 0 4095 1 
i 5 116.7 0.3 68 99 0 0 0 0 4095 1 
i 1 117 0.3 54 88 0 0 0 0 4095 1 
i 1 117 0.3 76 87 0 0 0 0 4095 1 
i 4 117 0.6 73 95 0 0 0 0 4095 1 
i 4 117 0.3 76 90 0 0 0 0 4095 1 
i 6 117 0.3 79 95 0 0 0 0 4095 1 
i 1 117.3 0.3 71 88 0 0 0 0 4095 1 
i 1 117.3 0.3 85 86 0 0 0 0 4095 1 
i 3 117.3 0.3 88 88 0 0 0 0 4095 1 
i 4 117.3 0.3 69 85 0 0 0 0 4095 1 
i 1 117.6 0.3 88 93 0 0 0 0 4095 1 
i 1 117.6 0.3 91 92 0 0 0 0 4095 1 
i 2 117.6 0.3 47 99 0 0 0 0 4095 1 
i 3 117.6 0.3 90 92 0 0 0 0 4095 1 
i 3 117.6 0.3 93 93 0 0 0 0 4095 1 
i 6 117.6 0.3 61 91 0 0 0 0 4095 1 
i 1 117.9 0.3 76 94 0 0 0 0 4095 1 
i 1 117.9 0.9 81 87 0 0 0 0 4095 1 
i 2 117.9 0.3 81 99 0 0 0 0 4095 1 
i 4 117.9 0.3 71 97 0 0 0 0 4095 1 
i 6 117.9 0.3 55 95 0 0 0 0 4095 1 
i 6 117.9 0.3 59 99 0 0 0 0 4095 1 
i 3 118.2 0.3 92 95 0 0 0 0 4095 1 
i 3 118.2 0.3 97 88 0 0 0 0 4095 1 
i 4 118.2 0.3 64 93 0 0 0 0 4095 1 
i 4 118.2 0.3 68 97 0 0 0 0 4095 1 
i 6 118.2 0.3 73 93 0 0 0 0 4095 1 
i 1 118.5 0.3 80 87 0 0 0 0 4095 1 
i 1 118.5 0.3 85 88 0 0 0 0 4095 1 
i 3 118.5 0.3 61 91 0 0 0 0 4095 1 
i 4 118.5 0.3 71 89 0 0 0 0 4095 1 
i 5 118.5 0.6 76 90 0 0 0 0 4095 1 
i 3 118.8 0.3 85 87 0 0 0 0 4095 1 
i 5 118.8 0.3 81 95 0 0 0 0 4095 1 
i 6 118.8 0.6 59 98 0 0 0 0 4095 1 
i 6 118.8 0.3 64 97 0 0 0 0 4095 1 
i 1 119.1 0.3 83 88 0 0 0 0 4095 1 
i 1 119.1 0.3 85 88 0 0 0 0 4095 1 
i 6 119.1 0.3 54 93 0 0 0 0 4095 1 
i 6 119.1 0.3 56 88 0 0 0 0 4095 1 
i 6 119.1 0.3 61 89 0 0 0 0 4095 1 
i 1 119.4 0.3 88 92 0 0 0 0 4095 1 
i 2 119.4 0.3 85 90 0 0 0 0 4095 1 
i 2 119.4 0.3 86 99 0 0 0 0 4095 1 
i 3 119.4 0.3 90 93 0 0 0 0 4095 1 
i 4 119.4 0.3 69 92 0 0 0 0 4095 1 
i 5 119.4 0.3 81 90 0 0 0 0 4095 1 
i 1 119.7 0.3 45 97 0 0 0 0 4095 1 
i 1 119.7 0.3 50 91 0 0 0 0 4095 1 
i 1 119.7 0.3 52 88 0 0 0 0 4095 1 
i 1 119.7 0.3 55 89 0 0 0 0 4095 1 
i 2 119.7 0.3 84 90 0 0 0 0 4095 1 

</CsScore>
        </CsoundSynthesizer>
    