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
gS_cloud5_soundfile_name init "Julia-Suite-10.wav"


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
          
gk_MasterOutput_level init 0 ; Updated from: gk_MasterOutput_level init 0
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
          
i 1 0 0.72 95 91 0 0 0 0 4095 1 
i 2 0 0.36 81 87 0 0 0 0 4095 1 
i 3 0 0.36 80 85 0 0 0 0 4095 1 
i 4 0 0.36 49 86 0 0 0 0 4095 1 
i 4 0 0.36 54 93 0 0 0 0 4095 1 
i 4 0 0.36 59 83 0 0 0 0 4095 1 
i 2 0.36 0.36 61 99 0 0 0 0 4095 1 
i 2 0.36 1.08 66 83 0 0 0 0 4095 1 
i 3 0.36 0.36 76 85 0 0 0 0 4095 1 
i 3 0.36 0.36 78 84 0 0 0 0 4095 1 
i 4 0.36 0.72 64 95 0 0 0 0 4095 1 
i 1 0.72 0.36 90 89 0 0 0 0 4095 1 
i 2 0.72 0.36 68 86 0 0 0 0 4095 1 
i 3 0.72 0.72 83 91 0 0 0 0 4095 1 
i 1 1.08 0.36 88 87 0 0 0 0 4095 1 
i 2 1.08 0.36 69 87 0 0 0 0 4095 1 
i 3 1.08 0.36 86 87 0 0 0 0 4095 1 
i 4 1.08 0.36 66 86 0 0 0 0 4095 1 
i 1 1.44 0.36 66 83 0 0 0 0 4095 1 
i 2 1.44 1.08 54 91 0 0 0 0 4095 1 
i 2 1.44 0.36 57 86 0 0 0 0 4095 1 
i 2 1.44 0.36 61 85 0 0 0 0 4095 1 
i 3 1.44 0.36 81 84 0 0 0 0 4095 1 
i 4 1.44 0.36 69 84 0 0 0 0 4095 1 
i 1 1.8 0.72 59 89 0 0 0 0 4095 1 
i 1 1.8 0.36 61 93 0 0 0 0 4095 1 
i 2 1.8 0.36 55 84 0 0 0 0 4095 1 
i 2 1.8 0.36 69 84 0 0 0 0 4095 1 
i 3 1.8 0.36 64 81 0 0 0 0 4095 1 
i 1 2.16 0.36 64 84 0 0 0 0 4095 1 
i 2 2.16 0.72 59 94 0 0 0 0 4095 1 
i 2 2.16 0.72 64 83 0 0 0 0 4095 1 
i 4 2.16 0.36 71 84 0 0 0 0 4095 1 
i 3 2.52 0.36 69 88 0 0 0 0 4095 1 
i 3 2.52 0.72 71 84 0 0 0 0 4095 1 
i 4 2.52 0.72 73 89 0 0 0 0 4095 1 
i 1 2.88 0.36 66 94 0 0 0 0 4095 1 
i 2 2.88 0.36 62 88 0 0 0 0 4095 1 
i 4 2.88 0.36 69 87 0 0 0 0 4095 1 
i 4 2.88 0.36 76 93 0 0 0 0 4095 1 
i 4 2.88 0.72 81 85 0 0 0 0 4095 1 
i 1 3.24 0.36 64 81 0 0 0 0 4095 1 
i 1 3.24 0.72 69 98 0 0 0 0 4095 1 
i 2 3.24 0.36 66 81 0 0 0 0 4095 1 
i 3 3.24 0.36 67 83 0 0 0 0 4095 1 
i 1 3.6 0.72 71 89 0 0 0 0 4095 1 
i 2 3.6 0.36 64 99 0 0 0 0 4095 1 
i 3 3.6 0.36 69 100 0 0 0 0 4095 1 
i 4 3.6 1.08 66 88 0 0 0 0 4095 1 
i 1 3.96 0.36 66 84 0 0 0 0 4095 1 
i 3 3.96 0.36 73 83 0 0 0 0 4095 1 
i 4 3.96 1.08 61 81 0 0 0 0 4095 1 
i 4 3.96 0.36 68 81 0 0 0 0 4095 1 
i 1 4.32 0.36 56 82 0 0 0 0 4095 1 
i 1 4.32 0.72 59 83 0 0 0 0 4095 1 
i 1 4.32 0.36 64 98 0 0 0 0 4095 1 
i 1 4.68 0.36 57 95 0 0 0 0 4095 1 
i 2 4.68 1.08 61 83 0 0 0 0 4095 1 
i 3 4.68 0.36 64 85 0 0 0 0 4095 1 
i 4 4.68 0.36 62 88 0 0 0 0 4095 1 
i 1 5.04 0.72 66 83 0 0 0 0 4095 1 
i 1 5.04 0.72 69 83 0 0 0 0 4095 1 
i 2 5.04 0.72 64 89 0 0 0 0 4095 1 
i 3 5.04 0.36 66 84 0 0 0 0 4095 1 
i 2 5.4 0.36 54 95 0 0 0 0 4095 1 
i 2 5.4 0.36 57 90 0 0 0 0 4095 1 
i 1 5.76 0.36 71 89 0 0 0 0 4095 1 
i 1 5.76 0.36 73 93 0 0 0 0 4095 1 
i 3 5.76 0.36 64 89 0 0 0 0 4095 1 
i 3 5.76 0.36 66 88 0 0 0 0 4095 1 
i 3 5.76 0.36 69 89 0 0 0 0 4095 1 
i 4 5.76 0.72 69 98 0 0 0 0 4095 1 
i 1 6.12 0.36 69 86 0 0 0 0 4095 1 
i 2 6.12 0.72 49 89 0 0 0 0 4095 1 
i 3 6.12 0.36 71 86 0 0 0 0 4095 1 
i 4 6.12 0.36 64 88 0 0 0 0 4095 1 
i 4 6.12 0.36 73 90 0 0 0 0 4095 1 
i 1 6.48 0.36 64 92 0 0 0 0 4095 1 
i 3 6.48 0.36 62 97 0 0 0 0 4095 1 
i 4 6.48 0.36 66 99 0 0 0 0 4095 1 
i 4 6.48 0.36 68 84 0 0 0 0 4095 1 
i 1 6.84 0.72 54 91 0 0 0 0 4095 1 
i 1 6.84 0.36 59 93 0 0 0 0 4095 1 
i 3 6.84 0.36 61 98 0 0 0 0 4095 1 
i 3 6.84 0.36 66 92 0 0 0 0 4095 1 
i 3 6.84 0.36 68 85 0 0 0 0 4095 1 
i 4 6.84 0.36 69 87 0 0 0 0 4095 1 
i 1 7.2 0.36 56 84 0 0 0 0 4095 1 
i 1 7.2 0.36 57 84 0 0 0 0 4095 1 
i 2 7.2 0.36 47 84 0 0 0 0 4095 1 
i 2 7.2 0.36 66 90 0 0 0 0 4095 1 
i 4 7.2 0.36 73 94 0 0 0 0 4095 1 
i 1 7.56 0.36 49 89 0 0 0 0 4095 1 
i 1 7.56 0.36 52 85 0 0 0 0 4095 1 
i 2 7.56 0.36 68 89 0 0 0 0 4095 1 
i 3 7.56 0.36 62 89 0 0 0 0 4095 1 
i 3 7.56 0.36 64 85 0 0 0 0 4095 1 
i 4 7.56 0.36 68 85 0 0 0 0 4095 1 
i 1 7.92 0.36 47 81 0 0 0 0 4095 1 
i 2 7.92 0.36 71 81 0 0 0 0 4095 1 
i 3 7.92 0.36 66 83 0 0 0 0 4095 1 
i 3 7.92 0.36 68 81 0 0 0 0 4095 1 
i 4 7.92 0.36 64 99 0 0 0 0 4095 1 
i 4 7.92 0.72 69 93 0 0 0 0 4095 1 
i 1 8.28 0.36 52 83 0 0 0 0 4095 1 
i 1 8.28 0.72 69 84 0 0 0 0 4095 1 
i 3 8.28 0.36 71 94 0 0 0 0 4095 1 
i 4 8.28 0.36 66 85 0 0 0 0 4095 1 
i 1 8.64 0.72 68 87 0 0 0 0 4095 1 
i 2 8.64 0.72 73 84 0 0 0 0 4095 1 
i 2 8.64 0.36 76 86 0 0 0 0 4095 1 
i 3 8.64 0.36 73 83 0 0 0 0 4095 1 
i 3 8.64 0.36 78 91 0 0 0 0 4095 1 
i 1 9 0.36 71 83 0 0 0 0 4095 1 
i 3 9 0.36 80 86 0 0 0 0 4095 1 
i 4 9 1.44 71 90 0 0 0 0 4095 1 
i 4 9 0.36 73 93 0 0 0 0 4095 1 
i 1 9.36 0.72 75 87 0 0 0 0 4095 1 
i 2 9.36 0.72 78 82 0 0 0 0 4095 1 
i 3 9.36 0.36 68 90 0 0 0 0 4095 1 
i 3 9.36 0.36 71 83 0 0 0 0 4095 1 
i 1 9.72 0.36 71 87 0 0 0 0 4095 1 
i 2 9.72 0.36 75 96 0 0 0 0 4095 1 
i 4 9.72 0.36 73 93 0 0 0 0 4095 1 
i 2 10.08 0.36 71 83 0 0 0 0 4095 1 
i 3 10.08 0.36 68 88 0 0 0 0 4095 1 
i 1 10.44 1.44 69 96 0 0 0 0 4095 1 
i 2 10.44 1.08 69 92 0 0 0 0 4095 1 
i 3 10.44 1.08 69 88 0 0 0 0 4095 1 
i 4 10.44 0.72 69 89 0 0 0 0 4095 1 
i 1 11.52 0.36 64 86 0 0 0 0 4095 1 
i 1 11.52 0.36 66 89 0 0 0 0 4095 1 
i 2 11.52 0.36 66 84 0 0 0 0 4095 1 
i 4 11.52 0.72 73 86 0 0 0 0 4095 1 
i 4 11.52 0.36 76 88 0 0 0 0 4095 1 
i 1 11.88 0.36 71 84 0 0 0 0 4095 1 
i 1 11.88 0.36 76 83 0 0 0 0 4095 1 
i 2 11.88 0.36 71 85 0 0 0 0 4095 1 
i 4 11.88 0.36 68 100 0 0 0 0 4095 1 
i 2 12.24 0.36 57 92 0 0 0 0 4095 1 
i 2 12.24 0.36 62 94 0 0 0 0 4095 1 
i 3 12.24 0.72 57 87 0 0 0 0 4095 1 
i 3 12.24 1.08 61 89 0 0 0 0 4095 1 
i 4 12.24 0.36 69 91 0 0 0 0 4095 1 
i 1 12.6 0.36 68 85 0 0 0 0 4095 1 
i 1 12.6 0.36 73 89 0 0 0 0 4095 1 
i 2 12.6 0.36 61 85 0 0 0 0 4095 1 
i 4 12.6 0.36 64 94 0 0 0 0 4095 1 
i 2 12.96 0.36 64 91 0 0 0 0 4095 1 
i 2 12.96 0.36 66 96 0 0 0 0 4095 1 
i 2 12.96 1.08 68 92 0 0 0 0 4095 1 
i 4 12.96 0.36 68 91 0 0 0 0 4095 1 
i 1 13.32 0.36 68 87 0 0 0 0 4095 1 
i 1 13.32 0.36 71 87 0 0 0 0 4095 1 
i 4 13.32 0.72 61 88 0 0 0 0 4095 1 
i 4 13.32 0.36 62 95 0 0 0 0 4095 1 
i 4 13.32 0.36 64 88 0 0 0 0 4095 1 
i 1 13.68 0.36 66 93 0 0 0 0 4095 1 
i 2 13.68 0.36 73 85 0 0 0 0 4095 1 
i 3 13.68 0.36 57 84 0 0 0 0 4095 1 
i 4 13.68 0.72 56 94 0 0 0 0 4095 1 
i 1 14.04 0.72 62 83 0 0 0 0 4095 1 
i 1 14.04 0.36 64 82 0 0 0 0 4095 1 
i 2 14.04 0.36 64 92 0 0 0 0 4095 1 
i 1 14.4 0.72 59 89 0 0 0 0 4095 1 
i 2 14.4 0.36 61 85 0 0 0 0 4095 1 
i 3 14.4 0.36 59 92 0 0 0 0 4095 1 
i 4 14.4 0.36 59 84 0 0 0 0 4095 1 
i 1 14.76 0.72 61 85 0 0 0 0 4095 1 
i 2 14.76 0.36 68 91 0 0 0 0 4095 1 
i 3 14.76 0.36 56 90 0 0 0 0 4095 1 
i 4 14.76 0.36 64 86 0 0 0 0 4095 1 
i 4 14.76 0.36 69 83 0 0 0 0 4095 1 
i 2 15.12 0.36 71 84 0 0 0 0 4095 1 
i 3 15.12 0.36 57 86 0 0 0 0 4095 1 
i 3 15.12 0.36 59 85 0 0 0 0 4095 1 
i 4 15.12 0.72 73 87 0 0 0 0 4095 1 
i 1 15.48 0.36 66 89 0 0 0 0 4095 1 
i 2 15.48 0.36 64 85 0 0 0 0 4095 1 
i 4 15.48 0.36 78 82 0 0 0 0 4095 1 
i 1 15.84 0.36 68 85 0 0 0 0 4095 1 
i 3 15.84 0.36 59 89 0 0 0 0 4095 1 
i 3 15.84 0.36 64 92 0 0 0 0 4095 1 
i 4 15.84 0.36 66 95 0 0 0 0 4095 1 
i 4 15.84 0.36 69 84 0 0 0 0 4095 1 
i 1 16.2 0.36 64 86 0 0 0 0 4095 1 
i 1 16.2 0.36 66 88 0 0 0 0 4095 1 
i 2 16.2 0.36 62 93 0 0 0 0 4095 1 
i 2 16.2 0.36 66 85 0 0 0 0 4095 1 
i 4 16.2 0.36 68 88 0 0 0 0 4095 1 
i 2 16.56 0.36 69 87 0 0 0 0 4095 1 
i 3 16.56 0.36 64 94 0 0 0 0 4095 1 
i 3 16.56 0.36 68 92 0 0 0 0 4095 1 
i 3 16.56 0.36 71 97 0 0 0 0 4095 1 
i 4 16.56 0.72 61 87 0 0 0 0 4095 1 
i 1 16.92 0.72 61 91 0 0 0 0 4095 1 
i 2 16.92 0.36 66 94 0 0 0 0 4095 1 
i 3 16.92 0.36 76 87 0 0 0 0 4095 1 
i 4 16.92 0.36 57 90 0 0 0 0 4095 1 
i 4 16.92 0.36 62 91 0 0 0 0 4095 1 
i 1 17.28 0.36 57 90 0 0 0 0 4095 1 
i 1 17.28 0.36 62 89 0 0 0 0 4095 1 
i 2 17.28 0.36 62 98 0 0 0 0 4095 1 
i 2 17.28 0.72 64 99 0 0 0 0 4095 1 
i 3 17.28 0.36 74 91 0 0 0 0 4095 1 
i 1 17.64 0.36 54 94 0 0 0 0 4095 1 
i 2 17.64 0.36 69 86 0 0 0 0 4095 1 
i 3 17.64 0.36 61 84 0 0 0 0 4095 1 
i 3 17.64 0.36 66 83 0 0 0 0 4095 1 
i 3 17.64 0.36 67 88 0 0 0 0 4095 1 
i 1 18 0.72 52 100 0 0 0 0 4095 1 
i 2 18 0.36 54 84 0 0 0 0 4095 1 
i 2 18 0.36 55 85 0 0 0 0 4095 1 
i 2 18 0.36 61 93 0 0 0 0 4095 1 
i 3 18 0.72 64 90 0 0 0 0 4095 1 
i 4 18 0.36 61 84 0 0 0 0 4095 1 
i 1 18.36 0.36 49 94 0 0 0 0 4095 1 
i 3 18.36 0.36 59 91 0 0 0 0 4095 1 
i 3 18.36 0.36 69 90 0 0 0 0 4095 1 
i 1 18.72 0.36 54 91 0 0 0 0 4095 1 
i 1 18.72 0.72 57 97 0 0 0 0 4095 1 
i 2 18.72 0.36 45 92 0 0 0 0 4095 1 
i 2 18.72 0.36 49 98 0 0 0 0 4095 1 
i 2 18.72 1.08 52 90 0 0 0 0 4095 1 
i 3 19.08 0.36 71 88 0 0 0 0 4095 1 
i 4 19.08 0.36 59 89 0 0 0 0 4095 1 
i 4 19.08 0.36 62 89 0 0 0 0 4095 1 
i 4 19.08 0.36 64 89 0 0 0 0 4095 1 
i 1 19.44 0.36 61 85 0 0 0 0 4095 1 
i 2 19.44 0.72 57 88 0 0 0 0 4095 1 
i 2 19.44 0.72 61 92 0 0 0 0 4095 1 
i 3 19.44 0.36 61 92 0 0 0 0 4095 1 
i 3 19.44 0.36 62 88 0 0 0 0 4095 1 
i 3 19.8 0.36 59 95 0 0 0 0 4095 1 
i 4 19.8 0.36 54 87 0 0 0 0 4095 1 
i 4 19.8 0.36 59 93 0 0 0 0 4095 1 
i 1 20.16 0.36 54 95 0 0 0 0 4095 1 
i 1 20.16 0.36 59 92 0 0 0 0 4095 1 
i 1 20.16 0.36 62 89 0 0 0 0 4095 1 
i 2 20.16 0.36 59 90 0 0 0 0 4095 1 
i 3 20.16 0.36 54 92 0 0 0 0 4095 1 
i 3 20.16 0.36 69 90 0 0 0 0 4095 1 
i 1 20.52 0.36 52 88 0 0 0 0 4095 1 
i 1 20.52 0.72 68 95 0 0 0 0 4095 1 
i 4 20.52 0.36 54 90 0 0 0 0 4095 1 
i 4 20.52 0.36 59 91 0 0 0 0 4095 1 
i 4 20.52 1.08 71 90 0 0 0 0 4095 1 
i 2 20.88 0.36 59 91 0 0 0 0 4095 1 
i 2 20.88 0.36 64 88 0 0 0 0 4095 1 
i 2 20.88 0.36 66 90 0 0 0 0 4095 1 
i 4 20.88 0.36 76 87 0 0 0 0 4095 1 
i 1 21.24 0.36 73 86 0 0 0 0 4095 1 
i 2 21.24 0.36 71 91 0 0 0 0 4095 1 
i 4 21.24 0.36 69 87 0 0 0 0 4095 1 
i 4 21.24 0.36 75 88 0 0 0 0 4095 1 
i 1 21.6 0.72 69 89 0 0 0 0 4095 1 
i 2 21.6 0.72 69 85 0 0 0 0 4095 1 
i 3 21.6 0.72 69 85 0 0 0 0 4095 1 
i 4 21.6 0.36 76 95 0 0 0 0 4095 1 
i 4 21.96 0.36 69 94 0 0 0 0 4095 1 
i 1 22.32 0.36 68 90 0 0 0 0 4095 1 
i 3 22.32 0.36 68 92 0 0 0 0 4095 1 
i 4 22.32 0.36 68 85 0 0 0 0 4095 1 
i 2 22.68 0.36 69 89 0 0 0 0 4095 1 
i 3 22.68 0.36 69 82 0 0 0 0 4095 1 
i 4 22.68 0.36 69 88 0 0 0 0 4095 1 
i 1 23.04 0.36 64 88 0 0 0 0 4095 1 
i 1 23.04 1.08 68 88 0 0 0 0 4095 1 
i 2 23.04 0.72 66 84 0 0 0 0 4095 1 
i 3 23.04 0.36 66 88 0 0 0 0 4095 1 
i 4 23.04 0.36 64 99 0 0 0 0 4095 1 
i 4 23.04 0.36 66 90 0 0 0 0 4095 1 
i 3 23.4 0.72 71 89 0 0 0 0 4095 1 
i 4 23.4 0.72 68 91 0 0 0 0 4095 1 
i 1 23.76 0.36 64 87 0 0 0 0 4095 1 
i 2 23.76 0.36 64 84 0 0 0 0 4095 1 
i 4 23.76 0.36 62 87 0 0 0 0 4095 1 
i 1 24.12 0.36 73 83 0 0 0 0 4095 1 
i 1 24.12 0.36 74 83 0 0 0 0 4095 1 
i 3 24.12 0.36 76 89 0 0 0 0 4095 1 
i 3 24.12 0.36 80 85 0 0 0 0 4095 1 
i 3 24.12 0.36 83 92 0 0 0 0 4095 1 
i 4 24.12 0.36 66 95 0 0 0 0 4095 1 
i 1 24.48 0.36 78 93 0 0 0 0 4095 1 
i 2 24.48 0.72 59 87 0 0 0 0 4095 1 
i 2 24.48 0.36 66 87 0 0 0 0 4095 1 
i 3 24.48 0.36 69 93 0 0 0 0 4095 1 
i 3 24.48 0.36 71 96 0 0 0 0 4095 1 
i 3 24.48 0.36 73 94 0 0 0 0 4095 1 
i 1 24.84 0.36 80 94 0 0 0 0 4095 1 
i 2 24.84 0.36 61 87 0 0 0 0 4095 1 
i 2 24.84 0.36 64 93 0 0 0 0 4095 1 
i 3 24.84 0.36 68 87 0 0 0 0 4095 1 
i 4 24.84 0.36 66 88 0 0 0 0 4095 1 
i 1 25.2 0.36 85 88 0 0 0 0 4095 1 
i 2 25.2 0.36 68 87 0 0 0 0 4095 1 
i 2 25.2 0.36 71 92 0 0 0 0 4095 1 
i 3 25.2 0.36 71 88 0 0 0 0 4095 1 
i 3 25.2 0.36 73 90 0 0 0 0 4095 1 
i 4 25.2 0.36 62 88 0 0 0 0 4095 1 
i 1 25.56 0.36 79 90 0 0 0 0 4095 1 
i 2 25.56 0.36 52 92 0 0 0 0 4095 1 
i 2 25.56 0.36 57 81 0 0 0 0 4095 1 
i 2 25.56 0.36 61 83 0 0 0 0 4095 1 
i 2 25.56 1.08 64 85 0 0 0 0 4095 1 
i 2 25.56 0.36 66 88 0 0 0 0 4095 1 
i 1 25.92 0.36 78 92 0 0 0 0 4095 1 
i 2 25.92 0.72 62 90 0 0 0 0 4095 1 
i 2 25.92 0.36 69 88 0 0 0 0 4095 1 
i 3 25.92 0.36 76 89 0 0 0 0 4095 1 
i 4 25.92 0.36 66 91 0 0 0 0 4095 1 
i 1 26.28 0.36 73 85 0 0 0 0 4095 1 
i 2 26.28 0.72 59 85 0 0 0 0 4095 1 
i 2 26.28 0.36 71 88 0 0 0 0 4095 1 
i 4 26.28 0.36 64 85 0 0 0 0 4095 1 
i 1 26.64 0.36 78 89 0 0 0 0 4095 1 
i 2 26.64 1.08 73 83 0 0 0 0 4095 1 
i 3 26.64 0.36 69 85 0 0 0 0 4095 1 
i 3 26.64 0.36 71 84 0 0 0 0 4095 1 
i 4 26.64 0.36 71 83 0 0 0 0 4095 1 
i 1 27 0.36 71 88 0 0 0 0 4095 1 
i 1 27 0.36 73 87 0 0 0 0 4095 1 
i 1 27 1.08 76 88 0 0 0 0 4095 1 
i 3 27 0.36 62 87 0 0 0 0 4095 1 
i 4 27 0.36 66 99 0 0 0 0 4095 1 
i 1 27.36 0.36 74 89 0 0 0 0 4095 1 
i 2 27.36 0.36 71 89 0 0 0 0 4095 1 
i 4 27.36 0.36 62 88 0 0 0 0 4095 1 
i 4 27.36 0.36 67 86 0 0 0 0 4095 1 
i 1 27.72 0.72 73 85 0 0 0 0 4095 1 
i 2 27.72 0.36 57 89 0 0 0 0 4095 1 
i 2 27.72 0.36 61 85 0 0 0 0 4095 1 
i 2 27.72 0.72 66 95 0 0 0 0 4095 1 
i 4 27.72 0.36 71 89 0 0 0 0 4095 1 
i 2 28.08 0.72 62 89 0 0 0 0 4095 1 
i 2 28.08 0.36 64 94 0 0 0 0 4095 1 
i 4 28.08 0.72 66 91 0 0 0 0 4095 1 
i 1 28.44 0.72 71 89 0 0 0 0 4095 1 
i 1 28.44 0.36 76 89 0 0 0 0 4095 1 
i 3 28.44 0.36 61 91 0 0 0 0 4095 1 
i 4 28.44 0.72 68 99 0 0 0 0 4095 1 
i 1 28.8 0.36 69 88 0 0 0 0 4095 1 
i 2 28.8 0.36 56 99 0 0 0 0 4095 1 
i 3 28.8 0.36 68 87 0 0 0 0 4095 1 
i 1 29.16 0.36 64 87 0 0 0 0 4095 1 
i 2 29.16 0.36 57 91 0 0 0 0 4095 1 
i 2 29.16 0.72 61 88 0 0 0 0 4095 1 
i 2 29.16 0.36 62 94 0 0 0 0 4095 1 
i 4 29.16 0.36 64 98 0 0 0 0 4095 1 
i 1 29.52 0.36 69 93 0 0 0 0 4095 1 
i 2 29.52 0.72 64 89 0 0 0 0 4095 1 
i 3 29.52 0.36 62 85 0 0 0 0 4095 1 
i 4 29.52 0.36 62 86 0 0 0 0 4095 1 
i 1 29.88 0.72 66 85 0 0 0 0 4095 1 
i 2 29.88 0.36 59 98 0 0 0 0 4095 1 
i 3 29.88 0.36 57 87 0 0 0 0 4095 1 
i 3 29.88 0.72 59 83 0 0 0 0 4095 1 
i 3 29.88 0.36 66 92 0 0 0 0 4095 1 
i 1 30.24 0.72 71 92 0 0 0 0 4095 1 
i 2 30.24 0.36 69 91 0 0 0 0 4095 1 
i 3 30.24 0.36 61 89 0 0 0 0 4095 1 
i 4 30.24 0.36 66 94 0 0 0 0 4095 1 
i 1 30.6 0.36 64 95 0 0 0 0 4095 1 
i 1 30.6 0.36 67 95 0 0 0 0 4095 1 
i 2 30.6 0.36 66 92 0 0 0 0 4095 1 
i 3 30.6 0.36 67 87 0 0 0 0 4095 1 
i 4 30.6 0.36 61 86 0 0 0 0 4095 1 
i 1 30.96 0.36 61 95 0 0 0 0 4095 1 
i 1 30.96 0.36 66 82 0 0 0 0 4095 1 
i 2 30.96 0.36 62 87 0 0 0 0 4095 1 
i 3 30.96 0.72 61 90 0 0 0 0 4095 1 
i 3 30.96 0.36 62 91 0 0 0 0 4095 1 
i 1 31.32 0.36 62 84 0 0 0 0 4095 1 
i 1 31.32 0.36 71 94 0 0 0 0 4095 1 
i 2 31.32 0.72 61 87 0 0 0 0 4095 1 
i 4 31.32 0.36 59 86 0 0 0 0 4095 1 
i 4 31.32 0.36 61 88 0 0 0 0 4095 1 
i 1 31.68 0.72 73 87 0 0 0 0 4095 1 
i 2 31.68 0.36 66 88 0 0 0 0 4095 1 
i 2 31.68 0.36 71 91 0 0 0 0 4095 1 
i 3 31.68 0.36 64 86 0 0 0 0 4095 1 
i 4 31.68 0.36 57 86 0 0 0 0 4095 1 
i 1 32.04 0.36 69 88 0 0 0 0 4095 1 
i 2 32.04 0.36 68 88 0 0 0 0 4095 1 
i 2 32.04 0.72 73 90 0 0 0 0 4095 1 
i 3 32.04 0.72 68 92 0 0 0 0 4095 1 
i 3 32.04 0.36 71 88 0 0 0 0 4095 1 
i 1 32.4 0.36 66 87 0 0 0 0 4095 1 
i 3 32.4 0.36 62 90 0 0 0 0 4095 1 
i 3 32.4 0.36 64 89 0 0 0 0 4095 1 
i 4 32.4 0.36 57 86 0 0 0 0 4095 1 
i 1 32.76 0.36 68 99 0 0 0 0 4095 1 
i 3 32.76 1.8 61 90 0 0 0 0 4095 1 
i 4 32.76 0.36 52 94 0 0 0 0 4095 1 
i 4 32.76 0.36 54 94 0 0 0 0 4095 1 
i 4 32.76 0.36 56 94 0 0 0 0 4095 1 
i 4 32.76 0.36 59 90 0 0 0 0 4095 1 
i 1 33.12 0.72 69 85 0 0 0 0 4095 1 
i 2 33.12 2.16 73 84 0 0 0 0 4095 1 
i 4 33.12 1.44 61 85 0 0 0 0 4095 1 
i 1 34.2 0.36 69 88 0 0 0 0 4095 1 
i 2 34.56 0.36 68 89 0 0 0 0 4095 1 
i 2 34.56 0.72 69 86 0 0 0 0 4095 1 
i 2 34.56 0.36 71 91 0 0 0 0 4095 1 
i 2 34.56 0.36 74 96 0 0 0 0 4095 1 
i 2 34.56 0.36 76 89 0 0 0 0 4095 1 
i 3 34.92 0.36 56 92 0 0 0 0 4095 1 
i 3 34.92 0.36 59 84 0 0 0 0 4095 1 
i 3 34.92 0.36 66 85 0 0 0 0 4095 1 
i 4 34.92 0.72 64 83 0 0 0 0 4095 1 
i 2 35.28 0.72 71 94 0 0 0 0 4095 1 
i 2 35.28 0.36 74 95 0 0 0 0 4095 1 
i 3 35.28 0.72 61 87 0 0 0 0 4095 1 
i 4 35.28 0.36 61 86 0 0 0 0 4095 1 
i 4 35.28 0.36 68 90 0 0 0 0 4095 1 
i 2 35.64 0.72 66 90 0 0 0 0 4095 1 
i 3 35.64 0.36 57 91 0 0 0 0 4095 1 
i 1 36 0.36 66 92 0 0 0 0 4095 1 
i 4 36 0.36 59 86 0 0 0 0 4095 1 
i 4 36 0.36 61 89 0 0 0 0 4095 1 
i 4 36 0.36 71 89 0 0 0 0 4095 1 
i 2 36.36 0.36 56 85 0 0 0 0 4095 1 
i 2 36.36 0.36 62 97 0 0 0 0 4095 1 
i 2 36.36 0.36 68 86 0 0 0 0 4095 1 
i 3 36.36 0.36 61 88 0 0 0 0 4095 1 
i 3 36.36 0.36 64 95 0 0 0 0 4095 1 
i 1 36.72 0.36 62 97 0 0 0 0 4095 1 
i 2 36.72 0.36 71 93 0 0 0 0 4095 1 
i 3 36.72 0.36 66 86 0 0 0 0 4095 1 
i 4 36.72 0.36 59 85 0 0 0 0 4095 1 
i 4 36.72 0.36 61 88 0 0 0 0 4095 1 
i 4 36.72 0.36 66 90 0 0 0 0 4095 1 
i 1 37.08 0.36 64 88 0 0 0 0 4095 1 
i 2 37.08 0.36 69 90 0 0 0 0 4095 1 
i 2 37.08 0.36 73 95 0 0 0 0 4095 1 
i 3 37.08 0.36 57 96 0 0 0 0 4095 1 
i 3 37.08 0.36 61 86 0 0 0 0 4095 1 
i 3 37.08 0.36 62 89 0 0 0 0 4095 1 
i 1 37.44 0.36 56 93 0 0 0 0 4095 1 
i 1 37.44 0.72 62 93 0 0 0 0 4095 1 
i 2 37.44 0.72 64 90 0 0 0 0 4095 1 
i 3 37.44 0.36 59 86 0 0 0 0 4095 1 
i 4 37.44 0.36 62 90 0 0 0 0 4095 1 
i 4 37.44 0.36 64 89 0 0 0 0 4095 1 
i 1 37.8 0.36 52 89 0 0 0 0 4095 1 
i 1 37.8 0.36 61 87 0 0 0 0 4095 1 
i 2 37.8 0.36 66 88 0 0 0 0 4095 1 
i 4 37.8 0.36 61 91 0 0 0 0 4095 1 
i 2 38.16 0.36 59 87 0 0 0 0 4095 1 
i 2 38.16 0.36 61 91 0 0 0 0 4095 1 
i 3 38.16 0.36 56 99 0 0 0 0 4095 1 
i 3 38.16 0.36 62 86 0 0 0 0 4095 1 
i 4 38.16 0.72 66 86 0 0 0 0 4095 1 
i 4 38.16 0.36 71 90 0 0 0 0 4095 1 
i 1 38.52 0.72 66 91 0 0 0 0 4095 1 
i 2 38.52 1.08 66 90 0 0 0 0 4095 1 
i 3 38.52 0.36 57 98 0 0 0 0 4095 1 
i 4 38.52 0.36 68 90 0 0 0 0 4095 1 
i 4 38.52 0.36 73 94 0 0 0 0 4095 1 
i 2 38.88 0.72 61 87 0 0 0 0 4095 1 
i 2 38.88 0.36 64 85 0 0 0 0 4095 1 
i 2 38.88 0.36 71 89 0 0 0 0 4095 1 
i 3 38.88 0.36 62 92 0 0 0 0 4095 1 
i 2 39.24 0.36 56 90 0 0 0 0 4095 1 
i 3 39.24 0.36 59 84 0 0 0 0 4095 1 
i 4 39.24 0.36 73 91 0 0 0 0 4095 1 
i 1 39.6 0.36 67 90 0 0 0 0 4095 1 
i 1 39.6 0.36 69 87 0 0 0 0 4095 1 
i 2 39.6 0.36 49 92 0 0 0 0 4095 1 
i 2 39.6 0.36 54 88 0 0 0 0 4095 1 
i 3 39.6 0.36 57 92 0 0 0 0 4095 1 
i 1 39.96 0.36 61 90 0 0 0 0 4095 1 
i 2 39.96 0.36 52 98 0 0 0 0 4095 1 
i 2 39.96 0.36 57 93 0 0 0 0 4095 1 
i 2 39.96 0.36 61 93 0 0 0 0 4095 1 
i 2 39.96 1.44 64 98 0 0 0 0 4095 1 
i 2 40.32 0.36 66 89 0 0 0 0 4095 1 
i 4 40.32 0.36 61 89 0 0 0 0 4095 1 
i 4 40.32 0.36 64 86 0 0 0 0 4095 1 
i 4 40.32 0.36 68 97 0 0 0 0 4095 1 
i 1 40.68 0.72 61 91 0 0 0 0 4095 1 
i 1 40.68 0.36 62 96 0 0 0 0 4095 1 
i 2 40.68 0.36 59 87 0 0 0 0 4095 1 
i 2 40.68 0.36 61 87 0 0 0 0 4095 1 
i 4 40.68 0.36 69 88 0 0 0 0 4095 1 
i 1 41.04 0.72 59 90 0 0 0 0 4095 1 
i 2 41.04 0.36 62 93 0 0 0 0 4095 1 
i 3 41.04 0.36 54 95 0 0 0 0 4095 1 
i 3 41.04 0.36 55 92 0 0 0 0 4095 1 
i 1 41.4 0.36 62 91 0 0 0 0 4095 1 
i 2 41.4 0.72 59 94 0 0 0 0 4095 1 
i 2 41.4 0.36 61 94 0 0 0 0 4095 1 
i 3 41.4 0.36 69 95 0 0 0 0 4095 1 
i 4 41.4 0.36 74 89 0 0 0 0 4095 1 
i 2 41.76 0.36 56 97 0 0 0 0 4095 1 
i 3 41.76 0.36 64 88 0 0 0 0 4095 1 
i 3 41.76 0.36 66 90 0 0 0 0 4095 1 
i 1 42.12 0.36 61 91 0 0 0 0 4095 1 
i 2 42.12 0.36 54 93 0 0 0 0 4095 1 
i 2 42.12 0.36 71 91 0 0 0 0 4095 1 
i 3 42.12 0.36 71 88 0 0 0 0 4095 1 
i 3 42.12 0.36 73 92 0 0 0 0 4095 1 
i 4 42.12 0.36 68 87 0 0 0 0 4095 1 
i 2 42.48 0.36 68 92 0 0 0 0 4095 1 
i 2 42.48 0.36 73 90 0 0 0 0 4095 1 
i 2 42.48 0.36 74 99 0 0 0 0 4095 1 
i 2 42.48 0.36 76 87 0 0 0 0 4095 1 
i 3 42.48 0.36 78 87 0 0 0 0 4095 1 
i 4 42.48 0.36 73 96 0 0 0 0 4095 1 
i 1 42.84 0.72 64 87 0 0 0 0 4095 1 
i 3 42.84 0.36 71 94 0 0 0 0 4095 1 
i 3 42.84 0.36 76 88 0 0 0 0 4095 1 
i 4 42.84 0.36 69 88 0 0 0 0 4095 1 
i 4 42.84 0.36 71 98 0 0 0 0 4095 1 
i 1 43.2 0.36 66 92 0 0 0 0 4095 1 
i 2 43.2 0.72 69 85 0 0 0 0 4095 1 
i 4 43.2 0.36 73 86 0 0 0 0 4095 1 
i 4 43.2 0.72 78 89 0 0 0 0 4095 1 
i 1 43.56 0.36 59 85 0 0 0 0 4095 1 
i 2 43.56 1.08 66 90 0 0 0 0 4095 1 
i 2 43.56 0.72 71 86 0 0 0 0 4095 1 
i 3 43.56 0.36 73 93 0 0 0 0 4095 1 
i 4 43.56 0.36 76 82 0 0 0 0 4095 1 
i 1 43.92 0.36 62 83 0 0 0 0 4095 1 
i 1 43.92 0.36 66 86 0 0 0 0 4095 1 
i 3 43.92 0.36 68 89 0 0 0 0 4095 1 
i 4 43.92 0.72 62 86 0 0 0 0 4095 1 
i 1 44.28 1.08 61 95 0 0 0 0 4095 1 
i 2 44.28 0.36 68 89 0 0 0 0 4095 1 
i 2 44.64 1.08 69 86 0 0 0 0 4095 1 
i 3 44.64 0.72 69 93 0 0 0 0 4095 1 
i 4 44.64 1.08 61 86 0 0 0 0 4095 1 
i 1 45.72 0.72 61 85 0 0 0 0 4095 1 
i 2 45.72 0.36 70 83 0 0 0 0 4095 1 
i 2 46.08 0.36 62 85 0 0 0 0 4095 1 
i 2 46.08 0.36 69 88 0 0 0 0 4095 1 
i 3 46.08 0.36 68 85 0 0 0 0 4095 1 
i 3 46.08 0.36 69 85 0 0 0 0 4095 1 
i 3 46.08 0.36 73 89 0 0 0 0 4095 1 
i 3 46.44 0.36 78 87 0 0 0 0 4095 1 
i 3 46.44 0.36 83 88 0 0 0 0 4095 1 
i 3 46.44 0.36 86 85 0 0 0 0 4095 1 
i 4 46.44 0.36 52 90 0 0 0 0 4095 1 
i 4 46.44 0.36 56 85 0 0 0 0 4095 1 
i 4 46.44 0.36 59 85 0 0 0 0 4095 1 
i 1 46.8 0.36 59 84 0 0 0 0 4095 1 
i 1 46.8 0.36 61 93 0 0 0 0 4095 1 
i 3 46.8 1.08 64 91 0 0 0 0 4095 1 
i 3 46.8 0.36 66 85 0 0 0 0 4095 1 
i 3 46.8 0.36 68 84 0 0 0 0 4095 1 
i 4 46.8 0.36 54 91 0 0 0 0 4095 1 
i 1 47.16 0.36 64 80 0 0 0 0 4095 1 
i 4 47.16 0.36 64 82 0 0 0 0 4095 1 
i 2 47.52 0.36 69 84 0 0 0 0 4095 1 
i 2 47.52 0.36 71 83 0 0 0 0 4095 1 
i 4 47.52 0.36 59 90 0 0 0 0 4095 1 
i 1 47.88 0.36 64 83 0 0 0 0 4095 1 
i 1 47.88 0.72 69 88 0 0 0 0 4095 1 
i 2 47.88 0.36 67 85 0 0 0 0 4095 1 
i 3 47.88 0.36 62 83 0 0 0 0 4095 1 
i 1 48.24 0.36 74 86 0 0 0 0 4095 1 
i 2 48.24 0.36 66 87 0 0 0 0 4095 1 
i 3 48.24 0.36 56 89 0 0 0 0 4095 1 
i 4 48.24 0.36 56 82 0 0 0 0 4095 1 
i 4 48.24 0.36 62 92 0 0 0 0 4095 1 
i 1 48.6 0.36 59 98 0 0 0 0 4095 1 
i 1 48.6 0.36 61 81 0 0 0 0 4095 1 
i 1 48.6 0.36 62 84 0 0 0 0 4095 1 
i 4 48.6 0.36 52 91 0 0 0 0 4095 1 
i 2 48.96 0.36 56 84 0 0 0 0 4095 1 
i 2 48.96 0.36 59 86 0 0 0 0 4095 1 
i 2 48.96 0.36 62 83 0 0 0 0 4095 1 
i 2 48.96 0.36 66 95 0 0 0 0 4095 1 
i 2 48.96 0.36 69 85 0 0 0 0 4095 1 
i 4 48.96 0.36 68 84 0 0 0 0 4095 1 
i 1 49.32 0.36 66 80 0 0 0 0 4095 1 
i 1 49.32 1.44 68 81 0 0 0 0 4095 1 
i 2 49.32 0.36 68 82 0 0 0 0 4095 1 
i 3 49.32 0.36 61 81 0 0 0 0 4095 1 
i 3 49.68 0.36 64 80 0 0 0 0 4095 1 
i 3 49.68 0.36 66 81 0 0 0 0 4095 1 
i 4 49.68 0.36 62 94 0 0 0 0 4095 1 
i 4 49.68 1.08 66 81 0 0 0 0 4095 1 
i 2 50.04 0.72 71 93 0 0 0 0 4095 1 
i 2 50.04 0.36 73 92 0 0 0 0 4095 1 
i 3 50.04 0.36 69 91 0 0 0 0 4095 1 
i 1 50.4 0.36 71 93 0 0 0 0 4095 1 
i 2 50.4 0.36 69 83 0 0 0 0 4095 1 
i 1 50.76 0.36 73 97 0 0 0 0 4095 1 
i 2 50.76 0.36 61 92 0 0 0 0 4095 1 
i 3 50.76 0.72 68 89 0 0 0 0 4095 1 
i 4 50.76 0.36 68 98 0 0 0 0 4095 1 
i 4 50.76 0.72 71 92 0 0 0 0 4095 1 
i 3 51.12 0.36 64 82 0 0 0 0 4095 1 
i 3 51.12 0.36 73 86 0 0 0 0 4095 1 
i 4 51.12 0.36 69 83 0 0 0 0 4095 1 
i 1 51.48 0.36 71 86 0 0 0 0 4095 1 
i 2 51.48 0.36 50 86 0 0 0 0 4095 1 
i 2 51.48 0.36 56 84 0 0 0 0 4095 1 
i 2 51.48 0.36 59 85 0 0 0 0 4095 1 
i 2 51.48 0.36 61 96 0 0 0 0 4095 1 
i 2 51.48 0.36 71 83 0 0 0 0 4095 1 
i 1 51.84 0.36 69 83 0 0 0 0 4095 1 
i 1 51.84 0.36 74 83 0 0 0 0 4095 1 
i 2 51.84 0.72 57 83 0 0 0 0 4095 1 
i 2 51.84 0.36 62 85 0 0 0 0 4095 1 
i 4 51.84 0.36 66 85 0 0 0 0 4095 1 
i 4 51.84 0.36 71 81 0 0 0 0 4095 1 
i 1 52.2 0.36 68 82 0 0 0 0 4095 1 
i 2 52.2 0.36 56 87 0 0 0 0 4095 1 
i 2 52.2 0.36 71 99 0 0 0 0 4095 1 
i 3 52.2 0.36 74 86 0 0 0 0 4095 1 
i 4 52.2 0.36 61 83 0 0 0 0 4095 1 
i 2 52.56 0.36 61 84 0 0 0 0 4095 1 
i 2 52.56 0.36 66 93 0 0 0 0 4095 1 
i 3 52.56 0.36 68 90 0 0 0 0 4095 1 
i 4 52.56 0.36 56 86 0 0 0 0 4095 1 
i 1 52.92 0.36 68 83 0 0 0 0 4095 1 
i 2 52.92 0.72 56 84 0 0 0 0 4095 1 
i 2 52.92 0.36 59 84 0 0 0 0 4095 1 
i 4 52.92 0.36 59 87 0 0 0 0 4095 1 
i 2 53.28 0.36 52 89 0 0 0 0 4095 1 
i 3 53.28 0.36 71 86 0 0 0 0 4095 1 
i 3 53.28 0.36 73 89 0 0 0 0 4095 1 
i 4 53.28 0.36 56 89 0 0 0 0 4095 1 
i 1 53.64 0.36 66 95 0 0 0 0 4095 1 
i 1 53.64 0.36 71 81 0 0 0 0 4095 1 
i 2 53.64 0.72 49 90 0 0 0 0 4095 1 
i 4 53.64 0.36 54 87 0 0 0 0 4095 1 
i 4 53.64 0.36 59 82 0 0 0 0 4095 1 
i 1 54 0.36 68 81 0 0 0 0 4095 1 
i 2 54 0.36 47 91 0 0 0 0 4095 1 
i 3 54 0.36 71 81 0 0 0 0 4095 1 
i 4 54 0.36 69 86 0 0 0 0 4095 1 
i 1 54.36 0.72 73 85 0 0 0 0 4095 1 
i 1 54.36 0.36 76 90 0 0 0 0 4095 1 
i 1 54.36 1.08 78 87 0 0 0 0 4095 1 
i 2 54.36 0.36 52 85 0 0 0 0 4095 1 
i 4 54.36 0.36 62 89 0 0 0 0 4095 1 
i 4 54.36 0.36 68 88 0 0 0 0 4095 1 
i 2 54.72 0.36 47 89 0 0 0 0 4095 1 
i 2 54.72 0.36 57 92 0 0 0 0 4095 1 
i 3 54.72 0.36 68 93 0 0 0 0 4095 1 
i 4 54.72 0.36 61 90 0 0 0 0 4095 1 
i 1 55.08 0.36 81 92 0 0 0 0 4095 1 
i 3 55.08 0.36 62 86 0 0 0 0 4095 1 
i 3 55.08 1.08 64 86 0 0 0 0 4095 1 
i 3 55.08 0.72 71 93 0 0 0 0 4095 1 
i 4 55.08 0.36 64 84 0 0 0 0 4095 1 
i 1 55.44 0.36 64 93 0 0 0 0 4095 1 
i 2 55.44 0.36 62 87 0 0 0 0 4095 1 
i 3 55.44 0.36 73 89 0 0 0 0 4095 1 
i 4 55.44 0.36 62 88 0 0 0 0 4095 1 
i 2 55.8 0.36 57 88 0 0 0 0 4095 1 
i 2 55.8 0.72 61 92 0 0 0 0 4095 1 
i 3 55.8 0.72 61 91 0 0 0 0 4095 1 
i 3 55.8 0.36 66 98 0 0 0 0 4095 1 
i 4 55.8 0.72 64 92 0 0 0 0 4095 1 
i 1 56.16 0.36 64 87 0 0 0 0 4095 1 
i 1 56.52 1.08 62 98 0 0 0 0 4095 1 
i 2 56.52 1.08 62 97 0 0 0 0 4095 1 
i 3 56.52 1.08 62 93 0 0 0 0 4095 1 
i 4 56.52 0.36 62 94 0 0 0 0 4095 1 
i 1 57.6 0.36 66 85 0 0 0 0 4095 1 
i 2 57.6 0.36 59 85 0 0 0 0 4095 1 
i 2 57.6 0.36 61 87 0 0 0 0 4095 1 
i 2 57.6 0.36 64 87 0 0 0 0 4095 1 
i 2 57.6 0.36 67 87 0 0 0 0 4095 1 
i 4 57.6 0.36 61 84 0 0 0 0 4095 1 
i 1 57.96 0.36 61 90 0 0 0 0 4095 1 
i 1 57.96 0.36 62 86 0 0 0 0 4095 1 
i 2 57.96 0.36 68 95 0 0 0 0 4095 1 
i 3 57.96 0.36 62 88 0 0 0 0 4095 1 
i 4 57.96 0.36 64 85 0 0 0 0 4095 1 
i 4 57.96 0.72 68 87 0 0 0 0 4095 1 
i 2 58.32 0.36 71 89 0 0 0 0 4095 1 
i 3 58.32 0.36 59 95 0 0 0 0 4095 1 
i 3 58.32 0.36 61 90 0 0 0 0 4095 1 
i 1 58.68 0.36 66 86 0 0 0 0 4095 1 
i 2 58.68 0.36 62 86 0 0 0 0 4095 1 
i 2 58.68 0.72 66 88 0 0 0 0 4095 1 
i 3 58.68 0.36 64 86 0 0 0 0 4095 1 
i 1 59.04 0.36 67 93 0 0 0 0 4095 1 
i 2 59.04 0.36 61 91 0 0 0 0 4095 1 
i 2 59.04 1.08 64 89 0 0 0 0 4095 1 
i 3 59.04 0.36 62 89 0 0 0 0 4095 1 
i 4 59.04 1.08 66 91 0 0 0 0 4095 1 
i 1 59.4 0.36 61 97 0 0 0 0 4095 1 
i 1 59.4 0.36 66 96 0 0 0 0 4095 1 
i 1 59.4 0.36 69 97 0 0 0 0 4095 1 
i 2 59.4 0.36 59 85 0 0 0 0 4095 1 
i 1 59.76 1.08 73 86 0 0 0 0 4095 1 
i 1 59.76 0.36 76 85 0 0 0 0 4095 1 
i 2 59.76 0.36 69 86 0 0 0 0 4095 1 
i 4 59.76 0.36 61 88 0 0 0 0 4095 1 
i 2 60.12 0.36 59 88 0 0 0 0 4095 1 
i 3 60.12 0.36 57 89 0 0 0 0 4095 1 
i 3 60.12 0.36 62 99 0 0 0 0 4095 1 
i 3 60.12 0.36 71 86 0 0 0 0 4095 1 
i 4 60.12 0.36 57 87 0 0 0 0 4095 1 
i 1 60.48 0.36 78 88 0 0 0 0 4095 1 
i 2 60.48 0.36 52 89 0 0 0 0 4095 1 
i 2 60.48 0.36 54 94 0 0 0 0 4095 1 
i 2 60.48 0.36 57 90 0 0 0 0 4095 1 
i 1 60.84 0.72 64 86 0 0 0 0 4095 1 
i 1 60.84 1.44 66 87 0 0 0 0 4095 1 
i 3 60.84 1.08 61 96 0 0 0 0 4095 1 
i 3 60.84 0.36 66 88 0 0 0 0 4095 1 
i 4 60.84 0.36 62 91 0 0 0 0 4095 1 
i 2 61.2 0.72 52 87 0 0 0 0 4095 1 
i 2 61.2 0.36 54 92 0 0 0 0 4095 1 
i 3 61.2 0.36 55 86 0 0 0 0 4095 1 
i 4 61.56 0.36 66 88 0 0 0 0 4095 1 
i 4 61.56 0.36 68 86 0 0 0 0 4095 1 
i 1 61.92 0.36 64 98 0 0 0 0 4095 1 
i 1 61.92 0.36 67 93 0 0 0 0 4095 1 
i 2 61.92 0.36 43 88 0 0 0 0 4095 1 
i 2 61.92 0.36 47 85 0 0 0 0 4095 1 
i 3 61.92 0.36 55 90 0 0 0 0 4095 1 
i 1 62.28 0.36 69 83 0 0 0 0 4095 1 
i 1 62.28 0.36 71 84 0 0 0 0 4095 1 
i 2 62.28 0.36 37 83 0 0 0 0 4095 1 
i 3 62.28 0.36 66 87 0 0 0 0 4095 1 
i 4 62.28 0.36 64 92 0 0 0 0 4095 1 
i 4 62.28 0.36 71 85 0 0 0 0 4095 1 
i 1 62.64 0.36 66 98 0 0 0 0 4095 1 
i 1 62.64 0.72 68 86 0 0 0 0 4095 1 
i 2 62.64 0.36 68 88 0 0 0 0 4095 1 
i 2 62.64 0.36 71 85 0 0 0 0 4095 1 
i 3 62.64 0.36 68 86 0 0 0 0 4095 1 
i 4 62.64 0.36 69 87 0 0 0 0 4095 1 
i 2 63 0.36 66 86 0 0 0 0 4095 1 
i 3 63 0.36 61 84 0 0 0 0 4095 1 
i 3 63 0.36 62 85 0 0 0 0 4095 1 
i 4 63 0.36 73 88 0 0 0 0 4095 1 
i 2 63.36 1.08 71 87 0 0 0 0 4095 1 
i 3 63.36 0.36 57 83 0 0 0 0 4095 1 
i 3 63.36 0.36 66 91 0 0 0 0 4095 1 
i 3 63.36 0.36 71 85 0 0 0 0 4095 1 
i 4 63.36 0.36 66 94 0 0 0 0 4095 1 
i 4 63.36 0.36 68 89 0 0 0 0 4095 1 
i 1 63.72 0.36 68 85 0 0 0 0 4095 1 
i 1 63.72 0.36 73 85 0 0 0 0 4095 1 
i 2 63.72 0.36 73 96 0 0 0 0 4095 1 
i 3 63.72 0.72 76 84 0 0 0 0 4095 1 
i 1 64.08 0.36 62 86 0 0 0 0 4095 1 
i 4 64.08 0.36 71 96 0 0 0 0 4095 1 
i 4 64.08 0.72 73 86 0 0 0 0 4095 1 
i 4 64.08 0.36 78 89 0 0 0 0 4095 1 
i 1 64.44 0.36 66 94 0 0 0 0 4095 1 
i 2 64.44 0.36 76 88 0 0 0 0 4095 1 
i 2 64.44 0.72 80 84 0 0 0 0 4095 1 
i 3 64.44 0.36 73 83 0 0 0 0 4095 1 
i 4 64.44 0.36 66 83 0 0 0 0 4095 1 
i 1 64.8 0.36 61 91 0 0 0 0 4095 1 
i 1 64.8 0.36 64 90 0 0 0 0 4095 1 
i 3 64.8 0.36 78 85 0 0 0 0 4095 1 
i 3 64.8 0.72 80 86 0 0 0 0 4095 1 
i 4 64.8 0.72 64 98 0 0 0 0 4095 1 
i 1 65.16 0.36 56 87 0 0 0 0 4095 1 
i 1 65.16 0.36 62 90 0 0 0 0 4095 1 
i 1 65.16 0.36 71 88 0 0 0 0 4095 1 
i 3 65.16 0.36 83 94 0 0 0 0 4095 1 
i 1 65.52 0.36 76 83 0 0 0 0 4095 1 
i 2 65.52 0.72 68 85 0 0 0 0 4095 1 
i 2 65.52 0.36 73 95 0 0 0 0 4095 1 
i 4 65.52 0.36 68 83 0 0 0 0 4095 1 
i 4 65.52 0.36 71 92 0 0 0 0 4095 1 
i 4 65.52 0.36 73 84 0 0 0 0 4095 1 
i 1 65.88 0.36 66 98 0 0 0 0 4095 1 
i 1 65.88 0.36 71 89 0 0 0 0 4095 1 
i 2 65.88 0.36 71 94 0 0 0 0 4095 1 
i 3 65.88 0.36 71 91 0 0 0 0 4095 1 
i 4 65.88 0.72 76 85 0 0 0 0 4095 1 
i 2 66.24 0.36 59 91 0 0 0 0 4095 1 
i 2 66.24 0.36 61 85 0 0 0 0 4095 1 
i 2 66.24 0.36 62 89 0 0 0 0 4095 1 
i 4 66.24 0.36 74 89 0 0 0 0 4095 1 
i 4 66.24 0.36 78 86 0 0 0 0 4095 1 
i 1 66.6 0.36 61 86 0 0 0 0 4095 1 
i 1 66.6 0.36 62 88 0 0 0 0 4095 1 
i 1 66.6 0.36 64 90 0 0 0 0 4095 1 
i 1 66.6 0.36 66 94 0 0 0 0 4095 1 
i 4 66.6 0.36 56 87 0 0 0 0 4095 1 
i 4 66.6 0.36 61 93 0 0 0 0 4095 1 
i 2 66.96 0.36 59 88 0 0 0 0 4095 1 
i 2 66.96 0.36 61 94 0 0 0 0 4095 1 
i 2 66.96 0.36 64 91 0 0 0 0 4095 1 
i 2 66.96 0.36 66 88 0 0 0 0 4095 1 
i 3 66.96 0.72 73 88 0 0 0 0 4095 1 
i 4 66.96 0.36 64 89 0 0 0 0 4095 1 
i 1 67.32 0.36 62 96 0 0 0 0 4095 1 
i 1 67.32 0.72 64 91 0 0 0 0 4095 1 
i 3 67.32 0.72 76 86 0 0 0 0 4095 1 
i 4 67.32 0.36 61 93 0 0 0 0 4095 1 
i 4 67.32 0.36 71 92 0 0 0 0 4095 1 
i 2 67.68 1.8 66 92 0 0 0 0 4095 1 
i 4 67.68 1.08 69 92 0 0 0 0 4095 1 
i 1 68.04 1.44 62 93 0 0 0 0 4095 1 
i 1 69.12 0.36 64 95 0 0 0 0 4095 1 
i 2 69.12 0.36 71 88 0 0 0 0 4095 1 
i 3 69.12 0.36 73 86 0 0 0 0 4095 1 
i 3 69.12 0.36 78 88 0 0 0 0 4095 1 
i 1 69.48 0.36 69 84 0 0 0 0 4095 1 
i 1 69.48 0.72 71 88 0 0 0 0 4095 1 
i 2 69.48 0.36 76 97 0 0 0 0 4095 1 
i 3 69.48 0.36 62 81 0 0 0 0 4095 1 
i 3 69.48 0.36 64 92 0 0 0 0 4095 1 
i 3 69.48 0.36 66 85 0 0 0 0 4095 1 
i 1 69.84 0.36 73 96 0 0 0 0 4095 1 
i 3 69.84 0.36 69 89 0 0 0 0 4095 1 
i 4 69.84 0.36 71 85 0 0 0 0 4095 1 
i 1 70.2 0.72 69 89 0 0 0 0 4095 1 
i 1 70.2 0.36 74 87 0 0 0 0 4095 1 
i 2 70.2 0.36 69 92 0 0 0 0 4095 1 
i 3 70.2 0.36 73 85 0 0 0 0 4095 1 
i 4 70.2 0.36 76 90 0 0 0 0 4095 1 
i 1 70.56 0.36 71 85 0 0 0 0 4095 1 
i 2 70.56 1.08 61 82 0 0 0 0 4095 1 
i 2 70.56 0.72 66 81 0 0 0 0 4095 1 
i 3 70.56 0.36 78 83 0 0 0 0 4095 1 
i 4 70.56 0.36 64 88 0 0 0 0 4095 1 
i 1 70.92 0.36 66 88 0 0 0 0 4095 1 
i 3 70.92 0.36 62 84 0 0 0 0 4095 1 
i 3 70.92 0.36 64 87 0 0 0 0 4095 1 
i 3 70.92 0.36 71 95 0 0 0 0 4095 1 
i 1 71.28 0.36 69 90 0 0 0 0 4095 1 
i 1 71.28 0.36 71 93 0 0 0 0 4095 1 
i 1 71.28 0.36 76 83 0 0 0 0 4095 1 
i 3 71.28 0.36 76 87 0 0 0 0 4095 1 
i 4 71.28 0.36 69 85 0 0 0 0 4095 1 
i 1 71.64 0.36 73 84 0 0 0 0 4095 1 
i 2 71.64 0.36 66 85 0 0 0 0 4095 1 
i 3 71.64 0.36 71 94 0 0 0 0 4095 1 
i 4 71.64 0.72 68 94 0 0 0 0 4095 1 
i 1 72 0.36 59 89 0 0 0 0 4095 1 
i 1 72 0.36 61 85 0 0 0 0 4095 1 
i 3 72 0.36 66 84 0 0 0 0 4095 1 
i 3 72 0.36 68 85 0 0 0 0 4095 1 
i 2 72.36 0.36 57 89 0 0 0 0 4095 1 
i 2 72.36 0.36 61 80 0 0 0 0 4095 1 
i 4 72.36 0.36 71 81 0 0 0 0 4095 1 
i 4 72.36 0.36 73 88 0 0 0 0 4095 1 
i 4 72.36 0.36 76 85 0 0 0 0 4095 1 
i 1 72.72 0.72 59 98 0 0 0 0 4095 1 
i 2 72.72 0.36 50 82 0 0 0 0 4095 1 
i 2 72.72 0.36 56 94 0 0 0 0 4095 1 
i 3 72.72 1.08 71 81 0 0 0 0 4095 1 
i 4 72.72 0.36 80 96 0 0 0 0 4095 1 
i 2 73.08 0.36 44 85 0 0 0 0 4095 1 
i 2 73.08 0.36 64 86 0 0 0 0 4095 1 
i 3 73.08 0.36 76 89 0 0 0 0 4095 1 
i 4 73.08 0.36 78 92 0 0 0 0 4095 1 
i 1 73.44 0.36 61 88 0 0 0 0 4095 1 
i 2 73.44 0.36 66 89 0 0 0 0 4095 1 
i 3 73.44 0.36 64 87 0 0 0 0 4095 1 
i 3 73.44 0.72 66 98 0 0 0 0 4095 1 
i 3 73.44 0.36 69 87 0 0 0 0 4095 1 
i 1 73.8 0.36 56 82 0 0 0 0 4095 1 
i 1 73.8 0.36 57 94 0 0 0 0 4095 1 
i 4 73.8 0.36 64 82 0 0 0 0 4095 1 
i 4 73.8 0.36 68 81 0 0 0 0 4095 1 
i 4 73.8 0.36 71 83 0 0 0 0 4095 1 
i 1 74.16 0.36 52 83 0 0 0 0 4095 1 
i 1 74.16 0.36 54 85 0 0 0 0 4095 1 
i 1 74.16 0.72 68 81 0 0 0 0 4095 1 
i 2 74.16 0.36 64 88 0 0 0 0 4095 1 
i 3 74.16 0.72 68 82 0 0 0 0 4095 1 
i 3 74.16 0.36 71 88 0 0 0 0 4095 1 
i 2 74.52 0.36 59 86 0 0 0 0 4095 1 
i 3 74.52 0.72 66 83 0 0 0 0 4095 1 
i 4 74.52 0.36 66 86 0 0 0 0 4095 1 
i 1 74.88 0.36 66 97 0 0 0 0 4095 1 
i 2 74.88 0.36 56 87 0 0 0 0 4095 1 
i 4 74.88 0.72 62 85 0 0 0 0 4095 1 
i 4 74.88 0.36 64 89 0 0 0 0 4095 1 
i 1 75.24 0.72 68 83 0 0 0 0 4095 1 
i 2 75.24 0.36 54 84 0 0 0 0 4095 1 
i 2 75.24 0.72 59 86 0 0 0 0 4095 1 
i 3 75.24 0.36 64 87 0 0 0 0 4095 1 
i 4 75.24 0.36 61 91 0 0 0 0 4095 1 
i 1 75.6 0.36 69 89 0 0 0 0 4095 1 
i 1 75.6 0.72 73 90 0 0 0 0 4095 1 
i 2 75.6 0.36 69 85 0 0 0 0 4095 1 
i 4 75.6 0.72 66 97 0 0 0 0 4095 1 
i 2 75.96 0.36 71 83 0 0 0 0 4095 1 
i 3 75.96 0.36 64 84 0 0 0 0 4095 1 
i 4 75.96 0.36 71 87 0 0 0 0 4095 1 
i 4 75.96 0.36 73 83 0 0 0 0 4095 1 
i 1 76.32 0.72 68 87 0 0 0 0 4095 1 
i 3 76.32 0.36 66 93 0 0 0 0 4095 1 
i 3 76.32 0.36 68 81 0 0 0 0 4095 1 
i 3 76.32 0.36 69 80 0 0 0 0 4095 1 
i 4 76.32 0.36 68 94 0 0 0 0 4095 1 
i 1 76.68 0.36 66 81 0 0 0 0 4095 1 
i 1 76.68 0.72 71 96 0 0 0 0 4095 1 
i 2 76.68 0.36 61 90 0 0 0 0 4095 1 
i 3 76.68 0.36 73 85 0 0 0 0 4095 1 
i 4 76.68 0.36 62 97 0 0 0 0 4095 1 
i 2 77.04 0.36 62 94 0 0 0 0 4095 1 
i 2 77.04 0.36 66 88 0 0 0 0 4095 1 
i 2 77.04 0.36 69 86 0 0 0 0 4095 1 
i 4 77.04 0.36 57 89 0 0 0 0 4095 1 
i 4 77.04 0.36 71 82 0 0 0 0 4095 1 
i 1 77.4 1.08 64 85 0 0 0 0 4095 1 
i 1 77.4 0.36 66 84 0 0 0 0 4095 1 
i 2 77.4 0.36 64 91 0 0 0 0 4095 1 
i 2 77.4 0.36 68 95 0 0 0 0 4095 1 
i 4 77.4 0.36 68 100 0 0 0 0 4095 1 
i 1 77.76 0.36 69 94 0 0 0 0 4095 1 
i 2 77.76 0.36 59 84 0 0 0 0 4095 1 
i 2 77.76 0.36 71 86 0 0 0 0 4095 1 
i 3 77.76 0.36 61 86 0 0 0 0 4095 1 
i 4 77.76 1.08 64 85 0 0 0 0 4095 1 
i 1 78.12 0.36 59 85 0 0 0 0 4095 1 
i 2 78.12 0.36 76 82 0 0 0 0 4095 1 
i 2 78.12 0.36 80 90 0 0 0 0 4095 1 
i 4 78.12 1.08 69 83 0 0 0 0 4095 1 
i 1 78.48 0.36 61 91 0 0 0 0 4095 1 
i 2 78.48 0.72 64 84 0 0 0 0 4095 1 
i 3 78.48 0.36 62 88 0 0 0 0 4095 1 
i 3 78.48 0.36 64 88 0 0 0 0 4095 1 
i 1 78.84 0.36 64 90 0 0 0 0 4095 1 
i 1 78.84 1.08 66 90 0 0 0 0 4095 1 
i 2 78.84 0.72 66 83 0 0 0 0 4095 1 
i 4 78.84 0.36 73 87 0 0 0 0 4095 1 
i 3 79.2 0.72 62 87 0 0 0 0 4095 1 
i 4 79.56 0.36 71 90 0 0 0 0 4095 1 
i 2 79.92 0.72 66 98 0 0 0 0 4095 1 
i 3 79.92 0.36 61 81 0 0 0 0 4095 1 
i 4 79.92 0.72 69 81 0 0 0 0 4095 1 
i 1 80.64 1.08 64 84 0 0 0 0 4095 1 
i 1 80.64 0.72 66 85 0 0 0 0 4095 1 
i 1 80.64 0.36 67 88 0 0 0 0 4095 1 
i 1 80.64 0.36 71 93 0 0 0 0 4095 1 
i 2 80.64 0.36 71 83 0 0 0 0 4095 1 
i 3 80.64 0.36 66 83 0 0 0 0 4095 1 
i 1 81 0.36 59 85 0 0 0 0 4095 1 
i 2 81 0.72 73 87 0 0 0 0 4095 1 
i 4 81 0.36 62 82 0 0 0 0 4095 1 
i 4 81 1.08 64 83 0 0 0 0 4095 1 
i 1 81.36 0.36 69 85 0 0 0 0 4095 1 
i 2 81.36 0.36 66 84 0 0 0 0 4095 1 
i 2 81.36 0.36 69 88 0 0 0 0 4095 1 
i 2 81.72 0.36 71 92 0 0 0 0 4095 1 
i 2 81.72 0.72 76 97 0 0 0 0 4095 1 
i 3 81.72 0.36 62 99 0 0 0 0 4095 1 
i 4 81.72 0.36 61 92 0 0 0 0 4095 1 
i 4 81.72 0.36 66 88 0 0 0 0 4095 1 
i 1 82.08 0.36 62 85 0 0 0 0 4095 1 
i 1 82.08 0.72 66 99 0 0 0 0 4095 1 
i 3 82.08 0.36 57 84 0 0 0 0 4095 1 
i 3 82.08 0.36 61 87 0 0 0 0 4095 1 
i 2 82.44 0.36 73 90 0 0 0 0 4095 1 
i 3 82.44 0.36 54 89 0 0 0 0 4095 1 
i 3 82.44 0.36 71 95 0 0 0 0 4095 1 
i 4 82.44 0.36 62 90 0 0 0 0 4095 1 
i 1 82.8 1.44 64 90 0 0 0 0 4095 1 
i 1 82.8 0.72 67 90 0 0 0 0 4095 1 
i 3 82.8 0.36 74 100 0 0 0 0 4095 1 
i 4 82.8 0.36 57 92 0 0 0 0 4095 1 
i 4 82.8 0.36 66 88 0 0 0 0 4095 1 
i 2 83.16 0.36 74 90 0 0 0 0 4095 1 
i 2 83.16 0.36 78 92 0 0 0 0 4095 1 
i 3 83.16 0.36 76 86 0 0 0 0 4095 1 
i 4 83.16 0.36 61 95 0 0 0 0 4095 1 
i 1 83.52 0.36 61 88 0 0 0 0 4095 1 
i 1 83.52 0.36 66 99 0 0 0 0 4095 1 
i 2 83.52 0.36 73 88 0 0 0 0 4095 1 
i 2 83.52 0.36 76 86 0 0 0 0 4095 1 
i 2 83.52 0.36 81 91 0 0 0 0 4095 1 
i 1 83.88 0.36 59 89 0 0 0 0 4095 1 
i 1 83.88 0.36 71 91 0 0 0 0 4095 1 
i 2 83.88 0.72 62 86 0 0 0 0 4095 1 
i 4 83.88 0.36 61 90 0 0 0 0 4095 1 
i 1 84.24 0.36 66 85 0 0 0 0 4095 1 
i 2 84.24 0.36 64 92 0 0 0 0 4095 1 
i 3 84.24 0.36 66 94 0 0 0 0 4095 1 
i 4 84.24 0.36 56 91 0 0 0 0 4095 1 
i 4 84.24 0.72 66 85 0 0 0 0 4095 1 
i 2 84.6 0.36 66 86 0 0 0 0 4095 1 
i 4 84.6 0.72 62 93 0 0 0 0 4095 1 
i 4 84.6 0.72 64 88 0 0 0 0 4095 1 
i 1 84.96 0.36 71 85 0 0 0 0 4095 1 
i 2 84.96 0.36 69 85 0 0 0 0 4095 1 
i 2 84.96 0.36 71 86 0 0 0 0 4095 1 
i 3 84.96 0.36 66 93 0 0 0 0 4095 1 
i 3 84.96 0.36 71 88 0 0 0 0 4095 1 
i 1 85.32 0.36 76 95 0 0 0 0 4095 1 
i 1 85.32 0.36 78 93 0 0 0 0 4095 1 
i 2 85.32 0.36 61 85 0 0 0 0 4095 1 
i 2 85.32 0.72 64 86 0 0 0 0 4095 1 
i 3 85.32 0.36 76 93 0 0 0 0 4095 1 
i 4 85.32 0.36 61 94 0 0 0 0 4095 1 
i 1 85.68 0.36 83 88 0 0 0 0 4095 1 
i 2 85.68 0.36 59 89 0 0 0 0 4095 1 
i 2 85.68 0.36 71 86 0 0 0 0 4095 1 
i 3 85.68 0.36 78 86 0 0 0 0 4095 1 
i 4 85.68 0.36 66 84 0 0 0 0 4095 1 
i 1 86.04 0.36 62 90 0 0 0 0 4095 1 
i 2 86.04 0.36 68 82 0 0 0 0 4095 1 
i 2 86.04 0.36 69 86 0 0 0 0 4095 1 
i 2 86.04 0.36 73 83 0 0 0 0 4095 1 
i 4 86.04 0.36 71 85 0 0 0 0 4095 1 
i 1 86.4 0.36 59 81 0 0 0 0 4095 1 
i 1 86.4 0.36 61 88 0 0 0 0 4095 1 
i 2 86.4 0.36 71 81 0 0 0 0 4095 1 
i 3 86.4 0.72 74 91 0 0 0 0 4095 1 
i 4 86.4 0.36 68 81 0 0 0 0 4095 1 
i 1 86.76 0.36 64 87 0 0 0 0 4095 1 
i 2 86.76 0.36 66 93 0 0 0 0 4095 1 
i 3 86.76 0.36 68 99 0 0 0 0 4095 1 
i 4 86.76 0.36 69 89 0 0 0 0 4095 1 
i 1 87.12 0.36 68 85 0 0 0 0 4095 1 
i 3 87.12 0.36 69 86 0 0 0 0 4095 1 
i 3 87.12 0.36 73 92 0 0 0 0 4095 1 
i 4 87.12 0.36 71 91 0 0 0 0 4095 1 
i 4 87.12 0.36 74 89 0 0 0 0 4095 1 
i 1 87.48 0.36 64 84 0 0 0 0 4095 1 
i 2 87.48 0.36 61 87 0 0 0 0 4095 1 
i 2 87.48 0.36 62 85 0 0 0 0 4095 1 
i 3 87.48 0.72 64 91 0 0 0 0 4095 1 
i 4 87.48 0.36 64 92 0 0 0 0 4095 1 
i 4 87.48 0.36 69 93 0 0 0 0 4095 1 
i 1 87.84 0.36 62 93 0 0 0 0 4095 1 
i 2 87.84 0.36 66 88 0 0 0 0 4095 1 
i 2 87.84 0.36 71 91 0 0 0 0 4095 1 
i 4 87.84 0.36 71 88 0 0 0 0 4095 1 
i 4 87.84 0.72 73 87 0 0 0 0 4095 1 
i 1 88.2 0.72 64 91 0 0 0 0 4095 1 
i 1 88.2 0.72 68 85 0 0 0 0 4095 1 
i 2 88.2 0.72 73 92 0 0 0 0 4095 1 
i 3 88.2 0.72 54 92 0 0 0 0 4095 1 
i 3 88.2 0.36 59 88 0 0 0 0 4095 1 
i 2 88.56 0.36 69 83 0 0 0 0 4095 1 
i 4 88.56 0.36 71 84 0 0 0 0 4095 1 
i 2 88.92 0.36 63 93 0 0 0 0 4095 1 
i 2 88.92 0.36 68 85 0 0 0 0 4095 1 
i 3 88.92 0.36 59 87 0 0 0 0 4095 1 
i 4 88.92 0.36 57 88 0 0 0 0 4095 1 
i 4 88.92 0.36 63 90 0 0 0 0 4095 1 
i 4 88.92 0.36 69 85 0 0 0 0 4095 1 
i 1 89.28 0.36 63 88 0 0 0 0 4095 1 
i 2 89.28 0.72 73 92 0 0 0 0 4095 1 
i 3 89.28 0.72 63 92 0 0 0 0 4095 1 
i 4 89.28 0.36 52 91 0 0 0 0 4095 1 
i 4 89.28 0.36 56 90 0 0 0 0 4095 1 
i 4 89.28 0.36 59 92 0 0 0 0 4095 1 
i 3 89.64 0.72 64 80 0 0 0 0 4095 1 
i 4 89.64 0.36 63 81 0 0 0 0 4095 1 
i 4 89.64 0.36 66 82 0 0 0 0 4095 1 
i 4 89.64 1.08 71 91 0 0 0 0 4095 1 
i 1 90 0.36 57 90 0 0 0 0 4095 1 
i 1 90 0.36 61 85 0 0 0 0 4095 1 
i 1 90 0.36 71 84 0 0 0 0 4095 1 
i 4 90 0.36 75 89 0 0 0 0 4095 1 
i 1 90.36 0.72 66 89 0 0 0 0 4095 1 
i 4 90.36 0.36 74 81 0 0 0 0 4095 1 
i 2 90.72 0.36 73 91 0 0 0 0 4095 1 
i 3 90.72 1.08 64 83 0 0 0 0 4095 1 
i 4 90.72 0.36 73 82 0 0 0 0 4095 1 
i 2 91.08 0.72 69 90 0 0 0 0 4095 1 
i 4 91.08 0.72 69 92 0 0 0 0 4095 1 
i 1 91.44 0.36 66 91 0 0 0 0 4095 1 
i 1 91.8 0.36 65 84 0 0 0 0 4095 1 
i 2 91.8 0.36 68 94 0 0 0 0 4095 1 
i 1 92.16 0.36 71 98 0 0 0 0 4095 1 
i 2 92.16 0.36 69 88 0 0 0 0 4095 1 
i 2 92.16 0.36 74 92 0 0 0 0 4095 1 
i 3 92.16 0.36 59 86 0 0 0 0 4095 1 
i 3 92.16 0.36 66 89 0 0 0 0 4095 1 
i 4 92.16 0.72 71 85 0 0 0 0 4095 1 
i 1 92.52 0.36 57 84 0 0 0 0 4095 1 
i 1 92.52 0.36 59 86 0 0 0 0 4095 1 
i 1 92.52 0.72 62 84 0 0 0 0 4095 1 
i 3 92.52 0.72 68 88 0 0 0 0 4095 1 
i 4 92.52 0.36 61 86 0 0 0 0 4095 1 
i 1 92.88 0.36 61 89 0 0 0 0 4095 1 
i 2 92.88 0.36 76 94 0 0 0 0 4095 1 
i 4 92.88 0.36 73 90 0 0 0 0 4095 1 
i 1 93.24 0.36 68 88 0 0 0 0 4095 1 
i 2 93.24 0.36 64 89 0 0 0 0 4095 1 
i 3 93.24 0.72 71 93 0 0 0 0 4095 1 
i 4 93.24 0.36 64 90 0 0 0 0 4095 1 
i 4 93.24 0.36 71 100 0 0 0 0 4095 1 
i 1 93.6 0.36 64 91 0 0 0 0 4095 1 
i 2 93.6 1.08 61 85 0 0 0 0 4095 1 
i 4 93.6 0.36 66 94 0 0 0 0 4095 1 
i 4 93.6 0.36 68 85 0 0 0 0 4095 1 
i 1 93.96 0.72 68 92 0 0 0 0 4095 1 
i 2 93.96 0.36 64 85 0 0 0 0 4095 1 
i 4 93.96 0.36 73 85 0 0 0 0 4095 1 
i 4 93.96 0.36 78 86 0 0 0 0 4095 1 
i 4 93.96 0.36 81 84 0 0 0 0 4095 1 
i 1 94.32 0.36 66 89 0 0 0 0 4095 1 
i 4 94.32 0.36 61 86 0 0 0 0 4095 1 
i 4 94.32 0.36 66 84 0 0 0 0 4095 1 
i 4 94.32 0.36 68 95 0 0 0 0 4095 1 
i 1 94.68 0.36 57 86 0 0 0 0 4095 1 
i 1 94.68 0.36 61 84 0 0 0 0 4095 1 
i 1 94.68 0.72 71 91 0 0 0 0 4095 1 
i 2 94.68 0.36 62 84 0 0 0 0 4095 1 
i 2 94.68 0.36 66 90 0 0 0 0 4095 1 
i 3 94.68 0.36 67 84 0 0 0 0 4095 1 
i 3 95.04 0.36 68 99 0 0 0 0 4095 1 
i 3 95.04 0.36 71 88 0 0 0 0 4095 1 
i 4 95.04 0.36 62 83 0 0 0 0 4095 1 
i 4 95.04 1.08 64 85 0 0 0 0 4095 1 
i 4 95.04 0.36 68 81 0 0 0 0 4095 1 
i 1 95.4 0.72 68 83 0 0 0 0 4095 1 
i 1 95.4 0.36 69 83 0 0 0 0 4095 1 
i 1 95.4 0.36 73 87 0 0 0 0 4095 1 
i 2 95.4 0.36 62 82 0 0 0 0 4095 1 
i 4 95.4 0.36 66 84 0 0 0 0 4095 1 
i 1 95.76 0.36 59 86 0 0 0 0 4095 1 
i 1 95.76 0.36 61 86 0 0 0 0 4095 1 
i 3 95.76 0.72 66 87 0 0 0 0 4095 1 
i 4 95.76 0.72 68 88 0 0 0 0 4095 1 
i 1 96.12 0.36 54 89 0 0 0 0 4095 1 
i 2 96.12 0.72 56 92 0 0 0 0 4095 1 
i 2 96.12 1.44 61 98 0 0 0 0 4095 1 
i 3 96.12 0.36 64 99 0 0 0 0 4095 1 
i 1 96.48 0.36 62 87 0 0 0 0 4095 1 
i 1 96.48 0.36 64 90 0 0 0 0 4095 1 
i 2 96.48 0.36 49 83 0 0 0 0 4095 1 
i 2 96.48 0.36 52 86 0 0 0 0 4095 1 
i 2 96.48 0.36 55 86 0 0 0 0 4095 1 
i 1 96.84 0.36 66 91 0 0 0 0 4095 1 
i 1 96.84 0.36 67 96 0 0 0 0 4095 1 
i 2 96.84 0.72 66 91 0 0 0 0 4095 1 
i 4 96.84 0.36 64 95 0 0 0 0 4095 1 
i 1 97.2 0.36 69 92 0 0 0 0 4095 1 
i 2 97.2 0.36 71 86 0 0 0 0 4095 1 
i 2 97.2 0.36 74 85 0 0 0 0 4095 1 
i 3 97.2 0.36 64 92 0 0 0 0 4095 1 
i 3 97.56 0.36 56 85 0 0 0 0 4095 1 
i 3 97.56 0.36 57 87 0 0 0 0 4095 1 
i 3 97.56 0.36 59 85 0 0 0 0 4095 1 
i 3 97.56 0.36 71 98 0 0 0 0 4095 1 
i 4 97.56 0.36 68 86 0 0 0 0 4095 1 
i 4 97.56 0.36 73 91 0 0 0 0 4095 1 
i 1 97.92 0.72 71 85 0 0 0 0 4095 1 
i 1 97.92 0.72 73 94 0 0 0 0 4095 1 
i 2 97.92 0.36 73 91 0 0 0 0 4095 1 
i 2 97.92 0.72 78 93 0 0 0 0 4095 1 
i 3 97.92 0.36 66 87 0 0 0 0 4095 1 
i 1 98.28 0.36 76 82 0 0 0 0 4095 1 
i 2 98.28 0.36 81 85 0 0 0 0 4095 1 
i 3 98.28 0.72 69 84 0 0 0 0 4095 1 
i 1 98.64 0.36 74 90 0 0 0 0 4095 1 
i 2 98.64 0.36 83 88 0 0 0 0 4095 1 
i 2 98.64 0.36 85 94 0 0 0 0 4095 1 
i 4 98.64 0.36 67 97 0 0 0 0 4095 1 
i 4 98.64 0.72 69 89 0 0 0 0 4095 1 
i 1 99 0.36 59 84 0 0 0 0 4095 1 
i 1 99 0.36 62 87 0 0 0 0 4095 1 
i 1 99 0.36 64 88 0 0 0 0 4095 1 
i 1 99 0.36 66 100 0 0 0 0 4095 1 
i 4 99 0.36 66 95 0 0 0 0 4095 1 
i 1 99.36 0.36 61 89 0 0 0 0 4095 1 
i 1 99.36 0.36 67 83 0 0 0 0 4095 1 
i 2 99.36 0.36 66 83 0 0 0 0 4095 1 
i 2 99.36 0.36 71 80 0 0 0 0 4095 1 
i 4 99.36 0.36 71 81 0 0 0 0 4095 1 
i 2 99.72 0.36 64 86 0 0 0 0 4095 1 
i 2 99.72 0.36 68 85 0 0 0 0 4095 1 
i 2 99.72 0.36 69 84 0 0 0 0 4095 1 
i 3 99.72 0.36 62 86 0 0 0 0 4095 1 
i 3 99.72 0.36 66 84 0 0 0 0 4095 1 
i 1 100.08 0.36 62 90 0 0 0 0 4095 1 
i 3 100.08 0.36 61 91 0 0 0 0 4095 1 
i 4 100.08 1.08 76 89 0 0 0 0 4095 1 
i 1 100.44 0.36 68 88 0 0 0 0 4095 1 
i 2 100.44 0.36 61 89 0 0 0 0 4095 1 
i 2 100.44 0.36 71 93 0 0 0 0 4095 1 
i 3 100.44 0.72 56 92 0 0 0 0 4095 1 
i 3 100.44 1.08 59 96 0 0 0 0 4095 1 
i 1 100.8 0.36 66 94 0 0 0 0 4095 1 
i 2 100.8 0.72 76 86 0 0 0 0 4095 1 
i 2 100.8 0.36 78 88 0 0 0 0 4095 1 
i 2 101.16 0.36 73 85 0 0 0 0 4095 1 
i 4 101.16 0.36 64 85 0 0 0 0 4095 1 
i 4 101.16 0.36 69 88 0 0 0 0 4095 1 
i 4 101.16 0.36 71 93 0 0 0 0 4095 1 
i 1 101.52 0.36 62 81 0 0 0 0 4095 1 
i 1 101.52 0.36 67 81 0 0 0 0 4095 1 
i 2 101.52 0.36 61 84 0 0 0 0 4095 1 
i 1 101.88 0.36 59 82 0 0 0 0 4095 1 
i 1 101.88 0.72 61 82 0 0 0 0 4095 1 
i 1 101.88 0.36 64 90 0 0 0 0 4095 1 
i 3 101.88 0.36 59 82 0 0 0 0 4095 1 
i 3 101.88 0.36 64 89 0 0 0 0 4095 1 
i 3 101.88 1.08 66 83 0 0 0 0 4095 1 
i 2 102.24 0.36 61 89 0 0 0 0 4095 1 
i 4 102.24 1.44 69 100 0 0 0 0 4095 1 
i 1 102.6 0.72 62 93 0 0 0 0 4095 1 
i 2 102.6 0.72 62 85 0 0 0 0 4095 1 
i 1 103.32 0.36 61 85 0 0 0 0 4095 1 
i 2 103.32 0.36 61 91 0 0 0 0 4095 1 
i 1 103.68 0.36 66 94 0 0 0 0 4095 1 
i 2 103.68 0.36 64 86 0 0 0 0 4095 1 
i 2 103.68 0.36 68 90 0 0 0 0 4095 1 
i 3 103.68 0.36 66 96 0 0 0 0 4095 1 
i 3 103.68 0.36 68 86 0 0 0 0 4095 1 
i 1 104.04 0.36 61 87 0 0 0 0 4095 1 
i 1 104.04 0.36 68 93 0 0 0 0 4095 1 
i 2 104.04 0.36 62 86 0 0 0 0 4095 1 
i 3 104.04 0.36 71 86 0 0 0 0 4095 1 
i 4 104.04 0.36 66 86 0 0 0 0 4095 1 
i 1 104.4 0.72 66 91 0 0 0 0 4095 1 
i 2 104.4 0.36 64 89 0 0 0 0 4095 1 
i 3 104.4 0.36 56 91 0 0 0 0 4095 1 
i 3 104.4 0.36 61 86 0 0 0 0 4095 1 
i 3 104.4 0.36 66 90 0 0 0 0 4095 1 
i 4 104.4 1.08 64 91 0 0 0 0 4095 1 
i 3 104.76 0.36 57 88 0 0 0 0 4095 1 
i 4 104.76 0.36 56 85 0 0 0 0 4095 1 
i 4 104.76 0.36 59 86 0 0 0 0 4095 1 
i 1 105.12 1.08 68 86 0 0 0 0 4095 1 
i 2 105.12 0.36 62 86 0 0 0 0 4095 1 
i 3 105.12 0.36 59 99 0 0 0 0 4095 1 
i 4 105.12 0.36 69 87 0 0 0 0 4095 1 
i 3 105.48 0.36 57 83 0 0 0 0 4095 1 
i 3 105.48 0.36 68 82 0 0 0 0 4095 1 
i 4 105.48 0.72 68 82 0 0 0 0 4095 1 
i 4 105.48 0.72 73 97 0 0 0 0 4095 1 
i 1 105.84 0.36 71 91 0 0 0 0 4095 1 
i 3 105.84 0.72 61 83 0 0 0 0 4095 1 
i 4 105.84 0.36 76 83 0 0 0 0 4095 1 
i 1 106.2 0.36 64 88 0 0 0 0 4095 1 
i 2 106.2 0.36 56 88 0 0 0 0 4095 1 
i 2 106.2 0.36 59 93 0 0 0 0 4095 1 
i 2 106.2 0.36 61 90 0 0 0 0 4095 1 
i 2 106.2 0.36 64 97 0 0 0 0 4095 1 
i 2 106.56 0.36 63 83 0 0 0 0 4095 1 
i 3 106.56 0.36 66 83 0 0 0 0 4095 1 
i 4 106.56 0.36 61 84 0 0 0 0 4095 1 
i 4 106.56 0.72 66 84 0 0 0 0 4095 1 
i 4 106.56 0.72 68 88 0 0 0 0 4095 1 
i 1 106.92 0.72 66 98 0 0 0 0 4095 1 
i 2 106.92 0.72 68 82 0 0 0 0 4095 1 
i 3 106.92 0.36 68 93 0 0 0 0 4095 1 
i 4 106.92 0.72 64 92 0 0 0 0 4095 1 
i 2 107.28 0.36 61 83 0 0 0 0 4095 1 
i 2 107.28 0.36 62 83 0 0 0 0 4095 1 
i 4 107.28 0.36 61 89 0 0 0 0 4095 1 
i 1 107.64 0.36 64 90 0 0 0 0 4095 1 
i 2 107.64 0.36 56 90 0 0 0 0 4095 1 
i 3 107.64 0.36 66 90 0 0 0 0 4095 1 
i 3 107.64 0.36 68 90 0 0 0 0 4095 1 
i 4 107.64 0.36 62 88 0 0 0 0 4095 1 
i 4 107.64 0.36 66 95 0 0 0 0 4095 1 
i 1 108 0.36 61 94 0 0 0 0 4095 1 
i 1 108 0.36 66 91 0 0 0 0 4095 1 
i 1 108 0.72 69 93 0 0 0 0 4095 1 
i 4 108 0.36 61 90 0 0 0 0 4095 1 
i 4 108 1.08 68 95 0 0 0 0 4095 1 
i 1 108.36 0.36 74 87 0 0 0 0 4095 1 
i 2 108.36 0.36 66 97 0 0 0 0 4095 1 
i 3 108.36 0.36 68 95 0 0 0 0 4095 1 
i 4 108.72 0.36 71 95 0 0 0 0 4095 1 
i 4 108.72 0.36 73 86 0 0 0 0 4095 1 
i 4 108.72 0.36 78 85 0 0 0 0 4095 1 
i 4 108.72 0.36 83 85 0 0 0 0 4095 1 
i 1 109.08 0.72 66 84 0 0 0 0 4095 1 
i 1 109.08 0.72 68 86 0 0 0 0 4095 1 
i 2 109.08 0.36 71 91 0 0 0 0 4095 1 
i 3 109.08 1.08 66 88 0 0 0 0 4095 1 
i 2 109.44 0.36 66 89 0 0 0 0 4095 1 
i 4 109.44 0.36 88 94 0 0 0 0 4095 1 
i 1 109.8 0.36 71 90 0 0 0 0 4095 1 
i 1 109.8 0.72 73 93 0 0 0 0 4095 1 
i 1 109.8 0.36 76 89 0 0 0 0 4095 1 
i 3 109.8 0.36 64 89 0 0 0 0 4095 1 
i 4 109.8 0.36 78 88 0 0 0 0 4095 1 
i 1 110.16 0.72 78 91 0 0 0 0 4095 1 
i 2 110.16 0.36 61 90 0 0 0 0 4095 1 
i 2 110.16 0.36 64 94 0 0 0 0 4095 1 
i 2 110.16 0.36 66 89 0 0 0 0 4095 1 
i 3 110.16 0.36 59 92 0 0 0 0 4095 1 
i 1 110.52 0.72 83 97 0 0 0 0 4095 1 
i 4 110.52 0.36 73 83 0 0 0 0 4095 1 
i 4 110.52 0.36 74 84 0 0 0 0 4095 1 
i 4 110.52 0.36 78 94 0 0 0 0 4095 1 
i 1 110.88 0.36 67 87 0 0 0 0 4095 1 
i 1 110.88 0.36 71 84 0 0 0 0 4095 1 
i 2 110.88 0.36 71 96 0 0 0 0 4095 1 
i 4 110.88 0.36 67 87 0 0 0 0 4095 1 
i 1 111.24 0.36 68 96 0 0 0 0 4095 1 
i 2 111.24 0.36 66 85 0 0 0 0 4095 1 
i 3 111.24 0.36 64 85 0 0 0 0 4095 1 
i 3 111.24 0.36 69 84 0 0 0 0 4095 1 
i 4 111.24 0.36 59 92 0 0 0 0 4095 1 
i 4 111.24 0.36 61 91 0 0 0 0 4095 1 
i 1 111.6 0.36 64 98 0 0 0 0 4095 1 
i 2 111.6 0.36 61 97 0 0 0 0 4095 1 
i 2 111.6 0.36 62 85 0 0 0 0 4095 1 
i 2 111.6 0.36 64 89 0 0 0 0 4095 1 
i 2 111.6 0.36 67 84 0 0 0 0 4095 1 
i 3 111.6 0.36 66 88 0 0 0 0 4095 1 
i 1 111.96 0.36 68 95 0 0 0 0 4095 1 
i 1 111.96 0.36 73 86 0 0 0 0 4095 1 
i 4 111.96 0.36 56 96 0 0 0 0 4095 1 
i 4 111.96 0.36 57 91 0 0 0 0 4095 1 
i 4 111.96 0.72 61 86 0 0 0 0 4095 1 
i 4 111.96 0.72 62 89 0 0 0 0 4095 1 
i 1 112.32 0.36 76 87 0 0 0 0 4095 1 
i 1 112.32 1.08 78 87 0 0 0 0 4095 1 
i 3 112.32 0.36 56 89 0 0 0 0 4095 1 
i 3 112.32 0.36 62 93 0 0 0 0 4095 1 
i 1 112.68 0.36 73 80 0 0 0 0 4095 1 
i 2 112.68 0.36 64 98 0 0 0 0 4095 1 
i 3 112.68 0.36 57 86 0 0 0 0 4095 1 
i 4 112.68 0.36 64 81 0 0 0 0 4095 1 
i 1 113.04 0.72 57 84 0 0 0 0 4095 1 
i 1 113.04 0.72 59 99 0 0 0 0 4095 1 
i 1 113.04 0.36 62 96 0 0 0 0 4095 1 
i 2 113.04 0.36 66 87 0 0 0 0 4095 1 
i 1 113.4 0.72 55 85 0 0 0 0 4095 1 
i 2 113.4 0.72 61 80 0 0 0 0 4095 1 
i 3 113.4 2.16 61 80 0 0 0 0 4095 1 
i 4 113.4 1.8 61 82 0 0 0 0 4095 1 
i 1 113.76 1.44 61 87 0 0 0 0 4095 1 
i 2 114.48 0.72 61 84 0 0 0 0 4095 1 
i 1 115.2 0.72 64 88 0 0 0 0 4095 1 
i 1 115.2 0.36 69 93 0 0 0 0 4095 1 
i 3 115.2 0.36 64 83 0 0 0 0 4095 1 
i 4 115.2 0.72 66 84 0 0 0 0 4095 1 
i 1 115.56 0.36 54 83 0 0 0 0 4095 1 
i 1 115.56 0.36 59 93 0 0 0 0 4095 1 
i 2 115.56 0.36 64 82 0 0 0 0 4095 1 
i 3 115.56 0.36 62 83 0 0 0 0 4095 1 
i 2 115.92 0.36 68 93 0 0 0 0 4095 1 
i 2 115.92 0.36 73 88 0 0 0 0 4095 1 
i 3 115.92 0.36 64 85 0 0 0 0 4095 1 
i 3 115.92 0.36 66 88 0 0 0 0 4095 1 
i 4 115.92 0.36 71 86 0 0 0 0 4095 1 
i 4 115.92 0.72 76 83 0 0 0 0 4095 1 
i 1 116.28 0.36 54 88 0 0 0 0 4095 1 
i 1 116.28 0.72 56 94 0 0 0 0 4095 1 
i 3 116.28 0.36 71 87 0 0 0 0 4095 1 
i 4 116.28 0.36 80 93 0 0 0 0 4095 1 
i 1 116.64 0.36 49 83 0 0 0 0 4095 1 
i 2 116.64 0.36 71 83 0 0 0 0 4095 1 
i 2 116.64 0.36 74 86 0 0 0 0 4095 1 
i 2 116.64 0.72 76 84 0 0 0 0 4095 1 
i 4 116.64 0.36 83 89 0 0 0 0 4095 1 
i 2 117 0.72 73 93 0 0 0 0 4095 1 
i 3 117 0.36 76 90 0 0 0 0 4095 1 
i 4 117 0.36 76 86 0 0 0 0 4095 1 
i 4 117 0.36 80 99 0 0 0 0 4095 1 
i 4 117 0.36 85 92 0 0 0 0 4095 1 
i 1 117.36 0.36 61 85 0 0 0 0 4095 1 
i 2 117.36 0.36 71 90 0 0 0 0 4095 1 
i 3 117.36 0.36 68 86 0 0 0 0 4095 1 
i 3 117.36 1.08 71 85 0 0 0 0 4095 1 
i 4 117.36 0.36 73 93 0 0 0 0 4095 1 
i 1 117.72 0.36 64 86 0 0 0 0 4095 1 
i 1 117.72 0.72 66 90 0 0 0 0 4095 1 
i 2 117.72 0.36 74 88 0 0 0 0 4095 1 
i 3 117.72 0.36 67 87 0 0 0 0 4095 1 
i 4 117.72 0.36 71 92 0 0 0 0 4095 1 
i 1 118.08 0.36 59 90 0 0 0 0 4095 1 
i 1 118.08 0.72 61 92 0 0 0 0 4095 1 
i 1 118.08 0.36 69 84 0 0 0 0 4095 1 
i 1 118.44 0.72 64 97 0 0 0 0 4095 1 
i 1 118.44 0.36 67 92 0 0 0 0 4095 1 
i 1 118.44 0.36 71 95 0 0 0 0 4095 1 
i 4 118.44 0.36 69 96 0 0 0 0 4095 1 
i 4 118.44 0.36 73 84 0 0 0 0 4095 1 
i 1 118.8 0.36 59 89 0 0 0 0 4095 1 
i 1 118.8 0.36 66 84 0 0 0 0 4095 1 
i 1 118.8 0.36 69 84 0 0 0 0 4095 1 
i 4 118.8 0.36 74 83 0 0 0 0 4095 1 
i 4 118.8 0.36 76 87 0 0 0 0 4095 1 
i 1 119.16 0.36 68 82 0 0 0 0 4095 1 
i 2 119.16 0.36 78 82 0 0 0 0 4095 1 
i 2 119.16 0.72 83 83 0 0 0 0 4095 1 
i 3 119.16 0.72 71 81 0 0 0 0 4095 1 
i 3 119.16 0.36 76 81 0 0 0 0 4095 1 
i 4 119.16 0.36 73 89 0 0 0 0 4095 1 
i 1 119.52 0.36 71 83 0 0 0 0 4095 1 
i 2 119.52 0.36 81 95 0 0 0 0 4095 1 
i 4 119.52 0.36 78 86 0 0 0 0 4095 1 
i 1 119.88 0.72 73 84 0 0 0 0 4095 1 
i 1 119.88 0.36 76 89 0 0 0 0 4095 1 
i 2 119.88 0.36 86 90 0 0 0 0 4095 1 
i 2 119.88 0.36 90 86 0 0 0 0 4095 1 
i 3 119.88 0.36 67 86 0 0 0 0 4095 1 
i 3 120.24 0.72 69 81 0 0 0 0 4095 1 
i 4 120.24 0.36 80 99 0 0 0 0 4095 1 
i 4 120.24 0.72 83 83 0 0 0 0 4095 1 
i 1 120.6 0.36 76 94 0 0 0 0 4095 1 
i 1 120.6 0.36 78 98 0 0 0 0 4095 1 
i 1 120.6 0.36 83 89 0 0 0 0 4095 1 
i 3 120.6 0.36 73 87 0 0 0 0 4095 1 
i 1 120.96 0.36 80 88 0 0 0 0 4095 1 
i 1 120.96 0.36 81 89 0 0 0 0 4095 1 
i 3 120.96 0.36 64 90 0 0 0 0 4095 1 
i 4 120.96 0.36 85 98 0 0 0 0 4095 1 
i 1 121.32 0.36 71 92 0 0 0 0 4095 1 
i 1 121.32 0.36 76 86 0 0 0 0 4095 1 
i 2 121.32 0.36 90 83 0 0 0 0 4095 1 
i 4 121.32 0.36 74 86 0 0 0 0 4095 1 
i 4 121.32 0.36 80 85 0 0 0 0 4095 1 
i 1 121.68 0.36 73 83 0 0 0 0 4095 1 
i 1 121.68 0.36 74 84 0 0 0 0 4095 1 
i 2 121.68 0.36 95 84 0 0 0 0 4095 1 
i 3 121.68 0.36 66 83 0 0 0 0 4095 1 
i 4 121.68 0.36 68 86 0 0 0 0 4095 1 
i 1 122.04 0.36 78 87 0 0 0 0 4095 1 
i 1 122.04 0.72 80 88 0 0 0 0 4095 1 
i 1 122.04 1.08 83 87 0 0 0 0 4095 1 
i 3 122.04 0.36 68 89 0 0 0 0 4095 1 
i 4 122.04 0.36 66 90 0 0 0 0 4095 1 
i 1 122.4 0.36 85 86 0 0 0 0 4095 1 
i 3 122.4 1.08 66 85 0 0 0 0 4095 1 
i 3 122.4 0.36 69 88 0 0 0 0 4095 1 
i 4 122.4 0.36 68 86 0 0 0 0 4095 1 
i 2 122.76 0.36 92 89 0 0 0 0 4095 1 
i 3 122.76 0.36 64 91 0 0 0 0 4095 1 
i 3 122.76 0.36 68 84 0 0 0 0 4095 1 
i 1 123.12 0.36 80 84 0 0 0 0 4095 1 
i 1 123.12 0.36 85 85 0 0 0 0 4095 1 
i 1 123.12 0.72 86 84 0 0 0 0 4095 1 
i 4 123.12 0.36 66 93 0 0 0 0 4095 1 
i 1 123.48 0.36 76 92 0 0 0 0 4095 1 
i 2 123.48 0.36 97 84 0 0 0 0 4095 1 
i 3 123.48 0.36 68 86 0 0 0 0 4095 1 
i 4 123.48 0.72 71 85 0 0 0 0 4095 1 
i 2 123.84 0.72 78 92 0 0 0 0 4095 1 
i 2 123.84 0.36 80 84 0 0 0 0 4095 1 
i 2 123.84 0.72 86 86 0 0 0 0 4095 1 
i 3 123.84 0.36 71 85 0 0 0 0 4095 1 
i 3 123.84 0.36 74 88 0 0 0 0 4095 1 
i 2 124.2 0.36 73 81 0 0 0 0 4095 1 
i 3 124.2 0.36 68 83 0 0 0 0 4095 1 
i 4 124.2 0.36 66 83 0 0 0 0 4095 1 
i 4 124.2 0.36 68 82 0 0 0 0 4095 1 
i 1 124.56 0.36 71 87 0 0 0 0 4095 1 
i 3 124.56 0.72 61 82 0 0 0 0 4095 1 
i 3 124.56 0.36 66 93 0 0 0 0 4095 1 
i 3 124.56 0.36 71 84 0 0 0 0 4095 1 
i 4 124.56 0.36 69 82 0 0 0 0 4095 1 
i 4 124.56 0.72 71 86 0 0 0 0 4095 1 
i 2 124.92 1.08 81 86 0 0 0 0 4095 1 
i 2 124.92 0.36 83 87 0 0 0 0 4095 1 
i 4 124.92 0.36 66 98 0 0 0 0 4095 1 
i 1 125.28 0.36 69 89 0 0 0 0 4095 1 
i 3 125.64 0.36 61 88 0 0 0 0 4095 1 
i 4 125.64 1.44 66 91 0 0 0 0 4095 1 
i 1 126 0.72 69 84 0 0 0 0 4095 1 
i 2 126 0.72 69 89 0 0 0 0 4095 1 
i 1 126.72 0.72 68 84 0 0 0 0 4095 1 
i 3 126.72 0.36 57 81 0 0 0 0 4095 1 
i 4 126.72 0.36 61 82 0 0 0 0 4095 1 
i 4 126.72 0.36 62 93 0 0 0 0 4095 1 
i 4 126.72 0.36 71 83 0 0 0 0 4095 1 
i 1 127.08 0.36 73 83 0 0 0 0 4095 1 
i 2 127.08 0.72 62 83 0 0 0 0 4095 1 
i 2 127.08 0.36 64 86 0 0 0 0 4095 1 
i 3 127.08 0.36 66 86 0 0 0 0 4095 1 
i 4 127.08 0.36 76 85 0 0 0 0 4095 1 
i 1 127.44 0.36 64 85 0 0 0 0 4095 1 
i 2 127.44 0.36 59 93 0 0 0 0 4095 1 
i 2 127.44 1.08 61 86 0 0 0 0 4095 1 
i 3 127.44 0.36 61 86 0 0 0 0 4095 1 
i 4 127.44 0.72 78 85 0 0 0 0 4095 1 
i 1 127.8 0.36 61 84 0 0 0 0 4095 1 
i 1 127.8 0.36 66 85 0 0 0 0 4095 1 
i 3 127.8 0.72 59 87 0 0 0 0 4095 1 
i 4 127.8 0.36 74 82 0 0 0 0 4095 1 
i 1 128.16 1.08 64 90 0 0 0 0 4095 1 
i 3 128.16 0.36 54 83 0 0 0 0 4095 1 
i 3 128.16 0.36 57 82 0 0 0 0 4095 1 
i 3 128.16 0.36 61 87 0 0 0 0 4095 1 
i 1 128.52 0.36 62 84 0 0 0 0 4095 1 
i 2 128.52 0.72 57 80 0 0 0 0 4095 1 
i 4 128.52 0.36 61 85 0 0 0 0 4095 1 
i 4 128.52 0.36 66 81 0 0 0 0 4095 1 
i 1 128.88 0.36 66 83 0 0 0 0 4095 1 
i 2 128.88 0.36 64 90 0 0 0 0 4095 1 
i 3 128.88 0.36 61 84 0 0 0 0 4095 1 
i 4 128.88 0.36 62 91 0 0 0 0 4095 1 
i 1 129.24 0.36 61 85 0 0 0 0 4095 1 
i 2 129.24 0.36 61 86 0 0 0 0 4095 1 
i 2 129.24 0.36 66 85 0 0 0 0 4095 1 
i 3 129.24 0.36 62 88 0 0 0 0 4095 1 
i 3 129.24 0.36 64 85 0 0 0 0 4095 1 
i 4 129.24 0.36 66 96 0 0 0 0 4095 1 
i 2 129.6 0.36 62 88 0 0 0 0 4095 1 
i 3 129.6 1.44 61 86 0 0 0 0 4095 1 
i 1 129.96 1.8 61 90 0 0 0 0 4095 1 
i 2 129.96 0.36 61 83 0 0 0 0 4095 1 
i 3 129.96 0.36 66 87 0 0 0 0 4095 1 
i 4 129.96 0.36 62 83 0 0 0 0 4095 1 
i 1 130.32 1.44 66 88 0 0 0 0 4095 1 
i 1 130.32 0.36 67 82 0 0 0 0 4095 1 
i 2 130.68 0.36 59 82 0 0 0 0 4095 1 
i 2 130.68 0.72 61 84 0 0 0 0 4095 1 
i 1 131.04 0.36 57 87 0 0 0 0 4095 1 
i 2 131.04 0.36 54 82 0 0 0 0 4095 1 
i 2 131.4 0.36 59 82 0 0 0 0 4095 1 
i 3 131.4 0.36 55 84 0 0 0 0 4095 1 
i 4 131.4 0.72 62 84 0 0 0 0 4095 1 
i 2 131.76 0.36 64 92 0 0 0 0 4095 1 
i 3 131.76 0.36 49 82 0 0 0 0 4095 1 
i 4 131.76 0.36 57 85 0 0 0 0 4095 1 
i 4 131.76 0.36 61 85 0 0 0 0 4095 1 
i 2 132.12 1.08 62 89 0 0 0 0 4095 1 
i 3 132.12 0.36 61 88 0 0 0 0 4095 1 
i 1 132.48 1.08 56 84 0 0 0 0 4095 1 
i 2 132.48 0.36 56 84 0 0 0 0 4095 1 
i 3 132.48 0.36 66 81 0 0 0 0 4095 1 
i 4 132.48 0.36 52 82 0 0 0 0 4095 1 
i 4 132.48 0.36 54 82 0 0 0 0 4095 1 
i 1 132.84 0.36 52 89 0 0 0 0 4095 1 
i 2 132.84 0.72 66 86 0 0 0 0 4095 1 
i 3 132.84 0.36 64 90 0 0 0 0 4095 1 
i 1 133.2 0.36 66 88 0 0 0 0 4095 1 
i 2 133.2 0.72 64 93 0 0 0 0 4095 1 
i 4 133.2 0.36 59 91 0 0 0 0 4095 1 
i 3 133.56 0.36 57 88 0 0 0 0 4095 1 
i 3 133.56 0.36 59 96 0 0 0 0 4095 1 
i 4 133.56 0.36 54 93 0 0 0 0 4095 1 
i 4 133.56 0.36 62 85 0 0 0 0 4095 1 
i 1 133.92 0.36 64 91 0 0 0 0 4095 1 
i 1 133.92 0.36 66 92 0 0 0 0 4095 1 
i 2 133.92 0.36 61 89 0 0 0 0 4095 1 
i 3 133.92 0.36 61 89 0 0 0 0 4095 1 
i 4 133.92 0.72 61 90 0 0 0 0 4095 1 
i 2 134.28 0.72 59 95 0 0 0 0 4095 1 
i 3 134.28 0.72 51 92 0 0 0 0 4095 1 
i 3 134.28 0.36 56 81 0 0 0 0 4095 1 
i 4 134.28 0.36 57 81 0 0 0 0 4095 1 
i 3 134.64 0.72 54 83 0 0 0 0 4095 1 
i 3 134.64 0.36 61 86 0 0 0 0 4095 1 
i 4 134.64 0.36 63 93 0 0 0 0 4095 1 
i 4 134.64 0.36 64 89 0 0 0 0 4095 1 
i 2 135 0.36 63 97 0 0 0 0 4095 1 
i 3 135 0.36 66 89 0 0 0 0 4095 1 
i 3 135 1.08 68 99 0 0 0 0 4095 1 
i 3 135 0.36 71 88 0 0 0 0 4095 1 
i 4 135 0.36 66 89 0 0 0 0 4095 1 
i 1 135.36 0.72 61 89 0 0 0 0 4095 1 
i 2 135.36 0.36 59 83 0 0 0 0 4095 1 
i 2 135.36 0.36 61 86 0 0 0 0 4095 1 
i 4 135.36 0.36 68 85 0 0 0 0 4095 1 
i 4 135.36 0.36 71 86 0 0 0 0 4095 1 
i 1 135.72 2.16 69 91 0 0 0 0 4095 1 
i 3 135.72 0.36 71 88 0 0 0 0 4095 1 
i 4 135.72 0.36 64 95 0 0 0 0 4095 1 
i 1 136.08 0.36 64 90 0 0 0 0 4095 1 
i 3 136.08 0.72 52 86 0 0 0 0 4095 1 
i 3 136.08 0.36 56 89 0 0 0 0 4095 1 
i 3 136.08 0.36 61 87 0 0 0 0 4095 1 
i 4 136.08 0.36 61 90 0 0 0 0 4095 1 
i 1 136.44 0.36 68 90 0 0 0 0 4095 1 
i 2 136.44 0.36 64 91 0 0 0 0 4095 1 
i 2 136.44 1.08 69 84 0 0 0 0 4095 1 
i 2 136.44 0.36 71 97 0 0 0 0 4095 1 
i 3 136.44 0.36 47 87 0 0 0 0 4095 1 
i 3 136.8 0.36 45 85 0 0 0 0 4095 1 
i 4 136.8 1.44 61 91 0 0 0 0 4095 1 
i 3 137.52 0.72 45 83 0 0 0 0 4095 1 
i 1 137.88 0.72 68 95 0 0 0 0 4095 1 
i 3 137.88 0.36 44 93 0 0 0 0 4095 1 
i 1 138.24 0.36 64 86 0 0 0 0 4095 1 
i 2 138.24 0.36 68 93 0 0 0 0 4095 1 
i 4 138.24 0.72 57 96 0 0 0 0 4095 1 
i 2 138.6 0.36 66 85 0 0 0 0 4095 1 
i 2 138.6 0.36 69 84 0 0 0 0 4095 1 
i 3 138.6 0.36 47 87 0 0 0 0 4095 1 
i 4 138.6 0.72 56 83 0 0 0 0 4095 1 
i 1 138.96 0.36 68 88 0 0 0 0 4095 1 
i 1 138.96 0.36 71 86 0 0 0 0 4095 1 
i 1 138.96 0.36 73 90 0 0 0 0 4095 1 
i 2 138.96 0.36 68 93 0 0 0 0 4095 1 
i 2 139.32 0.72 66 89 0 0 0 0 4095 1 
i 2 139.32 0.36 71 88 0 0 0 0 4095 1 
i 3 139.32 0.36 47 100 0 0 0 0 4095 1 
i 3 139.32 0.36 64 87 0 0 0 0 4095 1 
i 4 139.32 0.36 54 88 0 0 0 0 4095 1 
i 3 139.68 0.72 69 99 0 0 0 0 4095 1 
i 3 139.68 0.36 73 85 0 0 0 0 4095 1 
i 4 139.68 0.36 59 87 0 0 0 0 4095 1 
i 4 139.68 0.36 61 93 0 0 0 0 4095 1 
i 3 140.04 0.36 71 92 0 0 0 0 4095 1 
i 3 140.04 0.36 76 93 0 0 0 0 4095 1 
i 4 140.04 0.36 56 99 0 0 0 0 4095 1 
i 4 140.04 0.36 69 99 0 0 0 0 4095 1 
i 1 140.4 0.36 66 91 0 0 0 0 4095 1 
i 1 140.4 0.36 68 90 0 0 0 0 4095 1 
i 1 140.4 0.36 69 93 0 0 0 0 4095 1 
i 2 140.4 0.36 71 90 0 0 0 0 4095 1 
i 3 140.4 0.72 66 89 0 0 0 0 4095 1 
i 2 140.76 0.36 61 100 0 0 0 0 4095 1 
i 2 140.76 1.08 64 88 0 0 0 0 4095 1 
i 2 140.76 0.36 66 96 0 0 0 0 4095 1 
i 2 140.76 0.36 68 87 0 0 0 0 4095 1 
i 3 140.76 0.72 68 92 0 0 0 0 4095 1 
i 1 141.12 0.36 68 91 0 0 0 0 4095 1 
i 4 141.12 0.36 59 84 0 0 0 0 4095 1 
i 4 141.12 0.72 61 89 0 0 0 0 4095 1 
i 4 141.12 0.36 62 90 0 0 0 0 4095 1 
i 3 141.48 0.36 62 89 0 0 0 0 4095 1 
i 4 141.48 0.36 52 87 0 0 0 0 4095 1 
i 4 141.48 0.36 56 83 0 0 0 0 4095 1 
i 1 141.84 0.72 73 88 0 0 0 0 4095 1 
i 3 141.84 0.36 66 96 0 0 0 0 4095 1 
i 3 141.84 0.36 68 89 0 0 0 0 4095 1 
i 3 141.84 0.36 71 86 0 0 0 0 4095 1 
i 3 141.84 0.36 76 86 0 0 0 0 4095 1 
i 1 142.2 0.36 69 85 0 0 0 0 4095 1 
i 1 142.2 0.36 74 90 0 0 0 0 4095 1 
i 2 142.2 0.72 66 84 0 0 0 0 4095 1 
i 3 142.2 0.36 73 85 0 0 0 0 4095 1 
i 1 142.56 0.36 76 85 0 0 0 0 4095 1 
i 2 142.56 0.36 64 100 0 0 0 0 4095 1 
i 3 142.56 0.36 64 85 0 0 0 0 4095 1 
i 3 142.56 0.36 69 94 0 0 0 0 4095 1 
i 4 142.56 0.36 66 89 0 0 0 0 4095 1 
i 1 142.92 0.72 62 84 0 0 0 0 4095 1 
i 1 142.92 0.72 66 88 0 0 0 0 4095 1 
i 1 142.92 0.36 67 93 0 0 0 0 4095 1 
i 3 142.92 0.36 66 82 0 0 0 0 4095 1 
i 4 142.92 0.36 71 91 0 0 0 0 4095 1 
i 1 143.28 0.36 55 91 0 0 0 0 4095 1 
i 1 143.28 0.36 57 95 0 0 0 0 4095 1 
i 1 143.28 0.36 59 94 0 0 0 0 4095 1 
i 3 143.28 0.36 62 83 0 0 0 0 4095 1 
i 1 143.64 0.36 61 85 0 0 0 0 4095 1 
i 2 143.64 0.36 66 83 0 0 0 0 4095 1 
i 3 143.64 0.72 66 91 0 0 0 0 4095 1 
i 4 143.64 0.72 68 91 0 0 0 0 4095 1 
i 1 144 0.36 66 89 0 0 0 0 4095 1 
i 2 144 0.36 64 91 0 0 0 0 4095 1 
i 4 144 0.36 64 90 0 0 0 0 4095 1 
i 2 144.36 0.36 68 88 0 0 0 0 4095 1 
i 2 144.36 0.36 71 84 0 0 0 0 4095 1 
i 3 144.36 0.36 51 94 0 0 0 0 4095 1 
i 3 144.36 0.36 56 95 0 0 0 0 4095 1 
i 3 144.36 0.36 61 88 0 0 0 0 4095 1 
i 3 144.36 0.36 68 84 0 0 0 0 4095 1 
i 2 144.72 0.36 66 84 0 0 0 0 4095 1 
i 3 144.72 0.36 69 86 0 0 0 0 4095 1 
i 3 144.72 0.36 71 83 0 0 0 0 4095 1 
i 4 144.72 0.72 57 86 0 0 0 0 4095 1 
i 4 144.72 0.36 61 85 0 0 0 0 4095 1 
i 4 144.72 0.36 66 95 0 0 0 0 4095 1 
i 1 145.08 0.36 61 100 0 0 0 0 4095 1 
i 3 145.08 0.36 68 87 0 0 0 0 4095 1 
i 4 145.08 0.36 69 87 0 0 0 0 4095 1 
i 4 145.08 0.36 71 97 0 0 0 0 4095 1 
i 3 145.44 0.36 63 86 0 0 0 0 4095 1 
i 3 145.44 0.36 64 84 0 0 0 0 4095 1 
i 4 145.44 0.36 59 87 0 0 0 0 4095 1 
i 4 145.44 0.36 61 81 0 0 0 0 4095 1 
i 4 145.44 0.36 66 83 0 0 0 0 4095 1 
i 4 145.44 0.36 68 83 0 0 0 0 4095 1 
i 1 145.8 0.72 59 92 0 0 0 0 4095 1 
i 1 145.8 0.36 61 89 0 0 0 0 4095 1 
i 2 145.8 0.36 61 92 0 0 0 0 4095 1 
i 2 145.8 0.36 66 92 0 0 0 0 4095 1 
i 3 145.8 0.72 61 88 0 0 0 0 4095 1 
i 4 145.8 0.36 62 88 0 0 0 0 4095 1 
i 1 146.16 0.36 56 95 0 0 0 0 4095 1 
i 1 146.16 0.72 66 89 0 0 0 0 4095 1 
i 4 146.16 1.44 61 95 0 0 0 0 4095 1 
i 4 146.16 0.72 64 84 0 0 0 0 4095 1 
i 1 146.52 0.36 61 84 0 0 0 0 4095 1 
i 1 146.52 0.72 71 85 0 0 0 0 4095 1 
i 2 146.52 0.36 66 86 0 0 0 0 4095 1 
i 1 146.88 0.36 68 80 0 0 0 0 4095 1 
i 3 146.88 0.36 63 90 0 0 0 0 4095 1 
i 4 146.88 0.72 66 84 0 0 0 0 4095 1 
i 1 147.24 0.36 64 82 0 0 0 0 4095 1 
i 3 147.24 0.36 61 89 0 0 0 0 4095 1 
i 3 147.24 0.36 66 87 0 0 0 0 4095 1 
i 3 147.6 0.36 63 85 0 0 0 0 4095 1 
i 3 147.6 0.72 68 83 0 0 0 0 4095 1 
i 3 147.6 0.36 71 82 0 0 0 0 4095 1 
i 4 147.6 0.36 56 88 0 0 0 0 4095 1 
i 4 147.6 0.36 63 83 0 0 0 0 4095 1 
i 1 147.96 0.36 69 89 0 0 0 0 4095 1 
i 3 147.96 0.36 66 87 0 0 0 0 4095 1 
i 3 147.96 0.36 73 88 0 0 0 0 4095 1 
i 3 147.96 0.72 78 98 0 0 0 0 4095 1 
i 4 147.96 0.72 61 86 0 0 0 0 4095 1 
i 2 148.32 0.36 66 95 0 0 0 0 4095 1 
i 1 148.68 1.08 69 87 0 0 0 0 4095 1 
i 3 148.68 0.36 69 82 0 0 0 0 4095 1 
i 2 149.04 0.36 66 86 0 0 0 0 4095 1 
i 4 149.04 1.44 61 90 0 0 0 0 4095 1 
i 1 149.76 0.72 64 95 0 0 0 0 4095 1 
i 2 149.76 0.36 66 91 0 0 0 0 4095 1 
i 3 149.76 0.36 68 90 0 0 0 0 4095 1 
i 3 149.76 0.36 73 93 0 0 0 0 4095 1 
i 4 149.76 0.36 68 87 0 0 0 0 4095 1 
i 1 150.12 0.36 57 85 0 0 0 0 4095 1 
i 1 150.12 0.36 61 85 0 0 0 0 4095 1 
i 4 150.12 0.72 62 83 0 0 0 0 4095 1 
i 1 150.48 1.08 66 87 0 0 0 0 4095 1 
i 2 150.48 0.36 66 87 0 0 0 0 4095 1 
i 2 150.48 0.36 69 87 0 0 0 0 4095 1 
i 4 150.48 0.36 64 95 0 0 0 0 4095 1 
i 2 150.84 0.36 61 95 0 0 0 0 4095 1 
i 3 150.84 0.36 68 90 0 0 0 0 4095 1 
i 3 150.84 0.36 74 88 0 0 0 0 4095 1 
i 4 150.84 0.36 61 96 0 0 0 0 4095 1 
i 4 150.84 0.36 66 92 0 0 0 0 4095 1 
i 1 151.2 0.36 69 90 0 0 0 0 4095 1 
i 2 151.2 0.36 57 90 0 0 0 0 4095 1 
i 3 151.2 0.72 71 90 0 0 0 0 4095 1 
i 3 151.2 0.36 73 89 0 0 0 0 4095 1 
i 1 151.56 0.36 71 94 0 0 0 0 4095 1 
i 1 151.56 0.36 73 93 0 0 0 0 4095 1 
i 2 151.56 0.36 50 88 0 0 0 0 4095 1 
i 2 151.56 0.36 54 97 0 0 0 0 4095 1 
i 2 151.56 0.36 56 93 0 0 0 0 4095 1 
i 1 151.92 0.36 68 86 0 0 0 0 4095 1 
i 3 151.92 0.36 61 84 0 0 0 0 4095 1 
i 3 151.92 0.36 66 85 0 0 0 0 4095 1 
i 4 151.92 0.72 66 91 0 0 0 0 4095 1 
i 4 151.92 0.36 71 97 0 0 0 0 4095 1 
i 1 152.28 0.72 73 86 0 0 0 0 4095 1 
i 2 152.28 0.36 50 87 0 0 0 0 4095 1 
i 3 152.28 0.36 64 88 0 0 0 0 4095 1 
i 3 152.28 0.36 68 94 0 0 0 0 4095 1 
i 4 152.28 0.36 69 88 0 0 0 0 4095 1 
i 1 152.64 0.36 68 87 0 0 0 0 4095 1 
i 1 152.64 0.72 71 91 0 0 0 0 4095 1 
i 3 152.64 0.36 66 89 0 0 0 0 4095 1 
i 4 152.64 0.36 64 87 0 0 0 0 4095 1 
i 4 152.64 0.72 68 89 0 0 0 0 4095 1 
i 1 153 0.36 66 99 0 0 0 0 4095 1 
i 2 153 0.36 45 90 0 0 0 0 4095 1 
i 2 153 0.72 52 91 0 0 0 0 4095 1 
i 3 153 0.36 69 93 0 0 0 0 4095 1 
i 2 153.36 0.36 49 91 0 0 0 0 4095 1 
i 3 153.36 0.36 62 86 0 0 0 0 4095 1 
i 3 153.36 0.36 66 100 0 0 0 0 4095 1 
i 3 153.36 0.72 68 86 0 0 0 0 4095 1 
i 4 153.36 0.36 73 87 0 0 0 0 4095 1 
i 1 153.72 0.72 66 94 0 0 0 0 4095 1 
i 1 153.72 0.36 69 91 0 0 0 0 4095 1 
i 2 153.72 0.36 56 94 0 0 0 0 4095 1 
i 3 153.72 0.36 64 91 0 0 0 0 4095 1 
i 4 153.72 0.36 78 92 0 0 0 0 4095 1 
i 3 154.08 0.36 66 97 0 0 0 0 4095 1 
i 4 154.08 0.36 76 84 0 0 0 0 4095 1 
i 4 154.08 0.36 81 95 0 0 0 0 4095 1 
i 1 154.44 0.72 71 88 0 0 0 0 4095 1 
i 2 154.44 0.36 56 89 0 0 0 0 4095 1 
i 3 154.44 0.36 71 90 0 0 0 0 4095 1 
i 3 154.44 0.36 74 92 0 0 0 0 4095 1 
i 4 154.44 0.36 80 98 0 0 0 0 4095 1 
i 4 154.44 0.36 85 91 0 0 0 0 4095 1 
i 2 154.8 0.36 59 98 0 0 0 0 4095 1 
i 3 154.8 0.36 66 94 0 0 0 0 4095 1 
i 3 154.8 0.36 69 91 0 0 0 0 4095 1 
i 4 154.8 0.36 90 100 0 0 0 0 4095 1 
i 4 154.8 0.72 95 99 0 0 0 0 4095 1 
i 2 155.16 0.36 64 86 0 0 0 0 4095 1 
i 2 155.16 0.36 66 91 0 0 0 0 4095 1 
i 2 155.16 0.36 68 85 0 0 0 0 4095 1 
i 3 155.16 0.36 68 92 0 0 0 0 4095 1 
i 4 155.16 0.36 100 87 0 0 0 0 4095 1 
i 1 155.52 0.36 73 99 0 0 0 0 4095 1 
i 1 155.52 0.36 76 94 0 0 0 0 4095 1 
i 2 155.52 0.36 73 90 0 0 0 0 4095 1 
i 2 155.52 0.72 76 89 0 0 0 0 4095 1 
i 3 155.52 0.36 66 90 0 0 0 0 4095 1 
i 4 155.52 0.36 104 90 0 0 0 0 4095 1 
i 1 155.88 0.72 66 84 0 0 0 0 4095 1 
i 1 155.88 0.36 68 83 0 0 0 0 4095 1 
i 1 155.88 0.72 69 85 0 0 0 0 4095 1 
i 2 155.88 0.36 78 85 0 0 0 0 4095 1 
i 3 156.24 0.72 62 86 0 0 0 0 4095 1 
i 4 156.24 0.36 80 88 0 0 0 0 4095 1 
i 4 156.24 0.36 85 88 0 0 0 0 4095 1 
i 1 156.6 0.36 56 85 0 0 0 0 4095 1 
i 1 156.6 0.36 61 83 0 0 0 0 4095 1 
i 1 156.6 0.36 62 86 0 0 0 0 4095 1 
i 1 156.6 0.36 68 88 0 0 0 0 4095 1 
i 4 156.6 0.36 78 83 0 0 0 0 4095 1 
i 4 156.6 0.72 81 84 0 0 0 0 4095 1 
i 2 156.96 1.08 83 84 0 0 0 0 4095 1 
i 2 156.96 0.36 86 88 0 0 0 0 4095 1 
i 4 156.96 0.36 76 85 0 0 0 0 4095 1 
i 4 156.96 0.36 80 88 0 0 0 0 4095 1 
i 2 157.32 0.36 85 88 0 0 0 0 4095 1 
i 2 157.32 0.36 88 82 0 0 0 0 4095 1 
i 3 157.32 0.36 64 90 0 0 0 0 4095 1 
i 4 157.32 0.36 68 89 0 0 0 0 4095 1 
i 4 157.32 0.36 71 87 0 0 0 0 4095 1 
i 1 157.68 0.36 73 81 0 0 0 0 4095 1 
i 1 157.68 0.36 74 87 0 0 0 0 4095 1 
i 4 157.68 0.36 66 84 0 0 0 0 4095 1 
i 1 158.04 0.36 68 84 0 0 0 0 4095 1 
i 2 158.04 0.36 85 85 0 0 0 0 4095 1 
i 3 158.04 0.36 66 88 0 0 0 0 4095 1 
i 4 158.04 0.36 59 91 0 0 0 0 4095 1 
i 4 158.04 0.72 64 86 0 0 0 0 4095 1 
i 4 158.04 0.36 71 83 0 0 0 0 4095 1 
i 2 158.4 0.36 90 82 0 0 0 0 4095 1 
i 2 158.4 0.36 92 84 0 0 0 0 4095 1 
i 3 158.4 0.72 61 82 0 0 0 0 4095 1 
i 4 158.4 0.36 68 85 0 0 0 0 4095 1 
i 4 158.4 0.36 69 87 0 0 0 0 4095 1 
i 1 158.76 0.36 64 85 0 0 0 0 4095 1 
i 1 158.76 0.36 66 86 0 0 0 0 4095 1 
i 1 158.76 0.36 69 88 0 0 0 0 4095 1 
i 3 158.76 0.72 62 86 0 0 0 0 4095 1 
i 4 158.76 0.72 66 83 0 0 0 0 4095 1 
i 2 159.12 0.36 92 82 0 0 0 0 4095 1 
i 2 159.12 0.36 95 85 0 0 0 0 4095 1 
i 3 159.12 0.36 64 85 0 0 0 0 4095 1 
i 4 159.12 0.36 68 82 0 0 0 0 4095 1 
i 1 159.48 0.36 74 92 0 0 0 0 4095 1 
i 1 159.48 0.36 76 85 0 0 0 0 4095 1 
i 1 159.48 0.36 78 86 0 0 0 0 4095 1 
i 1 159.48 0.36 83 83 0 0 0 0 4095 1 
i 4 159.48 1.44 73 86 0 0 0 0 4095 1 
i 2 159.84 0.72 81 91 0 0 0 0 4095 1 
i 3 159.84 0.36 64 88 0 0 0 0 4095 1 
i 1 160.2 1.08 81 89 0 0 0 0 4095 1 
i 3 160.56 1.08 64 89 0 0 0 0 4095 1 
i 2 160.92 0.36 81 87 0 0 0 0 4095 1 
i 1 161.28 0.36 86 93 0 0 0 0 4095 1 
i 1 161.28 0.36 90 82 0 0 0 0 4095 1 
i 3 161.28 0.36 69 95 0 0 0 0 4095 1 
i 4 161.28 0.36 62 92 0 0 0 0 4095 1 
i 4 161.28 0.36 68 90 0 0 0 0 4095 1 
i 1 161.64 0.36 81 81 0 0 0 0 4095 1 
i 1 161.64 0.36 85 94 0 0 0 0 4095 1 
i 2 161.64 0.72 86 83 0 0 0 0 4095 1 
i 3 161.64 0.36 71 86 0 0 0 0 4095 1 
i 4 161.64 0.36 61 96 0 0 0 0 4095 1 
i 1 162 0.36 86 94 0 0 0 0 4095 1 
i 1 162 0.36 88 96 0 0 0 0 4095 1 
i 1 162 0.36 91 94 0 0 0 0 4095 1 
i 2 162 0.36 90 89 0 0 0 0 4095 1 
i 2 162 0.36 93 85 0 0 0 0 4095 1 
i 1 162.36 0.36 92 87 0 0 0 0 4095 1 
i 3 162.36 0.36 66 82 0 0 0 0 4095 1 
i 3 162.36 0.36 68 89 0 0 0 0 4095 1 
i 4 162.36 0.36 61 85 0 0 0 0 4095 1 
i 4 162.36 0.72 64 86 0 0 0 0 4095 1 
i 4 162.36 0.36 66 83 0 0 0 0 4095 1 
i 1 162.72 0.36 95 83 0 0 0 0 4095 1 
i 2 162.72 0.36 88 83 0 0 0 0 4095 1 
i 2 162.72 0.36 92 84 0 0 0 0 4095 1 
i 3 162.72 0.36 69 85 0 0 0 0 4095 1 
i 4 162.72 0.36 71 86 0 0 0 0 4095 1 
i 1 163.08 0.36 83 96 0 0 0 0 4095 1 
i 1 163.08 0.36 85 84 0 0 0 0 4095 1 
i 1 163.08 0.36 88 88 0 0 0 0 4095 1 
i 2 163.08 0.36 91 87 0 0 0 0 4095 1 
i 3 163.08 0.36 71 84 0 0 0 0 4095 1 
i 4 163.08 0.36 73 92 0 0 0 0 4095 1 
i 1 163.44 0.36 86 82 0 0 0 0 4095 1 
i 3 163.44 0.36 76 82 0 0 0 0 4095 1 
i 4 163.44 0.36 74 86 0 0 0 0 4095 1 
i 4 163.44 0.36 78 81 0 0 0 0 4095 1 
i 1 163.8 0.36 79 81 0 0 0 0 4095 1 
i 1 163.8 0.36 81 87 0 0 0 0 4095 1 
i 2 163.8 0.36 83 85 0 0 0 0 4095 1 
i 2 163.8 0.36 85 97 0 0 0 0 4095 1 
i 1 164.16 0.36 83 83 0 0 0 0 4095 1 
i 2 164.16 0.36 88 87 0 0 0 0 4095 1 
i 2 164.16 0.36 91 99 0 0 0 0 4095 1 
i 2 164.16 0.36 93 81 0 0 0 0 4095 1 
i 3 164.16 0.36 78 81 0 0 0 0 4095 1 
i 1 164.88 0.72 81 81 0 0 0 0 4095 1 
i 3 164.88 0.36 81 87 0 0 0 0 4095 1 
i 4 164.88 0.36 78 90 0 0 0 0 4095 1 
i 1 165.24 0.36 83 81 0 0 0 0 4095 1 
i 2 165.24 0.36 93 83 0 0 0 0 4095 1 
i 1 165.6 0.36 85 83 0 0 0 0 4095 1 
i 3 165.6 0.36 78 85 0 0 0 0 4095 1 
i 3 165.6 0.36 83 81 0 0 0 0 4095 1 
i 4 165.6 0.72 78 85 0 0 0 0 4095 1 
i 1 166.32 0.72 90 83 0 0 0 0 4095 1 
i 3 166.32 0.36 81 87 0 0 0 0 4095 1 
i 1 166.68 0.36 93 81 0 0 0 0 4095 1 
i 4 166.68 0.72 78 86 0 0 0 0 4095 1 

</CsScore>
        </CsoundSynthesizer>
    