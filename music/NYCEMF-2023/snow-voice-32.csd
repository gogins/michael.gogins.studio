<CsoundSyntheizer>
<CsLicense>

snow-voice

Michael Gogins, 2023

</CsLicense>
<CsOptions>
--m-amps=1 --m-range=1 --m-dB=1 --m-benchmarks=1 --m-warnings=0 -+msg_color=0 -d -odac 
</CsOptions>
<CsInstruments>

//////////////////////////////////////////////////////////////////////////////
// Change to sr=96000 with ksmps=1 for final rendering to soundfile.
//////////////////////////////////////////////////////////////////////////////

sr = 48000
ksmps = 128
nchnls = 2
0dbfs = 100

//////////////////////////////////////////////////////////////////////////////
// This random seed ensures that the same random stream  is used for each 
// rendering. Note that rand, randh, randi, rnd(x) and birnd(x) are not 
// affected by seed.
//////////////////////////////////////////////////////////////////////////////

seed 88818145

//////////////////////////////////////////////////////////////////////////////
// Turn printing of VST plugin parameters off and on globally.
//////////////////////////////////////////////////////////////////////////////

gi_vstinfo init 0

//////////////////////////////////////////////////////////////////////////////
// We will load plugins from different locations on different operating 
// systems.
//////////////////////////////////////////////////////////////////////////////

gS_os, gS_macros cxx_os
prints "Operating system: %s\n", gS_os

gi_base init 0

opcode instrument_position, kk, iii
i_onset, i_radius, i_rate xin
k_pan = (p1 / 10) + (1 / 10)
if floor(p1) == 1 then
k_pan = .4
endif
if floor(p1) == 2 then
k_pan = .6
endif

xout 0, k_pan
endop

connect "BandedWG", "outleft", "ReverbSC", "inleft"
connect "BandedWG", "outright", "ReverbSC", "inright"
connect "Blower", "outleft", "ReverbSC", "inleft"
connect "Blower", "outright", "ReverbSC", "inright"
connect "STKBowed", "outleft", "ReverbSC", "inleft"
connect "STKBowed", "outright", "ReverbSC", "inright"
connect "Buzzer", "outleft", "ReverbSC", "inleft"
connect "Buzzer", "outright", "ReverbSC", "inright"
connect "Droner", "outleft", "ReverbSC", "inleft"
connect "Droner", "outright", "ReverbSC", "inright"
connect "FilteredSines", "outleft", "ReverbSC", "inleft"
connect "FilteredSines", "outright", "ReverbSC", "inright"
connect "FMWaterBell", "outleft", "ReverbSC", "inleft"
connect "FMWaterBell", "outright", "ReverbSC", "inright"
connect "Harpsichord", "outleft", "ReverbSC", "inleft"
connect "Harpsichord", "outright", "ReverbSC", "inright"
connect "Phaser", "outleft", "ReverbSC", "inleft"
connect "Phaser", "outright", "ReverbSC", "inright"
connect "PianoOutPianoteq", "outleft", "ReverbSC", "inleft"
connect "PianoOutPianoteq", "outright", "ReverbSC", "inright"
connect "Plucked", "outleft", "ReverbSC", "inleft"
connect "Plucked", "outright", "ReverbSC", "inright"
connect "SeidelHarmOsc", "outleft", "ReverbSC", "inleft"
connect "SeidelHarmOsc", "outright", "ReverbSC", "inright"
connect "Shiner", "outleft", "ReverbSC", "inleft"
connect "Shiner", "outright", "ReverbSC", "inright"
connect "Sweeper", "outleft", "ReverbSC", "inleft"
connect "Sweeper", "outright", "ReverbSC", "inright"
connect "Xing", "outleft", "ReverbSC", "inleft"
connect "Xing", "outright", "ReverbSC", "inright"
connect "ZakianFlute", "outleft", "ReverbSC", "inleft"
connect "ZakianFlute", "outright", "ReverbSC", "inright"

connect "ReverbSC", "outleft", "MasterOutput", "inleft"
connect "ReverbSC", "outright", "MasterOutput", "inright"

//////////////////////////////////////////////////////////////////////////////
// These are all the Csound instruments and effects used in this piece.
//////////////////////////////////////////////////////////////////////////////

if strcmp(gS_os, "Linux") == 0 then
gi_Pianoteq vstinit "/home/mkg/Pianoteq\ 7/x86-64bit/Pianoteq\ 7.so", gi_vstinfo
endif
if strcmp(gS_os, "macOS") == 0 then
gi_Pianoteq vst3init "/Library/Audio/Plug-Ins/VST3/Pianoteq\ 7.vst3", "Pianoteq 7", 0
endif

// The order of #includes defines the instrument numbers.

#include "PianoNotePianoteqVst3.inc"  // Normalized.
#include "Harpsichord.inc"            // Normalized.
#include "Plucked.inc"                // Normalized.
#include "SeidelHarmOsc.inc"          // Normalized.
#include "ZakianFlute.inc"            // Normalized.
#include "STKBowed.inc"               // Normalized.
#include "FMWaterBell.inc"            // Normalized.
#include "Sweeper.inc"                // Normalized.

#include "Phaser.inc"                 // Normalized.
#include "Droner.inc"                 // Normalized.
#include "Buzzer.inc"                 // Normalized.
#include "Shiner.inc"                 // Normalized.
#include "Blower.inc"                 // Normalized.
#include "FilteredSines.inc"          // Normalized.
#include "BandedWG.inc"               // Normalized.
#include "Xing.inc"                   // Normalized.

#include "PianoOutPianoteqVst3.inc"
gk_PianoOutPianoteq_front_to_back init -3
gk_PianoOutPianoteq_left_to_right init .4
gk_PianoOutPianoteq_bottom_to_top init 3

alwayson "PianoOutPianoteq"

#include "ReverbSC.inc"
alwayson "ReverbSC"

#include "MasterOutput.inc"
alwayson "MasterOutput"

gk_FMWaterBell_front_to_back init -3
gk_FMWaterBell_left_to_right init .6
gk_FMWaterBell_bottom_to_top init -3

//////////////////////////////////////////////////////////////////////////////
// Define the initial values of all global variables/control channels.
//////////////////////////////////////////////////////////////////////////////

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
gk_MasterOutput_level init 34.43789784288879
gk_Spatialize_SpeakerRigRadius init 0
gk_LocalReverbByDistance_ReverbDecay init 0
gk_LocalReverbByDistance_Wet init 0
gk_SpatialReverb_ReverbDecay init 0
gi_instrument_position_rate init 0
gk_BformatDecoder2_MasterLevel init 0
gk_ReverbSC_feedback init 0.72
gk_ReverbSC_wet init 0.5
gi_ReverbSC_delay_modulation init 0.0075
gk_ReverbSC_frequency_cutoff init 15000
gk_PianoOutPianoteq_level init 10.035980495554469
gi_FMWaterBell_attack init 0.002936276551436901
gi_FMWaterBell_release init 0.022698875468554768
gi_FMWaterBell_exponent init 0
gi_FMWaterBell_sustain init 5.385256143273636
gi_FMWaterBell_sustain_level init 0.08267388588088297
gk_FMWaterBell_crossfade init 0.1234039047697504
gk_FMWaterBell_index init 1.1401499375260309
gk_FMWaterBell_vibrato_depth init 0.28503171595683335
gk_FMWaterBell_vibrato_rate init 2.4993821566850647
gk_FMWaterBell_level init -14.991627040173022
gk_Phaser_ratio1 init 0.5
gk_Phaser_ratio2 init 2
gk_Phaser_index1 init 0.46410256573457687
gk_Phaser_index2 init 0.8551589334803189
gk_Phaser_level init -30
gk_Plucked_level init 2
gk_SeidelHarmOsc_level init 0.6506276696566573

gk_STKBowed_vibrato_level init 0
gk_STKBowed_bow_pressure init 106.85582357509631
gk_STKBowed_bow_position init 21.81769218869982
gk_STKBowed_vibrato_frequency init 42.6065254256644
gk_STKBowed_level init 12
gk_Droner_partial1 init 0.4664927441708788
gk_Droner_partial2 init 0.16386760150008153
gk_Droner_partial3 init 0.13777922713190935
gk_Droner_partial4 init 0.4664927441708788
gk_Droner_partial5 init 0.15343225175281267
gk_Droner_level init 36
gk_Sweeper_britel init 0.01
gk_Sweeper_briteh init 2.5514444322021332
gk_Sweeper_britels init 0.5
gk_Sweeper_britehs init 0.5
gk_Sweeper_level init -18.416203372802613
gk_Buzzer_harmonics init 4
gk_Buzzer_level init 12
gk_Shiner_level init 15
gk_Blower_grainDensity init 132.3332789825534
gk_Blower_grainDuration init 0.2854231208217838
gk_Blower_grainAmplitudeRange init 174.0746779716289
gk_Blower_grainFrequencyRange init 62.82406652535464
gk_Blower_level init 4
gk_ZakianFlute_level init -19.997148547318524
gk_BandedWG_level init 9.05200375059259
gk_FilteredSines_level init 40
gk_Harpsichord_level init 9
gk_Xing_level init 42
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
gk_MasterOutput_level init 34.43789784288879
gk_Spatialize_SpeakerRigRadius init 0
gk_LocalReverbByDistance_ReverbDecay init 0
gk_LocalReverbByDistance_Wet init 0
gk_SpatialReverb_ReverbDecay init 0
gi_instrument_position_rate init 0
gk_BformatDecoder2_MasterLevel init 0
gk_ReverbSC_feedback init 0.72
gk_ReverbSC_wet init 0.5
gi_ReverbSC_delay_modulation init 0.0075
gk_ReverbSC_frequency_cutoff init 15000
gk_PianoOutPianoteq_level init 6
gi_FMWaterBell_attack init 0.002936276551436901
gi_FMWaterBell_release init 0.022698875468554768
gi_FMWaterBell_exponent init 0
gi_FMWaterBell_sustain init 5.385256143273636
gi_FMWaterBell_sustain_level init 0.08267388588088297
gk_FMWaterBell_crossfade init 0.1234039047697504
gk_FMWaterBell_index init 1.1401499375260309
gk_FMWaterBell_vibrato_depth init 0.28503171595683335
gk_FMWaterBell_vibrato_rate init 2.4993821566850647
gk_FMWaterBell_level init -14.991627040173022
gk_Phaser_ratio1 init 0.5
gk_Phaser_ratio2 init 2
gk_Phaser_index1 init 0.46410256573457687
gk_Phaser_index2 init 0.8551589334803189
gk_Phaser_level init -30
gk_Plucked_level init 2
gk_SeidelHarmOsc_level init 0.6506276696566573
gi_SeidelHarmOsc_attack init 0.003
gi_SeidelHarmOsc_petals init 5.242484129360516
gi_SeidelHarmOsc_release init 0.21521517008645225
gk_STKBowed_vibrato_level init 0
gk_STKBowed_bow_pressure init 106.85582357509631
gk_STKBowed_bow_position init 21.81769218869982
gk_STKBowed_vibrato_frequency init 42.6065254256644
gk_STKBowed_level init 12
gk_Droner_partial1 init 0.4664927441708788
gk_Droner_partial2 init 0.16386760150008153
gk_Droner_partial3 init 0.13777922713190935
gk_Droner_partial4 init 0.4664927441708788
gk_Droner_partial5 init 0.15343225175281267
gk_Droner_level init 36
gk_3_britel init 0
gk_Sweeper_briteh init 2.5514444322021332
gk_Sweeper_britel init 0.01
gk_Sweeper_britels init 0.5
gk_Sweeper_britehs init 0.15838093281543064
gk_Sweeper_level init -18.416203372802613
gk_Buzzer_harmonics init 4
gk_Buzzer_level init 12
gk_Shiner_level init 15
gk_Blower_grainDensity init 132.3332789825534
gk_Blower_grainDuration init 0.2854231208217838
gk_Blower_grainAmplitudeRange init 174.0746779716289
gk_Blower_grainFrequencyRange init 62.82406652535464
gk_Blower_level init 4
gk_ZakianFlute_level init -19.997148547318524
gk_BandedWG_level init 9.05200375059259
gk_FilteredSines_level init 40
gk_Harpsichord_level init 9
gk_Xing_level init 42
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  


gS_html init {{<!DOCTYPE html>
<html>
<head>
    <title>snow-voice-31</title>
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <!--
//////////////////////////////////////////////////////////////////////////////
// Override the CSS style of the numerical values shown for dat-gui sliders.
//////////////////////////////////////////////////////////////////////////////    
    -->
    <style type="text/css">
    input[type='text']{
        font-size: 9pt;
        height: 100%;
        width: 100%;
        vertical-align: middle;
    }
    input[type='range'] {
        font-size: 9pt;
        -webkit-appearance: none;
        box-shadow: inset 0 0 3px #333;
        background-color: gray;
        height: 10px;
        width: 100%;
        vertical-align: middle;
    }
    input[type=range]::-webkit-slider-thumb {
        -webkit-appearance: none;
        border: none;
        height: 12px;
        width: 12px;
        border-radius: 50%;
        box-shadow: inset 0 0 5px #234;
        background: chartreuse;
        margin-top: -4px;
        border-radius: 10px;
    }
    textarea {
        min-width: 100%;
        max-width: 100%;
        min-height: 100%;
        max-height: 100%;
        -webkit-box-sizing: border-box; /* Safari/Chrome, other WebKit */
        -moz-box-sizing: border-box;    /* Firefox, other Gecko */
        box-sizing: border-box;         /* Opera/IE 8+ */
    }
    </style>
    <!--
    //////////////////////////////////////////////////////////////////////////
    // All dependencies that are in some sense standard and widely used, are 
    // loaded from content delivery networks.
    //////////////////////////////////////////////////////////////////////////  
    -->
    <script src="https://code.jquery.com/jquery-3.6.0.js" integrity="sha256-H+K7U5CnXl1h5ywQfKtSj8PCmoN9aaq30gDh27Xc0jk=" crossorigin="anonymous"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/dat-gui/0.7.7/dat.gui.js"></script>    
    <script src="https://cdnjs.cloudflare.com/ajax/libs/three.js/r128/three.js" integrity="sha512-NLtnLBS9Q2w7GKK9rKxdtgL7rA7CAS85uC/0xd9im4J/yOL4F9ZVlv634NAM7run8hz3wI2GabaA6vv8vJtHiQ==" crossorigin="anonymous" referrerpolicy="no-referrer"></script>    
    <!--
    //////////////////////////////////////////////////////////////////////////
    // All other dependencies are incorporated into this repository, and are 
    // loaded as local files.
    //////////////////////////////////////////////////////////////////////////  
    -->
    <script src="silencio/js/TrackballControls.js"></script>
    <script src="PianoRoll3D.js"></script>    
    <script src="csound_jsonrpc_stub.js"></script>
</head>
<body style="background-color:black;box-sizing:border-box;padding:10px;:fullscreen">
    <div>
        <canvas id = 'piano_roll' class='canvas' style="width:98vw;height:98vh;"/>
        </canvas>
        <textarea id="csound_diagnostics" style="position:absolute;top:1vh;left:1vw;color:#87CEEBC0;background-color:transparent;border:none;text-align:left;overflow:auto;padding:0;margin:0;border-collapse:collapse;font-family:Courier, sans-serif;font-size:7pt;">
        </textarea>
    </div>
    <script>
        //////////////////////////////////////////////////////////////////////
        // This is the JSON-RPC proxy of the instance of Csound that is 
        // performing this piece.
        //////////////////////////////////////////////////////////////////////
        csound = new Csound(origin);
        let message_callback_ = function(message) {
            let notifications_textarea = document.getElementById("csound_diagnostics");
            let existing_notifications = notifications_textarea.value;
            notifications_textarea.value = existing_notifications + message;
            notifications_textarea.scrollTop = notifications_textarea.scrollHeight;
        }; 
        csound.SetMessageCallback(message_callback_, true);
        //////////////////////////////////////////////////////////////////////
        // This hooks up JavaScript code for displaying a 3-dimensional piano 
        // roll display of the algorithmically generated score.
        //////////////////////////////////////////////////////////////////////
        var canvas = document.getElementById("piano_roll");
        var piano_roll = new PianoRoll.PianoRoll3D(canvas);
        let score_time = 0;
        let video_frame = 0;
        csound.SetEventSourceCallback("score_display", function(data) {
            console.log("score_display callback...");
            console.log(typeof data);
            piano_roll.fromJson(data);        
        });
        //////////////////////////////////////////////////////////////////////
        // The piano roll display is animated (a) to show a red ball that 
        // follows the score, and (b) to enable the use of the mouse or 
        // trackball to rotate, and zoom around in, the piano roll display.
        //////////////////////////////////////////////////////////////////////
        async function animate() {
            //////////////////////////////////////////////////////////////////
            // By rendering the visual display only on every nth video frame, 
            // more time is given to Csound's audio rendering.
            //////////////////////////////////////////////////////////////////
            if (video_frame % 60 == 0) {
                score_time = await csound.GetScoreTime(function(id, result) {
                    score_time = result; 
                });
                if (typeof piano_roll !== "undefined") {
                    if (typeof piano_roll.controls !== "undefined") {
                        piano_roll.controls.update()
                        piano_roll.render3D()
                        piano_roll.progress3D(score_time);
                    }
                }
            }
            video_frame = video_frame + 1;
            requestAnimationFrame(animate)
        }
        
        /**
         * Placeholder or stub, later replaced with actual function.
         */
        var save_controls = function() {
        }

        var recenter = function() {
            piano_roll.draw3D(canvas);
        }
        
        var toggle_messages = function() {
            if (csound_diagnostics.style.display === "none") {
                csound_diagnostics.style.display = "block";
            } else {
                csound_diagnostics.style.display = "none";
            }
        }
        
        //////////////////////////////////////////////////////////////////////
        // These are the controllers and commands that are initialized in the 
        // dat.gui controls. The controllers must have the same names and 
        // types as the Csound orchestra's control channels/global variables, 
        // but the initial values actually come from the CSound channels.
        //////////////////////////////////////////////////////////////////////
        var parameters = {
            gk_MasterOutput_level: 23.199086906897115,
            gk_Spatialize_SpeakerRigRadius: 15.,
            gk_MasterOutput_level:  34.43789784288879,
            gk_Spatialize_SpeakerRigRadius:  0,
            gk_LocalReverbByDistance_ReverbDecay:  0,
            gk_LocalReverbByDistance_Wet:  0,
            gk_SpatialReverb_ReverbDecay:  0,
            gi_instrument_position_rate:  0,
            gk_BformatDecoder2_MasterLevel:  0,
            gk_ReverbSC_feedback:  0.72,
            gk_ReverbSC_wet:  0.5,
            gi_ReverbSC_delay_modulation:  0.0075,
            gk_ReverbSC_frequency_cutoff:  15000,
            gk_PianoOutPianoteq_level:  10.035980495554469,
            gi_FMWaterBell_attack:  0.002936276551436901,
            gi_FMWaterBell_release:  0.022698875468554768,
            gi_FMWaterBell_exponent:  0,
            gi_FMWaterBell_sustain:  5.385256143273636,
            gi_FMWaterBell_sustain_level:  0.08267388588088297,
            gk_FMWaterBell_crossfade:  0.1234039047697504,
            gk_FMWaterBell_index:  1.1401499375260309,
            gk_FMWaterBell_vibrato_depth:  0.28503171595683335,
            gk_FMWaterBell_vibrato_rate:  2.4993821566850647,
            gk_FMWaterBell_level:  -14.991627040173022,
            gk_Phaser_ratio1:  0.5,
            gk_Phaser_ratio2:  2,
            gk_Phaser_index1:  0.46410256573457687,
            gk_Phaser_index2:  0.8551589334803189,
            gk_Phaser_level:  -30,
            gk_Plucked_level:  2,
            gk_SeidelHarmOsc_level:  0.6506276696566573,
            gi_SeidelHarmOsc_attack: .003,
            gi_SeidelHarmOsc_petals: 2.99,
            gi_SeidelHarmOsc_release: .01,
            gk_STKBowed_vibrato_level:  0,
            gk_STKBowed_bow_pressure:  106.85582357509631,
            gk_STKBowed_bow_position:  21.81769218869982,
            gk_STKBowed_vibrato_frequency:  42.6065254256644,
            gk_STKBowed_level:  12,
            gk_Droner_partial1:  0.4664927441708788,
            gk_Droner_partial2:  0.16386760150008153,
            gk_Droner_partial3:  0.13777922713190935,
            gk_Droner_partial4:  0.4664927441708788,
            gk_Droner_partial5:  0.15343225175281267,
            gk_Droner_level:  36,
            gk_3_britel: .01,
            gk_Sweeper_briteh: 5,
            gk_Sweeper_britel
            : .5,
            gk_Sweeper_britels: .5,
            gk_Sweeper_britehs: .5,
            gk_Sweeper_level: 0,
            gk_Buzzer_harmonics:  4,
            gk_Buzzer_level:  12,
            gk_Shiner_level:  15,
            gk_Blower_grainDensity:  132.3332789825534,
            gk_Blower_grainDuration:  0.2854231208217838,
            gk_Blower_grainAmplitudeRange:  174.0746779716289,
            gk_Blower_grainFrequencyRange:  62.82406652535464,
            gk_Blower_level:  4,
            gk_ZakianFlute_level:  -19.997148547318524,
            gk_BandedWG_level:  14,
            gk_FilteredSines_level:  40,
            gk_Harpsichord_level:  9,
            gk_Xing_level:  42,
            save_controls: save_controls,
            toggle_messages: toggle_messages,
            recenter: recenter
        };
        
        var number_format = new Intl.NumberFormat('en-US', {minimumFractionDigits: 3, maximumFractionDigits: 3 });

        //////////////////////////////////////////////////////////////////////
        // The use of jQuery _greatly_ simplifies using JavaScript event 
        // handlers for HTML elements.
        //
        // jQuery can only set itself up to handle events for elements when 
        // the page has been loaded.
        //
        // We do NOT use dat.gui's persistence mechanism based on HTML5 local 
        // storage.
        //////////////////////////////////////////////////////////////////////
        window.onload = async function() {
            gui = new dat.GUI({width: 500});
            gui.remember(parameters);
            gui.add(parameters, 'save_controls').name('Save controls as Csound values [Ctrl-S]');
            gui.add(parameters, 'toggle_messages').name('Toggle Csound diagnostics');
            gui.add(parameters, 'recenter').name('Re-center piano roll [Ctrl-C]');
            
            var Master = gui.addFolder('Master');
            add_slider(Master, 'gk_ReverbSC_feedback', 0, 1.);
            add_slider(Master, 'gk_ReverbSC_wet', 0, 1.);
            add_slider(Master, 'gi_ReverbSC_delay_modulation', 0, 4.);
            add_slider(Master, 'gk_ReverbSC_frequency_cutoff', 0., 20000.);
            add_slider(Master, 'gk_MasterOutput_level', -60, 60.);
            
            var Pianoteq = gui.addFolder('Pianoteq');
            add_slider(Pianoteq, 'gk_PianoOutPianoteq_level', -60, 60.);
            
            var Xing = gui.addFolder('Xing');
            add_slider(Xing, 'gk_Xing_level', -60, 60.);
            
            var SeidelHarmOsc = gui.addFolder('SeidelHarmOsc');
            add_slider(SeidelHarmOsc, 'gi_SeidelHarmOsc_attack', 0., 1.);
            add_slider(SeidelHarmOsc, 'gi_SeidelHarmOsc_petals', 1., 10.);
            add_slider(SeidelHarmOsc, 'gi_SeidelHarmOsc_release', .005, 1.);
            add_slider(SeidelHarmOsc, 'gk_SeidelHarmOsc_level', -60, 60.);
 
            var Plucked = gui.addFolder('Plucked');
            add_slider(Plucked, 'gk_Plucked_level', -60, 60.);
            
            var Flute = gui.addFolder('Zakian Flute');
            add_slider(Flute, 'gk_ZakianFlute_level', -60, 60.);

            var STKBowed = gui.addFolder('STKBowed');
            add_slider(STKBowed, 'gk_STKBowed_vibrato_level', 0, 127);
            add_slider(STKBowed, 'gk_STKBowed_bow_pressure', 0, 127);
            add_slider(STKBowed, 'gk_STKBowed_bow_position', 0, 127);
            add_slider(STKBowed, 'gk_STKBowed_vibrato_frequency', 0, 127);
            add_slider(STKBowed, 'gk_STKBowed_level', -60, 60.);

            var FMWaterBell = gui.addFolder('FMWaterBell');
            add_slider(FMWaterBell, 'gi_FMWaterBell_attack', 0, .1);
            add_slider(FMWaterBell, 'gi_FMWaterBell_release', 0, .1);
            add_slider(FMWaterBell, 'gi_FMWaterBell_exponent', -30, 30);
            add_slider(FMWaterBell, 'gi_FMWaterBell_sustain', 0, 20);
            add_slider(FMWaterBell, 'gi_FMWaterBell_sustain_level', 0, 1.);
            add_slider(FMWaterBell, 'gk_FMWaterBell_crossfade', 0, 1.);
            add_slider(FMWaterBell, 'gk_FMWaterBell_index', 0, 15.);
            add_slider(FMWaterBell, 'gk_FMWaterBell_vibrato_depth', 0, 10.);
            add_slider(FMWaterBell, 'gk_FMWaterBell_vibrato_rate', 0, 10.);
            add_slider(FMWaterBell, 'gk_FMWaterBell_level',-60, 60);

            var Sweeper = gui.addFolder('Sweeper');
            
            add_slider(Sweeper, 'gk_Sweeper_britel', 0, 4);
            add_slider(Sweeper, 'gk_Sweeper_briteh', 0, 4);
            add_slider(Sweeper, 'gk_Sweeper_britels', 0, 4);
            add_slider(Sweeper, 'gk_Sweeper_britehs', 0, 4);
            add_slider(Sweeper, 'gk_Sweeper_level', -60, 60.);

            //~ var Harpsichord = gui.addFolder('Harpsichord');
            //~ add_slider(Harpsichord, 'gk_Harpsichord_level', -60, 60.);
             //~ var FilteredSines = gui.addFolder('FilteredSines');
            //~ add_slider(FilteredSines, 'gk_FilteredSines_level', -60, 60.);
          
            //~ var Phaser = gui.addFolder('Phaser');
            //~ add_slider(Phaser, 'gk_Phaser_ratio1', 0, 5);
            //~ add_slider(Phaser, 'gk_Phaser_ratio2', 0, 5);
            //~ add_slider(Phaser, 'gk_Phaser_index1', 0, 15);
            //~ add_slider(Phaser, 'gk_Phaser_index2', 0, 15);
            //~ add_slider(Phaser, 'gk_Phaser_level', -60, 60.);
            //~ var Droner = gui.addFolder('Droner');
            //~ add_slider(Droner, 'gk_Droner_partial1', 0, 1);
            //~ add_slider(Droner, 'gk_Droner_partial2', 0, 1);
            //~ add_slider(Droner, 'gk_Droner_partial3', 0, 1);
            //~ add_slider(Droner, 'gk_Droner_partial4', 0, 1);
            //~ add_slider(Droner, 'gk_Droner_partial5', 0, 1);
            //~ add_slider(Droner, 'gk_Droner_level', -60, 60.);
            //~ var Buzzer = gui.addFolder('Buzzer');
            //~ add_slider(Buzzer, 'gk_Buzzer_harmonics', 0, 20);
            //~ add_slider(Buzzer, 'gk_Buzzer_level', -60, 60.);
            //~ var Shiner = gui.addFolder('Shiner');
            //~ add_slider(Shiner, 'gk_Shiner_level', -60, 60.);
            //~ var Blower = gui.addFolder('Blower');
            //~ add_slider(Blower, 'gk_Blower_grainDensity', 0, 400);
            //~ add_slider(Blower, 'gk_Blower_grainDuration', 0, .5);
            //~ add_slider(Blower, 'gk_Blower_grainAmplitudeRange', 0, 400);
            //~ add_slider(Blower, 'gk_Blower_grainFrequencyRange', 0, 100);
            //~ add_slider(Blower, 'gk_Blower_level', -60, 60.);
            //~ var BandedWG = gui.addFolder('BandedWG');
            //~ add_slider(BandedWG, 'gk_BandedWG_level', -60, 60.);
            
           
            $('input').on('input', function(event) {
                var slider_value = parseFloat(event.target.value);
                csound.SetControlChannel(event.target.id, slider_value, function(id, result){}, function(id,message){});
                var output_selector = '#' + event.target.id + '_output';
                var formatted = number_format.format(slider_value);
                $(output_selector).val(formatted);
            });
            //////////////////////////////////////////////////////////////////////
            // Initializes the values of HTML controls with the values of the 
            // Csound control channels/variables with the same names.
            //////////////////////////////////////////////////////////////////////
            console.log("Updating widgets with Csound control values...");
            for (const [key, value] of Object.entries(parameters)) {
                if (typeof value !== 'function') {
                    let value_ = await csound.GetControlChannel(key);
                    parameters[key] = value_;
                    let initialization_message = `Initialized gui: ${key} = ${value_}\n`;
                    console.log(initialization_message);
                    csound.Message(initialization_message);
                    let input_selector = '#' + key;
                    let formatted = number_format.format(value_);
                    $(input_selector).val(formatted);
                    let output_selector = '#' + key + '_output';
                    $(output_selector).val(formatted);
                }
            };
            var saved_gui_parameters = gui.getSaveObject();
            console.log(saved_gui_parameters);
            gui.remember(saved_gui_parameters);
            console.log("Updated widgets with Csound control values.");
            //////////////////////////////////////////////////////////////////////
            // When the user clicks on the "Save control values" command, the 
            // current state of the control parameters is printed to the terminal
            // in the form of Csound orchestra code. This can be copied from the 
            // terminal, and pasted over the existing initial control channel 
            // values in the Csound orchestra. Currently, Web browsers do not 
            // permit writing to the user's filesystem except in the Downloads 
            // directory.
            //////////////////////////////////////////////////////////////////////
            parameters.save_controls = function() {
                console.log("Saving control values...");
                let delimiter = ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\\n";
                let text = delimiter;
                for (const [key, value] of Object.entries(parameters)) {
                    if (typeof value !== 'function') {
                        console.log(`parameter: ${key} = ${value}`);
                        let line = `${key} init ${value}\n`;
                        text = text + line;
                    }
                };
                text = text + delimiter;
                navigator.clipboard.writeText(text);
                console.log("Saved control values:\\n" + text);
            };
            //////////////////////////////////////////////////////////////////
            // The 'Ctrl-H' key hides and unhides everything on the Web page 
            // except for the piano roll score display.
            //////////////////////////////////////////////////////////////////    
            document.addEventListener("keydown", function (e) {
                console.log(e);
                let e_char = String.fromCharCode(e.keyCode || e.charCode);
                if (e.ctrlKey === true) {
                    if (e_char === 'H') {
                        gui.closed = true;
                        gui.closed = false;
                        toggle_messages();
                    } else if (e_char === 'S') {
                        parameters.save_controls();
                    }
                }
            });
            requestAnimationFrame(animate)
        };
        
        var gk_update = function(name, value) {
            var numberValue = parseFloat(value);
            csound.SetControlChannel(name, numberValue, function(id, value) {}, function(id, value) {});
        }

        var add_slider = function(gui_folder, token, minimum, maximum, name) {
            var on_parameter_change = function(value) {
                gk_update(token, value);
            };
            gui_folder.add(parameters, token, minimum, maximum).onChange(on_parameter_change).listen();
        };
        
        window.addEventListener("unload", function(event) { 
            parameters.save_controls();
        });
    
</script>
</body>
</html>
}}

if strcmp(gS_os, "Linux") == 0 then
gi_webserver webserver_create "/home/mkg/michael.gogins.studio/music/NYCEMF-2023/", 8080, 0
endif
if strcmp(gS_os, "macOS") == 0 then
gi_webserver webserver_create "/Users/michaelgogins/michael.gogins.studio/music/NYCEMF-2023/", 8080, 0
endif
// The following event source has to be created before we actually send it a 
// score to display.
webserver_send gi_webserver, "score_display", ""
webserver_open_html gi_webserver, gS_html

//////////////////////////////////////////////////////////////////////////////
// The following C++ code defines and executes a score generator that 
// implements the "multiple copy reducing machine" algorithm for computing a 
// recurrent iterated function system (RIFS).
//
// Only a limited number of iterations are computed. On the final iteration, 
// each point in the attractor of the RIFS is translated to a single note of 
// music.
//
// This code uses my CsoundAC library, the Eigen 3 header file-only library, 
// and the C++ standard library.
//////////////////////////////////////////////////////////////////////////////

S_score_generator_code init {{

#include <Eigen/Dense>
#include <csignal>
#include <csound.h>
#include <csdl.h>
#include <dlfcn.h>
#include <iostream>
#include <cstdio>
#include <random>
#include <vector>
#include <Composition.hpp>
#include <functional>
#include <memory>
#include <MusicModel.hpp>
#include <random>
#include <ScoreNode.hpp>
#include <VoiceleadingNode.hpp>

//////////////////////////////////////////////////////////////////////////////
// This symbol is used by a number of C++ libraries, but is not defined in the
// startup code. Therefore, we may need to define it here.
//////////////////////////////////////////////////////////////////////////////

/// void* __dso_handle = (void *)&__dso_handle;

static bool diagnostics_enabled = false;

extern "C" { 

struct Cursor
{
    csound::Event note;
    csound::Chord chord;
    csound::Scale scale;
};

auto generator = [] (const Cursor &cursor, int depth, int target_depth, csound::Score &score)
{
    Cursor result = cursor;
    return result;
};

//////////////////////////////////////////////////////////////////////////////
// Computes a deterministic, finite recurrent iterated function system by
// recursively applying a set of generators (transformations) to a pen
// that represents the position of a "pen" on a "score." The entries in
// the transitions matrix represent open or closed paths of recurrence through
// the tree of calls. Because the pen is passed by value, it is in effect
// copied at each call of a generator and at each layer of recursion.
//////////////////////////////////////////////////////////////////////////////
void recurrent(std::vector< std::function<Cursor(const Cursor &, int, int, csound::Score &)> > &generators,
        Eigen::MatrixXd &transitions,
        int depth,
        int target_depth,
        int transformationIndex,
        const Cursor pen,
        csound::Score &score) {
    depth = depth + 1;
    if (depth == target_depth) {
        score.append(pen.note);    
        return;
    }
    for (int transitionIndex = 0, transitionN = transitions.rows(); transitionIndex < transitionN; ++transitionIndex) {
        if (transitions(transformationIndex, transitionIndex)) {
            auto newCursor = generators[transitionIndex](pen, depth, target_depth, score);
            recurrent(generators, transitions, depth, target_depth, transitionIndex, newCursor, score);
        }
    }
}

extern "C" {
    void (*webserver_send_message_)(CSOUND *csound, int webserver_handle, const char *channel_name, const char *message);
};

//////////////////////////////////////////////////////////////////////////////
// This is the entry point for this C++ module. It will be called by Csound 
// immediately once the module has been compiled and linked.
//////////////////////////////////////////////////////////////////////////////

// Try scales/chords not chord transformations.
// Try FM of time and pitch scales.
// The last quarter of the piece gets just one chord change, this will not do.

extern "C" int score_generator(CSOUND *csound) {
    // Turn off Csound-installed signal handlers.
    std::signal(SIGTERM, SIG_DFL);
    std::signal(SIGABRT, SIG_DFL);
    std::signal(SIGSEGV, SIG_DFL);

    // Load: extern "C" void webserver_send_message(CSOUND *csound, int webserver_handle, const char *channel_name, const char *message);
    // Library handle 0 means: search all symbols in the process.
    // Note: for this to work, libcsound_webserver.so (or the equivalent 
    // filename on other platforms) must be in the link library list for THIS 
    // code in order to be loaded, linked, and resolved.
    #ifdef __MACH__
    auto library_handle = dlopen("/Users/michaelgogins/csound-webserver-opcodes/build/libcsound_webserver.dylib", RTLD_NOW | RTLD_GLOBAL);
    #endif
    #ifdef __linux__
    auto library_handle = dlopen("/home/mkg/csound-webserver-opcodes/build/libcsound_webserver.so", RTLD_NOW | RTLD_GLOBAL);
    #endif
    webserver_send_message_ = (void (*)(CSOUND *, int, const char *, const char *)) csound->GetLibrarySymbol(library_handle, "webserver_send_message");
    csound->Message(csound, "webserver_send_message_: %p\\n", webserver_send_message_);
#if (defined(__linux__) || defined(__MACH__))
    if (webserver_send_message_ == 0) {
        csound->Message(csound, "dlerror: %s\\n", dlerror());
    }
#endif
    int result = OK;
    std::mt19937 mersenneTwister;
    std::uniform_real_distribution<> randomvariable(.05,.95);

    csound::ScoreModel model;
    std::map<double, csound::Chord> chordsForTimes;
    csound::Chord modality;
    Cursor pen;
    pen.scale = csound::Scale("F major");
    std::cout << "pen.scale: " << pen.scale.name() << std::endl;
    pen.chord = pen.scale.chord(1, 4);
    std::cout << "pen.chord: " << pen.chord.eOP().name() << std::endl;
    modality = pen.chord;
    pen.note = csound::Event{1,40,144,1,1,1,0,0,0,0,1};
    std::vector<std::function<Cursor(const Cursor &, int, int, csound::Score &)>> generators;
    generators.push_back([&chordsForTimes, &modality](const Cursor &pen_, int depth, int target_depth, csound::Score &score) {
        Cursor pen = pen_;
        ///if (depth == 5) {
        if (depth == 5) {
            pen.chord = pen.scale.transpose_degrees(pen.chord, 2);
            chordsForTimes[pen.note.getTime()] = pen.chord;
        }
        pen.note[csound::Event::TIME] = (pen.note[csound::Event::TIME]                * 1./4.)    + (0.)      * 100.;
        pen.note[csound::Event::KEY] =  (pen.note[csound::Event::KEY]                 * 1./2.)    + (1.)      * 100.;
        
        pen.note[csound::Event::VELOCITY] =  (pen.note[csound::Event::VELOCITY]       * (1.2));
        pen.note[csound::Event::DURATION] =  (pen.note[csound::Event::DURATION]       * .97);
        return pen;
    });
    generators.push_back([&chordsForTimes, &modality](const Cursor &pen_, int depth, int target_depth, csound::Score &score) {
        Cursor pen = pen_;
        pen.note[csound::Event::TIME] = (pen.note[csound::Event::TIME]                * 1./4.)    + (1./4.)   * 100.;
        pen.note[csound::Event::KEY] =  (pen.note[csound::Event::KEY]                 * 1./2.)    + (1.)      * 100.;
        return pen;
    });
    generators.push_back([&chordsForTimes, &modality](const Cursor &pen_, int depth, int target_depth, csound::Score &score) {
        Cursor pen = pen_;
        ///if (depth == 2) {
        if (depth == 2) {
            pen.chord = pen.scale.transpose_degrees(pen.chord, 5);
            chordsForTimes[pen.note.getTime()] = pen.chord;
        }
        pen.note[csound::Event::TIME] = (pen.note[csound::Event::TIME]                * 1./4.)    + (2./4.)   * 100.;
        pen.note[csound::Event::KEY] =  (pen.note[csound::Event::KEY]                 * 1./2.)    + (1.)      * 110.;
        return pen;
    });
    generators.push_back([&chordsForTimes, &modality](const Cursor &pen_, int depth, int target_depth, csound::Score &score) {
        Cursor pen = pen_;
        ///if (depth == 2) {
        if (depth == 2) {
            pen.chord = pen.scale.transpose_degrees(pen.chord, -2);
            chordsForTimes[pen.note.getTime()] = pen.chord;
        }
        pen.note[csound::Event::TIME] =         (pen.note[csound::Event::TIME]        * 1./4.)    + ( 3./4.) * 100.;
        pen.note[csound::Event::KEY] =          (pen.note[csound::Event::KEY]         * -1./2.)    + ( 1.)    * 120.;
        
        pen.note[csound::Event::INSTRUMENT] =   (pen.note[csound::Event::INSTRUMENT]  * 0.5)      + (90.);
        return pen;
    });
    generators.push_back([&chordsForTimes, &modality](const Cursor &pen_, int depth, int target_depth, csound::Score &score) {
        Cursor pen = pen_;
        ///if (depth == 4) {
        if (depth == 4) {
            pen.chord = pen.scale.transpose_degrees(pen.chord, 3);
            chordsForTimes[pen.note.getTime()] = pen.chord;
        }
        ///if (depth == 1) {
        if (depth == 1) {
            pen.chord = pen.scale.transpose_degrees(pen.chord, -2);
            chordsForTimes[pen.note.getTime()] = pen.chord;
        }
        pen.note[csound::Event::TIME] =         (pen.note[csound::Event::TIME]        * 1./3)     + (0.)    * 100.;
        pen.note[csound::Event::KEY] =          (pen.note[csound::Event::KEY]         * -1./2.)    + (0.)    * 100.;
        return pen;
    });
    generators.push_back([&chordsForTimes, &modality](const Cursor &pen_, int depth, int target_depth, csound::Score &score) {
        Cursor pen = pen_;
        ///if (depth == 2) {
        if (depth == 2) {
          auto modulations = pen.scale.modulations(pen.chord);
          auto modulations_count = modulations.size();
          auto random_index = std::floor(std::rand() % modulations_count);
          pen.scale = modulations[random_index];
          std::cout << "new scale (" << random_index << " of " << modulations_count << "): " << pen.scale.name() << std::endl;
          //  pen.chord = pen.scale.transpose_degrees(pen.chord, -2);
          chordsForTimes[pen.note.getTime()] = pen.chord;
        }
        pen.note[csound::Event::TIME] =         (pen.note[csound::Event::TIME]        * 1./3.)    + (1./3.) * 100.;
        pen.note[csound::Event::KEY] =          (pen.note[csound::Event::KEY]         * 1./4.)    + (0.)    * 100.;
        
        pen.note[csound::Event::DURATION] =     (pen.note[csound::Event::DURATION]    * 1.02);
        pen.note[csound::Event::INSTRUMENT] =   (pen.note[csound::Event::INSTRUMENT]  * 0.75)     + (-3.);
        pen.note[csound::Event::VELOCITY] =     (pen.note[csound::Event::VELOCITY]    * (.99));
        return pen;
    });
    generators.push_back([&chordsForTimes, &modality](const Cursor &pen_, int depth, int target_depth, csound::Score &score) {
        Cursor pen = pen_;
        ///if (depth == 2) {
        if (depth == 2) {
          auto modulations = pen.scale.modulations(pen.chord);
          auto modulations_count = modulations.size();
          auto random_index = std::floor(std::rand() % modulations_count);
          pen.scale = modulations[random_index];
          std::cout << "new scale: " << pen.scale.name() << std::endl;
          pen.chord = pen.scale.transpose_degrees(pen.chord, -2);
          chordsForTimes[pen.note.getTime()] = pen.chord;
        }
        pen.note[csound::Event::TIME] = (pen.note[csound::Event::TIME]                * 1./3.)    + (2./3.) * 100.;
        pen.note[csound::Event::KEY] =  (pen.note[csound::Event::KEY]                 * 1./2.)    + (0.)    * 201;
        
        pen.note[csound::Event::INSTRUMENT] =  (pen.note[csound::Event::INSTRUMENT]   * 0.75)     + (-3.);
        return pen;
     });
    // Generate the transition matrix.
    Eigen::MatrixXd transitions = Eigen::MatrixXd::Ones(generators.size(), generators.size());
    // Zero some paths of recurrence.
    transitions(2, 0) = 0;
    transitions(4, 0) = 0;
    transitions(3, 6) = 0;
    transitions(5, 6) = 0;
    std::cout << "transitions:" << std::endl << transitions << std::endl;
    csound::Score score;
    //////////////////////////////////////////////////////////////////////////////
    // Before iterating, ensure that the score does start and end with a chord.
    //////////////////////////////////////////////////////////////////////////////
    chordsForTimes[ 1000.] = pen.chord;
    chordsForTimes[-1000.] = pen.chord;
    recurrent(generators, transitions, 0, 7, 0, pen, score);
    std::cout << "Generated duration:     " << score.getDuration() << std::endl;
    //////////////////////////////////////////////////////////////////////////////
    // We apply the chords that were generated along WITH the notes, TO the notes.
    // This creates an algorithmically generated chord progression.
    //////////////////////////////////////////////////////////////////////////////
    score.sort();
    score.rescale(csound::Event::KEY, true, 25.0, true,  74.0);
    score.temper(12.);
    std::cout << "Generated notes:        " << score.size() << std::endl;
    double endTime = score.back().getTime();
    std::cout << "Chord segments:         " << chordsForTimes.size() << std::endl;
    int size = 0;
    int segment_count = 0;
    for (auto it = chordsForTimes.rbegin(); it != chordsForTimes.rend(); ++it, ++segment_count) {
        auto startTime = it->first;
        auto &chord = it->second;
        auto segment = csound::slice(score, startTime, endTime);
        size += segment.size();
        if (segment.size() > 0) {
            std::fprintf(stderr, "From %9.4f to %9.4f apply %s to %d notes.\\n", startTime, endTime, chord.eOP().name().c_str(), segment.size());
        }
        csound::apply(score, chord, startTime, endTime, true);
        endTime = startTime;
    }
    std::cout << "Conformed notes:        " << size << std::endl;
    //score.rescale(csound::Event::TIME,          true,  0.0, false,  0.0);
    score.rescale(csound::Event::INSTRUMENT,    true,  1.0, true,   7.999);
    //score.rescale(csound::Event::INSTRUMENT,  true,  1.0, true,   0.);
    score.rescale(csound::Event::VELOCITY,      true, 40.0, true,  20.0);
    score.rescale(csound::Event::PAN,           true,  0.0, true,   0.0);
    std::cout << "Move to origin duration:" << score.getDuration() << std::endl;
    score.setDuration(600.);
    score.findScale();
    score.setDuration(610.);
    for (int i = 0, n = score.size(); i < n; ++i) {
        auto insno = std::floor(score[i].getInstrument());
        auto pan = .08 + (insno / 10.);
        score[i].setPan((randomvariable(mersenneTwister) * .01) + pan);
        score[i].setDepth(randomvariable(mersenneTwister));
        score[i].setPhase(randomvariable(mersenneTwister));
        auto duration = score[i].getDuration() / 150.;
        score[i].setDuration(duration);
    }
    score.tieOverlappingNotes(true);
    score.rescale(csound::Event::TIME,          true,  2.0, false,  0.0);
    std::cout << "score:" << std::endl << score.getCsoundScore() << std::endl;
    std::cout << "Final duration:         " << score.getDuration() << std::endl;
    //////////////////////////////////////////////////////////////////////////
    // Using the EVTBLK struct for each note is more efficient than using a 
    // string for each note, or for the entire score.
    //////////////////////////////////////////////////////////////////////////
    EVTBLK evtblk;
    // Multiply duration by some function of time so later notes get longer?
    std::memset(&evtblk, 0, sizeof(EVTBLK));
    for (const auto &note : score) {
        evtblk.strarg = nullptr;
        evtblk.scnt = 0;
        evtblk.opcod = 'i';
        evtblk.pcnt = 9;
        evtblk.p[1] = std::floor(note.getInstrument());
        evtblk.p[2] = note.getTime();
        evtblk.p[3] = note.getDuration();
        evtblk.p[4] = note.getKey();
        evtblk.p[5] = note.getVelocity();
        evtblk.p[6] = note.getDepth();
        evtblk.p[7] = note.getPan();
        evtblk.p[8] = note.getHeight();
        //std::fprintf(stderr, "%c %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f\\n", evtblk.opcod, evtblk.p[1], evtblk.p[2], evtblk.p[3], evtblk.p[4], evtblk.p[5], evtblk.p[6], evtblk.p[7], evtblk.p[8]);
        int result = csound->insert_score_event(csound, &evtblk, 0.);
    }
    //////////////////////////////////////////////////////////////////////////
    // This translates the just-generated CsoundAC Score to a textual JSON 
    // form.
    //////////////////////////////////////////////////////////////////////////
    auto json_score = score.toJson();
    //////////////////////////////////////////////////////////////////////////
    // The Web page has already defined a canvas with a PianoRoll3D attached.
    // Here, the PianoRoll3D instance is called to send our JSON score to 
    // the Web page, which will render it as an animated, three-dimensional 
    // piano roll.
    //////////////////////////////////////////////////////////////////////////
    webserver_send_message_(csound, 0, "score_display", json_score.c_str());
    return result;
};

};

}}

//////////////////////////////////////////////////////////////////////////////
// This compiles the above C++ module and then calls its entry point function.
//////////////////////////////////////////////////////////////////////////////

if strcmp(gS_os, "Linux") == 0 then
i_result cxx_compile "score_generator", S_score_generator_code, "g++ -v -g -O2 -std=c++17 -shared -fPIC -DUSE_DOUBLE -I. -I/usr/local/include/csound -I/home/mkg/csound/interfaces -I/usr/include/eigen3 -I/home/mkg/csound-ac/CsoundAC -lCsoundAC -lpthread -lm", "libcsound_webserver.so libCsoundAC.so"
endif
if strcmp(gS_os, "macOS") == 0 then
i_result cxx_compile "score_generator", S_score_generator_code, "g++ -v -g -O2 -std=c++17 -shared -fPIC -DUSE_DOUBLE -I. -I/usr/local/include/csound -I/Users/michaelgogins/csound/interfaces -I/usr/include/eigen3 -I/System/Volumes/Data/opt/homebrew/include/eigen3 -I/Library/Frameworks/CsoundLib64.framework/Versions/6.0/Headers -I/opt/homebrew/Cellar/boost/1.81.0_1/include -I/home/mkg/csound-ac/CsoundAC -lCsoundAC -lpthread -lm", "/Users/michaelgogins/csound-webserver-opcodes/build/libcsound_webserver.dylib libCsoundAC.dylib"
endif


instr Exit
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
exitnow
endin

</CsInstruments>
<CsScore>
; f 0 does not work here, we actually need to schedule an instrument that 
; turns off Csound.
; a 0 1 270
i "Exit" [610]
;f 0 [6 * 60 + 5]
</CsScore>
</CsoundSynthesizer>
