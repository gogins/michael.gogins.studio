<CsoundSynthesizer>
<CsLicense>
</CsLicense>
<CsOptions>
--m-amps=1 --m-range=1 --m-dB=1 --m-benchmarks=1 --m-warnings=0 -+msg_color=0 -d -odac
</CsOptions>
<CsInstruments>
sr              =           48000
ksmps           =           128
nchnls          =           2
0dbfs           =           100

gS_os, gS_macros cxx_os
prints "Operating system: %s\n", gS_os

S_grain_code init {{

//void* __dso_handle = (void *)&__dso_handle;

static bool diagnostics_enabled = false;

#include "cxx_invokable.hpp"
#include <cmath>
#include <complex>
#include <csdl.h>
#include <cstdio>

    /**
     * Compute a cosine grain. If the synchronous_phase argument is true
     * (the default value), then all grains of the same frequency
     * will have synchronous phases, which can be useful in avoiding certain artifacts.
     * For example, if cosine grains of the same frequency have synchronous phases,
     * they can be overlapped by 1/2 their duration without artifacts
     * to produce a continuous cosine tone.
     *
     * The algorithm uses an efficient difference equation.
     */
struct InvokableCosineGrain : public CxxInvokableBase {
    virtual ~InvokableCosineGrain() {
    };
    int init(CSOUND *csound_, OPDS *opds_, MYFLT **outputs, MYFLT **inputs) override {
        if (diagnostics_enabled) std::fprintf(stderr, ">>>>>>> InvokableCosineGrain::init...\\n");
        int result = OK;
        csound = csound_;
        opds = opds_;
        // Inputs.
        if (diagnostics_enabled) std::fprintf(stderr, ">>>>>>> InvokableCosineGrain::init: csound: %p opds: %p inputs: %p.\\n", csound, opds, inputs);
        center_time_seconds = *(inputs[0]);
        if (diagnostics_enabled) std::fprintf(stderr, ">>>>>>> InvokableCosineGrain::init: center_time_seconds: %9.4f\\n", center_time_seconds);
        duration_seconds = *(inputs[1]);
        if (diagnostics_enabled) std::fprintf(stderr, ">>>>>>> InvokableCosineGrain::init: duration_seconds: %9.4f\\n", duration_seconds);
        frequency_hz = *(inputs[2]);
        if (diagnostics_enabled) std::fprintf(stderr, ">>>>>>> InvokableCosineGrain::init: frequency_hz: %9.4f\\n", frequency_hz);
        amplitude = *(inputs[3]);
        if (diagnostics_enabled) std::fprintf(stderr, ">>>>>>> InvokableCosineGrain::init: amplitude: %9.4f\\n", amplitude);
        phase_offset_radians = *(inputs[4]);
        if (diagnostics_enabled) std::fprintf(stderr, ">>>>>>> InvokableCosineGrain::init: phase_offset_radians: %9.4f\\n", phase_offset_radians);
        if (*(inputs[5]) != 0.) {
            synchronous_phase = true;
        } else {
            synchronous_phase = false;
        }
        if (diagnostics_enabled) std::fprintf(stderr, ">>>>>>> InvokableCosineGrain::init: synchronous_phase: %i\\n", synchronous_phase);
        if (synchronous_phase) {
            wavelength_seconds = 1.0 / frequency_hz;
            wavelengths = center_time_seconds / wavelength_seconds;
            whole_cycles = 0;
            fractional_cycle = std::modf(wavelengths, &whole_cycles);
            phase_offset_radians = 2.0 * M_PI * fractional_cycle;
        }
        frames_per_second = csound->GetSr(csound);
        frame_count = size_t(std::round(duration_seconds * frames_per_second));
        // The signal is a cosine sinusoid.
        sinusoid_radians_per_frame = 2.0 * M_PI * frequency_hz / frames_per_second;
        sinusoid_coefficient = 2.0 * std::cos(sinusoid_radians_per_frame);
        if (diagnostics_enabled) std::fprintf(stderr, ">>>>>>> InvokableCosineGrain::init: sinusoid_coefficient: %9.4f\\n", sinusoid_coefficient);
        // The initial frame.
        sinusoid_1 = std::cos(phase_offset_radians);
        // What would have been the previous frame.
        sinusoid_2 = std::cos(-sinusoid_radians_per_frame + phase_offset_radians);
        // The envelope is exactly 1 cycle of a cosine sinusoid, offset by -1.
        envelope_frequency_hz = 1.0 / duration_seconds;
        envelope_radians_per_frame = 2.0 * M_PI * envelope_frequency_hz / frames_per_second;
        envelope_coefficient = 2.0 * std::cos(envelope_radians_per_frame);
        if (diagnostics_enabled) std::fprintf(stderr, ">>>>>>> InvokableCosineGrain::init: envelope_coefficient: %9.4f\\n", envelope_coefficient);
        // The initial frame.
        envelope_1 = std::cos(0.0);
        // What would have been the previous frame.
        envelope_2 = std::cos(-envelope_radians_per_frame);
        signal = 0.0;
        temporary = 0.0;
        if (diagnostics_enabled) std::fprintf(stderr, ">>>>>>> InvokableCosineGrain::init: envelope_2: %9.4f\\n", envelope_2);
        return result;
    }
    int kontrol(CSOUND *csound_, MYFLT **outputs, MYFLT **inputs) override {
        if (diagnostics_enabled) std::fprintf(stderr, ">>>>>>> InvokableCosineGrain::kontrol...\\n");
        int result = OK;
        int frame_index = 0;
        for( ; frame_index < kperiodOffset(); ++frame_index) {
            outputs[0][frame_index] = 0;
        }
        for( ; frame_index < kperiodEnd(); ++frame_index) {
            signal = (sinusoid_1 * (envelope_1 - 1.0)) * amplitude;
            temporary = sinusoid_1;
            sinusoid_1 = sinusoid_coefficient * sinusoid_1 - sinusoid_2;
            sinusoid_2 = temporary;
            temporary = envelope_1;
            envelope_1 = envelope_coefficient * envelope_1 - envelope_2;
            envelope_2 = temporary;
            outputs[0][frame_index] = signal; //sample;
        }
        for( ; frame_index < ksmps(); ++frame_index) {
            outputs[0][frame_index] = 0;
        }
        return result;
    }
    double center_time_seconds;
    double duration_seconds;
    double frequency_hz;
    double amplitude;
    double phase_offset_radians;
    double wavelengths;
    double wavelength_seconds;
    double whole_cycles;
    double fractional_cycle;
    bool synchronous_phase;
    int frame_count;
    double frames_per_second;
    double sinusoid_radians_per_frame;
    double sinusoid_coefficient;    
    double sinusoid_1;
    double sinusoid_2;
    double envelope_frequency_hz;
    double envelope_radians_per_frame;
    double envelope_coefficient;
    double envelope_1;
    double envelope_2;
    double signal;
    double temporary;
};

extern "C" {
    
    int grain_main(CSOUND *csound) {
        int result = OK;
        if (diagnostics_enabled) std::fprintf(stderr, ">>>>>>> This is \\"grain_main\\".\\n");
        return result;
    }
    
    CxxInvokable *cosine_grain_factory() {
        if (diagnostics_enabled) std::fprintf(stderr, ">>>>>>> This is \\"cosine_grain_factory\\".\\n");
        auto result = new InvokableCosineGrain;
        if (diagnostics_enabled) std::fprintf(stderr, ">>>>>>> \\"cosine_grain_factory\\" created %p.\\n", result);
        return result;
    }

};

}}

if strcmp(gS_os, "Linux") == 0 then
i_result cxx_compile "grain_main", S_grain_code, "g++ -v -g -O2 -std=c++17 -shared -fPIC -DUSE_DOUBLE -I. -I/usr/local/include/csound -I/home/mkg/csound/interfaces -I/usr/include/eigen3 -I/home/mkg/csound-extended/CsoundAC -lCsoundAC -lpthread -lm", "libcsound_webserver.so libCsoundAC.so"
endif
if strcmp(gS_os, "macOS") == 0 then
i_result cxx_compile "grain_main", S_grain_code, "clang++ -g -O2 -std=c++14 -shared -fPIC -DUSE_DOUBLE -I. -I/System/Volumes/Data/opt/homebrew/include/eigen3 -I/opt/homebrew/Cellar/csound/6.17.0_5/Frameworks/CsoundLib64.framework/Versions/6.0/Headers -I/opt/homebrew/Cellar/boost/1.79.0/include -I/Users/michaelgogins/csound-extended/CsoundAC -lpthread -lm", "libCsoundAC.dylib"
endif

gk_CosineGrain_level chnexport "gk_CosineGrain_level", 3
gk_CosineGrain_midi_dynamic_range chnexport "gk_CosineGrain_midi_dynamic_range", 3

gk_CosineGrain_level init 0
gk_CosineGrain_midi_dynamic_range init 20
instr CosineGrain
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_CosineGrain_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 65
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_CosineGrain_level)

; Grain inputs.
i_center_time_seconds init i_time
i_duration_seconds init i_duration
i_frequency_hz init i_frequency
; i_amplitude init i_amplitude
i_phase_offset_radians init 0
i_synchronous_phase init 1
a_signal init 0
a_signal cxx_invoke "cosine_grain_factory", 3, i_center_time_seconds, i_duration_seconds, i_frequency_hz, i_amplitude, i_phase_offset_radians, i_synchronous_phase 
a_signal = a_signal * k_gain
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
;printks "a_signal: %9.4f a_out_left: %9.4f a_out_right: %9.4f\\n", 0, k(a_signal), k(a_out_left), k(a_out_right)
outleta "outleft", a_out_left
outleta "outright", a_out_right
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_ReverbSC_feedback chnexport "gk_ReverbSC_feedback", 3
gk_ReverbSC_wet chnexport "gk_ReverbSC_wet", 3
gi_ReverbSC_delay_modulation chnexport "gi_ReverbSC_delay_modulation", 3
gk_ReverbSC_frequency_cutoff chnexport "gk_ReverbSC_frequency_cutoff", 3

gk_ReverbSC_feedback init 0.875
gk_ReverbSC_wet init 0.5
gi_ReverbSC_delay_modulation init 0.0075
gk_ReverbSC_frequency_cutoff init 15000

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

instr MasterOutput
aleft inleta "inleft"
aright inleta "inright"
k_gain = ampdb(gk_MasterOutput_level)
printks2 "Master gain: %f\n", k_gain
iamp init 1
aleft butterlp aleft, 18000
aright butterlp aright, 18000
outs aleft * k_gain, aright * k_gain
; We want something that will play on my phone.
i_amplitude_adjustment = ampdbfs(-3) / 32767
i_filename_length strlen gS_MasterOutput_filename
if i_filename_length > 0 then
prints sprintf("Output filename: %s\n", gS_MasterOutput_filename)
fout gS_MasterOutput_filename, 18, aleft * i_amplitude_adjustment, aright * i_amplitude_adjustment
endif
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

instr Exit
prints "Exit"
endin

connect "CosineGrain", "outleft", "ReverbSC", "inleft"
connect "CosineGrain", "outright", "ReverbSC", "inright"
connect "ReverbSC", "outleft", "MasterOutput", "inleft"
connect "ReverbSC", "outright", "MasterOutput", "inright"

alwayson "ReverbSC"
alwayson "MasterOutput"

//////////////////////////////////////////////////////////////////////////
// This is all of the HTML5 code for the embedded Web page that controls 
// this piece.
//////////////////////////////////////////////////////////////////////////  
gS_html init {{
<!DOCTYPE html>
<html>
<head>
    <title>1-Dimensional Cellular Automata</title>
    <style type="text/css">
    input[type='range'] {
        -webkit-appearance: none;
        box-shadow: inset 0 0 5px #333;
        background-color: gray;
        height: 10px;
        width: 100%;
        vertical-align: middle;
    }
    input[type=range]::-webkit-slider-thumb {
        -webkit-appearance: none;
        border: none;
        height: 16px;
        width: 16px;
        border-radius: 50%;
        box-shadow: inset 0 0 7px #234;
        background: chartreuse;
        margin-top: -4px;
        border-radius: 10px;
    }
    table td {
        border-width: 2px;
        padding: 8px;
        border-style: solid;
        border-color: transparent;
        color:yellow;
        background-color: teal;
        font-family: sans-serif
    }
    table th {
        border-width: 2px;
        padding: 8px;
        border-style: solid;
        border-color: transparent;
        color:white;
        background-color:teal;
         font-family: sans-serif
   }
    textarea {
        border-width: 2px;
        padding: 8px;
        border-style: solid;
        border-color: transparent;
        color:chartreuse;
        background-color:black;
        font-family: 'Courier', sans-serif
    }
    h1 {
        margin: 1em 0 0.5em 0;
        color: #343434;
        font-weight: normal;
        font-family: 'Ultra', sans-serif;   
        font-size: 36px;
        line-height: 42px;
        text-transform: uppercase;
    }
    h2 {
        margin: 1em 0 0.5em 0;
        color: #343434;
        font-weight: normal;
        font-size: 30px;
        line-height: 40px;
        font-family: 'Orienta', sans-serif;
    }    
    h3 {
        margin: 1em 0 0.5em 0;
        color: #343434;
        font-weight: normal;
        font-size:24px;
        line-height: 30px;
        font-family: 'Orienta', sans-serif;
    }    
    canvas {
        overflow: true;
    }
    </style>
    <!--
    //////////////////////////////////////////////////////////////////////////
    // All HTML dependencies that are in some sense standard and widely used, 
    // are loaded from content delivery networks.
    //////////////////////////////////////////////////////////////////////////  
    -->
    <script src="https://code.jquery.com/jquery-3.6.0.js" integrity="sha256-H+K7U5CnXl1h5ywQfKtSj8PCmoN9aaq30gDh27Xc0jk=" crossorigin="anonymous"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/p5.js/1.4.1/p5.js" integrity="sha512-4P0ZJ49OMXe3jXT+EsEaq82eNwFeyYL81LeHGzGcEhowFbTqeQ80q+NEkgsE8tHPs6aCqvi7U+XWliAjDmT5Lg==" crossorigin="anonymous" referrerpolicy="no-referrer"></script>
</head>
<body style="background-color:CadetBlue">
    <h1>1-Dimensional Cellular Automata</h1>
    <h3>Adapted for Csound with HTML5 by Michael Gogins, from <a href="https://p5js.org/examples/simulate-wolfram-ca.html">p5.js Wolfram CA example</a><h3>
    <form id='persist'>
        <table>
            <col width="2*">
            <col width="5*">
            <col width="100px">
            <tr>
            <td>
            <label for=gk_rule>Rule</label>
            <td>
            <input class=persistent-element type=range min=0 max=255 value=110 id=gk_rule step=1>
            <td>
            <output for=gk_rule id=gk_rule_output>110</output>
            </tr>
            <tr>
            <td>
            <label for=gk_Pinitial_state>Initial state</label>
            <td>
            <input class=persistent-element type=range min=0 max=4 value=1 id=gk_initial_state step=.001>
            <td>
            <output for=gk_initial_state id=gk_initial_state_output>1</output>
            </tr>
            <tr>
            <td>
            <label for=gk_seconds_per_time_step>Seconds per time step</label>
            <td>
            <input class=persistent-element type=range min=0.01 max=4 value=.25 id=gk_seconds_per_time_step step=.01>
            <td>
            <output for=gk_seconds_per_time_step id=gk_seconds_per_time_step_output>.25</output>
            </tr>
            <tr>
            <td>
            <label for=gk_reverb_delay>Reverb delay feedback</label>
            <td>
            <input class=persistent-element type=range min=0 max=1 value=.89 id=gk_reverb_delay step=.001>
            <td>
            <output for=gk_reverb_delay id=gk_reverb_delay_output>.89</output>
            </tr>
            <tr>
            <td>
            <label for=gk_reverb_hipass>Reverb highpass cutoff (Hz)</label>
            <td>
            <input class=persistent-element type=range min=0 max=20000 value=12000 id=gk_reverb_hipass step=.001>
            <td>
            <output for=gk_reverb_hipass id=gk_reverb_hipass_output>12000</output>
            </tr>
            <tr>
            <td>
            <label for=gk_MasterOutput_level>Master output level (dB)</label>
            <td>
            <input class=persistent-element type=range min=-40 max=40 value=-6 id=gk_MasterOutput_level step=.001>
            <td>
            <output for=gk_MasterOutput_level id=gk_MasterOutput_level_output>-6</output>
            </tr>
        </table>
        <p>
        <input type="button" id='start' value="Start" />
        <input type="button" id='stop' value="Stop" />
        <input type="button" id='save_controls' value="Save" />
        <input type="button" id='restore_controls' value="Restore" />
    </form>   
    <p>
    <script src="csound_jsonrpc_stub.js"></script>
    <script>

    function decimal_to_ruleset(decimal, size) {
        const rule_set = [0];
        for (let i=0; i<size; i++) {
            let mask = 1;
            const bit = decimal & (mask << i);
            if (bit === 0) {
                rule_set[i] = 0;
            } else {
                rule_set[i] = 1;
            }
        }
        return rule_set;
    }
     
    continue_generating_score = false;
    let w = 1;
    // An array of 0s and 1s
    let cells;

    let generation = 0;
    let generations = 1000;

    // Rule 110 -- proved to be Turing-complete.
    //let ruleset =   [0,1,1,0,1,1,1,0];
    // Rule 54 -- suspected to be Turing-complete.
    //let ruleset =   [0,0,1,1,0,1,1,0];

    function setup() {
        createCanvas(1000, generations);
        cells = Array(floor(width / w));
        for (let i = 0; i < cells.length; i++) {
            cells[i] = 0;
        }
        //cells[cells.length-1] = 1;
        cells[cells.length-100] = 1;
    }

    function draw_() {
        if (continue_generating_score === false) {
            return;
        }
        if (generation >= height / w) {
            generation = 0;
            clear();
        }
        for (let i = 0; i < cells.length; i++) {
            if (cells[i] === 1) {
                fill("green");
                noStroke();
                rect(i * w, generation * w, w, w);
            } else {
                fill("black");
                noStroke();
                rect(i * w, generation * w, w, w);
            }
        }
        generate();
        let seconds_per_time_step = $('#gk_seconds_per_time_step').val()
        play_cells(seconds_per_time_step);
        //setTimeout(draw_, seconds_per_time_step * 1000);
    }

    function generate() {
        // First we create an empty array for the new values
        let nextgen = Array(cells.length);
        // For every spot, determine new state by examing current state, and neighbor states
        // Ignore edges that only have one neighor (i.e. do not wrap).
        for (let i = 1; i < cells.length-1; i++) {
            let left   = cells[i-1];   // Left neighbor state
            let me     = cells[i];     // Current state
            let right  = cells[i+1];   // Right neighbor state
            nextgen[i] = rules(left, me, right); // Compute next generation state based on ruleset
        }
        cells = nextgen;
        generation++;
    }

    function rules(a, b, c) {
        if (a == 1 && b == 1 && c == 1) return ruleset[7];
        if (a == 1 && b == 1 && c == 0) return ruleset[6];
        if (a == 1 && b == 0 && c == 1) return ruleset[5];
        if (a == 1 && b == 0 && c == 0) return ruleset[4];
        if (a == 0 && b == 1 && c == 1) return ruleset[3];
        if (a == 0 && b == 1 && c == 0) return ruleset[2];
        if (a == 0 && b == 0 && c == 1) return ruleset[1];
        if (a == 0 && b == 0 && c == 0) return ruleset[0];
        return 0;
    }
    
    // Synthesizes a stack of phase-synchronous grains, one grain for each 
    // cell in the automaton. The grain duration is exactly twice the duration 
    // of the time step.
    
    function play_cells(time_step) {
        const bass = 24;
        const range = 96;
        const instrument = 1;
        const pitch_step = range / cells.length;
        let duration = time_step * 2.;
        const midi_velocity = 50;
        let score = Array();
        let top = bass + range;
        for (let i = 1; i < cells.length - 1; i++) {
            if (cells[i] == 1) {
                // For some reason the display is backwards.
                let tyme = generation * time_step;
                let midi_key = top - (i * pitch_step);
                let note = `i ${instrument} ${tyme} ${duration} ${midi_key} ${midi_velocity} 0 .5`;
                score.push(note);
            }
        }
        let notes = score.join("\\n");
        csound.ReadScore(notes);
    }
    
    $(document).ready(function() {
        //////////////////////////////////////////////////////////////////////
        // This is the JSON-RPC proxy of the instance of Csound that is 
        // performing this piece.
        //////////////////////////////////////////////////////////////////////
        csound = new Csound(origin);
        message_callback_ = function(message) {
            let notifications_textarea = document.getElementById("csound_diagnostics");
            let existing_notifications = notifications_textarea.value;
            notifications_textarea.value = existing_notifications + message;
            notifications_textarea.scrollTop = notifications_textarea.scrollHeight;
        }; 
        csound.SetMessageCallback(message_callback_, true);
        $('input').on('input', async function(event) {
            var slider_value = parseFloat(event.target.value);
            csound.SetControlChannel(event.target.id, slider_value);
            var output_selector = '#' + event.target.id + '_output';
            $(output_selector).val(slider_value);
            csound.Message(event.target.id + " = " + event.target.value + "\\n");
            if (event.target.id == 'gk_rule') {
                ruleset = decimal_to_ruleset(slider_value, 8);
                csound.Message(`rule_number ${rule_number} = binary rule ${ruleset}\n`);
            }
        });
        $('#start').on('click', async function() {
            rule_number = $('#gk_rule').val();
            ruleset = decimal_to_ruleset(rule_number, 8);
            csound.Message(`rule_number ${rule_number} = binary rule ${ruleset}\n`);
            continue_generating_score = true;
            for (let i = 0; i < generations; i++) {
                draw_();
            }
        });
        $('#stop').on('click', async function() {
            continue_generating_score = false;
            noCanvas();
            generation = 0;
            setup();
        });
        $('#save_controls').on('click', async function() {
            $('.persistent-element').each(function() {
                localStorage.setItem(this.id, this.value);
            });
        });
        $('#restore_controls').on('click', async function() {
            $('.persistent-element').each(function() {
                this.value = localStorage.getItem(this.id);
                csound.SetControlChannel(this.id, parseFloat(this.value));
                var output_selector = '#' + this.id + '_output';
                $(output_selector).val(this.value);
            });
        });
        $('#start').click();
     });
</script>
</body>
</html>
}}

; For Linux, uncomment and if necessary edit this line:
; gi_webserver webserver_create "/home/mkg//csound-webserver-opcodes/examples/", 8080, 0

; For macOS, uncomment and if necessary edit this line:
gi_webserver webserver_create "/Users/michaelgogins/csound-webserver-opcodes/examples/", 8080, 0

webserver_open_html gi_webserver, gS_html

</CsInstruments>
<CsScore>
i "Exit" 360 1
</CsScore>
</CsoundSynthesizer>

