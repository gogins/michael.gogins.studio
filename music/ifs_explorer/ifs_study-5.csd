<CsoundSyntheizer>
<CsLicense>
IFS Study No. 4
Michael Gogins, 2021

This piece demonstrates the use of the Faust opcodes, the Clang opcodes, the 
vst4cs opcodes, the signal flow graph opcodes, and the WebKit opcodes, all in 
one .csd file.

Comments are provided in the piece. 
</CsLicense>
<CsOptions>
-m0 -d -RWfoifs_study-4.wav"
</CsOptions>
<CsInstruments>

sr = 48000
ksmps = 128
nchnls = 2
0dbfs = 20

// VST plugins must be loaded ahead of instruments that use them.

gi_Pianoteq vstinit "/home/mkg/Pianoteq\ 7/x86-64bit/Pianoteq\ 7.so", 0
gi_Mverb2020 vstinit "/home/mkg/.local/lib/Mverb2020.so", 1

// The following C++ code defines two invokable "opcodes" that compute a 
// phase-synchronous Gaussian chirp grain, or a phase-synchronous cosine 
// grain.

S_grain_code init {{

void* __dso_handle = (void *)&__dso_handle;

static bool diagnostics_enabled = false;

#include "clang_invokable.hpp"
#include <cmath>
#include <complex>
#include <csound/csdl.h>
#include <cstdio>

/**
 * Synthesize a Jones-Parks grain, i.e. a Gaussian chirp, which can be a 
 * simple grain without any actual chirp. If the synchronous_phase argument is 
 * true, then all grains of the same frequency will have synchronous phases, 
 * which can be useful in avoiding certain artifacts.
 *
 * The algorithm uses an efficient difference equation.
 */
struct InvokableGrain : public ClangInvokableBase {
    virtual ~InvokableGrain() {
    };
    int init(CSOUND *csound_, OPDS *opds_, MYFLT **outputs, MYFLT **inputs) override {
        if (diagnostics_enabled) std::fprintf(stderr, ">>>>>>> InvokableGrain::init...\\n");
        int result = OK;
        csound = csound_;
        opds = opds_;
        // Inputs:
        center_time_seconds = *(inputs[0]);
        duration_seconds = *(inputs[1]);
        starting_frequency_hz = *(inputs[2]);
        center_frequency_hz = *(inputs[3]);
        center_amplitude = *(inputs[4]);
        center_phase_offset_radians = *(inputs[5]);
        if (*(inputs[6]) != 0.) {
            synchronous_phase = true;
        } else {
            synchronous_phase = false;
        }
        if (synchronous_phase) {
            wavelength_seconds = 1.0 / center_frequency_hz;
            wavelengths = center_time_seconds / wavelength_seconds;
            whole_cycles = 0;
            fractional_cycle = std::modf(wavelengths, &whole_cycles);
            center_phase_offset_radians = 2.0 * M_PI * fractional_cycle;
        }
        center_time = - (duration_seconds / 2.0);
        sampling_rate = csound->GetSr(csound);
        sampling_interval = 1.0 / double(sampling_rate);
        frame_count = size_t(2.0 * duration_seconds / sampling_interval);
        gaussian_width = std::exp(1.0) / std::pow(duration_seconds / 4.0, 2.0);
        ending_frequency_hz = center_frequency_hz + (center_frequency_hz - starting_frequency_hz);
        chirp_rate = (ending_frequency_hz - starting_frequency_hz) / duration_seconds;
        omega = 2.0 * M_PI * center_frequency_hz;
        c0 = std::complex<double>(std::log(center_amplitude) - (gaussian_width * std::pow(center_time, 2.0)), 
                                (center_phase_offset_radians - (chirp_rate / 2.0) * center_time) - (omega * center_time));
        c1 = std::complex<double>(-2.0 * gaussian_width * sampling_interval * center_time,
                                - (sampling_interval * (chirp_rate * center_time + omega)));
        c2 = std::complex<double>(-std::complex<double>(gaussian_width, chirp_rate / 2.0)) * std::pow(sampling_interval, 2.0);
        exp_2_c2 = std::exp(2.0 * c2);
        h0 = std::exp(c1 + c2);
        h1 =std::complex<double>(0.0, 0.0);
        f0 = std::exp(c0);
        f1 = std::complex<double>(0.0, 0.0);
        if (diagnostics_enabled) std::fprintf(stderr, ">>>>>>> InvokableGrain::init.\\n");
        return result;
    }
    // The difference equation.
    MYFLT tick() {
        MYFLT sample = f0.real();
        h1 = h0 * exp_2_c2;
        h0 = h1;
        f1 = h1 * f0;
        f0 = f1;
        return sample;
    }
    int kontrol(CSOUND *csound_, MYFLT **outputs, MYFLT **inputs) override {
        int result = OK;
        int frame_index = 0;
        for( ; frame_index < kperiodOffset(); ++frame_index) {
            outputs[0][frame_index] = 0;
        }
        for( ; frame_index < kperiodEnd(); ++frame_index) {
            MYFLT sample = tick();
            outputs[0][frame_index] = sample;
        }
        for( ; frame_index < ksmps(); ++frame_index) {
            outputs[0][frame_index] = 0;
        }
        return result;
    }
    double center_time_seconds;
    double duration_seconds;
    double starting_frequency_hz;
    double center_frequency_hz;
    double center_amplitude;
    double center_phase_offset_radians;
    bool synchronous_phase;
    double wavelength_seconds;
    double wavelengths;
    double whole_cycles;
    double fractional_cycle;
    double center_time;
    int sampling_rate;
    double sampling_interval;
    size_t frame_count;
    double gaussian_width;
    double ending_frequency_hz;
    double chirp_rate;
    double omega;
    // Difference equation terms.
    std::complex<double> c0;
    std::complex<double> c1;
    std::complex<double> c2;
    std::complex<double> exp_2_c2;
    std::complex<double> h0;
    std::complex<double> h1;
    std::complex<double> f0;
    std::complex<double> f1;
};

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
struct InvokableCosineGrain : public ClangInvokableBase {
    virtual ~InvokableCosineGrain() {
    };
    int init(CSOUND *csound_, OPDS *opds_, MYFLT **outputs, MYFLT **inputs) override {
        if (diagnostics_enabled) std::fprintf(stderr, ">>>>>>> InvokableCosineGrain::init...\\n");
        int result = OK;
        csound = csound_;
        opds = opds_;
        // Inputs.
        center_time_seconds = *(inputs[0]);
        duration_seconds = *(inputs[1]);
        frequency_hz = *(inputs[2]);
        amplitude = *(inputs[3]);
        phase_offset_radians = *(inputs[4]);
        if (*(inputs[5]) != 0.) {
            synchronous_phase = true;
        } else {
            synchronous_phase = false;
        }
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
        // The initial frame.
        sinusoid_1 = std::cos(phase_offset_radians);
        // What would have been the previous frame.
        sinusoid_2 = std::cos(-sinusoid_radians_per_frame + phase_offset_radians);
        // The envelope is exactly 1 cycle of a cosine sinusoid, offset by -1.
        envelope_frequency_hz = 1.0 / duration_seconds;
        envelope_radians_per_frame = 2.0 * M_PI * envelope_frequency_hz / frames_per_second;
        envelope_coefficient = 2.0 * std::cos(envelope_radians_per_frame);
        // The initial frame.
        envelope_1 = std::cos(0.0);
        // What would have been the previous frame.
        envelope_2 = std::cos(-envelope_radians_per_frame);
        signal = 0.0;
        temporary = 0.0;
        return result;
    }
    MYFLT tick(MYFLT  **inputs) override {
        signal = (sinusoid_1 * (envelope_1 - 1.0)) * amplitude;
        temporary = sinusoid_1;
        sinusoid_1 = sinusoid_coefficient * sinusoid_1 - sinusoid_2;
        sinusoid_2 = temporary;
        temporary = envelope_1;
        envelope_1 = envelope_coefficient * envelope_1 - envelope_2;
        envelope_2 = temporary;
        return signal;
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
    
    ClangInvokable *grain_factory() {
        if (diagnostics_enabled) std::fprintf(stderr, ">>>>>>> This is \\"grain_factory\\".\\n");
        auto result = new InvokableGrain;
        if (diagnostics_enabled) std::fprintf(stderr, ">>>>>>> \\"grain_factory\\" created %p.\\n", result);
        return result;
    }
    
    ClangInvokable *cosine_grain_factory() {
        if (diagnostics_enabled) std::fprintf(stderr, ">>>>>>> This is \\"cosine_grain_factory\\".\\n");
        auto result = new InvokableCosineGrain;
        if (diagnostics_enabled) std::fprintf(stderr, ">>>>>>> \\"cosine_grain_factory\\" created %p.\\n", result);
        return result;
    }

};

}}

i_result clang_compile "grain_main", S_grain_code, "-g -Ofast -march=native -std=c++14 -I/home/mkg/clang-opcodes -I/usr/local/include/csound  -I. -stdlib=libstdc++", "/usr/lib/gcc/x86_64-linux-gnu/9/libstdc++.so /usr/lib/gcc/x86_64-linux-gnu/9/libgcc_s.so /usr/local/lib/libstk.so /usr/lib/x86_64-linux-gnu/libm.so /usr/lib/x86_64-linux-gnu/libpthread.so"

// Instruments are defined in blocks, along with their signal flow graph 
// connections, the initial values of their control parameters, and whether 
// they are "always on." 
//
// This ensures that the whole block for an instrument can be cut and pasted 
// as a unit without affecting other instruments.

#include "FMWaterBell.inc"
connect "FMWaterBell", "outleft",  "Mverb2020", "inleft"
connect "FMWaterBell", "outright", "Mverb2020", "inright"
gk_FMWaterBell_level init 0
gi_FMWaterBell_attack init 0.002
gi_FMWaterBell_release init 0.01
gi_FMWaterBell_sustain init 20
gi_FMWaterBell_sustain_level init .1
gk_FMWaterBell_index init .5
gk_FMWaterBell_crossfade init .5
gk_FMWaterBell_vibrato_depth init 0.05
gk_FMWaterBell_vibrato_rate init 6
gk_FMWaterBell_midi_dynamic_range init 20

#include "FaustBubble.inc"
connect "FaustBubble", "outleft",  "Mverb2020", "inleft"
connect "FaustBubble", "outright", "Mverb2020", "inright"
gk_FaustBubble_level init 0

#include "Rhodes.inc"
connect "Rhodes", "outleft",  "Mverb2020", "inleft"
connect "Rhodes", "outright", "Mverb2020", "inright"
gk_Rhodes_level init 0

#include "ZakianFlute.inc"
connect "ZakianFlute", "outleft",  "Mverb2020", "inleft"
connect "ZakianFlute", "outright", "Mverb2020", "inright"
gk_ZakianFlute_midi_dynamic_range init 20
gk_ZakianFlute_level init 0

#include "PianoNotePianoteq.inc"

gk_CosineGrain_level chnexport "gk_CosineGrain_level", 3
gk_CosineGrain_midi_dynamic_range chnexport "gk_CosineGrain_midi_dynamic_range", 3
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
i_level_correction = 90
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_CosineGrain_level)

; Grain inputs.
i_center_time_seconds init i_time
i_duration_seconds init i_duration
i_frequency_hz init i_frequency
i_amplitude init i_amplitude
i_phase_offset_radians init 0
i_synchronous_phase init 1
a_signal clang_invoke "cosine_grain_factory", 3, i_center_time_seconds, i_duration_seconds, i_frequency_hz, i_amplitude, i_phase_offset_radians, i_synchronous_phase 
a_signal = a_signal * k_gain
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
; printks "a_signal: %9.4f a_out_left: %9.4f a_out_right: %9.4f\\n", 0, k(a_signal), k(a_out_left), k(a_out_right)
outleta "outleft", a_out_left
outleta "outright", a_out_right
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin
connect "CosineGrain", "outleft",  "Mverb2020", "inleft"
connect "CosineGrain", "outright", "Mverb2020", "inright"
gk_CosineGrain_level init 0
gk_CosineGrain_midi_dynamic_range init 20

gk_JonesParksGrain_level chnexport "gk_JonesParksGrain_level", 3
gk_JonesParksGrain_midi_dynamic_range chnexport "gk_JonesParksGrain_midi_dynamic_range", 3
instr JonesParksGrain
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_JonesParksGrain_midi_dynamic_range)
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
k_gain = ampdb(gk_JonesParksGrain_level)

; Grain inputs.
i_center_time_seconds init i_time
i_duration_seconds init i_duration
i_starting_frequency_hz init i_frequency
i_center_frequency_hz init i_frequency
i_center_amplitude init i_amplitude
i_center_phase_offset_radians init 0
i_synchronous_phase init 1
a_signal clang_invoke "grain_factory", 3, i_center_time_seconds, i_duration_seconds, i_starting_frequency_hz, i_center_frequency_hz, i_center_amplitude, i_center_phase_offset_radians, i_synchronous_phase 
a_signal = a_signal * k_gain
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
; printks "a_signal: %9.4f a_out_left: %9.4f a_out_right: %9.4f\\n", 0, k(a_signal), k(a_out_left), k(a_out_right)
outleta "outleft", a_out_left
outleta "outright", a_out_right
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin
connect "JonesParksGrain", "outleft",  "Mverb2020", "inleft"
connect "JonesParksGrain", "outright", "Mverb2020", "inright"
gk_JonesParksGrain_level init 0
gk_JonesParksGrain_midi_dynamic_range init 20

#include "PianoOutPianoteq.inc"
connect "PianoOutPianoteq", "outleft",  "Mverb2020", "inleft"
connect "PianoOutPianoteq", "outright", "Mverb2020", "inright"
gk_PianoOutPianoteq_level init -25
alwayson "PianoOutPianoteq"

#include "Mverb2020.inc"
connect "Mverb2020", "outleft",  "MasterOutput", "inleft"
connect "Mverb2020", "outright", "MasterOutput", "inright"
gk_Mverb2020_level init 0
gk_Mverb2020_Mix init .5
gk_Mverb2020_Pre_delay init 0.5
gk_Mverb2020_Early_late_mix init 0.5
gk_Mverb2020_Size init 0.5
gk_Mverb2020_Density init 0.5
gk_Mverb2020_Bandwith_Frequency init 0.5
gk_Mverb2020_Decay init 0.85
gk_Mverb2020_Damping_Frequency init 0.5
gk_Mverb2020_Gain init 1
gi_Mverb2020_Program init 2
alwayson "Mverb2020"

#include "MasterOutput.inc"
gk_MasterOutput_level init 10
gS_MasterOutput_filename init ""
alwayson "MasterOutput"

// This instrument defines a WebKit browser that provides sliders for 
// real-time tweaking of instrument parameters, as well as a display of 
// Csound's diagnostic messages.
//
// Control values that are saved by clicking the "Save" button will
// automatically be restored on the next run of Csound.

instr Browser

// The following HTML5 code is pretty much the standard sort of thing for Web 
// pages.
//
// However, the <csound.js> script brings a proxy for the instance of Csound 
// that is performing into the JavaScript context of the Web page, so the 
// event handlers of the sliders on the page can call Csound to set control 
// channel values.

gS_html init {{<!DOCTYPE html>
<html>
<head>
    <title>Iterated Function System Study No. 4</title>
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
        height: 12px;
        width: 12px;
        border-radius: 50%;
        box-shadow: inset 0 0 7px #234;
        background: chartreuse;
        margin-top: -4px;
        border-radius: 10px;
    }
    table td {
        border-width: 2px;
        padding: 6px;
        border-style: solid;
        border-color: transparent;
        color:yellow;
        background-color: teal;
        font-family: sans-serif
    }
    table th {
        border-width: 2px;
        padding: 6px;
        border-style: solid;
        border-color: transparent;
        color:white;
        background-color:teal;
         font-family: sans-serif
   }
    textarea {
        border-width: 2px;
        padding: 6px;
        border-style: solid;
        border-color: transparent;
        color:chartreuse;
        background-color:black;
        font-size:10pt;
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
    </style>
</head>
<body style="background-color:CadetBlue;box-sizing:border-box;padding:10px;:fullscreen">
    <script>
        csound_message = function(message) {
            if (typeof webkit_initialized == "undefined" || webkit_initialized === null) {
                return;
            }
            let messages_textarea = document.getElementById("console");
            if (typeof messages_textarea == "undefined" || messages_textarea === null) {
                return;
            }
            let existing = messages_textarea.value;
            messages_textarea.value = existing + message;
            messages_textarea.scrollTop = messages_textarea.scrollHeight;
        };
    </script>
    <h2>Iterated Function System Study No. 5</h2>
    <h3>Michael Gogins, 2021
    <input type="button" id='save' value="Save" />
    <input type="button" id='restore' value="Restore" />
    </h3>
    <form id='persist'>
    <p>
    <table>
    <col>
    <col>
    <tr>
    <td colspan=2>
    <textarea class="code" id="console" rows=15 style="width:99%;">
    </textarea>
    </tr>
    <td>
    <table>
    <col width="2*">
    <col width="5*">
    <col width="100px">
        
    <tr>
    <td>
    <label for=gk_FMWaterBell_level>FMWaterBell level (dB)</label>
    <td>
    <input class=persistent-element type=range min=-40 max=40 value=0 id=gk_FMWaterBell_level step=.001>
    <td>
    <output for=gk_FMWaterBell_level id=gk_FMWaterBell_level_output>0</output>
    </tr>
    <tr>
    <td>
    <label for=i>FMWaterBell index</label>
    <td>
    <input class=persistent-element type=range min=0 max=20 value=0 id=gk_FMWaterBell_index step=.001>
    <td>
    <output for=gk_FMWaterBell_index id=gk_FMWaterBell_index_output>0</output>
    </tr>
    <tr>
    <td>
    <label for=i>FMWaterBell crossfade</label>
    <td>
    <input class=persistent-element type=range min=0 max=1 value=0 id=gk_FMWaterBell_crossfade step=.001>
    <td>
    <output for=gk_FMWaterBell_crossfade id=gk_FMWaterBell_crossfade_output>0</output>
    </tr>
    <tr>
    <td>
    <label for=i>FMWaterBell vibrato depth</label>
    <td>
    <input class=persistent-element type=range min=0 max=10 value=0 id=gk_FMWaterBell_vibrato_depth step=.001>
    <td>
    <output for=gk_FMWaterBell_vibrato_depth id=gk_FMWaterBell_vibrato_depth_output>0</output>
    </tr>
    <tr>
    <td>
    <label for=i>FMWaterBell vibrato rate</label>
    <td>
    <input class=persistent-element type=range min=0 max=10 value=0 id=gk_FMWaterBell_vibrato_rate step=.001>
    <td>
    <output for=gk_FMWaterBell_vibrato_rate id=gk_FMWaterBell_vibrato_rate_output>0</output>
    </tr>
    
    <tr/>
    <tr>
    <td>
    <label for=gk_FaustBubble_level>FaustBubble output level (dB)</label>
    <td>
    <input class=persistent-element type=range min=-40 max=40 value=-6 id=gk_FaustBubble_level step=.001>
    <td>
    <output for=gk_FaustBubble_level id=gk_FaustBubble_level_output>-6</output>
    <tr>
    <td>
    <label for=gk_FaustBubble_midi_dynamic_range>FaustBubble MIDI dynamic range</label>
    <td>
    <input class=persistent-element type=range min=0 max=127 value=20 id=gk_FaustBubble_midi_dynamic_range step=.001>
    <td>
    <output for=gk_FaustBubble_midi_dynamic_range id=gk_FaustBubble_midi_dynamic_range_output>-6</output>
    </tr>
    
    <tr/>
    <tr>
    <td>
    <label for=gk_Rhodes_level>Rhodes output level (dB)</label>
    <td>
    <input class=persistent-element type=range min=-40 max=40 value=-6 id=gk_Rhodes_level step=.001>
    <td>
    <output for=gk_Rhodes_level id=gk_Rhodes_level_output>-6</output>
    </tr>
    
    <tr/>
    <tr>
    <td>
    <label for=gk_ZakianFlute_level>ZakianFlute output level (dB)</label>
    <td>
    <input class=persistent-element type=range min=-40 max=40 value=-6 id=gk_ZakianFlute_level step=.001>
    <td>
    <output for=gk_ZakianFlute_level id=gk_ZakianFlute_level_output>-6</output>
    <tr>
    <td>
    <label for=gk_ZakianFlute_midi_dynamic_range>ZakianFlute MIDI dynamic range</label>
    <td>
    <input class=persistent-element type=range min=0 max=127 value=20 id=gk_ZakianFlute_midi_dynamic_range step=.001>
    <td>
    <output for=gk_ZakianFlute_midi_dynamic_range id=gk_ZakianFlute_midi_dynamic_range_output>-6</output>
    </tr>
    
    <tr/>
    <tr>
    <td>
    <label for=gk_PianoOutPianoteq_level>Pianoteq output level (dB)</label>
    <td>
    <input class=persistent-element type=range min=-40 max=40 value=-6 id=gk_PianoOutPianoteq_level step=.001>
    <td>
    <output for=gk_PianoOutPianoteq_level id=gk_PianoOutPianoteq_level_output>-6</output>
    </tr>
    </table>
    
    <td>
    <table>
    
    <tr/>
    <tr>
    <td>
    <label for=gk_CosineGrain_level>Cosine grain output level (dB)</label>
    <td>
    <input class=persistent-element type=range min=-40 max=40 value=0 id=gk_CosineGrain_level step=.001>
    <td>
    <output for=gk_CosineGrain_level id=gk_CosineGrain_level_output>0/output>
    </tr>
    <tr>
    <td>
    <label for=gk_CosineGrain_MIDI_dynamic_range>Cosine grain MIDI dynamic range (dB)</label>
    <td>
    <input class=persistent-element type=range min=0 max=127 value=20 id=gk_CosineGrain_MIDI_dynamic_range step=.001>
    <td>
    <output for=gk_CosineGrain_MIDI_dynamic_range id=gk_CosineGrain_MIDI_dynamic_range_output>-6</output>
    </tr>

    <tr>
    <td>
    <label for=gk_Mverb2020_level>Mverb2020 reverb level (dB)</label>
    <td>
    <input class=persistent-element type=range min=-40 max=40 value=-6 id=gk_Mverb2020_level step=.001>
    <td>
    <output for=gk_Mverb2020_level id=gk_Mverb2020_level_output>-6</output>
    </tr>
    
    <tr>
    <td>
    <label for=gk_Mverb2020_Mix>Mverb2020 reverb Mix</label>
    <td>
    <input class=persistent-element type=range min=0 max=1 value=.5 id=gk_Mverb2020_Mix step=.001>
    <td>
    <output for=gk_Mverb2020_Mix id=gk_Mverb2020_Mix_output>.5</output>
    </tr>
    
    <tr>
    <td>
    <label for=gk_Mverb2020_Decay>Mverb2020 reverb Decay</label>
    <td>
    <input class=persistent-element type=range min=0 max=1 value=.5 id=gk_Mverb2020_Decay step=.001>
    <td>
    <output for=gk_Mverb2020_Decay id=gk_Mverb2020_Decay_output>.5</output>
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
    </tr>
    </table>
    </form>   
</script>
<script src="https://code.jquery.com/jquery-3.6.0.js" integrity="sha256-H+K7U5CnXl1h5ywQfKtSj8PCmoN9aaq30gDh27Xc0jk=" crossorigin="anonymous"></script>
<script src="csound.js"></script>
<script>   
    $(document).ready(function() {
        var csound = new Csound("http://localhost:8383");
        $('input').on('input', function(event) {
            var slider_value = parseFloat(event.target.value);
            csound.SetControlChannel(event.target.id, slider_value);
            var output_selector = '#' + event.target.id + '_output';
            $(output_selector).val(slider_value);
        });
        $('#save').on('click', function() {
            $('.persistent-element').each(function() {
                localStorage.setItem(this.id, this.value);
            });
        });
        $('#restore').on('click', function() {
            $('.persistent-element').each(function() {
                this.value = localStorage.getItem(this.id);
                csound.SetControlChannel(this.id, parseFloat(this.value));
                var output_selector = '#' + this.id + '_output';
                $(output_selector).val(this.value);
            });
        });
        $('#restore').click();
    });
    webkit_initialized = true;
</script>
</body>
</html>
}}

gi_browser webkit_create 8383, 0
// The following lines find the current working directory from Csound 
// and then use that for the base URI of the Web page.
S_pwd pwd
S_base_uri sprintf "file://%s/", S_pwd
prints S_base_uri
webkit_open_html gi_browser, "Iterated Function System Study No. 4", gS_html, S_base_uri, 12000, 10000, 0
endin
alwayson "Browser"

// The following C++ code defines and executes a score generator that 
// implements the "multiple copy reducing machine" algorithm for computing an 
// iterated function system (IFS).
//
// Only a limited number of iterations are computed. On the final iteration, 
// each point in the attractor of the IFS is translated to a single note of 
// music.
//
// This code uses only the Eigen 3 header file-only library and the C++ 
// standard library.

S_score_generator_code init {{

#include <eigen3/Eigen/Dense>
#include <csound/csdl.h>
#include <iostream>
#include <cstdio>
//#include <sstream>
#include <random>
#include <vector>
#include "clang_invokable.hpp"

/**
 * Multiple Copy Reducing Machine for dimensions:
 * 0 instrument
 * 1 time
 * 2 duration
 * 3 key
 * 4 velocity
 * 5 pan
 * 6 homogeneity
 */
 
typedef Eigen::Matrix<double, 7, 1> Note;
typedef Eigen::Matrix<double, 7, 7> Transformation;
typedef std::vector<Note> Score;
 
struct Scaling {
    Note minima;
    Note maxima;
    Note ranges;
};

void update_bounds(Scaling &scaling, const Note &note) {
    for (int i = 0; i < 6; ++i) {
        if (note[i] < scaling.minima[i]) {
            scaling.minima[i] = note[i];
        }
        if (note[i] > scaling.maxima[i]) {
            scaling.maxima[i] = note[i];
        }
    }
    scaling.ranges = scaling.maxima - scaling.minima;
}

void multiple_copy_reducing_machine(const Note &note, const std::vector<Transformation> &hutchinson, Score &score, int depth) {
    --depth;
    if (depth < 0) {
        return;
    }
    for (const auto &transformation : hutchinson) {
        auto new_note = transformation * note;
        if (depth == 1) {
            score.push_back(new_note);
        }
        multiple_copy_reducing_machine(new_note, hutchinson, score, depth);
    }
}

void print_note(const Note &note) {
    std::fprintf(stderr, "note: i: %9.4f t: %9.4f d: %9.4f k: %9.4f v: %9.4f p: %9.4f\\n", note[0], note[1], note[2], note[3], note[4], note[5]);
}

void rescale(Scaling &scaling, Score &score, int dimension, bool rescale_minimum, bool rescale_range, double minimum, double range) {
    scaling.minima = score.front();
    scaling.maxima = score.front();
    for (const auto &note : score) {
        update_bounds(scaling, note);
    }
    std::fprintf(stderr, "rescale: dimension: %2d rescale minimum: %d rescale range: %d actual minimum: %9.4f actual range: %9.4f target_minimum: %9.4f target_range: %9.4f\\n", dimension, rescale_minimum, rescale_range, scaling.minima[dimension], scaling.ranges[dimension], minimum, range);
    double scaling_factor = range / scaling.ranges[dimension];
    double move_to_origin = scaling.minima[dimension];
    for (auto &note : score) {
         // Move note to origin.
        double value = note[dimension];
        value -= move_to_origin;
        // Rescale to fit target range.
        if (scaling.ranges[dimension]  != 0.) {
            value *= scaling_factor;
        }
        // Move back from origin to target.
        value += minimum;
        note[dimension] = value;
    }
}

void rescale_time_and_duration(Score &score, double starting_time, double total_duration) {
    std::fprintf(stderr, "rescale_time_and_duration: starting_time: %9.4f total_duration: %9.4f\\n", starting_time, total_duration);
    // Handle negative durations.
    for (auto &note : score) {
        auto start = note[1];
        auto duration = note[2];
        if (duration < 0.) {
            duration = std::fabs(duration);
            start = start - duration;
            note[1] = start;
            note[2] = duration;
        }
     }
    // Find the total duration of that part of the score that contains notes.
    double minimum_start = score.front()[1];
    double maximum_end = minimum_start + score.front()[2];
    for (auto &note : score) {
        auto start = note[1];
        auto duration = note[2];
        auto end = start + duration;
        if (start < minimum_start) {
            minimum_start = start;
        }
        if (end > maximum_end) {
            maximum_end = end;
        }
    }
    auto actual_duration = maximum_end - minimum_start; 
    auto rescaling_factor = total_duration / actual_duration;
    std::fprintf(stderr, "rescale_time_and_duration: actual_duration: %9.4f rescaling_factor: %9.4f\\n", actual_duration, rescaling_factor);
    // Move the notes to the origin, rescale times and durations, 
    // and move the notes to the target starting time.
    for (auto &note : score) {
        auto start = note[1];
        auto duration = note[2];
        start = start - minimum_start;
        start = start * rescaling_factor;
        duration = duration * rescaling_factor;
        start = start + starting_time;
        note[1] = start;
        note[2] = duration;
        //print_note(note);
    }
}

struct {
    bool operator()(const Note &a, const Note &b) const 
    { 
        if (a[1] < b[1]) {
            return true;
        } else if (a[1] > b[1]) {
            return false;
        }
        if (a[2] < b[2]) {
            return true;
        } else if (a[2] > b[2]) {
            return false;
        }
        if (a[3] < b[3]) {
            return true;
        } else if (a[3] > b[3]) {
            return false;
        }
        if (a[0] < b[0]) {
            return true;
        } else if (a[0] > b[0]) {
            return false;
        }
        return false;
    }
} Note_less;

void to_csound_score(CSOUND *csound, Score &score, bool twelve_tet=false) {
    // Sort score.
    std::sort(score.begin(), score.end(), Note_less);
    // Randomize all stereo pans.
    std::mt19937 mersenne_twister(49850);
    std::uniform_real_distribution<double> random_pan(.05, .95);
    // Combine overlapping notes (same instrument and MIDI key, simultaneously sounding).
    for (int laterI = score.size() - 1; laterI > 1; --laterI) {
        auto &laterEvent = score[laterI];
        for (int earlierI = laterI - 1; earlierI > 0; --earlierI) {
            auto &earlierEvent = score[earlierI];
            if (earlierEvent[3] != laterEvent[3]) {
                continue;
            }
            if (earlierEvent[4] <= 0.0 || laterEvent[4] <= 0.0) {
                continue;
            }
            if ((earlierEvent[1] + earlierEvent[2]) < laterEvent[1]) {
                continue;
            }
            if (int(earlierEvent[0]) != int(laterEvent[0])) {
                continue;
            }
            // Ok, must be tied.
            std::fprintf(stderr, "erasing %d...\\n", laterI);
            auto later_event_off_time = laterEvent[1] + laterEvent[2];
            earlierEvent[2] = later_event_off_time = earlierEvent[1];
            score.erase(score.begin() + laterI);
            break;
        }
    }
    EVTBLK evtblk;
    std::memset(&evtblk, 0, sizeof(EVTBLK));
    for (auto &note : score) {
        evtblk.strarg = nullptr;
        evtblk.scnt = 0;
        evtblk.opcod = 'i';
        evtblk.pcnt = 7;
        evtblk.p[1] = note[0];
        evtblk.p[2] = note[1];
        evtblk.p[3] = note[2];
        auto midi_key = note[3];
        if (twelve_tet == true) {
            midi_key = std::round(midi_key);
            note[3] = midi_key;
        }
        evtblk.p[4] = note[3];
        evtblk.p[5] = note[4];
        evtblk.p[6] = 0.;
        evtblk.p[7] = random_pan(mersenne_twister);
        //print_note(note);
        int result = csound->insert_score_event(csound, &evtblk, 0.);
    }
    std::fprintf(stderr, "to_csound_score: generated %ld notes.\\n", score.size());
}

extern "C" { 

void webkit_execute(int browser_handle, const char *javascript_code);

// This function calls back into the Web page with Csound's diagnostic messages.

void csound_message_callback(CSOUND *csound, int attr, const char *format, va_list valist) {
    char message[0x2000];
    std::vsnprintf(message, 0x2000, format, valist);
    // Print the message also to stderr.
    std::fprintf(stderr, "%s", message);
    char javascript[0x3000];
    std::sprintf(javascript, "csound_message(`%s`);", message);
    // Calling JavaScript in the Web page is not immediately possible due to its 
    // asynchronous initialization. The actual number of calls to skip must be 
    // determined by trial and error.
    static int call_count = 0;
    if (call_count >=39) {
        webkit_execute(0, javascript);
    }
    call_count++;
}

int score_generator(CSOUND *csound) {
    int result = OK;
    csound->SetMessageCallback(csound, csound_message_callback);
    // Notes are column vectors. Notes and transformations are homogeneous.
    Note note;
    note << 1., 0., .1, 60., 60., .5, 1.;
    std::cerr << "initial note: " << std::endl << note << std::endl;
    std::vector<Transformation> hutchinson;
    hutchinson.resize(4);
    /*                 i   t   d   k   v   p   T          */
    hutchinson[0] <<  .5,  0,  0,  0,  0,  0, -1,    /* i */
                       0, .25, 0,  0,  0,  0,  0,    /* t */
                       0,  0, .5,  0,  0,  0,  0,    /* d */
                       0,  0,  0, .25, 0,  0,  0,    /* k */
                       0,  0,  0,  0, .5,  0,  0,    /* v */
                       0,  0,  0,  0,  0, .5,  0,    /* p */
                       0,  0,  0,  0,  0,  0,  1;    /* H */
    /*                 i   t   d   k   v   p   T          */
    hutchinson[1] <<  .5,  0,  0,  0,  0,  0, .15,   /* i */
                       0, .65, 0,  0,  0,  0,  1,    /* t */
                       0,  0, .5,  0,  0,  0,  0,    /* d */
                       0,  0,  0, .5,  0,  0,  0,    /* k */
                       0,  0,  0,  0, .5,  0,  0,    /* v */
                       0,  0,  0,  0,  0, .5,  0,    /* p */
                       0,  0,  0,  0,  0,  0,  1;    /* H */
    /*                 i   t   d   k   v   p   T          */
    hutchinson[2] <<  .5,  0,  0,  0,  0,  0,  0,    /* i */
                       0, .5,  0,  0,  0,  0,  0,    /* t */
                       0,  0, .175,0,  0,  0,  0,    /* d */
                       0,  0,  0, .5,  0,  0,  1.03, /* k */
                       0,  0,  0,  0, .5,  0,  0,    /* v */
                       0,  0,  0,  0,  0, .5,  0,    /* p */
                       0,  0,  0,  0,  0,  0,  1;    /* H */
    /*                 i   t   d   k   v   p   T          */
    hutchinson[3] <<  .5, .5, 0,  0,  0,  0,  .85,   /* i */
                       0, .5,  0,  0,  0,  0,  1.05, /* t */
                       0,  0, .5,  0,  0,  0,  0,    /* d */
                       0, -1.1,0, .5,  0,  0,  1,    /* k */
                       0,  0,  0,  0, .5,  0,  0,    /* v */
                       0,  0,  0,  0,  0, .5,  0,    /* p */
                       0,  0,  0,  0,  0,  0,  1;    /* H */
    Score score;
    Scaling scaling;
    multiple_copy_reducing_machine(note, hutchinson, score, 6);
    rescale(scaling, score, 0, true, true,  1.,     5.999);
    // Fit to the full range of a grand piano.
    rescale(scaling, score, 3, true, true, 21.,    88.0);
    rescale(scaling, score, 4, true, true, 60.,    30.0);
    rescale_time_and_duration(score, 2., 240.);
    rescale(scaling, score, 2, true, true,  .1,     8.0);
    to_csound_score(csound, score, true);
    return result;
};

};

}}

i_result clang_compile "score_generator", S_score_generator_code, "-g -O2 -std=c++14 -I/home/mkg/clang-opcodes -I/usr/local/include/csound -stdlib=libstdc++", "/usr/lib/gcc/x86_64-linux-gnu/9/libstdc++.so /usr/lib/gcc/x86_64-linux-gnu/9/libgcc_s.so /home/mkg/webkit-opcodes/webkit_opcodes.so /usr/lib/x86_64-linux-gnu/libm.so /usr/lib/x86_64-linux-gnu/libpthread.so"

</CsInstruments>
<CsScore>
// Because the score is generated programmatically, the intended duration of 
// of the performance must be set here.
f 0 244 
</CsScore>
</CsoundSynthesizer>