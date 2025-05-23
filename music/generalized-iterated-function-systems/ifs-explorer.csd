<CsoundSyntheizer>
<CsOptions>
--m-amps=1 --m-range=1 --m-dB=1 --m-benchmarks=1 --m-warnings=0 -+msg_color=0 -d -oifs-explorer.wav
</CsOptions>
<CsInstruments>

sr = 48000
ksmps = 128
nchnls = 2
0dbfs = 200

gS_os, gS_macros cxx_os
prints "Operating system: %s\n", gS_os

connect "FMWaterBell", "outleft",  "MasterOutput", "inleft"
connect "FMWaterBell", "outright", "MasterOutput", "inright"
connect "JonesParksGrain", "outleft",  "MasterOutput", "inleft"
connect "JonesParksGrain", "outright", "MasterOutput", "inright"
connect "CosineGrain", "outleft",  "MasterOutput", "inleft"
connect "CosineGrain", "outright", "MasterOutput", "inright"

alwayson "MasterOutput"

;#include "FMWaterBell.inc"

S_grain_code init {{

//void* __dso_handle = (void *)&__dso_handle;

static bool diagnostics_enabled = false;

#include "cxx_invokable.hpp"
#include <cmath>
#include <complex>
#include <csdl.h>
#include <cstdio>

/**
 * Synthesize a Jones-Parks grain, i.e. a Gaussian chirp, which can be a 
 * simple grain without any actual chirp. If the synchronous_phase argument is 
 * true, then all grains of the same frequency will have synchronous phases, 
 * which can be useful in avoiding certain artifacts.
 *
 * The algorithm uses an efficient difference equation.
 */
struct InvokableGrain : public CxxInvokableBase {
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
    
    CxxInvokable *grain_factory() {
        if (diagnostics_enabled) std::fprintf(stderr, ">>>>>>> This is \\"grain_factory\\".\\n");
        auto result = new InvokableGrain;
        if (diagnostics_enabled) std::fprintf(stderr, ">>>>>>> \\"grain_factory\\" created %p.\\n", result);
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
i_result cxx_compile "grain_main", S_grain_code, "clang++ -v -g -O2 -std=c++14 -shared -fPIC -DUSE_DOUBLE -I. -I/System/Volumes/Data/opt/homebrew/include/eigen3 -I/opt/homebrew/Cellar/csound/6.17.0_5/Frameworks/CsoundLib64.framework/Versions/6.0/Headers -I/opt/homebrew/Cellar/boost/1.79.0/include -I/Users/michaelgogins/csound-extended/CsoundAC -lpthread -lm", "libCsoundAC.dylib"
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

gk_JonesParksGrain_level chnexport "gk_JonesParksGrain_level", 3
gk_JonesParksGrain_midi_dynamic_range chnexport "gk_JonesParksGrain_midi_dynamic_range", 3

gk_JonesParksGrain_level init 0
gk_JonesParksGrain_midi_dynamic_range init 20
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
a_signal cxx_invoke "grain_factory", 3, i_center_time_seconds, i_duration_seconds, i_starting_frequency_hz, i_center_frequency_hz, i_center_amplitude, i_center_phase_offset_radians, i_synchronous_phase 
a_signal = a_signal * k_gain
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
; printks "a_signal: %9.4f a_out_left: %9.4f a_out_right: %9.4f\\n", 0, k(a_signal), k(a_out_left), k(a_out_right)
outleta "outleft", a_out_left
outleta "outright", a_out_right
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

#include "MasterOutput.inc"

S_score_generator_code init {{

#include <Silence.hpp>
#include <Eigen/Dense>
#include <csdl.h>
#include <iostream>
#include <cstdio>
#include <sstream>
#include <random>
#include <vector>

void* __dso_handle = (void *)&__dso_handle;

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

/**
 * An extended iterated function system consists of a set of 
 * functions; the action of the set is contractive. Each 
 * function is implemented with the following signature, and 
 * can be defined as a free function or as a closure. The 
 * function returns true if its application should be 
 * recursively iterated, or false if no further applications 
 * should be made. The prior_pen is the point of the set 
 * computed in the previous application. The function may 
 * compute a new point, pen, by any means. The function may
 * cease application either on reaching a depth of 0, or it 
 * may cease application when the distance between the pen
 * and the prior_pen is less than some minimum. The new pen
 * may be inserted into score at any time, but usually this 
 * is done when application has ceased.
 *
 * The implementation attempts to abstract from the definition 
 * of an iterated function system everything except its fixed point 
 * under recursive application.
 */

typedef std::function<bool(const csound::Event &prior_pen, const csound::Event &pen, int depth, csound::Score &score)> Contraction;

typedef std::vector<Contraction> Contractions;

void extended_ifs(bool &continuing, Contractions &contractions, const csound::Event &prior_pen, const csound::Event &pen, int depth, csound::Score &score) {
    if (continuing == false) {
        return;
    }
    continuing = false;
    for (auto &contraction : contractions) {
        auto continuing_ = contraction(prior_pen, pen, depth, score);
        if (continuing_ == true) {
            continuing = true;
        }
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
        print_note(note);
        if (duration < 0.) {
            duration = std::fabs(duration);
            start = start - duration;
            note[1] = start;
            note[2] = duration;
        }
        print_note(note);
        std::fprintf(stderr, "\\n");
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
        print_note(note);
        auto start = note[1];
        auto duration = note[2];
        start = start - minimum_start;
        start = start * rescaling_factor;
        duration = duration * rescaling_factor;
        start = start + starting_time;
        note[1] = start;
        note[2] = duration;
        print_note(note);
        std::fprintf(stderr, "\\n");
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

extern "C" int score_generator(CSOUND *csound) {
    csound->Message(csound, ">>>>>>> This is \\"score_generator\\".\\n");
    int result = OK;
    // Notes are column vectors. Notes and transformations are homogeneous.
    Note note;
    note << 1., 0., 1, 60., 60., .5, 1.;
    std::cerr << "initial note: " << std::endl << note << std::endl;
    std::vector<Transformation> hutchinson;
    hutchinson.resize(4);
    if (false) {
    /*                 i   t   d   k   v   p   T       */
    hutchinson[0] <<  .5,  0,  0,  0,  0,  0,  0, /* i */
                       0, .5,  0,  0,  0,  0,  0, /* t */
                       0,  0, .5,  0,  0,  0,  0, /* d */
                       0,  0,  0, .5,  0,  0,  0, /* k */
                       0,  0,  0,  0, .5,  0,  0, /* v */
                       0,  0,  0,  0,  0, .5,  0, /* p */
                       0,  0,  0,  0,  0,  0,  1; /* H */
    /*                 i   t   d   k   v   p   T       */
    hutchinson[1] <<  .5,  0,  0,  0,  0,  0,  0, /* i */
                       0, .5,  0,  0,  0,  0,  1, /* t */
                       0,  0, .5,  0,  0,  0,  0, /* d */
                       0,  0,  0, .5,  0,  0,  0, /* k */
                       0,  0,  0,  0, .5,  0,  0, /* v */
                       0,  0,  0,  0,  0, .5,  0, /* p */
                       0,  0,  0,  0,  0,  0,  1; /* H */
    /*                 i   t   d   k   v   p   T       */
    hutchinson[2] <<  .5,  0,  0,  0,  0,  0,  0, /* i */
                       0, .5,  0,  0,  0,  0,  0, /* t */
                       0,  0, .5,  0,  0,  0,  0, /* d */
                       0,  0,  0, .5,  0,  0,  1, /* k */
                       0,  0,  0,  0, .5,  0,  0, /* v */
                       0,  0,  0,  0,  0, .5,  0, /* p */
                       0,  0,  0,  0,  0,  0,  1; /* H */
    /*                 i   t   d   k   v   p   T       */
    hutchinson[3] <<  .5,  0,  0,  0,  0,  0,  0, /* i */
                       0, .5,  0,  0,  0,  0,  1, /* t */
                       0,  0, .5,  0,  0,  0,  0, /* d */
                       0,  0,  0, .5,  0,  0,  1, /* k */
                       0,  0,  0,  0, .5,  0,  0, /* v */
                       0,  0,  0,  0,  0, .5,  0, /* p */
                       0,  0,  0,  0,  0,  0,  1; /* H */
    }
    /*                 i   t   d   k   v   p   T          */
    hutchinson[0] <<  .5,  0,  0,  0,  0,  0,  0,    /* i */
                       0, .5,  0,  0,  0,  0,  0,    /* t */
                       0,  0, .425,0,  0,  0, .05,   /* d */
                       0,-.013,0, .5,  0,  0,  0,    /* k */
                       0,  0,  0,  0, .4,  0,  0,    /* v */
                       0,  0,  0,  0,  0, .5,  0,    /* p */
                       0,  0,  0,  0,  0,  0,  1;    /* H */
    /*                 i   t   d   k   v   p   T          */
    hutchinson[1] <<  .5,  0,  0,  0,  0,  0,  1,    /* i */
                       0, .5,  0,  0,  0,  0,  1,    /* t */
                       0,  0, .5,  0,  0,  0,  0,    /* d */
                       0,  0,  0,-.5, -1,  0,  0,    /* k */
                       0,  0,  0,  0, .5,  0,  0,    /* v */
                       0,  0,  0,  0,  0, .5,  0,    /* p */
                       0,  0,  0,  0,  0,  0,  1;    /* H */
    /*                 i   t   d   k   v   p   T          */
    hutchinson[2] <<  .5,  0,  0,  0,  0,  0,  1,    /* i */
                       0, .5,  0,  0,  0,  0,  0,    /* t */
                       0,  0, .525,0,  0,  0,  0,    /* d */
                       0,  0,  0, .5,  0,  0,  1.05, /* k */
                       0,  0,  0,  0, .45, 0,  0,    /* v */
                       0,  0,  0,  0,  0, .5,  0,    /* p */
                       0,  0,  0,  0,  0,  0,  1;    /* H */
    /*                 i   t   d   k   v   p   T          */
    hutchinson[3] <<  .5,  0,  0,  0,  0,  0,  0,    /* i */
                       0, .5,  0,  0,  0,  0,  1.05, /* t */
                       0,  0, .51, 0,  0,  0,  0,    /* d */
                       0, -1,  0, .512,0,  0,  1,    /* k */
                       0,  0,  0,  0, .45, 0,  0,    /* v */
                       0,  0,  0,  0,  0, .5,  0,    /* p */
                       0,  0,  0,  0,  0,  0,  1;    /* H */
                       
    Score score;
    Scaling scaling;
    multiple_copy_reducing_machine(note, hutchinson, score, 8);
    rescale(scaling, score, 0, true, true,  1.,     0.0);
    rescale(scaling, score, 3, true, true, 24.,    96.0);
    rescale(scaling, score, 4, true, true, 60.,    10.0);
    rescale_time_and_duration(score, 2., 60.);
    rescale(scaling, score, 2, true, true,  0.25, 0.0);

    // Sort score.
    std::sort(score.begin(), score.end(), Note_less);
    // Randomize all stereo pans.
    std::mt19937 mersenne_twister(49850);
    std::uniform_real_distribution<double> random_pan(.05, .95);
    
    EVTBLK evtblk;
    std::memset(&evtblk, 0, sizeof(EVTBLK));
    for (const auto &note : score) {
        evtblk.strarg = nullptr;
        evtblk.scnt = 0;
        evtblk.opcod = 'i';
        evtblk.pcnt = 8;
        auto instrument = note[0];
        auto time = note[1];
        auto duration = note[2];
        auto midi_key = note[3];
        auto midi_velocity = note[4];
        double depth = 0;
        double pan = 0.5; //random_pan(mersenne_twister);
        evtblk.p[1] = instrument;
        evtblk.p[2] = time;
        evtblk.p[3] = duration;
        evtblk.p[4] = midi_key;
        evtblk.p[5] = midi_velocity;
        evtblk.p[6] = depth;
        evtblk.p[7] = pan;
        int result = csound->insert_score_event(csound, &evtblk, 0.);
        std::fprintf(stderr, "%c %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f result %d\\n", evtblk.opcod, evtblk.p[1], evtblk.p[2], evtblk.p[3], evtblk.p[4], evtblk.p[5], evtblk.p[6], evtblk.p[7], result);
    }
    std::fprintf(stderr, "score_generator: generated %ld notes.\\n", score.size());
    return result;
}

}}

if strcmp(gS_os, "Linux") == 0 then
i_result cxx_compile "score_generator", S_score_generator_code, "g++ -v -g -O2 -std=c++17 -shared -fPIC -DUSE_DOUBLE -I. -I/usr/local/include/csound -I/home/mkg/csound/interfaces -I/usr/include/eigen3 -I/home/mkg/csound-extended/CsoundAC -lCsoundAC -lpthread -lm", "libcsound_webserver.so libCsoundAC.so"
endif
if strcmp(gS_os, "macOS") == 0 then
i_result cxx_compile "score_generator", S_score_generator_code, "g++ -v -g -O2 -std=c++17 -shared -fPIC -DUSE_DOUBLE -I. -I/usr/include/eigen3 -I/System/Volumes/Data/opt/homebrew/include/eigen3 -I/opt/homebrew/Cellar/csound/6.17.0_5/Frameworks/CsoundLib64.framework/Versions/6.0/Headers -I/opt/homebrew/Cellar/boost/1.79.0/include -I/opt/homebrew/Cellar/opencv/4.5.5_2/include/opencv4 -I/opt/local/include -I/Users/michaelgogins/csound-extended/CsoundAC -lCsoundAC -lpthread -lm", "libCsoundAC.dylib"
endif

instr Exit
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
cxx_raise "SIGTERM"
endin

</CsInstruments>
<CsScore>
f 0 65
i "Exit" [65]
</CsScore>
</CsoundSynthesizer>