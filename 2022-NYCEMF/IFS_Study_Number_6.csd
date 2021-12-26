<CsoundSyntheizer>
<CsLicense>

# I F S   S T U D Y   N O .   6

Michael Gogins, 2021

This piece demonstrates the use of the Faust opcodes, the Clang opcodes, the 
vst4cs opcodes, the signal flow graph opcodes, and the WebKit opcodes, all in 
one .csd file.

Comments are provided throughout the piece. 

External dependencies are brought into the .csd file -- as far as that is 
possible. The remaining external dependencies are the WebKitGTK+ system 
packages, the Clang/LLVM system packages, the Eigen header-only library 
for linear algrebra, the Faust system packages and Faust opcodes for Csound,
the vst4cs opcodes for Csound, and the Pianoteq VST plugin from Modartt.

It's a lot, but it makes for a very powerful computer music system.

# Program Notes

This piece is algorithmically composed using the multiple copy reducing 
machine algorithm, an abstract and flexible method of producing an 
approximation to the attractor of an iterated function system; in this case,
the attractor is a set of notes in a musical score.

This piece also demonstrates the use of some powerful Csound plugin opcodes, 
and some of these are new.

The piece uses the faustgen opcodes for embedding source code for the Faust 
digital signal processing language in the Csound orchestra, and compiling 
the Faust code to user-defined opcodes at Csound's run time.

The piece uses the new Clang opcodes, written by me, for 
embedding the C++ source code that implements the MCRM score generator in the 
Csound orchestra, and compiling and running the C++ code at Csound's run time.

C++ code for a user-defined phase-synchronous granular synthesis opcode is 
also embedded, and compiled and run at Csound's run time.

The piece uses the new WebKit opcodes, written by me, for embedding the 
HTML and JavaScript source code for a Web page showing interactive controls 
for adjusting the Csound instruments, and display the Web page at Csound's 
run time to enable tweaking the sounds of the instruments.

The piece uses the vst4cs opcodes, partly written and now maintained by 
me, for hosting the Pianoteq physically modelled piano as a VST2 plugin, and 
for using the freeware Mverb2020 plugin, which provides a good implementation 
of Jon Dattoro's mesh reverb.

Source code for this piece is available at 
https://github.com/gogins/michael.gogins.studio/blob/master/2022-NYCEMF/IFS_Study_Number_6.csd

# Biography

I was born in 1950 in Salt Lake City, Utah, and lived there till 1973, a 
wonderful place to grow up with many trips to mountains, desert, and unlocked 
university labs. My father was an inventor, my mother was a fine artist and 
commercial artist. I have pursued poetry, photography, music performance, and 
music composition. I have lived in Salt Lake City, Los Angeles, New York, 
and Seattle. I have a B.A. in comparative religion from the University of 
Washington, 1984.

At the same time as I was studying comparative religion, I was taking seminars 
in computer music with John Rahn. Computer music gradually became my major 
interest. It also enabled me to make a living as a software engineer. In the 
late 1980s, I benefited greatly from Brad Garton's openness to non-student 
participation in the woof user group and concerts at the Columbia-Princeton 
Electronic Music Center, now the Columbia Computer Music Center.

Currently, I contribute code to Csound, maintain the vst4cs opcodes for 
hosting VST plugins in Csound, maintain the Csound for Android app, and 
maintain the csound-extended package incorporating various facilities for 
algorithmic composition in JavaScript, C++, and Common Lisp. I write articles 
and papers and give talks on computer music, and I create computer music. I 
have a special interest in algorithmic composition. I am currently working to 
bring new developments in mathematical music theory into algorithmic 
composition software.  I am married to Heidi Rogers, who before she retired 
ran Frank Music Company, a classical sheet music store in New York. We live on 
a farm in the Catskills, and on the Upper West Side of Manhattan. 

</CsLicense>
<CsOptions>
-+msg_color=0 -m0 -d -odac:plughw:2,0
</CsOptions>
<CsInstruments>

sr = 48000
ksmps = 128
nchnls = 2
0dbfs = 20

gi_maximum_voices init 10

// VST plugins must be loaded ahead of instruments that use them.

gi_Pianoteq vstinit "/home/mkg/Pianoteq\ 7/x86-64bit/Pianoteq\ 7.so", 0
gi_Mverb2020 vstinit "/home/mkg/.local/lib/Mverb2020.so", 1

// The following C++ code defines two invokable "opcodes" that compute a 
// phase-synchronous Gaussian chirp grain, or a phase-synchronous cosine 
// grain.

S_grain_code init {{

// Some C++ modules use this symbol, but the Clang/LLVM startup code does not
// automatically define it. Therefore, we define it here.

void* __dso_handle = (void *)&__dso_handle;

static bool diagnostics_enabled = false;

#include <cmath>
#include <complex>
#include <csound/csdl.h>
#include <cstdio>

/*
clang_invokable.hpp - this file is part of clang-opcodes.

Copyright (C) 2021 by Michael Gogins

clang-opcodes is free software; you can redistribute it
and/or modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

clang-opcodes is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with clang-opcodes; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
02110-1301 USA
*/

#include <csdl.h>
#include <cstring>

extern "C" {
    void webkit_execute(int handle, const char *javascript_code);
};

/**
 * Defines the pure abstract interface implemented by Clang modules to be 
 * called by Csound using the `clang_invoke` opcode.
 */
struct ClangInvokable {
	virtual ~ClangInvokable() {};
	/**
	 * Called once at init time. The inputs are the same as the 
	 * parameters passed to the `clang_invoke` opcode. The outputs become 
	 * the values returned from the `clang_invoke` opcode. Performs the 
	 * same work as `iopadr` in a standard Csound opcode definition. The 
	 * `opds` argument can be used to find many things about the invoking 
     * opcde and its enclosing instrument.
	 */
	virtual int init(CSOUND *csound, OPDS *opds, MYFLT **outputs, MYFLT **inputs) = 0;
	/**
	 * Called once every kperiod. The inputs are the same as the 
	 * parameters passed to the `clang_invoke` opcode. The outputs become 
	 * the values returned from the `clang_invoke` opcode. Performs the 
	 * same work as `kopadr` in a standard Csound opcode definition.
	 */
	virtual int kontrol(CSOUND *csound, MYFLT **outputs, MYFLT **inputs) = 0;
	/**
	 * Called by Csound when the Csound instrument that contains this 
	 * instance of the ClangInvokable is turned off.
	 */
	virtual int noteoff(CSOUND *csound) = 0;
};

/**
 * Concrete base class that implements `ClangInvokable`, with some helper 
 * facilities. Most users will implement a ClangInvokable by inheriting from 
 * `ClangInvokableBase` and overriding one or more of its virtual methods.
 */
class ClangInvokableBase : public ClangInvokable {
    public:
        virtual ~ClangInvokableBase() {
        };
        int init(CSOUND *csound_, OPDS *opds_, MYFLT **outputs, MYFLT **inputs) override {
            int result = OK;
            csound = csound_;
            opds = opds_;
            return result;
        }
        /**
         * Computes one sample of one frame of output audio.
         * The `kontrol` method then calls this as appropriate for 
         * --sample-accurate rendering. The implemention of this method must 
         * be compatible with Csound's inputs to `clang_invoke`.
         */
        virtual MYFLT tick(MYFLT **inputs) {
            MYFLT value = 0;
            return value;
        }
         int kontrol(CSOUND *csound_, MYFLT **outputs, MYFLT **inputs) override {
            int result = OK;
            int frame_index = 0;
            MYFLT input = 0.;
            for( ; frame_index < kperiodOffset(); ++frame_index) {
                outputs[0][frame_index] = 0;
            }
            for( ; frame_index < kperiodEnd(); ++frame_index) {
                MYFLT sample = tick(inputs);
                outputs[0][frame_index] = sample;
            }
            for( ; frame_index < ksmps(); ++frame_index) {
                outputs[0][frame_index] = 0;
            }
            return result;
        }
        int noteoff(CSOUND *csound) override 
        {
            int result = OK;
            return result;
        }
        uint32_t kperiodOffset() const
        {
            return opds->insdshead->ksmps_offset;
        }
        uint32_t kperiodEnd() const
        {
            uint32_t end = opds->insdshead->ksmps_no_end;
            if (end) {
                return end;
            } else {
                return ksmps();
            }
        }
        uint32_t ksmps() const
        {
            return opds->insdshead->ksmps;
        }
        uint32_t output_arg_count()
        {
            return (uint32_t)opds->optext->t.outArgCount;
        }
        uint32_t input_arg_count()
        {
            // The first two input arguments belong to the invoking opcode.
            return (uint32_t)opds->optext->t.inArgCount - 2;
        }
        void log(const char *format,...)
        {
            va_list args;
            va_start(args, format);
            if(csound) {
                csound->MessageV(csound, 0, format, args);
            } else {
                vfprintf(stdout, format, args);
            }
            va_end(args);
        }
        void warn(const char *format,...)
        {
            if(csound) {
                if(csound->GetMessageLevel(csound) & WARNMSG) {
                    va_list args;
                    va_start(args, format);
                    csound->MessageV(csound, CSOUNDMSG_WARNING, format, args);
                    va_end(args);
                }
            } else {
                va_list args;
                va_start(args, format);
                vfprintf(stdout, format, args);
                va_end(args);
            }
        }
    protected:
        CSOUND *csound = nullptr;
        OPDS *opds = nullptr;
};

/**
 * Synthesizes a Jones-Parks grain, i.e. a Gaussian chirp, which can be a 
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
 * Computes a cosine grain. If the synchronous_phase argument is true
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
    // The difference equation.
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

//////////////////////////////////////////////////////////////////////////////
// Original by Steven Yi.
// Adapted by Michael Gogins.
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
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 75
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization * 1.6
k_gain = ampdb(gk_FMWaterBell_level)
a_signal fmbell	1, i_frequency, gk_FMWaterBell_index, gk_FMWaterBell_crossfade, gk_FMWaterBell_vibrato_depth, gk_FMWaterBell_vibrato_rate, gi_FMWaterBell_cosine, gi_FMWaterBell_cosine, gi_FMWaterBell_cosine, gi_FMWaterBell_cosine, gi_FMWaterBell_cosine ;, gi_FMWaterBell_sustain
;a_envelope linsegr 0, gi_FMWaterBell_attack, 1, i_sustain, gi_FMWaterBell_sustain_level, gi_FMWaterBell_release, 0
a_envelope linsegr 0, gi_FMWaterBell_attack, 1, i_sustain, 1, gi_FMWaterBell_release, 0
; ares transegr ia, idur, itype, ib [, idur2] [, itype] [, ic] ...
; a_envelope transegr 0, gi_FMWaterBell_attack, 12, 1, i_sustain, 12, gi_FMWaterBell_sustain_level, gi_FMWaterBell_release, 12, 0
a_signal = a_signal * i_amplitude * a_envelope * k_gain

#ifdef USE_SPATIALIZATION
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin
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
maxalloc "FMWaterBell", gi_maximum_voices
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
gi_FaustBubble_compiled faustcompile {{

declare name "bubble";
declare description "Production of a water drop bubble sound.";
declare license "MIT";
declare copyright "(c) 2017: Yann Orlarey, GRAME";

import("stdfaust.lib");

//---------------------------`bubble`--------------------------
// bubble(f0, trig) : produces a water drop bubble sound
//
// #### Usage
//
// ```
// bubble(f0, trig) : _
// ```
//
// Where:
//
// * ` f0 `: base frequency of bubble sound
// * `trig`: trigs the bubble sound on the rising front
//
// #### Example
//
// ```
// button("drop") : bubble(600) : _
// ```
//
// #### Reference:
//
// <http://www.cs.ubc.ca/~kvdoel/publications/tap05.pdf>
//------------------------------------------------------------

bubble(f0,trig) = os.osc(f) * (exp(-damp*time) : si.smooth(0.99))
	with {
		damp = 0.043*f0 + 0.0014*f0^(3/2);
		f = f0*(1+sigma*time);
		sigma = eta * damp;
		eta = 0.075;
		time = 0 : (select2(trig>trig'):+(1)) ~ _ : ba.samp2sec;
	};

process = button("drop") : bubble(hslider("v:bubble/freq", 600, 150, 2000, 1)) <: dm.freeverb_demo;

}}, "--import-dir \"/home/mkg/faust/libraries\"", 0

print gi_FaustBubble_compiled

gk_FaustBubble_level chnexport "gk_FaustBubble_level", 3
gk_FaustBubble_midi_dynamic_range chnexport "gk_FaustBubble_midi_dynamic_range", 3

gk_FaustBubble_level init 0
gk_FaustBubble_midi_dynamic_range init 20

instr FaustBubble
i_attack = .005
i_sustain = p3
i_release = .1
xtratim i_attack + i_release
i_instrument = p1
i_time = p2
i_midi_key = p4
while i_midi_key < 49 do
  i_midi_key = i_midi_key + 12
od
while i_midi_key > 99 do
  i_midi_key = i_midi_key - 12
od
i_midi_dynamic_range = i(gk_FaustBubble_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 68
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_FaustBubble_level)
i_faust_dsp faustdsp gi_FaustBubble_compiled
k_frequency cpsmidinn i_midi_key
faustctl i_faust_dsp, "freq", k_frequency
faustctl i_faust_dsp, "drop", k(1)
a_left, a_right faustplay i_faust_dsp
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_left = a_left * i_amplitude * a_declicking * k_gain
a_right = a_right * i_amplitude * a_declicking * k_gain

#ifdef USE_SPATIALIZATION
a_signal = a_left + a_right
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
outleta "outleft", a_left
outleta "outright", a_right
#endif
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin
connect "FaustBubble", "outleft",  "Mverb2020", "inleft"
connect "FaustBubble", "outright", "Mverb2020", "inright"
gk_FaustBubble_level init 0
maxalloc "FaustBubble", gi_maximum_voices
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
gk_Rhodes_level chnexport "gk_Rhodes_level", 3 ;  0
gk_Rhodes_level init 0

gi_Rhodes_sine ftgen 0, 0, 65537, 10, 1
gi_Rhodes_cosine ftgen 0, 0, 65537, 11, 1
gi_Rhodes_blank ftgen 0, 0, 65537, 10, 0 ; Blank wavetable for some Cook FM opcodes.

instr Rhodes
; Authors: Perry Cook, John ffitch, Michael Gogins
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_overall_amps = 82
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Rhodes_level)
iindex = 4
icrossfade = 3
ivibedepth = 0.2
iviberate = 6
ifn1 = gi_Rhodes_sine
ifn2 = gi_Rhodes_cosine
ifn3 = gi_Rhodes_sine
ifn4 = gi_Rhodes_blank
ivibefn = gi_Rhodes_sine
a_signal fmrhode i_amplitude, i_frequency, iindex, icrossfade, ivibedepth, iviberate, ifn1, ifn2, ifn3, ifn4, ivibefn
i_attack = .002
i_sustain = p3
i_release = 0.01
xtratim i_attack + i_release
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_signal * a_declicking * k_gain

#ifdef USE_SPATIALIZATION
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin
connect "Rhodes", "outleft",  "Mverb2020", "inleft"
connect "Rhodes", "outright", "Mverb2020", "inright"
gk_Rhodes_level init 0
maxalloc "Rhodes", gi_maximum_voices
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
gk_ZakianFlute_midi_dynamic_range chnexport "gk_ZakianFlute_midi_dynamic_range", 3 ;  20
gk_ZakianFlute_level chnexport "gk_ZakianFlute_level", 3 ;  0
gk_ZakianFlute_pan chnexport "gk_ZakianFlute_pan", 3 ;  .5
gi_ZakianFLute_seed chnexport "gi_ZakianFLute_seed", 3 ;  .5

gk_ZakianFlute_midi_dynamic_range init 20
gk_ZakianFlute_level init 0
gk_ZakianFlute_pan init .5
gi_ZakianFLute_seed init .5

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
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_overall_amps = 65.2
i_normalization = ampdb(-i_overall_amps) / 2
i_midi_dynamic_range = i(gk_ZakianFlute_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_ZakianFlute_level)
;;;iattack = .002
;;;isustain = p3
;;;irelease = .3
;;;xtratim iattack + irelease
iHz = cpsmidinn(i_midi_key)
kHz = k(iHz)
// Bug?
// aenvelope transeg 1.0, 20.0, -10.0, 0.05
aenvelope transegr 1.0, 20.0, -10.0, 0.05
ip3 = (p3 < 3.0 ? p3 : 3.0)
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
isustain = p3 - iattack - idecay
p3 = (isustain < 5/kr ? iattack+idecay+5/kr : p3) ; minimal sustain length
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
kvibrate linseg 2.5+ivibr1, p3, 4.5+ivibr2 ; if p6 positive vibrato gets faster
goto vibrato2
vibrato1:
ivibr3 = gi_ZakianFLute_seed
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
kvibrate linseg 3.5+ivibr1, .1, 4.5+ivibr2, p3-.1, 2.5+ivibr3 ; if p6 negative vibrato gets slower
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
i_sustain = p3
i_release = 0.01
xtratim i_attack + i_release
a_declicking linseg 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_signal * i_amplitude * a_declicking * k_gain

#ifdef USE_SPATIALIZATION
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
a_signal *= .7
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin
connect "ZakianFlute", "outleft",  "Mverb2020", "inleft"
connect "ZakianFlute", "outright", "Mverb2020", "inright"
gk_ZakianFlute_midi_dynamic_range init 20
gk_ZakianFlute_level init 0
maxalloc "ZakianFlute", gi_maximum_voices
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
gk_PianoNotePianoteq_midi_dynamic_range chnexport "gk_PianoNotePianoteq_midi_dynamic_range", 3 ;  20

gk_PianoNotePianoteq_midi_dynamic_range init 20

instr PianoNotePianoteq
if p3 == -1 then
  p3 = 1000000
endif
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_PianoNotePianoteq_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
i_homogeneity = p11
instances active p1
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
i_pitch_correction = 44100 / sr
; prints "Pitch factor:   %9.4f\n", i_pitch_correction
vstnote gi_Pianoteq, 0, i_midi_key, i_midi_velocity, i_duration
endin
maxalloc "PianoNotePianoteq", gi_maximum_voices
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
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
maxalloc "CosineGrain", gi_maximum_voices
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
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
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
// This must be initialized in the orc header before any #includes.

gi_Pianoteq vstinit "/home/mkg/Pianoteq\ 7/x86-64bit/Pianoteq\ 7.so", 0
vstinfo gi_Pianoteq 

gk_PianoOutPianoteq_level chnexport "gk_PianoOutPianoteq_level", 3 ;  0
gi_PianoOutPianoteq_print chnexport "gi_PianoOutPianoteq_print", 3 ;  1
gk_PianoOutPianoteq_front_to_back chnexport "gk_PianoOutPianoteq_front_to_back", 3 ;  0
gk_PianoOutPianoteq_left_to_right chnexport "gk_PianoOutPianoteq_left_to_right", 3 ;  0.5
gk_PianoOutPianoteq_bottom_to_top chnexport "gk_PianoOutPianoteq_bottom_to_top", 3 ;  0

gk_PianoOutPianoteq_level init 0
gi_PianoOutPianoteq_print init 1
gk_PianoOutPianoteq_front_to_back init 0
gk_PianoOutPianoteq_left_to_right init 0.5
gk_PianoOutPianoteq_bottom_to_top init 0

instr PianoOutPianoteq
; Should be "D4 Daily Practice".
vstprogset gi_Pianoteq, 0
; Sustain off.
vstparamset gi_Pianoteq, 0, 0
; Reverb off.
vstparamset gi_Pianoteq, 72, 0
k_gain = ampdb(gk_PianoOutPianoteq_level)
i_overall_amps = 87
i_normalization = ampdb(-i_overall_amps) * 2
i_amplitude = ampdb(80) * i_normalization
if gi_PianoOutPianoteq_print == 1 then
  vstinfo gi_PianoOutPianoteq_print
endif
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
ainleft init 0
ainright init 0
aoutleft, aoutright vstaudio gi_Pianoteq, ainleft, ainright
a_signal = aoutleft + aoutright
a_signal *= k_gain
a_signal *= i_amplitude
a_out_left, a_out_right pan2 a_signal, gk_PianoOutPianoteq_left_to_right
; printks "vstaudio:       %9.4f   %9.4f\n", 0.5, aoutleft, aoutright

#ifdef USE_SPATIALIZATION
a_signal = a_out_left + a_out_right
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, gk_PianoOutPianoteq_front_to_back, gk_PianoOutPianoteq_left_to_right, gk_PianoOutPianoteq_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
; printks "PianoOutPt     L %9.4f R %9.4f l %9.4f\\n", 0.5, a_out_left, a_out_right, gk_Piano_level
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin
connect "PianoOutPianoteq", "outleft",  "Mverb2020", "inleft"
connect "PianoOutPianoteq", "outright", "Mverb2020", "inright"
gk_PianoOutPianoteq_level init -25
alwayson "PianoOutPianoteq"
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
; Use with, e.g.: gi_Mverb2020 vstinit "/home/mkg/.local/lib/Mverb2020.so", 1
; Obtain from: http://socalabs.com/effects/mverb2020/

gk_Mverb2020_level chnexport "gk_Mverb2020_level", 3 ; 0
gk_Mverb2020_Mix chnexport "gk_Mverb2020_Mix", 3 ; .5
gk_Mverb2020_Pre_delay chnexport "gk_Mverb2020_Pre_delay", 3 ; 0.5
gk_Mverb2020_Early_late_mix chnexport "gk_Mverb2020_Early_late_mix", 3 ; 0.5
gk_Mverb2020_Size chnexport "gk_Mverb2020_Size", 3 ; 0.5
gk_Mverb2020_Density chnexport "gk_Mverb2020_Density", 3 ; 0.5
gk_Mverb2020_Bandwith_Frequency chnexport "gk_Mverb2020_Bandwith_Frequency", 3 ; 0.5
gk_Mverb2020_Decay chnexport "gk_Mverb2020_Decay", 3 ; 0.85
gk_Mverb2020_Damping_Frequency chnexport "gk_Mverb2020_Damping_Frequency", 3 ; 0.5
gk_Mverb2020_Gain chnexport "gk_Mverb2020_Gain", 3 ; 1
gi_Mverb2020_Program chnexport "gi_Mverb2020_Program", 3 ; 4

gk_Mverb2020_level init 0
gk_Mverb2020_Mix init .5
gk_Mverb2020_Pre_delay init 0.5
gk_Mverb2020_Early_late_mix init 0.5
gk_Mverb2020_Size init 0.5
gk_Mverb2020_Density init 0.5
gk_Mverb2020_Bandwith_Frequency init 0.5
gk_Mverb2020_Decay init 0.6
gk_Mverb2020_Damping_Frequency init 0.5
gk_Mverb2020_Gain init 1
gi_Mverb2020_Program init 4

instr Mverb2020
vstprogset gi_Mverb2020, gi_Mverb2020_Program
vstparamset gi_Mverb2020, 1, gk_Mverb2020_Mix
;vstparamset gi_Mverb2020, 1, gk_Mverb2020_Pre_delay
;vstparamset gi_Mverb2020, 2, gk_Mverb2020_Early_late_mix
;vstparamset gi_Mverb2020, 3, gk_Mverb2020_Size
;vstparamset gi_Mverb2020, 4, gk_Mverb2020_Density
;vstparamset gi_Mverb2020, 5, gk_Mverb2020_Bandwith_Frequency
vstparamset gi_Mverb2020, 6, gk_Mverb2020_Decay
;vstparamset gi_Mverb2020, 7, gk_Mverb2020_Damping_Frequency
;vstparamset gi_Mverb2020, 8, gk_Mverb2020_Gain
k_gain = ampdb(gk_Mverb2020_level)
ainleft inleta "inleft"
ainright inleta "inright"
aoutleft, aoutright vstaudio gi_Mverb2020, ainleft, ainright
outleta "outleft", aoutleft
outleta "outright", aoutright
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f -- %3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin
connect "Mverb2020", "outleft",  "MasterOutput", "inleft"
connect "Mverb2020", "outright", "MasterOutput", "inright"
gk_Mverb2020_level init 0
gk_Mverb2020_Mix init .5
gk_Mverb2020_Pre_delay init 0.5
gk_Mverb2020_Early_late_mix init 0.5
gk_Mverb2020_Size init 0.5
gk_Mverb2020_Density init 0.5
gk_Mverb2020_Bandwith_Frequency init 0.5
gk_Mverb2020_Decay init 0.45
gk_Mverb2020_Damping_Frequency init 0.5
gk_Mverb2020_Gain init 1
gi_Mverb2020_Program init 2
alwayson "Mverb2020"
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
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
if i_filename_length > 0 goto filename_exists
goto filename_endif
filename_exists:
prints sprintf("Output filename: %s\n", gS_MasterOutput_filename)
fout gS_MasterOutput_filename, 18, aleft * i_amplitude_adjustment, aright * i_amplitude_adjustment
filename_endif:
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin
gk_MasterOutput_level init 10
gS_MasterOutput_filename init ""
alwayson "MasterOutput"
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
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
    <title>Iterated Function System Study No. 6</title>
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
    table table td {
        border-width: 2px;
        padding: 8px;
        border-style: solid;
        border-color: transparent;
        color:yellow;
        background-color: teal;
        font-family: sans-serif;
        text-align:right;
    }
    table table th {
        border-width: 2px;
        padding: 8px;
        border-style: solid;
        border-color: transparent;
        color:white;
        background-color:teal;
        font-family: sans-serif;
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
    <h2>Iterated Function System Study No. 6</h2>
    <h3>Michael Gogins, 2021
    <input type="button" id='save' value="Save" />
    <input type="button" id='restore' value="Restore" />
    </h3>
    <form id='persist'>
    <p>
    <table>
    <col>
    <col>
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
    <label for=gk_FaustBubble_midi_dynamic_range>FaustBubble MIDI dynamic range (dB)</label>
    <td>
    <input class=persistent-element type=range min=0 max=127 value=20 id=gk_FaustBubble_midi_dynamic_range step=.001>
    <td>
    <output for=gk_FaustBubble_midi_dynamic_range id=gk_FaustBubble_midi_dynamic_range_output>-6</output>
    </tr>
    
    <tr/>
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
    <label for=gk_ZakianFlute_midi_dynamic_range>ZakianFlute MIDI dynamic range (dB)</label>
    <td>
    <input class=persistent-element type=range min=0 max=127 value=20 id=gk_ZakianFlute_midi_dynamic_range step=.001>
    <td>
    <output for=gk_ZakianFlute_midi_dynamic_range id=gk_ZakianFlute_midi_dynamic_range_output>-6</output>
    </tr>
    
    <tr/>
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

    <tr/>
    <tr/>
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
    
    <tr/>
    <tr/>
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
<script>
/**
 * This file is generated by jsonrpcstub, DO NOT CHANGE IT MANUALLY!
 */
function Csound(url) {
    this.url = url;
    var id = 1;
    
    function doJsonRpcRequest(method, params, methodCall, callback_success, callback_error) {
        var request = {};
        if (methodCall)
            request.id = id++;
        request.jsonrpc = "2.0";
        request.method = method;
        if (params !== null) {
            request.params = params;
        }
        JSON.stringify(request);
        
        $.ajax({
            type: "POST",
            url: url,
            data: JSON.stringify(request),
            success: function (response) {
                if (methodCall) {
                    if (response.hasOwnProperty("result") && response.hasOwnProperty("id")) {
                        callback_success(response.id, response.result);
                    } else if (response.hasOwnProperty("error")) {
                        if (callback_error != null)
                            callback_error(response.error.code,response.error.message);
                    } else {
                        if (callback_error != null)
                            callback_error(-32001, "Invalid Server response: " + response);
                    }
                }
            },
            error: function () {
                if (methodCall)
                    callback_error(-32002, "AJAX Error");
            },
            dataType: "json"
        });
        return id-1;
    }
    this.doRPC = function(method, params, methodCall, callback_success, callback_error) {
        return doJsonRpcRequest(method, params, methodCall, callback_success, callback_error);
    }
}

Csound.prototype.CompileCsdText = function(csd_text, callbackSuccess, callbackError) {
    var params = {csd_text : csd_text};
    return this.doRPC("CompileCsdText", params, true, callbackSuccess, callbackError);
};
Csound.prototype.CompileOrc = function(orc_code, callbackSuccess, callbackError) {
    var params = {orc_code : orc_code};
    return this.doRPC("CompileOrc", params, true, callbackSuccess, callbackError);
};
Csound.prototype.EvalCode = function(orc_code, callbackSuccess, callbackError) {
    var params = {orc_code : orc_code};
    return this.doRPC("EvalCode", params, true, callbackSuccess, callbackError);
};
Csound.prototype.Get0dBFS = function(callbackSuccess, callbackError) {
    var params = null;
    return this.doRPC("Get0dBFS", params, true, callbackSuccess, callbackError);
};
Csound.prototype.GetAudioChannel = function(channel_name, callbackSuccess, callbackError) {
    var params = {channel_name : channel_name};
    return this.doRPC("GetAudioChannel", params, true, callbackSuccess, callbackError);
};
Csound.prototype.GetControlChannel = function(channel_name, callbackSuccess, callbackError) {
    var params = {channel_name : channel_name};
    return this.doRPC("GetControlChannel", params, true, callbackSuccess, callbackError);
};
Csound.prototype.GetDebug = function(callbackSuccess, callbackError) {
    var params = null;
    return this.doRPC("GetDebug", params, true, callbackSuccess, callbackError);
};
Csound.prototype.GetKsmps = function(callbackSuccess, callbackError) {
    var params = null;
    return this.doRPC("GetKsmps", params, true, callbackSuccess, callbackError);
};
Csound.prototype.GetNchnls = function(callbackSuccess, callbackError) {
    var params = null;
    return this.doRPC("GetNchnls", params, true, callbackSuccess, callbackError);
};
Csound.prototype.GetNchnlsInput = function(callbackSuccess, callbackError) {
    var params = null;
    return this.doRPC("GetNchnlsInput", params, true, callbackSuccess, callbackError);
};
Csound.prototype.GetScoreOffsetSeconds = function(callbackSuccess, callbackError) {
    var params = null;
    return this.doRPC("GetScoreOffsetSeconds", params, true, callbackSuccess, callbackError);
};
Csound.prototype.GetScoreTime = function(callbackSuccess, callbackError) {
    var params = null;
    return this.doRPC("GetScoreTime", params, true, callbackSuccess, callbackError);
};
Csound.prototype.GetSr = function(callbackSuccess, callbackError) {
    var params = null;
    return this.doRPC("GetSr", params, true, callbackSuccess, callbackError);
};
Csound.prototype.GetStringChannel = function(channel_name, callbackSuccess, callbackError) {
    var params = {channel_name : channel_name};
    return this.doRPC("GetStringChannel", params, true, callbackSuccess, callbackError);
};
Csound.prototype.InputMessage = function(sco_code, callbackSuccess, callbackError) {
    var params = {sco_code : sco_code};
    return this.doRPC("InputMessage", params, true, callbackSuccess, callbackError);
};
Csound.prototype.IsScorePending = function(callbackSuccess, callbackError) {
    var params = null;
    return this.doRPC("IsScorePending", params, true, callbackSuccess, callbackError);
};
Csound.prototype.Message = function(message, callbackSuccess, callbackError) {
    var params = {message : message};
    this.doRPC("Message", params, false, callbackSuccess, callbackError);
};
Csound.prototype.ReadScore = function(sco_code, callbackSuccess, callbackError) {
    var params = {sco_code : sco_code};
    return this.doRPC("ReadScore", params, true, callbackSuccess, callbackError);
};
Csound.prototype.RewindScore = function(callbackSuccess, callbackError) {
    var params = null;
    return this.doRPC("RewindScore", params, true, callbackSuccess, callbackError);
};
Csound.prototype.ScoreEvent = function(opcode_code, pfields, callbackSuccess, callbackError) {
    var params = {opcode_code : opcode_code, pfields : pfields};
    return this.doRPC("ScoreEvent", params, true, callbackSuccess, callbackError);
};
Csound.prototype.SetControlChannel = function(channel_name, channel_value, callbackSuccess, callbackError) {
    var params = {channel_name : channel_name, channel_value : channel_value};
    return this.doRPC("SetControlChannel", params, true, callbackSuccess, callbackError);
};
Csound.prototype.SetDebug = function(enabled, callbackSuccess, callbackError) {
    var params = {enabled : enabled};
    return this.doRPC("SetDebug", params, true, callbackSuccess, callbackError);
};
Csound.prototype.SetMessageCallback = function(callback, callbackSuccess, callbackError) {
    var params = {callback : callback};
    return this.doRPC("SetMessageCallback", params, true, callbackSuccess, callbackError);
};
Csound.prototype.SetScoreOffsetSeconds = function(score_time, callbackSuccess, callbackError) {
    var params = {score_time : score_time};
    return this.doRPC("SetScoreOffsetSeconds", params, true, callbackSuccess, callbackError);
};
Csound.prototype.SetScorePending = function(pending, callbackSuccess, callbackError) {
    var params = {pending : pending};
    return this.doRPC("SetScorePending", params, true, callbackSuccess, callbackError);
};
Csound.prototype.SetStringChannel = function(channel_name, channel_value, callbackSuccess, callbackError) {
    var params = {channel_name : channel_name, channel_value : channel_value};
    return this.doRPC("SetStringChannel", params, true, callbackSuccess, callbackError);
};
Csound.prototype.TableLength = function(table_number, callbackSuccess, callbackError) {
    var params = {table_number : table_number};
    return this.doRPC("TableLength", params, true, callbackSuccess, callbackError);
};
Csound.prototype.TableGet = function(index, table_number, callbackSuccess, callbackError) {
    var params = {index : index, table_number : table_number};
    return this.doRPC("TableGet", params, true, callbackSuccess, callbackError);
};
Csound.prototype.TableSet = function(index, table_number, value, callbackSuccess, callbackError) {
    var params = {index : index, table_number : table_number, value : value};
    return this.doRPC("TableSet", params, true, callbackSuccess, callbackError);
};
</script>
<script>   

    function showResult(response) {
    };

    function showError(response) {
    };

    var number_format = new Intl.NumberFormat('en-US', {minimumFractionDigits: 3, maximumFractionDigits: 3 });
    $(document).ready(function() {
        var csound = new Csound("http://localhost:8383");
        $('input').on('input', function(event) {
            var slider_value = parseFloat(event.target.value);
            csound.SetControlChannel(event.target.id, slider_value, showResult, showError);
            var output_selector = '#' + event.target.id + '_output';
            var formatted = number_format.format(slider_value);
            $(output_selector).val(formatted);
        });
        $('#save').on('click', function() {
            $('.persistent-element').each(function() {
                localStorage.setItem(this.id, this.value);
            });
        });
        $('#restore').on('click', function() {
            $('.persistent-element').each(function() {
                this.value = localStorage.getItem(this.id);
                csound.SetControlChannel(this.id, parseFloat(this.value), showResult, showError);
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
// The following lines find the current working directory from Csound, 
// and then use that to construct the base URI of the HTML code.
S_pwd pwd
S_base_uri sprintf "file://%s/", S_pwd
prints S_base_uri
webkit_open_html gi_browser, "Iterated Function System Study No. 6", gS_html, S_base_uri, 12000, 10000, 0
endin
alwayson "Browser"
//////////////////////////////////////////////////////////////////////////////

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
#include <random>
#include <vector>

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

// This is the deterministic iterated function system algorithm, or multiple 
// copy reducing machine.
//
// An initial point (a note is a point in 6-dimensional space) is copied by 
// each of the transformations in a Hutchinson operator, in turn. Exactly the 
// same procedure is applied to each of the products of the transformations, 
// recursively. If iterated to infinity, and if the transformations are 
// contractive, the final set of points is a compact measure filling some 
// fraction of the 6-dimensional space -- a true fractal. Here, we stop after 
// a small number of iterations, in order to obtain a score with a reasonable 
// number of notes.

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
            std::fprintf(stderr, "erasing redundant note at %d...\\n", laterI);
            auto later_event_off_time = laterEvent[1] + laterEvent[2];
            earlierEvent[2] = later_event_off_time = earlierEvent[1];
            score.erase(score.begin() + laterI);
            break;
        }
    }
    
    // Using the EVTBLK struct for each note is more efficient than using a 
    // string for each note.
    
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
        print_note(note);
        int result = csound->insert_score_event(csound, &evtblk, 0.);
    }
    std::fprintf(stderr, "to_csound_score: generated %ld notes.\\n", score.size());
}

extern "C" { 

void webkit_execute(int browser_handle, const char *javascript_code);

int score_generator(CSOUND *csound) {
    int result = OK;
    // Transformations are homogeneous matrices, and notes are homogeneous 
    // column vectors.
    Note note;
    note << 1., 0., .1, 60., 60., .5, 1.;
    std::cerr << "initial note: " << std::endl << note << std::endl;
    // This operator generates the basic form of the score, which is then 
    // resized to the desired dimensions.
    std::vector<Transformation> hutchinson;
    hutchinson.resize(4);
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
                       0,  0,  0,-.5,  0,  0,  0,    /* k */
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
    multiple_copy_reducing_machine(note, hutchinson, score, 7);
    // Fit to the number of instruments in the orchestra.
    rescale(scaling, score, 0, true, true,  1.,     5.999);
    // Fit to the full range of a grand piano.
    rescale(scaling, score, 3, true, true, 21.,    88.0);
    // Fit to the desired dynamic range.
    rescale(scaling, score, 4, true, true, 60.,    30.0);
    // Fit to the desired time.
    rescale_time_and_duration(score, 2., 240.);
    to_csound_score(csound, score, true);
    return result;
};

};

}}

i_result clang_compile "score_generator", S_score_generator_code, "-g -O2 -std=c++14 -I/home/mkg/clang-opcodes -I/usr/local/include/csound -stdlib=libstdc++", "/usr/lib/gcc/x86_64-linux-gnu/9/libstdc++.so /usr/lib/gcc/x86_64-linux-gnu/9/libgcc_s.so /home/mkg/webkit-opcodes/webkit_opcodes.so /usr/lib/x86_64-linux-gnu/libm.so /usr/lib/x86_64-linux-gnu/libpthread.so"

instr Exit
exitnow 0
endin

</CsInstruments>
<CsScore>
; f 0 does not work here, we actually need to schedule an instrument that 
; turns off Csound.
i "Exit" 244
</CsScore>
</CsoundSynthesizer>