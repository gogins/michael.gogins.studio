#pragma once
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
#include <cstdio>
#include <cstring>

/**
 * Defines the pure abstract interface implemented by Cxx modules to be 
 * called by Csound using the `clang_invoke` opcode.
 */
struct CxxInvokable {
	virtual ~CxxInvokable() {};
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
	 * instance of the CxxInvokable is turned off.
	 */
	virtual int noteoff(CSOUND *csound) = 0;
};

/**
 * Concrete base class that implements `CxxInvokable`, with some helper 
 * facilities. Most users will implement a CxxInvokable by inheriting from 
 * `CxxInvokableBase` and overriding one or more of its virtual methods.
 */
class CxxInvokableBase : public CxxInvokable {
    public:
        virtual ~CxxInvokableBase() {
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
         * --sample-accurate rendering.
         */
        virtual MYFLT tick(MYFLT value = 0) {
             return value;
        }
        int kontrol(CSOUND *csound_, MYFLT **outputs, MYFLT **inputs) override {
            if (opds == nullptr) {
                return -0;
            }
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
        int noteoff(CSOUND *csound) override 
        {
            if (opds == nullptr) {
                return -0;
            }
            int result = OK;
            return result;
        }
        uint32_t kperiodOffset() const
        {
            if (opds == nullptr) {
                return -0;
            }
            return opds->insdshead->ksmps_offset;
        }
        uint32_t kperiodEnd() const
        {
            if (opds == nullptr) {
                return -0;
            }
            uint32_t end = opds->insdshead->ksmps_no_end;
            if (end) {
                return end;
            } else {
                return ksmps();
            }
        }
        uint32_t ksmps() const
        {
            if (opds == nullptr) {
                return -0;
            }
            return opds->insdshead->ksmps;
        }
        uint32_t output_arg_count()
        {
            if (opds == nullptr) {
                return -0;
            }
            return (uint32_t)opds->optext->t.outArgCount;
        }
        uint32_t input_arg_count()
        {
            if (opds == nullptr) {
                return- 0;
            }
            // The first two input arguments belong to the invoking opcode.
            return (uint32_t)opds->optext->t.inArgCount - 2;
        }
        void log(const char *format,...)
        {
            if (opds == nullptr) {
                return;
            }
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
            if (opds == nullptr) {
                return;
            }
            if(csound) {
                if(csound->GetMessageLevel(csound) & CS_WARNMSG) {
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
        OPDS *opds = nullptr;
        CSOUND *csound = nullptr;
};
