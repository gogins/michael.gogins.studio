gShelp init {{
S P A T I A L I Z E   3 D

Copyright (c) 2014 by Michael Gogins.
This software is licensed under the terms of the
GNU Lesser Public License.

Many thanks to Jan Jacob Hofmann for his advanced Csound
spatialization system (http://www.sonicarchitecture.de/), 
which inspired this work.

Please note the coordinate conventions. The listener is always the origin.
In this system, spatial locations are passed to the spatialize opcode in
Cartesian coordinates. These are the same as those used in physics:

-- x runs from ahead to behind.
-- y runs from left to right. This is the same as stereo pan.
-- z runs from below to above.

Spherical coordinates are more or less standard for Ambisonics, and are
similar to but not identical with those used in physics. The only differences
are that Ambisonics uses elevation but physics uses inclination, and
Ambisonics uses degrees but physics uses radians.

-- r is distance.
-- azimuth or theta is anti-clockwise angle from 0 in front of the listener.
-- elevation or phi is angle above or below the x-y plane. (Note that
   inclination, also called polar angle, would be angle away from the
   zenith pole.)
}}

opcode distance, k, kkk
kx, ky, kz xin
; Calculate the distance from the origin (the listener).
kdistance = sqrt(kx^2 + ky^2 + kz^2)
xout kdistance
endop

opcode CartesianToPolar, kkk, kkk
kx, ky, kz xin
; Calculate the distance from the origin (the listener).
kdistance = sqrt(kx^2 + ky^2 + kz^2)
; Normalize the coordinates to unit vectors from the origin.
kx = kx / kdistance
ky = ky / kdistance
kz = kz / kdistance
; Limit distance to prevent too close a sound becoming too loud.
kdistance = (kdistance < 0.3 ? 0.3 : kdistance)
; Calculate the elevation.
kelevation = cosinv(sqrt(1 - kz^2))
; If z is negative, make elevation negative also.
kelevation = (kz < 0 ? -kelevation : kelevation)
; Calculate the angle.
kangle = sininv(ky / cos(kelevation))
; Distinguish between positive x and negative x.
kangle = (kx >= 0 ? kangle : 3.14159265 - kangle)
; Distinguish between positive and negative y and x.
kangle = (ky <= 0 && kx >= 0 ? 6.28318531 + kangle : kangle)
xout kangle, kelevation, kdistance
endop

opcode CartesianToPolarDegrees, kkk, kkk
kx, ky, kz xin
; Calculate the distance from the origin (the listener).
kdistance = sqrt(kx^2 + ky^2 + kz^2)
; Normalize the coordinates to unit vectors from the origin.
kx = kx / kdistance
ky = ky / kdistance
kz = kz / kdistance
; Limit distance to prevent too close a sound becoming too loud.
kdistance = (kdistance < 0.3 ? 0.3 : kdistance)
; Calculate the elevation.
kelevation = cosinv(sqrt(1 - kz^2))
; If z is negative, make elevation negative also.
kelevation = (kz < 0 ? -kelevation : kelevation)
; Calculate the angle.
kangle = sininv(ky / cos(kelevation))
; Distinguish between positive x and negative x.
kangle = (kx >= 0 ? kangle : 3.14159265 - kangle)
; Distinguish between positive and negative y and x.
kangle = (ky <= 0 && kx >= 0 ? 6.28318531 + kangle : kangle)
kangle = kangle * 57.295779513
kelevation = kelevation * 57.295779513
xout kangle, kelevation, kdistance
endop

/**
 * The local speed of sound in meters per second.
 */
gi_Spatialize3D_Mach init 340.29
opcode DopplerByDistance, a, ak
asignal, kdistance xin
; ashifted doppler asource, ksourceposition, kmicposition [, isoundspeed, ifiltercutoff]
adopplered doppler asignal, kdistance, 0, i(gi_Spatialize3D_Mach), 6
xout adopplered
endop

; Zero distance blows this up.
opcode LowpassByDistance, a, ak
asignal, kdistance xin
afiltered butterlp asignal, 22000 * sqrt(1.0 / kdistance) + .000001
abalanced balance afiltered, asignal
xout abalanced
endop

opcode ReverbSC, a, akkkk
asignal, kReverbFeedback, kDelayModulation, kReverbWet, itone xin
; p4 = gain of reverb. Adjust empirically
; for desired reverb time. .6 gives
; a good small "live" room sound, .8
; a small hall, .9 a large hall,
; .99 an enormous stone cavern.

; p5 = amount of random pitch modulation
; for the delay lines. 1 is the "normal"
; amount, but this may be too high for
; held pitches such as piano tones.
; Adjust to taste.

; p6 = cutoff frequency of lowpass filters
; in feedback loops of delay lines,
; in Hz. Lower cutoff frequencies results
; in a sound with more high-frequency
; damping.

; 8 delay line FDN reverb, with feedback matrix based upon
; physical modeling scattering junction of 8 lossless waveguides
; of equal characteristic impedance. Based on Julius O. Smith III,
; "A New Approach to Digital Reverberation using Closed Waveguide
; Networks," Proceedings of the International Computer Music
; Conference 1985, p. 47-53 (also available as a seperate
; publication from CCRMA), as well as some more recent papers by
; Smith and others.
; Coded by Sean Costello, October 1999
afilt1 init 0
afilt2 init 0
afilt3 init 0
afilt4 init 0
afilt5 init 0
afilt6 init 0
afilt7 init 0
afilt8 init 0
idel1 = (2473.000/sr)
idel2 = (2767.000/sr)
idel3 = (3217.000/sr)
idel4 = (3557.000/sr)
idel5 = (3907.000/sr)
idel6 = (4127.000/sr)
idel7 = (2143.000/sr)
idel8 = (1933.000/sr)
; k1-k8 are used to add random pitch modulation to the
; delay lines. Helps eliminate metallic overtones
; in the reverb sound.
ak1 randi .001, 3.1, .06
ak2 randi .0011, 3.5, .9
ak3 randi .0017, 1.11, .7
ak4 randi .0006, 3.973, .3
ak5 randi .001, 2.341, .63
ak6 randi .0011, 1.897, .7
ak7 randi .0017, 0.891, .9
ak8 randi .0006, 3.221, .44
; apj is used to calculate "resultant junction pressure" for
; the scattering junction of 8 lossless waveguides
; of equal characteristic impedance. If you wish to
; add more delay lines, simply add them to the following
; equation, and replace the .25 by 2/N, where N is the
; number of delay lines.
apj = .25 * (afilt1 + afilt2 + afilt3 + afilt4 + afilt5 + afilt6 + afilt7 + afilt8)
adum1 delayr 1
adel1 deltapi idel1 + ak1 * kDelayModulation
 delayw asignal + apj - afilt1
adum2 delayr 1
adel2 deltapi idel2 + ak2 * kDelayModulation
 delayw asignal + apj - afilt2
adum3 delayr 1
adel3 deltapi idel3 + ak3 * kDelayModulation
 delayw asignal + apj - afilt3
adum4 delayr 1
adel4 deltapi idel4 + ak4 * kDelayModulation
 delayw asignal + apj - afilt4
adum5 delayr 1
adel5 deltapi idel5 + ak5 * kDelayModulation
 delayw asignal + apj - afilt5
adum6 delayr 1
adel6 deltapi idel6 + ak6 * kDelayModulation
 delayw asignal + apj - afilt6
adum7 delayr 1
adel7 deltapi idel7 + ak7 * kDelayModulation
 delayw asignal + apj - afilt7
adum8 delayr 1
adel8 deltapi idel8 + ak8 * kDelayModulation
 delayw asignal + apj - afilt8
; 1st order lowpass filters in feedback
; loops of delay lines.
afilt1 tone adel1 * kReverbFeedback, itone
afilt2 tone adel2 * kReverbFeedback, itone
afilt3 tone adel3 * kReverbFeedback, itone
afilt4 tone adel4 * kReverbFeedback, itone
afilt5 tone adel5 * kReverbFeedback, itone
afilt6 tone adel6 * kReverbFeedback, itone
afilt7 tone adel7 * kReverbFeedback, itone
afilt8 tone adel8 * kReverbFeedback, itone
; The outputs of the delay lines are summed
; and sent to the stereo outputs. This could
; easily be modified for a 4 or 8-channel
; sound system.
aout1 = (afilt1 + afilt3 + afilt5 + afilt7)
aout2 = (afilt2 + afilt4 + afilt6 + afilt8)
aoutput = aout1 + aout2
aoutwet = aoutput * kReverbWet
aoutdry = aoutput * (1 - kReverbWet)
aoutput = aoutwet + aoutdry
xout aoutput
endop

/**
 * Spatializes a one channel audio signal to 3 dimensions using Ambisonic 
 * panning with movement, Doppler effects, and distance cues. 
 *
 * Spatialize3D a_signal, k_x, k_y, k_z
 *
 * -- x runs from ahead to behind.
 * -- y runs from left to right. This is the same as stereo pan.
 * -- z runs from below to above.
 *
 * The spatialized audio is sent as an a-rate array of 1 to 8 channels
 * directly to Csound's audio output. The nchnls variable should be 
 * set in the orchestra header to match the speaker rig (see below).
 *
 * Much of the configuration for this opcode is of necessity contained in 
 * global variables, so that instances of the panner can share room data, etc.
 *
 * Room parameters are set by 54 numbers in a global function table:
 *
 * 0 -- Early reflection recursion depth (0 is the sound source, 1 is the 
 *      first reflection etc.). The number of echoes for four walls (front,  
 *      back, right, left) is: N = (2 * R + 2) * R. If all six walls are 
 *      enabled: N = (((4 * R + 6) * R + 8) * R) / 3.
 * 1 -- Late reflection recursion depth (not used by spat3d). 
 * 2 -- imdel for spat3d. Overrides opcode parameter if non-negative.
 * 3 -- irlen for spat3dt. Overrides opcode parameter if non-negative.
 * 4 -- idist value (not used by spat3d).
 * 5 -- Random seed (0 - 65535), -1 seeds from current time.
 * 6 through 53 define up to 6 walls --
 *           (w = 6: ceil, w = 14: floor, w = 22: front, w = 30: back, 
 *           w = 38: right, w = 46: left).
 * w + 0	-- Enable reflections from this wall (0: no, 1: yes).
 * w + 1	-- Wall distance from listener (in meters).
 * w + 2	-- Randomization of wall distance (0 - 1) (in units of 1 / 
 *           (wall distance)).
 * w + 3	-- Reflection level (-1 - 1).
 * w + 4	-- Parametric equalizer frequency in Hz.
 * w + 5	-- Parametric equalizer level (1.0: no filtering).
 * w + 6	-- Parametric equalizer Q (0.7071: no resonance).
 * w + 7	-- Parametric equalizer mode (0: peak EQ, 1: low shelf, 
 *           2: high shelf).
 */
gi_Spatialize3D_room_table ftgen 1, 0, 64, -2,                                               \                                       
/*  depth1  depth2  max delay  ir length  idist  seed                               */ \
		3,      0,      -1,        0,         0,     123,                                  \
/*  used   distance  random  reflection  eq hz   eq level  eq q  eq mode            */ \
		1,     21.982,   0.05,   0.87,       4000.0, 0.6,      0.7,  2,      /* ceiling */ \
		1,      1.753,   0.05,   0.87,       3500.0, 0.5,      0.7,  2,      /* floor   */ \
		1,     15.220,   0.05,   0.87,       5000.0, 0.8,      0.7,  2,      /* front   */ \
		1,      9.317,   0.05,   0.87,       5000.0, 0.8,      0.7,  2,      /* back    */ \
		1,     17.545,   0.05,   0.87,       5000.0, 0.8,      0.7,  2,      /* right   */ \
		1,     12.156,   0.05,   0.87,       5000.0, 0.8,      0.7,  2       /* left    */
gi_Spatialize3D_unit_circle_radius init 1
gi_Spatialize3D_mode init 0
gi_Spatialize3D_oversampling init 2
/**
 * The available speaker rigs are as follows. For users monitoring on 
 * headphones, rig 31 is optimal; for live audio in a hall, 
 * rigs 2 through 6 are useful; 5 is suitable for movie theaters, and 
 * 4 or 5 are optimal. 
 *  1. Stereo -- L(90), R(-90).
 *  2. Quad -- FL(45), BL(135), BR(-135), FR(-45). This is a first-order 
 *     decode.
 *  3. 5.0 -- L(30), R(-30), C(0), BL(110), BR(-110). This is the 
 *     configuration suggested by the International Telecommunication Union 
 *     for 5.1 surround systems for music listening. This is a first- or 
 *     second- order decode.
 *  4. Octagon -- FFL(22.5), FLL(67.5), BLL(112.5), BBL(157.5), BBR(-157.5), 
 *     BRR(-112.5), FRR(-67.5), FFR(-22.5). This is a first-, second- or 
 *     third-order decode, depending on the number of input channels.
 *  5. Cube -- FLD(45,-35.26), FLU(45,35.26), BLD(135,-35.26), BLU(135,35.26), 
 *     BRD(-135,-35.26), BRU(-135,35.26), FRD(-45,-35.26), FRU(-45,35.26). 
 *     This is a first-order decode.
 *  6. Hexagon -- FL(30), L(90) BL(150), BR(-150), R(-90), FR(-30). This is a 
 *     first- or second- order decode.
 * 21. 2D binaural configuration. This first decodes to an octagon 
 *     configuration, and then applies head-related transfer function filters.
 * 31. 3D binaural configuration. This first decodes to a dodecahedron 
 *     configuration, and then applies head-related transfer function filters.
 */
gi_Spatialize3D_speaker_rig init 31
/**
 * 0 -- Dual decoder (velocity and energy decoders using dual-band splitting),
 *      the default.
 * 1 -- Velocity decoder.
 * 2 -- Energy decoder.
 */
gi_Spatialize3D_decoder init 0
/**
 * Band splitter crossover frequency for dual decoders.
 */
gi_Spatialize3D_crossover_hz init 400
/**
 * 0 -- Energy (the default).
 * 1 -- RMS.
 * 2 -- Amplitude.
 */
gi_Spatialize3D_decoder_mix init 0
/**
 * Head-related transfer function data files for the left and right ears.
 * These files are specific to sampling rates. The default is for 
 * sr = 48000.
 */
gS_Spatialize3D_hrtf_left init "hrtf-48000-left.dat"
gS_Spatialize3D_hrtf_right init "hrtf-48000-right.dat"

opcode Spatialize3D, 0, akkk
a_spatialized[] init nchnls
a_signal, k_x, k_y, k_z xin
ab_signal[] init 4
i_r table 0, gi_Spatialize3D_room_table
i_ceiling table 7, gi_Spatialize3D_room_table
i_floor table 15, gi_Spatialize3D_room_table
i_front  table 23, gi_Spatialize3D_room_table
i_back table 31, gi_Spatialize3D_room_table
i_right table 39, gi_Spatialize3D_room_table
i_left table 47, gi_Spatialize3D_room_table
i_h = i_ceiling + i_floor
i_d = i_front + i_back
i_w = i_right + i_left
prints "i_floor:            %9.4f\n", i_floor
prints "i_ceiling:          %9.4f\n", i_ceiling
prints "i_back:             %9.4f\n", i_back
prints "i_front:            %9.4f\n", i_front
prints "i_left:             %9.4f\n", i_left
prints "i_right:            %9.4f\n", i_right
i_distance = sqrt(i_w * i_w + i_h * i_h + i_d * i_d)
prints "i_distance:         %9.4f\n", i_distance
k_distance = sqrt(k_x * k_x + k_y * k_y + k_z * k_z)
i_maximum_delay = ((i_r + 1) * i_distance / gi_Spatialize3D_Mach) * 2
prints "i_maximum_delay:    %9.4f\n", i_maximum_delay
i_minimum_distance = (gi_Spatialize3D_Mach * 18) / sr
;k_trigger changed k_distance
;printf "Spatialize3D: p%3d: i_minimum_distance: %9.4f k_distance: %9.4f\n", k_trigger, p1, i_minimum_distance, k_distance
a_w, a_x, a_y, a_z spat3d a_signal, k_x, k_y, k_z, gi_Spatialize3D_unit_circle_radius, gi_Spatialize3D_room_table, 3, i_maximum_delay, gi_Spatialize3D_oversampling
ab_signal[0] = a_w
ab_signal[1] = a_x
ab_signal[2] = a_y
ab_signal[3] = a_z
a_spatialized bformdec2 gi_Spatialize3D_speaker_rig, ab_signal, gi_Spatialize3D_decoder, i_distance, gi_Spatialize3D_crossover_hz, gi_Spatialize3D_decoder_mix, gS_Spatialize3D_hrtf_right, gS_Spatialize3D_hrtf_left
; if k_distance < i_minimum_distance then
  ; prints "too close to listener: p1: %9.4f i_minimum_distance: %9.4f k_distance: %9.4f\n", p1, i_minimum_distance, k_distance
; endif
out a_spatialized
endop

