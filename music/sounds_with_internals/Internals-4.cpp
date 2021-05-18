#include <Composition.hpp>
#include <HarmonyIFS2.hpp>
#include <cmath>
#include <eigen3/Eigen/Dense>
#include <functional>
#include <memory>
#include <MusicModel.hpp>
#include <random>
#include <Rescale.hpp>
#include <ScoreNode.hpp>
#include <VoiceleadingNode.hpp>
#include <vector>

int main(int argc, const char **argv)
{
    csound::MusicModel model;
    model.setAuthor("Michael Gogins");
    model.setTitle("Internals-3");
    model.setAlbum("Silence");
    model.setYear("2021");
    model.setPerformanceRightsOrganization("Irreducible Productions, ASCAP");
    csound::System::setMessageLevel(7);
    csound::HarmonyIFS2 harmony_ifs;
    // initialize(int voices_, double range_, double bass_, double note_duration_, bool tie_overlaps_, bool remove_duplicates, double g_ = 1.) {
    harmony_ifs.initialize(5 , 60., 30., 18., true, true, 1.);
    harmony_ifs.pitv().list(true, false, false);
    auto tonic = csound::chordForName("CM9");
    auto subdominant = csound::chordForName("Dm9");
    subdominant = csound::octavewiseRevoicing(subdominant, 3000, 60);
    auto dominant = csound::chordForName("G9");
    dominant = csound::octavewiseRevoicing(dominant, 100, 60);
    csound::System::message("I:\n%s\n", tonic.information_sector(0).c_str());
    csound::System::message("ii:\n%s\n", subdominant.information_sector(0).c_str());
    csound::System::message("V:\n%s\n", dominant.information_sector(0).c_str());
    ///auto &score = harmony_ifs.getScore();
    //                                                              1    2    3    4    5    6    7    8    9    10   11   12   13   14   15   16
    harmony_ifs.add_interpolation_point_as_chord(  0., tonic,       .70, .01, .01, .01, .01, .01, .01, .01, .01, .01, .01, .01, .01, .01, .01, .01);
    harmony_ifs.add_interpolation_point_as_chord(100., subdominant, .01, .01, .01, .01, .01, .01, .50, .01, .01, .01, .01, .01, .01, .01, .01, .01);
    harmony_ifs.add_interpolation_point_as_chord(220., dominant,    .01, .02, .01, .01, .01, .01, .01, .01, .01, .01, .01, .01, .50, .01, .01, .01);
    harmony_ifs.add_interpolation_point_as_chord(300., tonic,       .01, .01, .01, .01, .01, .01, .01, .01, .01, .01, .01, .01, .01, .01, .01, .50);
    harmony_ifs.initialize_hutchinson_operator();
    double A = 5.13 * M_PI / 180.;
    csound::System::message("A: %9.4f\n", A);
    harmony_ifs.generate_score_attractor(3);
    csound::Rescale rescale;
    rescale.setRescale(csound::Event::INSTRUMENT, true, true, 1., 6.999);
    rescale.setRescale(csound::Event::VELOCITY, true, true, 60., 6.);
    rescale.addChild(&harmony_ifs);
    model.addChild(&rescale);
    model.setDuration(600.);
    const char orc[] = R"(
    
#define FLTK #1#
sr = 48000
ksmps = 128
nchnls = 2
0dbfs = 20000000

gk_slider1 init 0.5
gk_slider2 init 0.2
gk_slider3 init 0
gk_slider4 init 0.2
gk_slider5 init 0
gk_slider6 init 0
gk_slider7 init 0
gk_slider8 init 0
gk_slider9 init 0
gk_butt1 init 0
gk_butt2 init 0
gk_butt3 init 0
gk_butt4 init 0
gk_butt5 init 0
gk_trackpadx init 0.95
gk_trackpady init 0.25
gk_accelerometerx init 0
gk_accelerometery init 0
gk_accelerometerz init 0

#ifdef FLTK
FLpanel "Csound 6", 510, 770, 150, 150
gk_slider1, i_hslider1 FLslider "slider1", 0, 1, 0, 23, -1, 500, 20, 5, 5
FLsetVal_i i(gk_slider1), i_hslider1
gk_slider2, i_hslider2 FLslider "slider2", 0, 1, 0, 23, -1, 500, 20, 5, 55
FLsetVal_i i(gk_slider2), i_hslider2
gk_slider3, i_hslider3 FLslider "slider3", 0, 1, 0, 23, -1, 500, 20, 5, 105
FLsetVal_i i(gk_slider3), i_hslider3
gk_slider4, i_hslider4 FLslider "slider4", 0, 1, 0, 23, -1, 500, 20, 5, 155
FLsetVal_i i(gk_slider4), i_hslider4
gk_slider5, i_hslider5 FLslider "slider5", 0, 1, 0, 23, -1, 500, 20, 5, 205
FLsetVal_i i(gk_slider5), i_hslider5
gk_slider6, i_hslider6 FLslider "slider6", 0, 1, 0, 23, -1, 500, 20, 5, 255
FLsetVal_i i(gk_slider6), i_hslider6
gk_slider7, i_hslider7 FLslider "slider7", 0, 1, 0, 23, -1, 500, 20, 5, 305
FLsetVal_i i(gk_slider7), i_hslider7
gk_slider8, i_hslider8 FLslider "slider8", 0, 1, 0, 23, -1, 500, 20, 5, 355
FLsetVal_i i(gk_slider8), i_hslider8
gk_slider9, i_hslider9 FLslider "slider9", 0, 1, 0, 23, -1, 500, 20, 5, 405
FLsetVal_i i(gk_slider9), i_hslider9
gk_trackpadx, gk_trackpady, ihtrackpadx, ihtrackpady FLjoy "trackpad.x/trackpad.y", 0, 1, 0, 1, 0, 0, -1, -1, 500, 250, 5, 450
FLsetVal_i i(gk_trackpadx), ihtrackpadx
FLsetVal_i i(gk_trackpady), ihtrackpady
gk_butt1, i_hbutt1 FLbutton "butt1", 1, 0, 21, 100, 40, 5,   720 , -1
gk_butt2, i_hbutt2 FLbutton "butt2", 1, 0, 21, 100, 40, 105, 720 , -1
gk_butt3, i_hbutt3 FLbutton "butt3", 1, 0, 21, 100, 40, 205, 720 , -1
gk_butt4, i_hbutt4 FLbutton "butt4", 1, 0, 21, 100, 40, 305, 720 , -1
gk_butt5, i_hbutt5 FLbutton "butt5", 1, 0, 21, 100, 40, 405, 720 , -1
FLrun
#endif

connect "Bower",        "outleft",  "ReverbLeft",   "input"
connect "Bower",        "outright", "ReverbRight",  "input"
connect "Phaser",       "outleft",  "ReverbLeft",   "input"
connect "Phaser",       "outright", "ReverbRight",  "input"
connect "Droner",       "outleft",  "ReverbLeft",   "input"
connect "Droner",       "outright", "ReverbRight",  "input"
connect "Sweeper",      "outleft",  "ReverbLeft",   "input"
connect "Sweeper",      "outright", "ReverbRight",  "input"
connect "Buzzer",       "outleft",  "ReverbLeft",   "input"
connect "Buzzer",       "outright", "ReverbRight",  "input"
connect "Blower",       "outleft",  "ReverbLeft",   "input"
connect "Blower",       "outright", "ReverbRight",  "input"
connect "Shiner",       "outleft",  "ReverbLeft",   "input"
connect "Shiner",       "outright", "ReverbRight",  "input"
connect "ReverbLeft",   "output",   "MasterOutput", "inleft"
connect "ReverbRight",  "output",   "MasterOutput", "inright"

#ifndef FLTK
alwayson "Controls"
#endif
alwayson "VariablesForControls"
alwayson "ReverbLeft"
alwayson "ReverbRight"
alwayson "MasterOutput"

opcode ratio2midinn, i, iii
i_fundamental, i_numerator, i_denominator xin
i_frequency = i_fundamental * i_numerator / i_denominator
; print i_frequency
; m = 12*log2(fm/440 Hz) + 69
i_log2 = log(2)
i_midinn = 12 * (log(i_frequency / 440) / i_log2) + 69
; print i_midinn
xout i_midinn
endop

instr 101
i_instrument = p1
i_time = p2
i_duration = p3
i_fundamental = p4
i_numerator = p5
i_denominator = p6
i_midi_velocity = p7
i_pan = p8
i_ratio = i_numerator / i_denominator
i_frequency = i_fundamental * i_ratio
i_midi_key ratio2midinn i_fundamental, i_numerator, i_denominator
event_i "i", "Bower", 0, i_duration, i_midi_key, i_midi_velocity, 0, i_pan
endin

instr 102
i_instrument = p1
i_time = p2
i_duration = p3
i_fundamental = p4
i_numerator = p5
i_denominator = p6
i_midi_velocity = p7
i_pan = p8
i_ratio = i_numerator / i_denominator
i_frequency = i_fundamental * i_ratio
i_midi_key ratio2midinn i_fundamental, i_numerator, i_denominator
event_i "i", "Phaser", 0, i_duration, i_midi_key, i_midi_velocity, 0, i_pan
endin

instr 103
i_instrument = p1
i_time = p2
i_duration = p3
i_fundamental = p4
i_numerator = p5
i_denominator = p6
i_midi_velocity = p7
i_pan = p8
i_ratio = i_numerator / i_denominator
i_frequency = i_fundamental * i_ratio
i_midi_key ratio2midinn i_fundamental, i_numerator, i_denominator
event_i "i", "Sweeper", 0, i_duration, i_midi_key, i_midi_velocity, 0, i_pan
endin

instr 104
i_instrument = p1
i_time = p2
i_duration = p3
i_fundamental = p4
i_numerator = p5
i_denominator = p6
i_midi_velocity = p7
i_pan = p8
i_ratio = i_numerator / i_denominator
i_frequency = i_fundamental * i_ratio
i_midi_key ratio2midinn i_fundamental, i_numerator, i_denominator
event_i "i", "Blower", 0, i_duration, i_midi_key, i_midi_velocity, 0, i_pan
endin

gi_Droner_sine ftgen 0, 0, 65537, 10, 1, 0, .02
instr Droner
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
i_phase = p6
i_pan = p7
k1 init .5
k2 init .05
k3 init .1
k4 init .2
k5 init .1
k6 init .05
k7 init .1
k8 init 0
k9 init 0
k10 init 0
k1 = gk_slider1
k2 = gk_slider2
k3 = gk_slider3
k4 = gk_slider4
k5 = gk_slider5
k6 = gk_slider6
k7 = gk_slider7
k8 = gk_slider8
k9 = gk_slider9
kwaveform init 2
i_amplitude = ampdb(i_midi_velocity)
i_attack =  p3 * (1 / 4) * (4 / 3)
i_sustain = p3 * (1 / 2) * (4 / 3)
i_decay =   p3 * (1 / 4) * (4 / 3)
p3 = i_attack + i_sustain + i_decay
kenvelope transeg 0.0, i_attack / 2.0, 1.5, i_amplitude / 2.0, i_attack / 2.0, -1.5, i_amplitude, i_sustain, 0.0, i_amplitude, i_decay / 2.0, 1.5, i_amplitude / 2.0, i_decay / 2.0, -1.5, 0
i_frequency = cpsmidinn(i_midi_key)
; print i_frequency
if kwaveform == 0 then
asignal poscil3 1, i_frequency, gi_Droner_sine
endif
if kwaveform == 1 then
asignal vco2 1, i_frequency, 8 ; integrated saw
endif
if kwaveform == 2 then
asignal vco2 1, i_frequency, 12 ; triangle
endif
asignal chebyshevpoly asignal, 0, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10
asignal = asignal * kenvelope * 10
aleft, aright pan2 asignal, i_pan
adamping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
aleft = adamping * aleft
aright = adamping * aright
outleta "outleft", aleft
outleta "outright", aright
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gi_Bower_sine ftgen 0,0,65537,10,1
instr Bower
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
i_phase = p6
i_pan = p7
i_amplitude = ampdb(i_midi_velocity) * 500
i_attack =  p3 * (1 / 4) * (4 / 3)
i_sustain = p3 * (1 / 2) * (4 / 3)
i_decay =   p3 * (1 / 4) * (4 / 3)
p3 = i_attack + i_sustain + i_decay
kenvelope transeg 0.0, i_attack / 2.0, 1.5, i_amplitude / 2.0, i_attack / 2.0, -1.5, i_amplitude, i_sustain, 0.0, i_amplitude, i_decay / 2.0, 1.5, i_amplitude / 2.0, i_decay / 2.0, -1.5, 0
i_frequency = cpsmidinn(i_midi_key)
kamp = kenvelope
kfreq = i_frequency
kpres = 0.25
; krat rspline 0.006,0.988,0.1,0.4
krat rspline 0.006,0.988,1,4
kvibf = 4.5
kvibamp = 0
iminfreq = 20
aSig wgbow kamp,kfreq,kpres,krat,kvibf,kvibamp,gi_Bower_sine,iminfreq
;aSig butlp aSig,2000
;aSig pareq aSig,80,6,0.707
aleft, aright pan2 aSig / 7, i_pan
adamping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
aleft = adamping * aleft
aright = adamping * aright
outleta "outleft", aleft
outleta "outright", aright
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_Phaser_attack init .125
gk_Phaser_release init .125
gk_Phaser_ratio1 init 1
gk_Phaser_ratio2 init 1/5
gk_Phaser_index1 init 1.01
gk_Phaser_index2 init .103
gk_Phaser_level init 20
gk_Phaser_midi_dynamic_range init 127
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
i_level_correction = 0
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Phaser_level)
i_attack =  p3 * (1 / 4) * (4 / 3)
i_sustain = p3 * (1 / 2) * (4 / 3)
i_decay =   p3 * (1 / 4) * (4 / 3)
p3 = i_attack + i_sustain + i_decay
a_envelope transegr 0.0, i_attack / 2.0, 1.5, i_amplitude / 2.0, i_attack / 2.0, -1.5, i_amplitude, i_sustain, 0.0, i_amplitude, i_decay / 2.0, 1.5, i_amplitude / 2.0, i_decay / 2.0, -1.5, 0
a1,a2 crosspm gk_Phaser_ratio1, gk_Phaser_ratio2, gk_Phaser_index1, gk_Phaser_index2, i_frequency, gi_Phaser_sine, gi_Phaser_sine
a_signal = (a1 + a2) * k_gain * a_envelope
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
;printks "Phaser         i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", 1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(aleft)), dbamp(rms(aright))
endin

gk_britel init 0
gk_briteh init 2.9
gk_britels init .2 / 3
gk_britehs init 2.5 / 2
gi_Sweeper_sine ftgen 0, 0, 65537, 10, 1
gi_Sweeper_octfn ftgen 0, 0, 65537, -19, 1, 0.5, 270, 0.5
instr Sweeper
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
i_phase = p6
i_pan = p7
i_amplitude = ampdb(i_midi_velocity)
i_attack =  p3 * (1 / 4) * (4 / 3)
i_sustain = p3 * (1 / 2) * (4 / 3)
i_decay =   p3 * (1 / 4) * (4 / 3)
p3 = i_attack + i_sustain + i_decay
kenvelope transeg 0.0, i_attack / 2.0, 1.5, i_amplitude / 2.0, i_attack / 2.0, -1.5, i_amplitude, i_sustain, 0.0, i_amplitude, i_decay / 2.0, 1.5, i_amplitude / 2.0, i_decay / 2.0, -1.5, 0
i_frequency = cpsmidinn(i_midi_key)
icps = i_frequency
kamp expseg 0.001,0.02,0.2,p3-0.01,0.001
ktonemoddep jspline 0.01,0.05,0.2
ktonemodrte jspline 6,0.1,0.2
ktone poscil3 ktonemoddep, ktonemodrte, gi_Sweeper_sine
kbrite rspline gk_britel, gk_briteh, gk_britels, gk_britehs
ibasfreq init icps
ioctcnt init 3
iphs init 0
;a1 hsboscil kamp, ktone, kbrite, ibasfreq, gi_Sweeper_sine, gi_Sweeper_octfn, ioctcnt, iphs
a1 hsboscil kenvelope, ktone, kbrite, ibasfreq, gi_Sweeper_sine, gi_Sweeper_octfn, ioctcnt, iphs
amod poscil3 0.25, ibasfreq*(1/3), gi_Sweeper_sine
arm = a1*amod
kmix expseg 0.001, 0.01, rnd(1), rnd(3)+0.3, 0.001
kmix=.25
a1 ntrpol a1, arm, kmix
;a1 pareq a1/10, 400, 15, .707
;a1 tone a1, 500
kpanrte jspline 5, 0.05, 0.1
kpandep jspline 0.9, 0.2, 0.4
kpan poscil3 kpandep, kpanrte, gi_Sweeper_sine
a1,a2 pan2 a1, kpan
a1 delay a1, rnd(0.1)
a2 delay a2, rnd(0.11)
kenv linsegr 1, 1, 0
kenv = kenvelope
aleft = a1*kenv*.02
aright = a2*kenv*.02
adamping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
aleft = adamping * aleft
aright = adamping * aright
outleta "outleft", aleft
outleta "outright", aright
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gi_Buzzer_sine ftgen 0, 0, 65537, 10, 1
instr Buzzer
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
i_phase = p6
i_pan = p7
i_amplitude = ampdb(i_midi_velocity) * 4
i_attack =  p3 * (1 / 4) * (4 / 3)
i_sustain = p3 * (1 / 2) * (4 / 3)
i_decay =   p3 * (1 / 4) * (4 / 3)
p3 = i_attack + i_sustain + i_decay
kenvelope transeg 0.0, i_attack / 2.0, 1.5, i_amplitude / 2.0, i_attack / 2.0, -1.5, i_amplitude, i_sustain, 0.0, i_amplitude, i_decay / 2.0, 1.5, i_amplitude / 2.0, i_decay / 2.0, -1.5, 0
i_frequency = cpsmidinn(i_midi_key)
;asignal gbuzz kenvelope, i_frequency, 3, gk_FirstHarmonic, gk_DistortFactor, gisine
gk_Harmonics = gk_slider1 * 20
asignal buzz kenvelope, i_frequency, gk_Harmonics, gi_Buzzer_sine
asignal = asignal * 3
;asignal vco2 kenvelope, i_frequency, 12
;asignal poscil3 kenvelope, i_frequency, giharmonics
;asignal distort asignal, gk_DistortFactor * .4, giwaveshaping
aleft, aright pan2 asignal, i_pan
adamping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
aleft = adamping * aleft
aright = adamping * aright
outleta "outleft", aleft
outleta "outright", aright
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

instr Shiner
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
i_phase = p6
i_pan = p7
i_amplitude = ampdb(i_midi_velocity) * 4
i_attack =  p3 * (1 / 4) * (4 / 3)
i_sustain = p3 * (1 / 2) * (4 / 3)
i_decay =   p3 * (1 / 4) * (4 / 3)
p3 = i_attack + i_sustain + i_decay
kenvelope transeg 0.0, i_attack / 2.0, 1.5, i_amplitude / 2.0, i_attack / 2.0, -1.5, i_amplitude, i_sustain, 0.0, i_amplitude, i_decay / 2.0, 1.5, i_amplitude / 2.0, i_decay / 2.0, -1.5, 0
i_frequency = cpsmidinn(i_midi_key)
;asignal gbuzz kenvelope, i_frequency, 3, gk_FirstHarmonic, gk_DistortFactor, gisine
gk_Harmonics = gk_slider1 * 20
;asignal buzz kenvelope, i_frequency, gk_Harmonics, gisine
;asignal = asignal
asignal vco2 kenvelope * 4, i_frequency, 12
;asignal poscil3 kenvelope, i_frequency, giharmonics
;asignal distort asignal, gk_DistortFactor * .4, giwaveshaping
aleft, aright pan2 asignal, i_pan
adamping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
aleft = adamping * aleft
aright = adamping * aright
outleta "outleft", aleft
outleta "outright", aright
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_Blower_grainDensity init 150
gk_Blower_grainDuration init 0.2
gk_Blower_grainAmplitudeRange init 100
gk_Blower_grainFrequencyRange init .033
gi_Blower_grtab ftgenonce 0, 0, 65537, 10, 1, .3, .1, 0, .2, .02, 0, .1, .04
gi_Blower_wintab ftgenonce 0, 0, 65537, 10, 1, 0, .5, 0, .33, 0, .25, 0, .2, 0, .167
instr Blower
//////////////////////////////////////////////
// Original by Hans Mikelson.
// Adapted by Michael Gogins.
//////////////////////////////////////////////
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
i_phase = p6
i_pan = p6
i_depth = p8
i_height = p9
i_pitchclassset = p10
i_homogeneity = p11
i_frequency = cpsmidinn(i_midi_key)
i_amplitude = ampdb(i_midi_velocity) / 200
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; f1 0 65537 1 "hahaha.aif" 0 4 0
; f2 0 1024 7 0 224 1 800 0
; f3 0 8192 7 1 8192 -1
; f4 0 1024 7 0 512 1 512 0
; f5 0 1024 10 1 .3 .1 0 .2 .02 0 .1 .04
; f6 0 1024 10 1 0 .5 0 .33 0 .25 0 .2 0 .167
; a0 14 50
; p1 p2 p3 p4 p5 p6 p7 p8 p9 p10
; Start Dur Amp Freq GrTab WinTab FqcRng Dens Fade
; i1 0.0 6.5 700 9.00 5 4 .210 200 1.8
; i1 3.2 3.5 800 7.08 . 4 .042 100 0.8
; i1 5.1 5.2 600 7.10 . 4 .0320 100 0.9
; i1 7.2 6.6 900 8.03 . 4 .021 150 1.6
; i1 21.3 4.5 1000 9.00 . 4 .031 150 1.2
; i1 26.5 13.5 1100 6.09 . 4 .121 150 1.5
; i1 30.7 9.3 900 8.05 . 4 .014 150 2.5
; i1 34.2 8.8 700 10.02 . 4 .14 150 1.6
iHz = i_frequency
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
i_amplitude = i_amplitude ; p4
ifqc = iHz ; cpspch(p5)
igrtab = ip6
iwintab = ip7
ifrng = ip8
idens = ip9
ifade = ip10
igdur = 0.2
i_attack =  p3 * (1 / 4) * (4 / 3)
i_sustain = p3 * (1 / 2) * (4 / 3)
i_decay =   p3 * (1 / 4) * (4 / 3)
p3 = i_attack + i_sustain + i_decay
kenvelope transeg 0.0, i_attack / 2.0, 1.5, i_amplitude / 2.0, i_attack / 2.0, -1.5, i_amplitude, i_sustain, 0.0, i_amplitude, i_decay / 2.0, 1.5, i_amplitude / 2.0, i_decay / 2.0, -1.5, 0
; kamp linseg 0, ifade, 1, idur - 2 * ifade, 1, ifade, 0
kamp = kenvelope
; Amp Fqc Dense AmpOff PitchOff GrDur GrTable WinTable MaxGrDur
aoutl grain ip4, ifqc, gk_Blower_grainDensity, gk_Blower_grainAmplitudeRange, ifqc * gk_Blower_grainFrequencyRange, gk_Blower_grainDuration, gi_Blower_grtab, gi_Blower_wintab, 5
aoutr grain ip4, ifqc, gk_Blower_grainDensity, gk_Blower_grainAmplitudeRange, ifqc * gk_Blower_grainFrequencyRange, gk_Blower_grainDuration, gi_Blower_grtab, gi_Blower_wintab, 5
aleft = aoutl * kamp * i_amplitude
aright = aoutr * kamp * i_amplitude
adamping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
aleft = adamping * aleft
aright = adamping * aright
outleta "outleft", aleft
outleta "outright", aright
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_ReverbFeedback init 0.975
gk_DelayModulation init 0.875

instr ReverbLeft
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
itone = 16000
asignal inleta "input"
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
// ares randi   xamp, xcps [, iseed] [, isize] [, ioffset]
// ares jspline xamp, kcpsMin, kcpsMax
k1 randi .001,  3.1,   .06
k2 randi .0011, 3.5,   .9
k3 randi .0017, 1.11,  .7
k4 randi .0006, 3.973, .3
k5 randi .001,  2.341, .63
k6 randi .0011, 1.897, .7
k7 randi .0017, 0.891, .9
k8 randi .0006, 3.221, .44
; apj is used to calculate "resultant junction pressure" for
; the scattering junction of 8 lossless waveguides
; of equal characteristic impedance. If you wish to
; add more delay lines, simply add them to the following
; equation, and replace the .25 by 2/N, where N is the
; number of delay lines.
apj = .25 * (afilt1 + afilt2 + afilt3 + afilt4 + afilt5 + afilt6 + afilt7 + afilt8)
adum1 delayr 1
adel1 deltap3 idel1 + k1 * gk_DelayModulation
delayw asignal + apj - afilt1
adum2 delayr 1
adel2 deltap3 idel2 + k2 * gk_DelayModulation
delayw asignal + apj - afilt2
adum3 delayr 1
adel3 deltap3 idel3 + k3 * gk_DelayModulation
delayw asignal + apj - afilt3
adum4 delayr 1
adel4 deltap3 idel4 + k4 * gk_DelayModulation
delayw asignal + apj - afilt4
adum5 delayr 1
adel5 deltap3 idel5 + k5 * gk_DelayModulation
delayw asignal + apj - afilt5
adum6 delayr 1
adel6 deltap3 idel6 + k6 * gk_DelayModulation
delayw asignal + apj - afilt6
adum7 delayr 1
adel7 deltap3 idel7 + k7 * gk_DelayModulation
delayw asignal + apj - afilt7
adum8 delayr 1
adel8 deltap3 idel8 + k8 * gk_DelayModulation
delayw asignal + apj - afilt8
; 1st order lowpass filters in feedback
; loops of delay lines.
afilt1 tone adel1 * gk_ReverbFeedback, itone
afilt2 tone adel2 * gk_ReverbFeedback, itone
afilt3 tone adel3 * gk_ReverbFeedback, itone
afilt4 tone adel4 * gk_ReverbFeedback, itone
afilt5 tone adel5 * gk_ReverbFeedback, itone
afilt6 tone adel6 * gk_ReverbFeedback, itone
afilt7 tone adel7 * gk_ReverbFeedback, itone
afilt8 tone adel8 * gk_ReverbFeedback, itone
; The outputs of the delay lines are summed
; and sent to the stereo outputs. This could
; easily be modified for a 4 or 8-channel
; sound system.
aout1 = (afilt1 + afilt3 + afilt5 + afilt7)
aout2 = (afilt2 + afilt4 + afilt6 + afilt8)
aout = aout1 + aout2
; To the master output.
outleta "output", aout
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

instr ReverbRight
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
itone = 16000
asignal inleta "input"
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
// ares randi   xamp, xcps             [, iseed] [, isize] [, ioffset]
// ares jspline xamp, kcpsMin, kcpsMax
k1 randi .001,  3.1,   .06
k2 randi .0011, 3.5,   .9
k3 randi .0017, 1.11,  .7
k4 randi .0006, 3.973, .3
k5 randi .001,  2.341, .63
k6 randi .0011, 1.897, .7
k7 randi .0017, 0.891, .9
k8 randi .0006, 3.221, .44
; apj is used to calculate "resultant junction pressure" for
; the scattering junction of 8 lossless waveguides
; of equal characteristic impedance. If you wish to
; add more delay lines, simply add them to the following
; equation, and replace the .25 by 2/N, where N is the
; number of delay lines.
apj = .25 * (afilt1 + afilt2 + afilt3 + afilt4 + afilt5 + afilt6 + afilt7 + afilt8)
adum1 delayr 1
adel1 deltap3 idel1 + k1 * gk_DelayModulation
delayw asignal + apj - afilt1
adum2 delayr 1
adel2 deltap3 idel2 + k2 * gk_DelayModulation
delayw asignal + apj - afilt2
adum3 delayr 1
adel3 deltap3 idel3 + k3 * gk_DelayModulation
delayw asignal + apj - afilt3
adum4 delayr 1
adel4 deltap3 idel4 + k4 * gk_DelayModulation
delayw asignal + apj - afilt4
adum5 delayr 1
adel5 deltap3 idel5 + k5 * gk_DelayModulation
delayw asignal + apj - afilt5
adum6 delayr 1
adel6 deltap3 idel6 + k6 * gk_DelayModulation
delayw asignal + apj - afilt6
adum7 delayr 1
adel7 deltap3 idel7 + k7 * gk_DelayModulation
delayw asignal + apj - afilt7
adum8 delayr 1
adel8 deltap3 idel8 + k8 * gk_DelayModulation
delayw asignal + apj - afilt8
; 1st order lowpass filters in feedback
; loops of delay lines.
afilt1 tone adel1 * gk_ReverbFeedback, itone
afilt2 tone adel2 * gk_ReverbFeedback, itone
afilt3 tone adel3 * gk_ReverbFeedback, itone
afilt4 tone adel4 * gk_ReverbFeedback, itone
afilt5 tone adel5 * gk_ReverbFeedback, itone
afilt6 tone adel6 * gk_ReverbFeedback, itone
afilt7 tone adel7 * gk_ReverbFeedback, itone
afilt8 tone adel8 * gk_ReverbFeedback, itone
; The outputs of the delay lines are summed
; and sent to the stereo outputs. This could
; easily be modified for a 4 or 8-channel
; sound system.
aout1 = (afilt1 + afilt3 + afilt5 + afilt7)
aout2 = (afilt2 + afilt4 + afilt6 + afilt8)
aout = aout1 + aout2
; To the master output.
outleta "output", aout
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_MasterLevel init 4
instr MasterOutput
aleft inleta "inleft"
aright inleta "inright"
aleft *= gk_MasterLevel
aright *= gk_MasterLevel
outs aleft, aright
;fout "Drone-IV-performance.wav", 16, aleft, aright
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

instr Controls
gk_slider1 chnget "slider1"
gk_slider2 chnget "slider2"
gk_slider3 chnget "slider3"
gk_slider4 chnget "slider4"
gk_slider5 chnget "slider5"
gk_slider6 chnget "slider6"
gk_slider7 chnget "slider7"
gk_slider8 chnget "slider8"
gk_slider9 chnget "slider9"
gk_butt1 chnget "butt1"
gk_butt2 chnget "butt2"
gk_butt3 chnget "butt3"
gk_butt4 chnget "butt4"
gk_butt5 chnget "butt5"
gk_trackpadx chnget "trackpad.x"
gk_trackpady chnget "trackpad.y"
gk_accelerometerx chnget "accelerometerX"
gk_accelerometery chnget "accelerometerY"
gk_accelerometerz chnget "accelerometerZ"
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

instr VariablesForControls
printks2 "gk_slider1:   %f\n", gk_slider1
printks2 "gk_slider2:   %f\n", gk_slider2
printks2 "gk_slider3:   %f\n", gk_slider3
printks2 "gk_slider4:   %f\n", gk_slider4
printks2 "gk_slider5:   %f\n", gk_slider5
printks2 "gk_slider6:   %f\n", gk_slider6
printks2 "gk_slider7:   %f\n", gk_slider7
printks2 "gk_slider8:   %f\n", gk_slider8
printks2 "gk_slider9:   %f\n", gk_slider9
printks2 "gk_trackpadx: %f\n", gk_trackpadx
printks2 "gk_trackpady: %f\n", gk_trackpady
if gk_slider1 > 0 then
gk_FirstHarmonic = gk_slider1 * 2
gk_Blower_grainDensity = gk_slider1 * 400
gk_ratio2 = gk_slider1 ;1/3
endif
if gk_slider2 > 0 then
gk_DistortFactor = gk_slider2 * 2
gk_Blower_grainDuration = 0.005 + gk_slider2 / 2
gk_index1 = gk_slider2 * 3;1
endif
if gk_slider3 > 0 then
gk_Volume = gk_slider3 * 5
gk_Blower_grainAmplitudeRange = gk_slider3 * 300
gk_index2 = gk_slider3 ;0.0125
endif
if gk_slider4 > 0 then
gk_Blower_grainFrequencyRange = gk_slider4 / 10
endif
if gk_trackpady > 0 then
gk_DelayModulation = gk_trackpady * 2
; gk_Gain = gk_trackpady * 2 - 1
endif
if gk_trackpadx > 0 then
gk_ReverbFeedback = (2/3) + (gk_trackpadx / 3)
; gk_CenterHz = 100 + gk_trackpadx * 3000
endif
kbutt1 trigger gk_butt1, .5, 0
if kbutt1 > 0 then
gk_britels = gk_britels / 1.5
gk_britehs = gk_britehs / 1.5
; gk_Q = gk_Q / 2
endif
kbutt2 trigger gk_butt2, .5, 0
if kbutt2 > 0 then
gk_britels = gk_britels * 1.5
gk_britehs = gk_britehs * 1.5
; gk_Q = gk_Q * 2
endif
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin


)";
    model.setCsoundOrchestra(orc);
    model.setCsoundScoreHeader("f 0 620\n");
    model.processArgv(argc, argv);
}

