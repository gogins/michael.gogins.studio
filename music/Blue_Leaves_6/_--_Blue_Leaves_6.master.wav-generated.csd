<CsoundSynthesizer>
<CsOptions>
--midi-key=4 --midi-velocity=5 -m195 -j1 -RWdfo/home/mkg/michael.gogins.studio/music/Blue_Leaves_6/_--_Blue_Leaves_6.master.wav
</CsOptions>
<CsInstruments>


sr                              =                       96000
ksmps                           =                       1
nchnls                          =                       2
0dbfs                           =                       1000
iampdbfs                        init                    32768
                                prints                  "Default amplitude at 0 dBFS:  %9.4f\n", iampdbfs
idbafs                          init                    dbamp(iampdbfs)
                                prints                  "dbA at 0 dBFS:                 %9.4f\n", idbafs
iheadroom                       init                    6
                                prints                  "Headroom (dB):                 %9.4f\n", iheadroom
idbaheadroom                    init                    idbafs - iheadroom
                                prints                  "dbA at headroom:               %9.4f\n", idbaheadroom
iampheadroom                    init                    ampdb(idbaheadroom)
                                prints                  "Amplitude at headroom:        %9.4f\n", iampheadroom
                                prints                  "Balance so the overall amps at the end of performance -6 dbfs.\n"

giFlatQ                         init                    sqrt(0.5)
giseed				            init                    0.5

gkHarpsichordGain               chnexport               "gkHarpsichordGain",            1
gkHarpsichordGain               init                    1
gkHarpsichordPan                chnexport               "gkHarpsichordPan",             1
gkHarpsichordPan                init                    0.5

gkChebyshevDroneCoefficient1    chnexport               "gkChebyshevDroneCoefficient1", 1
gkChebyshevDroneCoefficient1    init                    0.5
gkChebyshevDroneCoefficient2    chnexport               "gkChebyshevDroneCoefficient2", 1
gkChebyshevDroneCoefficient3    chnexport               "gkChebyshevDroneCoefficient3", 1
gkChebyshevDroneCoefficient4    chnexport               "gkChebyshevDroneCoefficient4", 1
gkChebyshevDroneCoefficient5    chnexport               "gkChebyshevDroneCoefficient5", 1
gkChebyshevDroneCoefficient6    chnexport               "gkChebyshevDroneCoefficient6", 1
gkChebyshevDroneCoefficient7    chnexport               "gkChebyshevDroneCoefficient7", 1
gkChebyshevDroneCoefficient8    chnexport               "gkChebyshevDroneCoefficient8", 1
gkChebyshevDroneCoefficient9    chnexport               "gkChebyshevDroneCoefficient9", 1
gkChebyshevDroneCoefficient10   chnexport               "gkChebyshevDroneCoefficient10", 1
gkChebyshevDroneCoefficient10   init                    0.05

gkReverberationEnabled          chnexport               "gkReverberationEnabled", 1
gkReverberationEnabled          init                    1
gkReverberationDelay            chnexport               "gkReverberationDelay", 1
gkReverberationDelay            init                    0.325
gkReverberationWet          	chnexport               "gkReverberationWet", 1
gkReverberationWet          	init                    0.15

gkCompressorEnabled             chnexport               "gkCompressorEnabled", 1
gkCompressorEnabled             init                    0
gkCompressorThreshold           chnexport               "gkCompressorThreshold", 1
gkCompressorLowKnee             chnexport               "gkCompressorLowKnee", 1
gkCompressorHighKnee            chnexport               "gkCompressorHighknee", 1
gkCompressorRatio               chnexport               "gkCompressorRatio", 1
gkCompressorAttack              chnexport               "gkCompressorAttack", 1
gkCompressorRelease             chnexport               "gkCompressorRelease", 1

gkParametricEq1Enabled          chnexport               "gkParametricEq1Enabled", 1
gkParametricEq1Enabled          init                    0
gkParametricEq1Mode             chnexport               "gkParametricEq1Mode", 1
gkParametricEq1Frequency        chnexport               "gkParametricEq1Frequency", 1
gkParametricEq1Gain             chnexport               "gkParametricEq1Gain", 1
gkParametricEq1Q                chnexport               "gkParametricEq1Q", 1

gkParametricEq2Enabled          chnexport               "gkParametricEq2Enabled", 1
gkParametricEq2Enabled          init                     0
gkParametricEq2Mode             chnexport               "gkParametricEq2Mode", 1
gkParametricEq2Frequency        chnexport               "gkParametricEq2Frequency", 1
gkParametricEq2Gain             chnexport               "gkParametricEq2Gain", 1
gkParametricEq2Q                chnexport               "gkParametricEq2Q", 1

gkMasterLevel                   chnexport               "gkMasterLevel", 1
gkMasterLevel                   init                    1.5

                                connect                 "BanchoffKleinBottle",  "outleft", 	"Reverberation",        "inleft"
                                connect                 "BanchoffKleinBottle",  "outright", "Reverberation",        "inright"
                                connect                 "BandedWG",             "outleft", 	"Reverberation",        "inleft"
                                connect                 "BandedWG",             "outright", "Reverberation",        "inright"
                                connect                 "BassModel",            "outleft", 	"Reverberation",        "inleft"
                                connect                 "BassModel",            "outright", "Reverberation",        "inright"
                                connect                 "ChebyshevDrone",       "outleft", 	"Reverberation",        "inleft"
                                connect                 "ChebyshevDrone",       "outright", "Reverberation",        "inright"
                                connect                 "ChebyshevMelody",      "outleft", 	"Reverberation",        "inleft"
                                connect                 "ChebyshevMelody",      "outright", "Reverberation",        "inright"
                                connect                 "Compressor",           "outleft", 	"ParametricEq1",        "inleft"
                                connect                 "Compressor",           "outright", "ParametricEq1",        "inright"
                                connect                 "DelayedPluckedString", "outleft", 	"Reverberation",        "inleft"
                                connect                 "DelayedPluckedString", "outright", "Reverberation",        "inright"
                                connect                 "EnhancedFMBell",       "outleft", 	"Reverberation",        "inleft"
                                connect                 "EnhancedFMBell",       "outright", "Reverberation",        "inright"
                                connect                 "FenderRhodesModel",    "outleft", 	"Reverberation",        "inleft"
                                connect                 "FenderRhodesModel",    "outright", "Reverberation",        "inright"
                                connect                 "FilteredSines",        "outleft", 	"Reverberation",        "inleft"
                                connect                 "FilteredSines",        "outright", "Reverberation",        "inright"
                                connect                 "Flute",                "outleft", 	"Reverberation",        "inleft"
                                connect                 "Flute",                "outright", "Reverberation",        "inright"
                                connect                 "FMModulatedChorusing", "outleft", 	"Reverberation",        "inleft"
                                connect                 "FMModulatedChorusing", "outright", "Reverberation",        "inright"
                                connect                 "FMModerateIndex",      "outleft", 	"Reverberation",        "inleft"
                                connect                 "FMModerateIndex",      "outright", "Reverberation",        "inright"
                                connect                 "FMModerateIndex2",     "outleft", 	"Reverberation",        "inleft"
                                connect                 "FMModerateIndex2",     "outright", "Reverberation",        "inright"
                                connect                 "FMWaterBell",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "FMWaterBell",          "outright", "Reverberation",        "inright"
                                connect                 "Granular",             "outleft", 	"Reverberation",        "inleft"
                                connect                 "Granular",             "outright", "Reverberation",        "inright"
                                connect                 "Guitar",               "outleft", 	"Reverberation",        "inleft"
                                connect                 "Guitar",               "outright", "Reverberation",        "inright"
                                connect                 "Guitar2",              "outleft", 	"Reverberation",        "inleft"
                                connect                 "Guitar2",              "outright", "Reverberation",        "inright"
                                connect                 "Harpsichord",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "Harpsichord",          "outright", "Reverberation",        "inright"
                                connect                 "HeavyMetalModel",      "outleft", 	"Reverberation",        "inleft"
                                connect                 "HeavyMetalModel",      "outright", "Reverberation",        "inright"
                                connect                 "Hypocycloid",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "Hypocycloid",          "outright", "Reverberation",        "inright"
                                connect                 "KungModulatedFM",      "outleft", 	"Reverberation",        "inleft"
                                connect                 "KungModulatedFM",      "outright", "Reverberation",        "inright"
                                connect                 "ModerateFM",          	"outleft", 	"Reverberation",        "inleft"
                                connect                 "ModerateFM",          	"outright", "Reverberation",        "inright"
                                connect                 "ModulatedFM",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "ModulatedFM",          "outright", "Reverberation",        "inright"
                                connect                 "Melody",               "outleft", 	"Reverberation",        "inleft"
                                connect                 "Melody",               "outright", "Reverberation",        "inright"
                                connect                 "ParametricEq1",        "outleft", 	"ParametricEq2",        "inleft"
                                connect                 "ParametricEq1",        "outright", "ParametricEq2",        "inright"
                                connect                 "ParametricEq2",        "outleft", 	"MasterOutput",         "inleft"
                                connect                 "ParametricEq2",        "outright", "MasterOutput",         "inright"
                                connect                 "PlainPluckedString",   "outleft", 	"Reverberation",        "inleft"
                                connect                 "PlainPluckedString",   "outright", "Reverberation",        "inright"
                                connect                 "PRCBeeThree",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "PRCBeeThree",          "outright", "Reverberation",        "inright"
                                connect                 "PRCBeeThreeDelayed",   "outleft", 	"Reverberation",        "inleft"
                                connect                 "PRCBeeThreeDelayed",   "outright", "Reverberation",        "inright"
                                connect                 "PRCBowed",             "outleft", 	"Reverberation",        "inleft"
                                connect                 "PRCBowed",             "outright", "Reverberation",        "inright"
                                connect                 "Reverberation",        "outleft", 	"Compressor",           "inleft"
                                connect                 "Reverberation",        "outright", "Compressor",           "inright"
                                connect                 "STKBandedWG",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKBandedWG",          "outright", "Reverberation",        "inright"
                                connect                 "STKBeeThree",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKBeeThree",          "outright", "Reverberation",        "inright"
                                connect                 "STKBlowBotl",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKBlowBotl",          "outright", "Reverberation",        "inright"
                                connect                 "STKBlowHole",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKBlowHole",          "outright", "Reverberation",        "inright"
                                connect                 "STKBowed",             "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKBowed",             "outright", "Reverberation",        "inright"
                                connect                 "STKClarinet",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKClarinet",          "outright", "Reverberation",        "inright"
                                connect                 "STKDrummer",           "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKDrummer",           "outright", "Reverberation",        "inright"
                                connect                 "STKFlute",             "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKFlute",             "outright", "Reverberation",        "inright"
                                connect                 "STKFMVoices",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKFMVoices",          "outright", "Reverberation",        "inright"
                                connect                 "STKHvyMetl",           "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKHvyMetl",           "outright", "Reverberation",        "inright"
                                connect                 "STKMandolin",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKMandolin",          "outright", "Reverberation",        "inright"
                                connect                 "STKModalBar",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKModalBar",          "outright", "Reverberation",        "inright"
                                connect                 "STKMoog",              "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKMoog",              "outright", "Reverberation",        "inright"
                                connect                 "STKPercFlut",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKPercFlut",          "outright", "Reverberation",        "inright"
                                connect                 "STKPlucked",           "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKPlucked",           "outright", "Reverberation",        "inright"
                                connect                 "STKResonate",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKResonate",          "outright", "Reverberation",        "inright"
                                connect                 "STKRhodey",            "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKRhodey",            "outright", "Reverberation",        "inright"
                                connect                 "STKSaxofony",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKSaxofony",          "outright", "Reverberation",        "inright"
                                connect                 "STKShakers",           "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKShakers",           "outright", "Reverberation",        "inright"
                                connect                 "STKSimple",            "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKSimple",            "outright", "Reverberation",        "inright"
                                connect                 "STKSitar",             "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKSitar",             "outright", "Reverberation",        "inright"
                                connect                 "STKTubeBell",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKTubeBell",          "outright", "Reverberation",        "inright"
                                connect                 "STKVoicForm",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKVoicForm",          "outright", "Reverberation",        "inright"
                                connect                 "STKWhistle",           "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKWhistle",           "outright", "Reverberation",        "inright"
                                connect                 "STKWurley",            "outleft", 	"Reverberation",        "inleft"
                                connect                 "STKWurley",            "outright", "Reverberation",        "inright"
                                connect                 "StringPad",            "outleft", 	"Reverberation",        "inleft"
                                connect                 "StringPad",            "outright", "Reverberation",        "inright"
                                connect                 "ToneWheelOrgan",       "outleft", 	"Reverberation",        "inleft"
                                connect                 "ToneWheelOrgan",       "outright", "Reverberation",        "inright"
                                connect                 "TubularBellModel",     "outleft", 	"Reverberation",        "inleft"
                                connect                 "TubularBellModel",     "outright", "Reverberation",        "inright"
                                connect                 "WaveguideGuitar",      "outleft", 	"Reverberation",        "inleft"
                                connect                 "WaveguideGuitar",      "outright", "Reverberation",        "inright"
                                connect                 "Xing",                 "outleft", 	"Reverberation",        "inleft"
                                connect                 "Xing",                 "outright", "Reverberation",        "inright"
                                connect                 "ZakianFlute",          "outleft", 	"Reverberation",        "inleft"
                                connect                 "ZakianFlute",          "outright", "Reverberation",        "inright"

                                alwayson                "Reverberation"
                                alwayson                "Compressor"
                                alwayson                "ParametricEq1"
                                alwayson                "ParametricEq2"
                                alwayson                "MasterOutput"

                                instr                   BanchoffKleinBottle
                                //////////////////////////////////////////////
                                // Original by Hans Mikelson.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity)
                                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                ;   p1  p2     p3   p4    p5     p6   p7
                                ;       Start  Dur  Amp   Frqc   U    V
                                ; i 4   32     6    6000  6.00   3    2
                                ; i 4   36     4    .     5.11   5.6  0.4
                                ; i 4   +      4    .     6.05   2    8.5
                                ; i 4   .      2    .     6.02   4    5
                                ; i 4   .      2    .     6.02   5    0.5
iHz                             =                       ifrequency
ifqc                            init                    iHz
ip4                             init                    iamplitude
iu                              init                    5 ; p6
iv                              init                    0.5 ; p7
irt2                            init                    sqrt(2)
aampenv                         linseg                  0, 0.02, ip4,  p3 - 0.04, ip4, 0.02, 0
isine                  	        ftgenonce               0, 0, 65536, 10, 1
icosine                  	    ftgenonce               0, 0, 65536, 11, 1
                                ; Cosines
acosu                           oscili                  1, iu * ifqc, icosine
acosu2                          oscili                  1, iu * ifqc / 2, icosine
acosv                           oscili                  1, iv * ifqc, icosine
                                ; Sines
asinu                           oscili                  1, iu * ifqc, isine
asinu2                          oscili                  1, iu * ifqc / 2, isine
asinv                           oscili                  1, iv * ifqc, isine
                                ; Compute X and Y
ax                              =                       acosu * (acosu2 * (irt2 + acosv) + asinu2 * asinv * acosv)
ay                              =                       asinu * (acosu2 * (irt2 + acosv) + asinu2 * asinv * acosv)
                                ; Low frequency rotation in spherical coordinates z, phi, theta.
klfsinth                        oscili                  1, 4, isine
klfsinph                        oscili                  1, 1, isine
klfcosth                        oscili                  1, 4, icosine
klfcosph                        oscili                  1, 1, icosine
aox                             =                       -ax * klfsinth + ay * klfcosth
aoy                             =                       -ax * klfsinth * klfcosph - ay * klfsinth * klfcosph + klfsinph
aoutleft                        =                       aampenv * aox
aoutright                       =                       aampenv * aoy
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   BandedWG
                                //////////////////////////////////////////////
                                // By Michael Gogins.
                                //////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 512
iattack                         =                       0.005
isustain                        =                       p3
irelease                        =                       0.06
p3                              =                       isustain + iattack + irelease
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
asignal                         STKBandedWG             ifrequency,1
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   BassModel
                                //////////////////////////////////////////////
                                // Original by Hans Mikelson.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) / 35
                                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                ; p1  p2     p3   p4    p5     p6
                                ;     Start  Dur  Amp   Pitch  PluckDur
                                ; i2  128    4    1400  6.00   0.25
                                ; i2  +      2    1200  6.01   0.25
                                ; i2  .      4    1000  6.05   0.5
                                ; i2  .      2     500  6.04   1
                                ; i2  .      4    1000  6.03   0.5
                                ; i2  .      16   1000  6.00   0.5
iHz                             =                       ifrequency
ifqc                            =                       iHz
ip4                             =                       iamplitude
ip6                             =                       0.5
ipluck                          =                       1 / ifqc * ip6
kcount                          init                    0
adline                          init                    0
ablock2                         init                    0
ablock3                         init                    0
afiltr                          init                    0
afeedbk                         init                    0
koutenv                         linseg                  0, .01, 1, p3 - .11 , 1, .1 , 0 ; Output envelope
kfltenv                         linseg                  0, 1.5, 1, 1.5, 0
                                ; This envelope loads the string with a triangle wave.
kenvstr                         linseg                  0, ipluck / 4, -ip4 / 2, ipluck / 2, ip4 / 2, ipluck / 4, 0, p3 - ipluck, 0
aenvstr                         =                       kenvstr
ainput                          tone                    aenvstr, 200
                                ; DC Blocker
ablock2                         =                       afeedbk - ablock3 + .99 * ablock2
ablock3                         =                       afeedbk
ablock                          =                       ablock2
                                ; Delay line with filtered feedback
adline                          delay                   ablock + ainput, 1 / ifqc - 15 / sr
afiltr                          tone                    adline, 400
                                ; Resonance of the body
abody1                          reson                   afiltr, 110, 40
abody1                          =                       abody1 / 5000
abody2                          reson                   afiltr, 70, 20
abody2                          =                       abody2 / 50000
afeedbk                         =                       afiltr
aout                            =                       afeedbk
asignal                         =                       50 * koutenv * (aout + kfltenv * (abody1 + abody2))
iattack                         =                       0.005
isustain                        =                       p3
irelease                        =                       0.06
p3                              =                       isustain + iattack + irelease
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   ChebyshevDrone
                                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                ; By Michael Gogins.
                                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ihertz                          =                       cpsmidinn(i_midikey)
iamp                            =                       ampdb(i_midivelocity) * 6
idampingattack                  =                       .01
idampingrelease                 =                       .02
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
iattack                         init                    p3 / 4.0
idecay                          init                    p3 / 4.0
isustain                        init                    p3 / 2.0
aenvelope                       transeg                 0.0, iattack / 2.0, 2.5, iamp / 2.0, iattack / 2.0, -2.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 2.5, iamp / 2.0, idecay / 2.0, -2.5, 0.
isinetable                      ftgenonce               0, 0, 65536, 10, 1, 0, .02
asignal                         poscil3                 1, ihertz, isinetable
asignal                         chebyshevpoly           asignal, 0, gkChebyshevDroneCoefficient1, gkChebyshevDroneCoefficient2, gkChebyshevDroneCoefficient3, gkChebyshevDroneCoefficient4, gkChebyshevDroneCoefficient5, gkChebyshevDroneCoefficient6, gkChebyshevDroneCoefficient7, gkChebyshevDroneCoefficient8, gkChebyshevDroneCoefficient9, gkChebyshevDroneCoefficient10
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
asignal                         =                       asignal * aenvelope
aoutleft, aoutright             pan2                    asignal * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   ChebyshevMelody
                                ///////////////////////////////////////////////////////
                                // Original by Jon Nelson.
                                // Adapted by Michael Gogins.
                                ///////////////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
iHz                             =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 7.
iattack                         =                       .01
isustain                        =                       p3
irelease                        =                       .01
p3                              =                       iattack + isustain + irelease
adeclick                        linsegr                 0, iattack, 1, isustain, 1, irelease, 0
kHz                             =                       k(iHz)
idB                             =                       i_midivelocity
i1                              =                       iHz
k100                            randi                   1,0.05
isine                           ftgenonce               0, 0, 65536, 10, 1
k101                            poscil                  1, 5 + k100, isine
k102                            linseg                  0, .5, 1, p3, 1
k100                            =                       i1 + (k101 * k102)
; Envelope for driving oscillator.
ip3                             init                    3.0
; k1                            linenr                  0.5, ip3 * .3, ip3 * 2, 0.01
k1                              linseg                  0, ip3 * .3, .5, ip3 * 2, 0.01, isustain, 0.01, irelease, 0
; k2                            line                    1, p3, .5
k2                              linseg                  1.0, ip3, .5, isustain, .5, irelease, 0
k1                              =                       k2 * k1
; Amplitude envelope.
k10                             expseg                  0.0001, iattack, 1.0, isustain, 0.8, irelease, .0001
k10                             =                       (k10 - .0001)
; Power to partials.
k20                             linseg                  1.485, iattack, 1.5, (isustain + irelease), 1.485
; a1-3 are for cheby with p6=1-4
icook3                          ftgenonce               0, 0, 65536,    10,     1, .4, 0.2, 0.1, 0.1, .05
a1                              poscil                  k1, k100 - .25, icook3
; Tables a1 to fn13, others normalize,
ip6                             ftgenonce               0, 0, 65536,    -7,    -1, 150, 0.1, 110, 0, 252, 0
a2                              tablei                  a1, ip6, 1, .5
a3                              balance                 a2, a1
; Try other waveforms as well.
a4                              foscili                 1, k100 + .04, 1, 2.000, k20, isine
a5                              poscil                  1, k100, isine
a6                              =                       ((a3 * .1) + (a4 * .1) + (a5 * .8)) * k10
a7                              comb                    a6, .5, 1 / i1
a8                              =                       (a6 * .9) + (a7 * .1)
asignal        		            balance         	    a8, a1
asignal                         =                       asignal * iamplitude
aoutleft, aoutright		        pan2			        asignal * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   DelayedPluckedString
                                //////////////////////////////////////////////////////
                                // By Michael Gogins.
                                //////////////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
iattack                         =                       0.02
isustain                        =                       p3
irelease                        =                       0.15
p3                              =                       isustain + iattack + irelease
adeclick                        linsegr                  0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
ikeyin                          =                       i_midikey
ihertz                          =                       cpsmidinn(ikeyin)
; Detuning of strings by 4 cents each way.
idetune                         =                       4.0 / 1200.0
ihertzleft                      =                       cpsmidinn(ikeyin + idetune)
ihertzright                     =                       cpsmidinn(ikeyin - idetune)
iamplitude                      =                       ampdb(i_midivelocity)
isine                          ftgenonce                   0, 0, 65536,    10,     1
icosine                        ftgenonce                   0, 0, 65536,    11,     1
igenleft                        =                       isine
igenright                       =                       icosine
kvibrato                        oscili                  1.0 / 120.0, 7.0, icosine
kexponential                    expseg                  1.0, p3 + iattack, 0.0001, irelease, 0.0001
aenvelope                       =                       (kexponential - 0.0001) * adeclick
ag                              pluck                   iamplitude, cpsmidinn(ikeyin + kvibrato), 200, igenleft, 1
agleft                          pluck                   iamplitude, ihertzleft, 200, igenleft, 1
agright                         pluck                   iamplitude, ihertzright, 200, igenright, 1
imsleft                         =                       0.2 * 1000
imsright                        =                       0.21 * 1000
adelayleft                      vdelay                  ag * aenvelope, imsleft, imsleft + 100
adelayright                     vdelay                  ag * aenvelope, imsright, imsright + 100
asignal                         =                       adeclick * (agleft + adelayleft + agright + adelayright)
                                ; Highpass filter to exclude speaker cone excursions.
asignal1                        butterhp                asignal, 32.0
asignal2                        balance                 asignal1, asignal
aoutleft, aoutright             pan2                    asignal2 * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   EnhancedFMBell
                                //////////////////////////////////////////////////////
                                // Original by John ffitch.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////////////
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
iattack                         =                       0.005
isustain                        =                       p3
irelease                        =                       0.25
i_duraton                       =                       15; isustain + iattack + irelease
p3                              =                       i_duration
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
ifrequency                      =                       cpsmidinn(i_midikey)
; Normalize so iamplitude for p5 of 80 == ampdb(80).
iamplitude                      =                       ampdb(i_midivelocity)
idur                            =                       50
iamp                            =                       iamplitude
iffitch1                        ftgenonce               0, 0, 65536,     10,     1
iffitch2                        ftgenonce               0, 0, 8193,     5,      1, 1024, 0.01
iffitch3                        ftgenonce               0, 0, 8193,     5,      1, 1024, 0.001
ifenv                           =                       iffitch2                       ; BELL SETTINGS:
ifdyn                           =                       iffitch3                       ; AMP AND INDEX ENV ARE EXPONENTIAL
ifq1                            =                       cpsmidinn(i_midikey) ;* 5              ; DECREASING, N1:N2 IS 5:7, imax=10
if1                             =                       iffitch1                               ; DURATION = 15 sec
ifq2                            =                       cpsmidinn(i_midikey) * 5/7
if2                             =                       iffitch1
imax                            =                       10
aenv                            oscili                  iamp, 1 / idur, ifenv           ; ENVELOPE
adyn                            oscili                  ifq2 * imax, 1 / idur, ifdyn    ; DYNAMIC
anoise                          rand                    50
amod                            oscili                  adyn + anoise, ifq2, if2        ; MODULATOR
acar                            oscili                  aenv, ifq1 + amod, if1          ; CARRIER
                                timout                  0.5, idur, noisend
knenv                           linsegr                 iamp, 0.2, iamp, 0.3, 0
anoise3                         rand                    knenv
anoise4                         butterbp                anoise3, iamp, 100
anoise5                         balance                 anoise4, anoise3
noisend:
arvb                            nreverb                 acar, 2, 0.1
aenvelope                       transeg                 1, idur, -3, 0
asignal                         =                       aenvelope * (acar + arvb) ;+ anoise5
aoutleft, aoutright             pan2                    asignal * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   FenderRhodesModel
                                //////////////////////////////////////////////////////
                                // Original by Perry Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
iattack                         =                       0.01
isustain                        =                       p3
irelease                        =                       0.125
p3                              =                       isustain + iattack + irelease
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
iindex                          =                       4
icrossfade                      =                       3
ivibedepth                      =                       0.2
iviberate                       =                       6
isine                           ftgenonce               0, 0, 65536,    10,     1
icosine                         ftgenonce               0, 0, 65536,    11,     1
icookblank                      ftgenonce               0, 0, 65536,     10,     0 ; Blank wavetable for some Cook FM opcodes.
ifn1                            =                       isine
ifn2                            =                       icosine
ifn3                            =                       isine
ifn4                            =                       icookblank
ivibefn                         =                       isine
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 6
asignal                         fmrhode                 iamplitude, ifrequency, iindex, icrossfade, ivibedepth, iviberate, ifn1, ifn2, ifn3, ifn4, ivibefn
aoutleft, aoutright		        pan2			        asignal * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   FilteredSines
                                //////////////////////////////////////////////////////
                                // Original by Michael Bergeman.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////////////
                                ; Original pfields
                                ; p1 p2 p3 p4 p5 p6 p7 p8 p9
                                ; ins st dur db func at dec freq1 freq2
                                pset                    0, 0, 3600
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
iattack                         =                       0.03
isustain                        =                       p3
irelease                        =                       0.52
p3                              =                       p3 + iattack + irelease
i_duration                      =                       p3
adeclick                        linsegr                  0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
ip4                             =                       i_midivelocity
idb                             =                       ampdb(i_midivelocity) * 4
ibergeman                       ftgenonce               0, 0, 65536,     10,     0.28, 1, 0.74, 0.66, 0.78, 0.48, 0.05, 0.33, 0.12, 0.08, 0.01, 0.54, 0.19, 0.08, 0.05, 0.16, 0.01, 0.11, 0.3, 0.02, 0.2 ; Bergeman f1
ip5                             =                       ibergeman
ip3                             =                       i_duration
ip6                             =                       i_duration * 0.25
ip7                             =                       i_duration * 0.75
ip8                             =                       cpsmidinn(i_midikey - 0.01)
ip9                             =                       cpsmidinn(i_midikey + 0.01)
isc                             =                       idb * 0.333
k1                              line                    40, p3, 800
k2                              line                    440, p3, 220
k3                              linen                   isc, ip6, p3, ip7
k4                              line                    800, ip3, 40
k5                              line                    220, ip3, 440
k6                              linen                   isc, ip6, ip3, ip7
k7                              linen                   1, ip6, ip3, ip7
a5                              oscili                  k3, ip8, ip5
a6                              oscili                  k3, ip8 * 0.999, ip5
a7                              oscili                  k3, ip8 * 1.001, ip5
a1                              =                       a5 + a6 + a7
a8                              oscili                  k6, ip9, ip5
a9                              oscili                  k6, ip9 * 0.999, ip5
a10                             oscili                  k6, ip9 * 1.001, ip5
a11                             =                       a8 + a9 + a10
a2                              butterbp                a1, k1, 40
a3                              butterbp                a2, k5, k2 * 0.8
a4                              balance                 a3, a1
a12                             butterbp                a11, k4, 40
a13                             butterbp                a12, k2, k5 * 0.8
a14                             balance                 a13, a11
a15                             reverb2                 a4, 5, 0.3
a16                             reverb2                 a4, 4, 0.2
                                ; Constant-power pan.
ipi                             =                       4.0 * taninv(1.0)
iradians                        =                       i_pan * ipi / 2.0
itheta                          =                       iradians / 2.0
; Translate angle in [-1, 1] to left and right gain factors.
irightgain                      =                       sqrt(2.0) / 2.0 * (cos(itheta) + sin(itheta))
ileftgain                       =                       sqrt(2.0) / 2.0 * (cos(itheta) - sin(itheta))
a17                             =                       (a15 + a4) * ileftgain * k7
a18                             =                       (a16 + a4) * irightgain * k7
aoutleft                        =                       a17 * adeclick
aoutright                       =                       a18 * adeclick
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   Flute
                                //////////////////////////////////////////////////////
                                // Original by James Kelley.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////////////
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ikellyflute                     ftgenonce               0, 0, 65536,     10,     1, 0.25, 0.1 ; Kelley flute.
; Do some phasing.
icpsp1                          =                       cpsmidinn(i_midikey - 0.0002)
icpsp2                          =                       cpsmidinn(i_midikey + 0.0002)
ip6                             =                       ampdb(i_midivelocity)
iattack                         =                       0.04
isustain                        =                       p3
irelease                        =                       0.15
p3                              =                       isustain + iattack + irelease
adeclick                        linsegr                  0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
ip4                             =                       0
                                if                      (ip4 == int(ip4 / 2) * 2) goto initslurs
                                ihold
initslurs:
iatttm                          =                       0.09
idectm                          =                       0.1
isustm                          =                       p3 - iatttm - idectm
idec                            =                       ip6
ireinit                         =                       -1
                                if                      (ip4 > 1) goto checkafterslur
ilast                           =                       0
checkafterslur:
                                if                      (ip4 == 1 || ip4 == 3) goto doneslurs
idec                            =                       0
ireinit                         =                       0
; KONTROL
doneslurs:
                                if                      (isustm <= 0)   goto simpleenv
kamp                            linsegr                  ilast, iatttm, ip6, isustm, ip6, idectm, idec, 0, idec
                                goto                    doneenv
simpleenv:
kamp                            linsegr                  ilast, p3 / 2,ip6, p3 / 2, idec, 0, idec
doneenv:
ilast                           =                       ip6
; Some vibrato.
kvrandamp                       rand                    0.04
kvamp                           =                       ((8 + p4) *.03 + kvrandamp) / 2
kvrandfreq                      rand                    1
kvfreq                          =                       4.5 + kvrandfreq
isine                           ftgenonce               0, 0, 65536,    10,     1
kvbra                           oscili                  kvamp, kvfreq, isine, ireinit
kfreq1                          =                       icpsp1 + kvbra
kfreq2                          =                       icpsp2 + kvbra
; Noise for burst at beginning of note.
knseenv                         expon                   ip6 / 4, 0.2, 1
; AUDIO
anoise1                         rand                    knseenv
anoise                          tone                    anoise1, 200
a1                              oscili                  kamp, kfreq1, ikellyflute, ireinit
a2                              oscili                  kamp, kfreq2, ikellyflute, ireinit
a3                              =                       a1 + a2 + anoise
aoutleft, aoutright             pan2                    a3 * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   FMModerateIndex
                                //////////////////////////////////////////////////////
                                // By Michael Gogins.
                                //////////////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 3
icarrier                        =                       1
iratio                          =                       1.25
ifmamplitude                    =                       8
index                           =                       5.4
iattack                         =                       0.01
isustain                        =                       p3
irelease                        =                       0.05
p3                              =                       isustain + iattack + irelease
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
ifrequencyb                     =                       ifrequency * 1.003
icarrierb                       =                       icarrier * 1.004
kindenv                         transeg                 0, iattack, -4, 1,  isustain, -2, 0.125, irelease, -4, 0
kindex                          =                       kindenv * index * ifmamplitude
isine                           ftgenonce               0, 0, 65536,    10,     1
aouta                           foscili                 1, ifrequency, icarrier, iratio, index, isine
aoutb                           foscili                 1, ifrequencyb, icarrierb, iratio, index, isine
asignal                         =                       (aouta + aoutb) * kindenv
aoutleft, aoutright		        pan2			        asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   FMModerateIndex2
                                //////////////////////////////////////////////////////
                                // By Michael Gogins.
                                //////////////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 3
icarrier                        =                       1
iratio                          =                       1
ifmamplitude                    =                       6
index                           =                       2.5
iattack                         =                       0.02
isustain                        =                       p3
irelease                        =                       0.05
p3                              =                       isustain + iattack + irelease
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
ifrequencyb                     =                       ifrequency * 1.003
icarrierb                       =                       icarrier * 1.004
kindenv                         expseg                  0.000001, iattack, 1.0, isustain, 0.0125, irelease, 0.000001
kindex                          =                       kindenv * index * ifmamplitude - 0.000001
isine                           ftgenonce               0, 0, 65536,    10,     1
aouta                           foscili                 1, ifrequency, icarrier, iratio, index, isine
aoutb                           foscili                 1, ifrequencyb, icarrierb, iratio, index, isine
asignal                         =                       (aouta + aoutb) * kindenv
aoutleft, aoutright		        pan2			        asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   FMModulatedChorusing
                                //////////////////////////////////////////////
                                // Original by Thomas Kung.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
iattack                         =                       0.333333
irelease                        =                       0.1
isustain                        =                       p3
p3                              =                       isustain + iattack + irelease
adeclick                        linsegr                  0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
//iamplitude                      =                       ampdb(i_midikey) / 1800
iamplitude                      =                       ampdb(i_midikey) / 1800
ip6                             =                       0.3
ip7                             =                       2.2
                                ; shift it.
ishift                          =                       4.0 / 12000
                                ; convert parameter 5 to cps.
ipch                            =                       cpsmidinn(i_midikey)
                                ; convert parameter 5 to oct.
ioct                            =                       i_midikey
kadsr                           linen                   1.0, iattack, irelease, 0.01
kmodi                           linseg                  0, iattack, 5, isustain, 2, irelease, 0
                                ; r moves from ip6 to ip7 in p3 secs.
kmodr                           linseg                  ip6, p3, ip7
a1                              =                       kmodi * (kmodr - 1 / kmodr) / 2
                                ; a1*2 is argument normalized from 0-1.
a1ndx                           =                       abs(a1 * 2 / 20)
a2                              =                       kmodi * (kmodr + 1 / kmodr) / 2
                                ; Look up table is in f43, normalized index.
iln                             ftgenonce               0, 0, 65536,     -12,    20.0 ; Unscaled ln(I(x)) from 0 to 20.0.
a3                              tablei                  a1ndx, iln, 1
icosine                         ftgenonce                   0, 0, 65536,    11,     1 ; Cosine wave. Get that noise down on the most widely used table!
ao1                             oscili                  a1, ipch, icosine
a4                              =                       exp(-0.5 * a3 + ao1)
                                ; Cosine
ao2                             oscili                  a2 * ipch, ipch, icosine
isine                           ftgenonce               2, 0, 65536,    10,     1
                                ; Final output left
aoutl                           oscili                  1 * kadsr * a4, ao2 + cpsmidinn(ioct + ishift), isine
                                ; Final output right
aoutr                           oscili                  1 * kadsr * a4, ao2 + cpsmidinn(ioct - ishift), isine
asignal                         =                       aoutl + aoutr
asignal                         =                       asignal * iamplitude / 1.9
aoutleft, aoutright		        pan2			        asignal * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   FMWaterBell
                                //////////////////////////////////////////////
                                // Original by Steven Yi.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ipch                            =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 2.0
ipch2                           =                       ipch
kpchline 	                    line                    ipch, i_duration, ipch2
iamp 	                        =                       2
ienvType	                    =                       0
kenv 	                        init 	                0
                                if ienvType == 0 kgoto env0  ; adsr
                                if ienvType == 1 kgoto env1  ; pyramid
                                if ienvType == 2 kgoto env2  ; ramp
env0:
kenv	                        adsr	                .3, .2, .9, .5
                                kgoto                   endEnvelope
env1:
kenv 	                        linseg	                0, i_duration * .5, 1, i_duration * .5, 0
                                kgoto                   endEnvelope
env2:
kenv	                        linseg 	                0, i_duration - .1, 1, .1, 0
kgoto                           endEnvelope
endEnvelope:
kc1                             =                       5
kc2                             =                       5
;kvdepth                         =                       0.005
kvdepth                         =                       0.0025
kvrate                          =                       6
icosine                  	    ftgenonce               0, 0, 65536, 11, 1
ifn1                            =                       icosine
ifn2                            =                       icosine
ifn3                            =                       icosine
ifn4                            =                       icosine
ivfn                            =                       icosine
asignal                         fmbell	                iamp, kpchline, kc1, kc2, kvdepth, kvrate, ifn1, ifn2, ifn3, ifn4, ivfn
iattack                         =                       0.003
isustain                        =                       p3
irelease                        =                       0.06
p3                              =                       isustain + iattack + irelease
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
aoutleft, aoutright             pan2                    iamplitude * asignal * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   Granular
                                //////////////////////////////////////////////
                                // Original by Hans Mikelson.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) / 135
                                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                ; f1  0 65536 1 "hahaha.aif" 0 4 0
                                ; f2  0 1024  7 0 224 1 800 0
                                ; f3  0 8192  7 1 8192 -1
                                ; f4  0 1024  7 0 512 1 512 0
                                ; f5  0 1024 10 1 .3 .1 0 .2 .02 0 .1 .04
                                ; f6  0 1024 10 1 0 .5 0 .33 0 .25 0 .2 0 .167
                                ; a0 14 50
                                ; p1   p2     p3    p4    p5    p6     p7      p8      p9    p10
                                ;      Start  Dur  Amp    Freq  GrTab  WinTab  FqcRng  Dens  Fade
                                ; i1   0.0    6.5  700    9.00  5      4       .210    200   1.8
                                ; i1   3.2    3.5  800    7.08  .      4       .042    100   0.8
                                ; i1   5.1    5.2  600    7.10  .      4       .032    100   0.9
                                ; i1   7.2    6.6  900    8.03  .      4       .021    150   1.6
                                ; i1  21.3    4.5  1000   9.00  .      4       .031    150   1.2
                                ; i1  26.5   13.5  1100   6.09  .      4       .121    150   1.5
                                ; i1  30.7    9.3  900    8.05  .      4       .014    150   2.5
                                ; i1  34.2    8.8  700   10.02  .      4       .14     150   1.6
igrtab                          ftgenonce               0, 0, 65536,    10,     1, .3, .1, 0, .2, .02, 0, .1, .04
iwintab                         ftgenonce               0, 0, 65536,    10,     1, 0, .5, 0, .33, 0, .25, 0, .2, 0, .167
iHz                             =                       ifrequency
ip4                             =                       iamplitude
ip5                             =                       iHz
ip6                             =                       igrtab
ip7                             =                       iwintab
ip8                             =                       0.033
ip9                             =                       150
ip10                            =                       1.6
idur                            =                       p3
iamp                            =                       iamplitude ; p4
ifqc                            =                       iHz ; cpspch(p5)
igrtab                          =                       ip6
iwintab                         =                       ip7
ifrng                           =                       ip8
idens                           =                       ip9
ifade                           =                       ip10
igdur                           =                       0.2
kamp                            linseg                  0, ifade, 1, idur - 2 * ifade, 1, ifade, 0
;                                                       Amp   Fqc    Dense  AmpOff PitchOff      GrDur  GrTable   WinTable  MaxGrDur
aoutl                           grain                   ip4,  ifqc,  idens, 100,   ifqc * ifrng, igdur, igrtab,   iwintab,  5
aoutr                           grain                   ip4,  ifqc,  idens, 100,   ifqc * ifrng, igdur, igrtab,   iwintab,  5
aoutleft                        =                       aoutl * kamp * iamplitude
aoutright                       =                       aoutr * kamp * iamplitude
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   Guitar
                                //////////////////////////////////////////////
                                // By Michael Gogins.
                                //////////////////////////////////////////////
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 20.0
iattack                         =                       0.01
isustain                        =                       p3
irelease                        =                       0.05
p3                              =                       isustain + iattack + irelease
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
kamp                            linsegr                 0.0, iattack, iamplitude, isustain, iamplitude, irelease, 0.0
asigcomp                        pluck                   1, 440, 440, 0, 1
asig                            pluck                   1, ifrequency, ifrequency, 0, 1
af1                             reson                   asig, 110, 80
af2                             reson                   asig, 220, 100
af3                             reson                   asig, 440, 80
aout                            balance                 0.6 * af1+ af2 + 0.6 * af3 + 0.4 * asig, asigcomp
kexp                            expseg                  1.0, iattack, 2.0, isustain, 1.0, irelease, 1.0
kenv                            =                       kexp - 1.0
asignal                         =                       aout * kenv * kamp
asignal                         dcblock2                 asignal
aoutleft, aoutright             pan2                    asignal * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                endin

                                instr                   Guitar2
                                //////////////////////////////////////////////////////
                                // By Michael Gogins.
                                //////////////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 12
iattack                         =                       0.01
isustain                        =                       p3
irelease                        =                       0.05
p3                              =                       isustain + iattack + irelease
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
kamp                            linsegr                  0.0, iattack, 1, isustain, 1, irelease, 0.0
asigcomp                        pluck                   kamp, 440, 440, 0, 1
asig                            pluck                   kamp, ifrequency, ifrequency, 0, 1
af1                             reson                   asig, 110, 80
af2                             reson                   asig, 220, 100
af3                             reson                   asig, 440, 80
aout                            balance                 0.6 * af1+ af2 + 0.6 * af3 + 0.4 * asig, asigcomp
kexp                            expseg                  1.0, iattack, 2.0, isustain, 1.0, irelease, 1.0
kenv                            =                       kexp - 1.0
asignal                         =                       aout * kenv
asignal                         dcblock2                 asignal
aoutleft, aoutright		        pan2			        asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr 			        Harpsichord
                                //////////////////////////////////////////////
                                // Original by James Kelley.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
insno           		        =                       p1
itime           		        =                       p2
iduration       		        =                       p3
ikey            		        =                       p4
ivelocity                       =                       p5
iphase                          =                       p6
ipan                            =                       p7
idepth                          =                       p8
iheight                         =                       p9
ipcs                            =                       p10
ihomogeneity                    =                       p11
gkHarpsichordGain               =                       .25
gkHarpsichordPan                =                       .5
iattack                         =                       .005
isustain                        =                       p3
irelease                        =                       .3
p3                              =                       iattack + isustain + irelease
iHz                             =                       cpsmidinn(ikey)
kHz                             =                       k(iHz)
iamplitude                      =                       ampdb(ivelocity) * 36
aenvelope               	    transeg                 1.0, 20.0, -10.0, 0.05
apluck                  	    pluck                   1, kHz, iHz, 0, 1
iharptable              	    ftgenonce               0, 0, 65536,  7, -1, 1024, 1, 1024, -1
aharp                   	    poscil                  1, kHz, iharptable
aharp2                  	    balance                 apluck, aharp
asignal			                =                       (apluck + aharp2) * iamplitude * aenvelope * gkHarpsichordGain
adeclick                        linsegr                 0, iattack, 1, isustain, 1, irelease, 0
aoutleft, aoutright             pan2                    asignal * adeclick, ipan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   HeavyMetalModel
                                //////////////////////////////////////////////
                                // Original by Perry Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
iattack                         =                       0.01
idecay                          =                       2.0
isustain                        =                       i_duration
irelease                        =                       0.125
p3                              =                       iattack + iattack + idecay + irelease
adeclick                        linsegr                 0.0, iattack, 1.0, idecay + isustain, 1.0, irelease, 0.0
iindex                          =                       1
icrossfade                      =                       3
ivibedepth                      =                       0.02
iviberate                       =                       4.8
isine                           ftgenonce               0, 0, 65536,    10,     1
icosine                         ftgenonce               0, 0, 65536,    11,     1 ; Cosine wave. Get that noise down on the most widely used table!
iexponentialrise                ftgenonce               0, 0, 65536,     5,      0.001, 513, 1 ; Exponential rise.
ithirteen                       ftgenonce               0, 0, 65536,     9,      1, 0.3, 0
ifn1                            =                       isine
ifn2                            =                       iexponentialrise
ifn3                            =                       ithirteen
ifn4                            =                       isine
ivibefn                         =                       icosine
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 48.0
adecay                          transeg                 0.0, iattack, 4, 1.0, idecay + isustain, -4, 0.1, irelease, -4, 0.0
asignal                         fmmetal                 0.1, ifrequency, iindex, icrossfade, ivibedepth, iviberate, ifn1, ifn2, ifn3, ifn4, ivibefn
asignal                         =                       asignal * iamplitude
aoutleft, aoutright             pan2                    asignal * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   Hypocycloid
                                //////////////////////////////////////////////
                                // Original by Hans Mikelson.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ; This set of parametric equations defines the path traced by
                                ; a point on a circle of radius B rotating inside a circle of
                                ; radius A.
                                ;   p1  p2     p3   p4    p5     p6   p7   p8
                                ;       Start  Dur  Amp   Frqc   A    B    Hole
                                ; i 3   16     6    8000  8.00  10    2    1
                                ; i 3   20     4    .     7.11   5.6  0.4  0.8
                                ; i 3   +      4    .     8.05   2    8.5  0.7
                                ; i 3   .      2    .     8.02   4    5    0.6
                                ; i 3   .      2    .     8.02   5    0.5  1.2
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 4
iHz                             =                       ifrequency
ifqc                            init                    iHz
ip4                             init                    iamplitude
ifqci                           init                    iHz
ia                              =                       0.6 ; p6
ib                              =                       0.4 ; p7
ihole                           =                       0.8 ; p8
iscale                          =                       (ia < ib ? 1 / ib : 1 / ia)
kampenv                         linseg                  0, .1, ip4 * iscale, p3 - .2, ip4 * iscale, .1, 0
kptchenv                        linseg                  ifqci, .2 * p3, ifqc, .8 * p3, ifqc
kvibenv                         linseg                  0, .5, 0, .2, 1, .2, 1
isine                  	        ftgenonce               0, 0, 65536, 10, 1
icosine                  	    ftgenonce               0, 0, 65536, 11, 1
kvibr                           oscili                  20, 8, icosine
kfqc                            =                       kptchenv+kvibr*kvibenv
                                ; Sine and Cosine
acos1                           oscili                  ia - ib, kfqc, icosine
acos2                           oscili                  ib * ihole, (ia - ib) / ib * kfqc, icosine
ax                              =                       acos1 + acos2
asin1                           oscili                  ia-ib, kfqc, isine
asin2                           oscili                  ib, (ia - ib) / ib * kfqc, isine
ay                              =                       asin1 - asin2
aoutleft                        =                       kampenv * ax
aoutright                       =                       kampenv * ay
                                ; Constant-power pan.
ipi                             =                       4.0 * taninv(1.0)
iradians                        =                       i_pan * ipi / 2.0
itheta                          =                       iradians / 2.0
                                ; Translate angle in [-1, 1] to left and right gain factors.
irightgain                      =                       sqrt(2.0) / 2.0 * (cos(itheta) + sin(itheta))
ileftgain                       =                       sqrt(2.0) / 2.0 * (cos(itheta) - sin(itheta))
                                outleta                 "outleft",  aoutleft * ileftgain
                                outleta                 "outright", aoutright * irightgain
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr			        ModerateFM
                                //////////////////////////////////////////////
                                // By Michael Gogins.
                                //////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
iattack			                =			            0.002
isustain		                =			            p3
idecay				            =			            8
irelease		                =			            0.05
iHz                             =                       cpsmidinn(i_midikey)
idB                             =                       i_midivelocity
iamplitude                      =                       ampdb(idB) * 4.0
icarrier                	    =                       1
imodulator              	    =                       0.5
ifmamplitude            	    =                       0.25
index                   	    =                       .5
ifrequencyb             	    =                       iHz * 1.003
icarrierb               	    =                       icarrier * 1.004
aindenv                 	    transeg                 0.0, iattack, -11.0, 1.0, idecay, -7.0, 0.025, isustain, 0.0, 0.025, irelease, -7.0, 0.0
aindex                  	    =                       aindenv * index * ifmamplitude
isinetable                      ftgenonce               0, 0, 65536, 10, 1, 0, .02
; ares                  	    foscili                 xamp, kcps, xcar, xmod, kndx, ifn [, iphs]
aouta                   	    foscili                 1.0, iHz, icarrier, imodulator, index / 4., isinetable
aoutb                   	    foscili                 1.0, ifrequencyb, icarrierb, imodulator, index, isinetable
; Plus amplitude correction.
asignal               		    =                       (aouta + aoutb) * aindenv
adeclick                        linsegr                 0, iattack, 1, isustain, 1, irelease, 0
asignal                         =                       asignal * iamplitude
aoutleft, aoutright             pan2                    asignal * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr 			        ModulatedFM
                                //////////////////////////////////////////////
                                // Original by Thomas Kung.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
iattack                         =                       .25
isustain                        =                       p3
irelease                        =                       .33333333
p3                              =                       iattack + isustain + irelease
iHz                             =                       cpsmidinn(i_midikey)
kHz                             =                       k(iHz)
idB                             =                       i_midivelocity
iamplitude                      =                       ampdb(i_midivelocity)
adeclick                        linsegr                 0, iattack, 1, isustain, 1, irelease, 0
ip6                     	    =                       0.3
ip7                     	    =                       2.2
ishift      		    	    =           		    4.0 / 12000.0
kpch       		                =           		    kHz
koct        		    	    =           		    octcps(kHz)
aadsr                   	    linen                   1.0, iattack, irelease, 0.01
amodi                   	    linseg                  0, iattack, 5, p3, 2, irelease, 0
; r moves from ip6 to ip7 in p3 secs.
amodr                   	    linseg                  ip6, p3, ip7
a1                      	    =                       amodi * (amodr - 1 / amodr) / 2
; a1*2 is argument normalized from 0-1.
a1ndx                   	    =                       abs(a1 * 2 / 20)
a2                      	    =                       amodi * (amodr + 1 / amodr) / 2
; Unscaled ln(I(x)) from 0 to 20.0.
iln                    		    ftgenonce               0, 0, 65536, -12, 20.0
a3                      	    tablei                  a1ndx, iln, 1
icosine                  	    ftgenonce               0, 0, 65536, 11, 1
ao1                     	    poscil                  a1, kpch, icosine
a4                      	    =                       exp(-0.5 * a3 + ao1)
; Cosine
ao2                     	    poscil                  a2 * kpch, kpch, icosine
isine                  		    ftgenonce               0, 0, 65536, 10, 1
; Final output left
aleft                   	    poscil                  a4, ao2 + cpsoct(koct + ishift), isine
; Final output right
aright                  	    poscil                  a4, ao2 + cpsoct(koct - ishift), isine
asignal                         =                       (aleft + aright) * iamplitude
aoutleft, aoutright             pan2                    asignal * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   PlainPluckedString
                                //////////////////////////////////////////////////////
                                // By Michael Gogins.
                                //////////////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
iattack                         =                       0.002
isustain                        =                       p3
irelease                        =                       0.075
p3                              =                       isustain + iattack + irelease
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 6
aenvelope                       transeg                 0, iattack, -4, iamplitude,  isustain, -4, iamplitude / 10.0, irelease, -4, 0
asignal1                        pluck                   1, ifrequency, ifrequency * 1.002, 0, 1
asignal2                        pluck                   1, ifrequency * 1.003, ifrequency, 0, 1
asignal                         =                       (asignal1 + asignal2) * aenvelope
aoutleft, aoutright		        pan2			        asignal * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   PRCBeeThree
                                //////////////////////////////////////////////////////
                                // By Michael Gogins.
                                //////////////////////////////////////////////////////
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                      cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 6
iattack                         =                       0.005
isustain                        =                       p3
irelease                        =                       0.06
p3                              =                       isustain + iattack + irelease
asignal                         STKBeeThree             ifrequency, 1
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                               instr                   PRCBeeThreeDelayed
                                //////////////////////////////////////////////////////
                                // By Michael Gogins.
                                //////////////////////////////////////////////////////
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 6
iattack                         =                       0.2
isustain                        =                       p3
irelease                        =                       0.3
p3                              =                       isustain + iattack + irelease
asignal                         STKBeeThree             ifrequency, 1, 2, 3, 1, 0, 11, 0
amodulator                      oscils                  0.00015, 0.2, 0.0
                                ; Read delayed signal, first delayr instance:
adump                           delayr                  4.0
adly1                           deltapi                 0.03 + amodulator; associated with first delayr instance
                                ; Read delayed signal, second delayr instance:
adump                           delayr                  4.0
adly2                           deltapi                 0.029 + amodulator      ; associated with second delayr instance
                                ; Do some cross-coupled manipulation:
afdbk1                          =                       0.7 * adly1 + 0.7 * adly2 + asignal
afdbk2                          =                       -0.7 * adly1 + 0.7 * adly2 + asignal
                                ; Feed back signal, associated with first delayr instance:
                                delayw                  afdbk1
                                ; Feed back signal, associated with second delayr instance:
                                delayw                  afdbk2
asignal2                        =                       adly1 + adly2
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
aoutleft, aoutright             pan2                    asignal2 * iamplitude * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   PRCBowed
                                //////////////////////////////////////////////////////
                                // By Michael Gogins.
                                //////////////////////////////////////////////////////
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 6
                                ; Controllers:
                                ;   1  Vibrato Gain
                                ;   2  Bow Pressure
                                ;   4  Bow Position
                                ;  11  Vibrato Frequency
                                ; 128  Volume
asignal 		                STKBowed 		        ifrequency, 1.0, 1, 1.8, 2, 120.0, 4, 50.0, 11, 20.0
iattack                         =                       0.005
isustain                        =                       p3
irelease                        =                       0.06
p3                              =                       isustain + iattack + irelease
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   STKBandedWG
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 256
asignal 		                STKBandedWG 		    ifrequency, 1.0
iattack                         =                       0.005
isustain                        =                       p3
irelease                        =                       0.06
p3                              =                       isustain + iattack + irelease
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   STKBeeThree
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 16
asignal 		                STKBeeThree 		    ifrequency, 1.0, 1, 1.5, 2, 4.8, 4, 2.1
aphased                         phaser1                 asignal, 4000, 16, .2, .9
idampingattack                  =                       .002
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    aphased * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   STKBlowBotl
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 4
asignal 		                STKBlowBotl 		    ifrequency, 1.0
idampingattack                  =                       .002
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   STKBlowHole
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 4
asignal 		                STKBlowHole 		    ifrequency, 1.0
idampingattack                  =                       .002
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   STKBowed
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 8
                                ; Controllers:
                                ;   1  Vibrato Gain
                                ;   2  Bow Pressure
                                ;   4  Bow Position
                                ;  11  Vibrato Frequency
                                ; 128  Volume
asignal 		                STKBowed 		        ifrequency, 1.0, 1, 0.8, 2, 120.0, 4, 20.0, 11, 20.0
idampingattack                  =                       .002
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   STKClarinet
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 4
asignal 		                STKClarinet 		    ifrequency, 1.0, 1, 1.5
idampingattack                  =                       .002
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   STKDrummer
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 8
asignal 		                STKDrummer 		        ifrequency, 1.0
idampingattack                  =                       .002
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   STKFlute
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 4
                                ; Control Change Numbers:
                                ;    * Jet Delay = 2
                                ;    * Noise Gain = 4
                                ;    * Vibrato Frequency = 11
                                ;    * Vibrato Gain = 1
                                ;    * Breath Pressure = 128
asignal 		                STKFlute 		        ifrequency, 1.0, 128, 100, 2, 70, 4, 10
idampingattack                  =                       .002
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   STKFMVoices
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 4
                                ; Control Change Numbers:
                                ;    * Vowel = 2
                                ;    * Spectral Tilt = 4
                                ;    * LFO Speed = 11
                                ;    * LFO Depth = 1
                                ;    * ADSR 2 & 4 Target = 128
asignal 		                STKFMVoices 		    ifrequency, 1.0, 2, 1, 4, 3.0, 11, 5, 1, .8
idampingattack                  =                       .002
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   STKHvyMetl
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 8
                                ; Control Change Numbers:
                                ;    * Total Modulator Index = 2
                                ;    * Modulator Crossfade = 4
                                ;    * LFO Speed = 11
                                ;    * LFO Depth = 1
                                ;    * ADSR 2 & 4 Target = 128
asignal 		                STKHevyMetl 		    ifrequency, 1.0, 2, 17.0, 4, 70, 128, 80
idampingattack                  =                       .0003
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   STKMandolin
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 24
asignal 		                STKMandolin 		    ifrequency, 1.0
idampingattack                  =                       .0003
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   STKModalBar
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 24
                                ; Control Change Numbers:
                                ;    * Stick Hardness = 2
                                ;    * Stick Position = 4
                                ;    * Vibrato Gain = 1
                                ;    * Vibrato Frequency = 11
                                ;    * Direct Stick Mix = 8
                                ;    * Volume = 128
                                ;    * Modal Presets = 16
                                ;          o Marimba = 0
                                ;          o Vibraphone = 1
                                ;          o Agogo = 2
                                ;          o Wood1 = 3
                                ;          o Reso = 4
                                ;          o Wood2 = 5
                                ;          o Beats = 6
                                ;          o Two Fixed = 7
                                ;          o Clump = 8
asignal 		                STKModalBar 		    ifrequency, 1.0, 16, 1
idampingattack                  =                       .0003
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                 instr                   STKMoog
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 8
asignal 		                STKMoog 		        ifrequency, 1.0
idampingattack                  =                       .0003
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   STKPercFlut
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 8
asignal 		                STKPercFlut 		    ifrequency, 1.0
idampingattack                  =                       .0003
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   STKPlucked
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 16
asignal 		                STKPlucked 		        ifrequency, 1.0
idampingattack                  =                       .0003
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   STKResonate
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity)
                                ;Control Change Numbers:
                                ;    * Resonance Frequency (0-Nyquist) = 2
                                ;    * Pole Radii = 4
                                ;    * Notch Frequency (0-Nyquist) = 11
                                ;    * Zero Radii = 1
                                ;    * Envelope Gain = 128
asignal 		                STKResonate 		    ifrequency, 1.;, 2, 40, 4, .7, 11, 120, 1, .5
idampingattack                  =                       .0003
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   STKRhodey
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 8
asignal 		                STKRhodey 		        ifrequency, 1
idampingattack                  =                       .0003
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   STKSaxofony
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 4
                                ; Control Change Numbers:
                                ;    * Reed Stiffness = 2
                                ;    * Reed Aperture = 26
                                ;    * Noise Gain = 4
                                ;    * Blow Position = 11
                                ;    * Vibrato Frequency = 29
                                ;    * Vibrato Gain = 1
                                ;    * Breath Pressure = 128
asignal 		                STKSaxofony 		    ifrequency, 1.0, 2, 80, 11, 100;, 29, 5, 1, 12
idampingattack                  =                       .0003
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   STKShakers
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 256
                                ;Control Change Numbers:
                                ;    * Shake Energy = 2
                                ;    * System Decay = 4
                                ;    * Number Of Objects = 11
                                ;    * Resonance Frequency = 1
                                ;    * Shake Energy = 128
                                ;    * Instrument Selection = 1071
                                ;          o Maraca = 0
                                ;          o Cabasa = 1
                                ;          o Sekere = 2
                                ;          o Guiro = 3
                                ;          o Water Drops = 4
                                ;          o Bamboo Chimes = 5
                                ;          o Tambourine = 6
                                ;          o Sleigh Bells = 7
                                ;          o Sticks = 8
                                ;          o Crunch = 9
                                ;          o Wrench = 10
                                ;          o Sand Paper = 11
                                ;          o Coke Can = 12
                                ;          o Next Mug = 13
                                ;          o Penny + Mug = 14
                                ;          o Nickle + Mug = 15
                                ;          o Dime + Mug = 16
                                ;          o Quarter + Mug = 17
                                ;          o Franc + Mug = 18
                                ;          o Peso + Mug = 19
                                ;          o Big Rocks = 20
                                ;          o Little Rocks = 21
                                ;          o Tuned Bamboo Chimes = 22
asignal 		                STKShakers 		        ifrequency, 1.0, 1071, 22, 11, 4;, 128, 100, 1, 30
idampingattack                  =                       .0003
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   STKSimple
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 64
                                ; Control Change Numbers:
                                ;    * Filter Pole Position = 2
                                ;    * Noise/Pitched Cross-Fade = 4
                                ;    * Envelope Rate = 11
                                ;    * Gain = 128
asignal 		                STKSimple 		        ifrequency, 1.0, 2, 98, 4, 50, 11, 3
idampingattack                  =                       .0003
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   STKSitar
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 8
asignal 		                STKSitar 		        ifrequency, 1.0
idampingattack                  =                       .0003
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   STKTubeBell
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 8
asignal 		                STKTubeBell 		    ifrequency, 1.0
idampingattack                  =                       .0003
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   STKVoicForm
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 16
asignal 		                STKVoicForm 		    ifrequency, 1.0
idampingattack                  =                       .0003
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   STKWhistle
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 4
asignal 		                STKWhistle 		        ifrequency, 1.0
idampingattack                  =                       .0003
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   STKWurley
                                //////////////////////////////////////////////
                                // Original by Perry R. Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 16
asignal 		                STKWurley 		        ifrequency, 1.0
idampingattack                  =                       .0003
idampingrelease                 =                       .01
idampingsustain                 =                       p3
iduration                       =                       idampingattack + idampingsustain + idampingrelease
p3                              =                       iduration
adeclick                        linsegr                 0, idampingattack, 1, idampingsustain, 1, idampingrelease, 0
aoutleft, aoutright             pan2                    asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   StringPad
                                //////////////////////////////////////////////
                                // Original by Anthony Kozar.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ; String-pad borrowed from the piece "Dorian Gray",
                                ; http://akozar.spymac.net/music/ Modified to fit my needs
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ihz                             =                       cpsmidinn(i_midikey)
iamp                            =                       ampdb(i_midivelocity) * 3
idb                             =                       i_midivelocity
ipos                            =                       i_pan
                                ; Slow attack and release
akctrl                           linsegr                     0, i_duration * 0.5, iamp, i_duration *.5, 0
                                ; Slight chorus effect
iwave                           ftgenonce                   0, 0, 65536,    10,     1, 0.5, 0.33, 0.25,  0.0, 0.1,  0.1, 0.1
afund                           oscili                      akctrl, ihz,        iwave       ; audio oscillator
acel1                           oscili                      akctrl, ihz - 0.1,  iwave       ; audio oscillator - flat
acel2                           oscili                      akctrl, ihz + 0.1,  iwave       ; audio oscillator - sharp
asig                            =                           afund + acel1 + acel2
                                ; Cut-off high frequencies depending on midi-velocity
                                ; (larger velocity implies more brighter sound)
asignal                         butterlp                asig, (i_midivelocity - 60) * 40 + 900
iattack                         =                       0.005
isustain                        =                       p3
irelease                        =                       0.06
p3                              =                       isustain + iattack + irelease
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
aoutleft, aoutright             pan2                    asignal * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   ToneWheelOrgan
                                //////////////////////////////////////////////
                                // Original by Hans Mikelson.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) / 8.0
iattack                         =                       0.02
isustain                        =                       i_duration
irelease                        =                       0.1
i_duration                      =                       iattack + isustain + irelease
p3                              =                       i_duration
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
                                ; Rotor Tables
itonewheel1                     ftgenonce               0, 0, 65536,     10,     1, 0.02, 0.01
itonewheel2                     ftgenonce               0, 0, 65536,     10,     1, 0,    0.2, 0, 0.1, 0, 0.05, 0, 0.02
                                ; Rotating Speaker Filter Envelopes
itonewheel3                     ftgenonce               0, 0, 65536,     7,      0, 110, 0, 18, 1, 18, 0, 110, 0
itonewheel4                     ftgenonce               0, 0, 65536,     7,      0, 80, 0.2, 16, 1, 64, 1, 16, 0.2, 80, 0
                                ; Distortion Tables
itonewheel5                     ftgenonce               0, 0, 65536,     8,      -.8, 336, -.78,  800, -.7, 5920, 0.7,  800, 0.78, 336, 0.8
itonewheel6                     ftgenonce               0, 0, 65536,     8,       -.8, 336, -.76, 3000, -.7, 1520, 0.7, 3000, 0.76, 336, 0.8
icosine                  	    ftgenonce               0, 0, 65536, 11, 1
iphase                          =                       p2
ikey                            =                       12 * int(i_midikey - 6) + 100 * (i_midikey - 6)
ifqc                            =                       ifrequency
                                ; The lower tone wheels have increased odd harmonic content.
iwheel1                         =                       ((ikey - 12) > 12 ? itonewheel1 : itonewheel2)
iwheel2                         =                       ((ikey +  7) > 12 ? itonewheel1 : itonewheel2)
iwheel3                         =                        (ikey       > 12 ? itonewheel1 : itonewheel2)
iwheel4                         =                       icosine
;   insno Start Dur   Amp   Pitch  SubFund  Sub3rd  Fund  2nd  3rd  4th  5th  6th  8th
; i 1     0     6     200   8.04   8        8       8     8    3    2    1    0    4
asubfund                        oscili                  8, 0.5    * ifqc,  iwheel1, iphase / (ikey - 12)
asub3rd                         oscili                  8, 1.4983 * ifqc,  iwheel2, iphase / (ikey +  7)
afund                           oscili                  8,          ifqc,  iwheel3, iphase /  ikey
a2nd                            oscili                  8, 2      * ifqc,  iwheel4, iphase / (ikey + 12)
a3rd                            oscili                  3, 2.9966 * ifqc,  iwheel4, iphase / (ikey + 19)
a4th                            oscili                  2, 4      * ifqc,  iwheel4, iphase / (ikey + 24)
a5th                            oscili                  1, 5.0397 * ifqc,  iwheel4, iphase / (ikey + 28)
a6th                            oscili                  0, 5.9932 * ifqc,  iwheel4, iphase / (ikey + 31)
a8th                            oscili                  4, 8      * ifqc,  iwheel4, iphase / (ikey + 36)
asignal                         =                       iamplitude * (asubfund + asub3rd + afund + a2nd + a3rd + a4th + a5th + a6th + a8th)
aoutleft, aoutright             pan2                    asignal * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   TubularBellModel
                                //////////////////////////////////////////////////////
                                // Original by Perry Cook.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 4
iattack                         =                       0.003
isustain                        =                       p3
irelease                        =                       0.125
p3                              =                       isustain + iattack + irelease
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
iindex                          =                       1
icrossfade                      =                       2
ivibedepth                      =                       0.2
iviberate                       =                       6
isine                           ftgenonce               0, 0, 65536,    10,     1
icosine                         ftgenonce               0, 0, 65536,    11,     1 ; Cosine wave. Get that noise down on the most widely used table!
icook3                          ftgenonce               0, 0, 65536,    10,     1, 0.4, 0.2, 0.1, 0.1, 0.05
ifn1                            =                       isine
ifn2                            =                       icook3
ifn3                            =                       isine
ifn4                            =                       isine
ivibefn                         =                       icosine
asignal                         fmbell                  1.0, ifrequency, iindex, icrossfade, ivibedepth, iviberate, ifn1, ifn2, ifn3, ifn4, ivibefn
aoutleft, aoutright		        pan2	                asignal * iamplitude * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   WaveguideGuitar
                                //////////////////////////////////////////////////////
                                // Original by Jeff Livingston.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////////////
                                ; pset                    0, 0, 11
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) / 16
iHz                             =                       ifrequency
                                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                ; The model takes pluck position, and pickup position (in % of string length), and generates
                                ; a pluck excitation signal, representing the string displacement.  The pluck consists
                                ; of a forward and backward traveling displacement wave, which are recirculated thru two
                                ; separate delay lines, to simulate the one dimensional string waveguide, with
                                ; fixed ends.
                                ;
                                ; Losses due to internal friction of the string, and with air, as well as
                                ; losses due to the mechanical impedance of the string terminations are simulated by
                                ; low pass filtering the signal inside the feedback loops.
                                ; Delay line outputs at the bridge termination are summed and fed into an IIR filter
                                ; modeled to simulate the lowest two vibrational modes (resonances) of the guitar body.
                                ; The theory implies that force due to string displacement, which is equivalent to
                                ; displacement velocity times bridge mechanical impedance, is the input to the guitar
                                ; body resonator model. Here we have modified the transfer fuction representing the bridge
                                ; mech impedance, to become the string displacement to bridge input force transfer function.
                                ; The output of the resulting filter represents the displacement of the guitar's top plate,
                                ; and sound hole, since thier respective displacement with be propotional to input force.
                                ; (based on a simplified model, viewing the top plate as a force driven spring).
                                ;
                                ; The effects of pluck hardness, and contact with frets during pluck release,
                                ; have been modeled by injecting noise into the initial pluck, proportional to initial
                                ; string displacement.
                                ;
                                ; Note on pluck shape: Starting with a triangular displacment, I found a decent sounding
                                ; initial pluck shape after some trial and error.  This pluck shape, which is a linear
                                ; ramp, with steep fall off, doesn't necessarily agree with the pluck string models I've
                                ; studied.  I found that initial pluck shape significantly affects the realism of the
                                ; sound output, but I the treatment of this topic in musical acoustics literature seems
                                ; rather limited as far as I've encountered.
                                ;
                                ; Original pfields
                                ; p1     p2   p3    p4    p5    p6      p7      p8       p9        p10         p11    p12   p13
                                ; in     st   dur   amp   pch   plklen  fbfac	pkupPos	 pluckPos  brightness  vibf   vibd  vibdel
                                ; i01.2	 0.5  0.75  5000  7.11	.85     0.9975	.0	    .25	       1	       0	  0	 0
ip4                             init                    iamplitude
ip6                             init                    0.85
ip7                             init                    0.9975
ip8                             init                    0
ip9                             init                    0.25
ip10                            init                    1.0
ip11                            init                    0.001
ip12                            init                    0.0
ip13                            init                    0.0
afwav                           init                    0
abkwav                          init                    0
abkdout                         init                    0
afwdout                         init                    0
iEstr	                        init                    1.0 / cpspch(6.04)
ifqc                            init                    iHz ; cpspch(p5)
                                ; note:delay time=2x length of string (time to traverse it)
idlt                            init                    1.0 / ifqc
ipluck                          =                       0.5 * idlt * ip6 * ifqc / cpspch(8.02)
ifbfac = ip7  			        ; feedback factor
                                ; (exponentialy scaled) additive noise to add hi freq content
ibrightness                     =                       ip10 * exp(ip6 * log(2)) / 2
ivibRate                        =                       ip11
ivibDepth                       pow                     2, ip12 / 12
                                ; vibrato depth, +,- ivibDepth semitones
ivibDepth                       =                       idlt - 1.0 / (ivibDepth * ifqc)
                                ; vibrato start delay (secs)
ivibStDly                       =                       ip13
                                ; termination impedance model
                                ; cutoff freq of LPF due to mech. impedance at the nut (2kHz-10kHz)
if0                             =                       10000
                                ; damping parameter of nut impedance
iA0                             =                       ip7
ialpha                          =                       cos(2 * 3.14159265 * if0 * 1 / sr)
                                ; FIR LPF model of nut impedance,  H(z)=a0+a1z^-1+a0z^-2
ia0                             =                       0.3 * iA0 / (2 * (1 - ialpha))
ia1                             =                       iA0 - 2 * ia0
                                ; NOTE each filter pass adds a sampling period delay,so subtract 1/sr from tap time to compensate
                                ; determine (in crude fashion) which string is being played
                                ; icurStr = (ifqc > cpspch(6.04) ? 2 : 1)
                                ; icurStr = (ifqc > cpspch(6.09) ? 3 : icurStr)
                                ; icurStr = (ifqc > cpspch(7.02) ? 4 : icurStr)
                                ; icurStr = (ifqc > cpspch(7.07) ? 5 : icurStr)
                                ; icurStr = (ifqc > cpspch(7.11) ? 6 : icurStr)
ipupos                          =                       ip8 * idlt / 2 ; pick up position (in % of low E string length)
ippos                           =                       ip9 * idlt / 2 ; pluck position (in % of low E string length)
isegF                           =                       1 / sr
isegF2                          =                       ipluck
iplkdelF                        =                       (ipluck / 2 > ippos ? 0 : ippos - ipluck / 2)
isegB                           =                       1 / sr
isegB2                          =                       ipluck
iplkdelB                        =                       (ipluck / 2 > idlt / 2 - ippos ? 0 : idlt / 2 - ippos - ipluck / 2)
                                ; EXCITATION SIGNAL GENERATION
                                ; the two excitation signals are fed into the fwd delay represent the 1st and 2nd
                                ; reflections off of the left boundary, and two accelerations fed into the bkwd delay
                                ; represent the the 1st and 2nd reflections off of the right boundary.
                                ; Likewise for the backward traveling acceleration waves, only they encouter the
                                ; terminations in the opposite order.
ipw                             =                       1
ipamp                           =                       ip4 * ipluck ; 4 / ipluck
aenvstrf                        linseg                  0, isegF, -ipamp / 2, isegF2, 0
adel1	                        delayr                  (idlt > 0) ? idlt : 0.01
                                ; initial forward traveling wave (pluck to bridge)
aenvstrf1                       deltapi                 iplkdelF
                                ; first forward traveling reflection (nut to bridge)
aenvstrf2                       deltapi                 iplkdelB + idlt / 2
                                delayw                  aenvstrf
                                ; inject noise for attack time string fret contact, and pre pluck vibrations against pick
anoiz                           rand	                ibrightness
aenvstrf1                       =                       aenvstrf1 + anoiz*aenvstrf1
aenvstrf2                       =                       aenvstrf2 + anoiz*aenvstrf2
                                ; filter to account for losses along loop path
aenvstrf2	                    filter2                 aenvstrf2, 3, 0, ia0, ia1, ia0
                                ; combine into one signal (flip refl wave's phase)
aenvstrf                        =                       aenvstrf1 - aenvstrf2
                                ; initial backward excitation wave
aenvstrb                        linseg                  0, isegB, - ipamp / 2, isegB2, 0
adel2	                        delayr                  (idlt > 0) ? idlt : 0.01
                                ; initial bdwd traveling wave (pluck to nut)
aenvstrb1                       deltapi                 iplkdelB
                                ; first forward traveling reflection (nut to bridge)
aenvstrb2                       deltapi                 idlt / 2 + iplkdelF
                                delayw                  aenvstrb
                                ; initial bdwd traveling wave (pluck to nut)
;  aenvstrb1	delay	aenvstrb,  iplkdelB
                                ; first bkwd traveling reflection (bridge to nut)
;  aenvstrb2	delay	aenvstrb, idlt/2+iplkdelF
                                ; inject noise
aenvstrb1                       =                       aenvstrb1 + anoiz*aenvstrb1
aenvstrb2                       =                       aenvstrb2 + anoiz*aenvstrb2
                                ; filter to account for losses along loop path
aenvstrb2	                    filter2                 aenvstrb2, 3, 0, ia0, ia1, ia0
                                ; combine into one signal (flip refl wave's phase)
aenvstrb	                    =	                    aenvstrb1 - aenvstrb2
                                ; low pass to band limit initial accel signals to be < 1/2 the sampling freq
ainputf                         tone                    aenvstrf, sr * 0.9 / 2
ainputb                         tone                    aenvstrb, sr * 0.9 / 2
                                ; additional lowpass filtering for pluck shaping\
                                ; Note, it would be more efficient to combine stages into a single filter
ainputf                         tone                    ainputf, sr * 0.9 / 2
ainputb                         tone                    ainputb, sr * 0.9 / 2
                                ; Vibrato generator
icosine                         ftgenonce               0, 0, 65536,    11,     1.0
avib                            poscil                  ivibDepth, ivibRate, icosine
avibdl		                    delayr		            (((ivibStDly * 1.1)) > 0.0) ? (ivibStDly * 1.1) : 0.01
avibrato	                    deltapi	                ivibStDly
                                delayw		            avib
                                ; Dual Delay line,
                                ; NOTE: delay length longer than needed by a bit so that the output at t=idlt will be interpolated properly
                                ;forward traveling wave delay line
afd  		                    delayr                  (((idlt + ivibDepth) * 1.1) > 0.0) ? ((idlt + ivibDepth) * 1.1) : 0.01
                                ; output tap point for fwd traveling wave
afwav  	                        deltapi                 ipupos
                                ; output at end of fwd delay (left string boundary)
afwdout	                        deltapi                 idlt - 1 / sr + avibrato
                                ; lpf/attn due to reflection impedance
afwdout	                        filter2                 afwdout, 3, 0, ia0, ia1, ia0
                                delayw                  ainputf + afwdout * ifbfac * ifbfac
                                ; backward trav wave delay line
abkwd  	                        delayr                  (((idlt + ivibDepth) * 1.1) > 0) ? ((idlt + ivibDepth) * 1.1) : 0.01
                                ; output tap point for bkwd traveling wave
abkwav  	                    deltapi                 idlt / 2 - ipupos
                                ; output at the left boundary
; abkterm	deltapi	idlt/2
                                ; output at end of bkwd delay (right string boundary)
abkdout	                        deltapi                 idlt - 1 / sr + avibrato
abkdout	                        filter2                 abkdout, 3, 0, ia0, ia1, ia0
                                delayw                  ainputb + abkdout * ifbfac * ifbfac
                                ; resonant body filter model, from Cuzzucoli and Lombardo
                                ; IIR filter derived via bilinear transform method
                                ; the theoretical resonances resulting from circuit model should be:
                                ; resonance due to the air volume + soundhole = 110Hz (strongest)
                                ; resonance due to the top plate = 220Hz
                                ; resonance due to the inclusion of the back plate = 400Hz (weakest)
aresbod                         filter2                 (afwdout + abkdout), 5, 4, 0.000000000005398681501844749, .00000000000001421085471520200, -.00000000001076383426834582, -00000000000001110223024625157, .000000000005392353230604385, -3.990098622573566, 5.974971737738533, -3.979630684599723, .9947612723736902
asignal                         =                       (1500 * (afwav + abkwav + aresbod * .000000000000000000003)) ; * adeclick
aoutleft, aoutright             pan2                    asignal * iamplitude, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr                   Xing
                                //////////////////////////////////////////////
                                // Original by Andrew Horner.
                                // Adapted by Michael Gogins.
                                // p4 pitch in octave.pch
                                // original pitch        = A6
                                // range                 = C6 - C7
                                // extended range        = F4 - C7
                                //////////////////////////////////////////////
insno           		        =                       p1
itime           		        =                       p2
iduration       		        =                       p3
ikey            		        =                       p4
ivelocity                       =                       p5
iphase                          =                       p6
ipan                            =                       p7
idepth                          =                       p8
iheight                         =                       p9
ipcs                            =                       p10
ihomogeneity                    =                       p11
kgain			    	=                       1.25
iHz                             =                       cpsmidinn(ikey)
kHz                             =                       k(iHz)
iattack                         =                       (440.0 / iHz) * 0.01
                                print                   iHz, iattack
isustain                        =                       p3
irelease                        =                       .3
p3                              =                       iattack + isustain + irelease
iduration                       =                       p3
iamplitude                      =                       ampdb(ivelocity) * 8.
isine                           ftgenonce               0, 0, 65536,    10,     1
kfreq                           =                       cpsmidinn(ikey)
iamp                            =                       1
inorm                           =                       32310
aamp1                           linseg                  0,.001,5200,.001,800,.001,3000,.0025,1100,.002,2800,.0015,1500,.001,2100,.011,1600,.03,1400,.95,700,1,320,1,180,1,90,1,40,1,20,1,12,1,6,1,3,1,0,1,0
adevamp1                        linseg                  0, .05, .3, iduration - .05, 0
adev1                           poscil                  adevamp1, 6.7, isine, .8
amp1                            =                       aamp1 * (1 + adev1)
aamp2                           linseg                  0,.0009,22000,.0005,7300,.0009,11000,.0004,5500,.0006,15000,.0004,5500,.0008,2200,.055,7300,.02,8500,.38,5000,.5,300,.5,73,.5,5.,5,0,1,1
adevamp2                        linseg                  0,.12,.5,iduration-.12,0
adev2                           poscil                  adevamp2, 10.5, isine, 0
amp2                            =                       aamp2 * (1 + adev2)
aamp3                           linseg                  0,.001,3000,.001,1000,.0017,12000,.0013,3700,.001,12500,.0018,3000,.0012,1200,.001,1400,.0017,6000,.0023,200,.001,3000,.001,1200,.0015,8000,.001,1800,.0015,6000,.08,1200,.2,200,.2,40,.2,10,.4,0,1,0
adevamp3                        linseg                  0, .02, .8, iduration - .02, 0
adev3                           poscil                  adevamp3, 70, isine ,0
amp3                            =                       aamp3 * (1 + adev3)
awt1                            poscil                  amp1, kfreq, isine
awt2                            poscil                  amp2, 2.7 * kfreq, isine
awt3                            poscil                  amp3, 4.95 * kfreq, isine
asig                            =                       awt1 + awt2 + awt3
arel                            linenr                  1,0, iduration, .06
; asignal                         =                       asig * arel * (iamp / inorm) * iamplitude * kgain
asignal                         =                       asig * (iamp / inorm) * iamplitude * kgain
adeclick                        linsegr                 0, iattack, 1, isustain, 1, irelease, 0
asignal                         =                       asignal
aoutleft, aoutright		        pan2			        asignal * adeclick, .875;ipan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
                                endin

                                instr			        ZakianFlute
                                //////////////////////////////////////////////
                                // Original by Lee Zakian.
                                // Adapted by Michael Gogins.
                                //////////////////////////////////////////////
                                prints                  "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
if1                    		    ftgenonce               0, 0, 65536,    10,     1
if2                    		    ftgenonce               0, 0, 16,       -2,     40, 40, 80, 160, 320, 640, 1280, 2560, 5120, 10240, 10240
if26                   		    ftgenonce               0, 0, 65536,    -10,    2000, 489, 74, 219, 125, 9, 33, 5, 5
if27                   		    ftgenonce               0, 0, 65536,    -10,    2729, 1926, 346, 662, 537, 110, 61, 29, 7
if28                   		    ftgenonce               0, 0, 65536,    -10,    2558, 2012, 390, 361, 534, 139, 53, 22, 10, 13, 10
if29                   		    ftgenonce               0, 0, 65536,    -10,    12318, 8844, 1841, 1636, 256, 150, 60, 46, 11
if30                   		    ftgenonce               0, 0, 65536,    -10,    1229, 16, 34, 57, 32
if31                   		    ftgenonce               0, 0, 65536,    -10,    163, 31, 1, 50, 31
if32                   		    ftgenonce               0, 0, 65536,    -10,    4128, 883, 354, 79, 59, 23
if33                   		    ftgenonce               0, 0, 65536,    -10,    1924, 930, 251, 50, 25, 14
if34                   		    ftgenonce               0, 0, 65536,    -10,    94, 6, 22, 8
if35                   		    ftgenonce               0, 0, 65536,    -10,    2661, 87, 33, 18
if36                   		    ftgenonce               0, 0, 65536,    -10,    174, 12
if37                   		    ftgenonce               0, 0, 65536,    -10,    314, 13
iwtsin				            init			        if1
i_instrument                    =                       p1
i_time                          =                       p2
i_duration                      =                       p3
i_midikey                       =                       p4
i_midivelocity                  =                       p5
i_phase                         =                       p6
i_pan                           =                       p7
i_depth                         =                       p8
i_height                        =                       p9
i_pitchclassset                 =                       p10
i_homogeneity                   =                       p11
ifrequency                      =                       cpsmidinn(i_midikey)
iamplitude                      =                       ampdb(i_midivelocity) * 4
iattack                         =                       .25
isustain                        =                       p3
irelease                        =                       .33333333
p3                              =                       iattack + isustain + irelease
iHz                             =                       ifrequency
kHz                             =                       k(iHz)
idB                             =                       i_midivelocity
adeclick                        linsegr                 0, iattack, 1, isustain, 1, irelease, 0
ip3                     	    =                       (p3 < 3.0 ? p3 : 3.0)
; parameters
; p4    overall amplitude scaling factor
ip4                     	    init                    iamplitude
; p5    pitch in Hertz (normal pitch range: C4-C7)
ip5                     	    init                    iHz
; p6    percent vibrato depth, recommended values in range [-1., +1.]
ip6                     	    init                    1
;        0.0    -> no vibrato
;       +1.     -> 1% vibrato depth, where vibrato rate increases slightly
;       -1.     -> 1% vibrato depth, where vibrato rate decreases slightly
; p7    attack time in seconds
;       recommended value:  .12 for slurred notes, .06 for tongued notes
;                            (.03 for short notes)
ip7                     	    init                    .08
; p8    decay time in seconds
;       recommended value:  .1 (.05 for short notes)
ip8                     	    init                    .08
; p9    overall brightness / filter cutoff factor
;       1 -> least bright / minimum filter cutoff frequency (40 Hz)
;       9 -> brightest / maximum filter cutoff frequency (10,240Hz)
ip9                     	    init                    4
; initial variables
iampscale               	    =                       ip4                              ; overall amplitude scaling factor
ifreq                   	    =                       ip5                              ; pitch in Hertz
ivibdepth               	    =                       abs(ip6*ifreq/100.0)             ; vibrato depth relative to fundamental frequency
iattack                 	    =                       ip7 * (1.1 - .2*giseed)          ; attack time with up to +-10% random deviation
giseed                  	    =                       frac(giseed*105.947)             ; reset giseed
idecay                  	    =                       ip8 * (1.1 - .2*giseed)          ; decay time with up to +-10% random deviation
giseed                  	    =                       frac(giseed*105.947)
ifiltcut                	    tablei                  ip9, if2                          ; lowpass filter cutoff frequency
iattack                 	    =                       (iattack < 6/kr ? 6/kr : iattack)               ; minimal attack length
idecay                  	    =                       (idecay < 6/kr ? 6/kr : idecay)                 ; minimal decay length
isustain                	    =                       p3 - iattack - idecay
p3                      	    =                       (isustain < 5/kr ? iattack+idecay+5/kr : p3)    ; minimal sustain length
isustain                	    =                       (isustain < 5/kr ? 5/kr : isustain)
iatt                    	    =                       iattack/6
isus                    	    =                       isustain/4
idec                    	    =                       idecay/6
iphase                  	    =                       giseed                          ; use same phase for all wavetables
giseed                  	    =                       frac(giseed*105.947)
; vibrato block
; kvibdepth               	    linseg                  .1, .8*p3, 1, .2*p3, .7
kvibdepth               	    linseg                  .1, .8*ip3, 1, isustain, 1, .2*ip3, .7
kvibdepth               	    =                       kvibdepth* ivibdepth            ; vibrato depth
kvibdepthr              	    randi                   .07*kvibdepth, 5, giseed         ; up to 10% vibrato depth variation
giseed                  	    =                       frac(giseed*105.947)
kvibdepth               	    =                       kvibdepth + kvibdepthr
ivibr1                  	    =                       giseed                          ; vibrato rate
giseed                  	    =                       frac(giseed*105.947)
ivibr2                  	    =                       giseed
giseed                  	    =                       frac(giseed*105.947)

                                if                      ip6 < 0 goto            vibrato1
kvibrate                	    linseg                  2.5+ivibr1, p3, 4.5+ivibr2      ; if p6 positive vibrato gets faster
                                goto                    vibrato2
vibrato1:
ivibr3                  	    =                       giseed
giseed                  	    =                       frac(giseed*105.947)
kvibrate                	    linseg                  3.5+ivibr1, .1, 4.5+ivibr2, p3-.1, 2.5+ivibr3   ; if p6 negative vibrato gets slower
vibrato2:
kvibrater               	    randi                   .1*kvibrate, 5, giseed          ; up to 10% vibrato rate variation
giseed                  	    =                       frac(giseed*105.947)
kvibrate                	    =                       kvibrate + kvibrater
kvib                    	    oscili                  kvibdepth, kvibrate, iwtsin
ifdev1                  	    =                       -.03 * giseed                           ; frequency deviation
giseed                  	    =                       frac(giseed*105.947)
ifdev2                  	    =                       .003 * giseed
giseed                  	    =                       frac(giseed*105.947)
ifdev3                  	    =                       -.0015 * giseed
giseed                  	    =                       frac(giseed*105.947)
ifdev4                  	    =                       .012 * giseed
giseed                  	    =                       frac(giseed*105.947)
kfreqr                  	    linseg                  ifdev1, iattack, ifdev2, isustain, ifdev3, idecay, ifdev4
kfreq                   	    =                       kHz * (1 + kfreqr) + kvib
                                if                      ifreq <  427.28 goto    range1                          ; (cpspch(8.08) + cpspch(8.09))/2
                                if                      ifreq <  608.22 goto    range2                          ; (cpspch(9.02) + cpspch(9.03))/2
                                if                      ifreq <  1013.7 goto    range3                          ; (cpspch(9.11) + cpspch(10.00))/2
                                goto                    range4
; wavetable amplitude envelopes
range1:                 	    ; for low range tones
kamp1                   	    linseg                  0, iatt, 0.002, iatt, 0.045, iatt, 0.146, iatt,  \
                                                        0.272, iatt, 0.072, iatt, 0.043, isus, 0.230, isus, 0.000, isus, \
                                                        0.118, isus, 0.923, idec, 1.191, idec, 0.794, idec, 0.418, idec, \
                                                        0.172, idec, 0.053, idec, 0
kamp2                   	    linseg                  0, iatt, 0.009, iatt, 0.022, iatt, -0.049, iatt,  \
                                                        -0.120, iatt, 0.297, iatt, 1.890, isus, 1.543, isus, 0.000, isus, \
                                                        0.546, isus, 0.690, idec, -0.318, idec, -0.326, idec, -0.116, idec, \
                                                        -0.035, idec, -0.020, idec, 0
kamp3                   	    linseg                  0, iatt, 0.005, iatt, -0.026, iatt, 0.023, iatt,    \
                                                        0.133, iatt, 0.060, iatt, -1.245, isus, -0.760, isus, 1.000, isus,  \
                                                        0.360, isus, -0.526, idec, 0.165, idec, 0.184, idec, 0.060, idec,   \
                                                        0.010, idec, 0.013, idec, 0
iwt1                    	    =                       if26                                      ; wavetable numbers
iwt2                    	    =                       if27
iwt3                    	    =                       if28
inorm                   	    =                       3949
                                goto                    end
range2:                 	    ; for low mid-range tones
kamp1                   	    linseg                  0, iatt, 0.000, iatt, -0.005, iatt, 0.000, iatt, \
                                                        0.030, iatt, 0.198, iatt, 0.664, isus, 1.451, isus, 1.782, isus, \
                                                        1.316, isus, 0.817, idec, 0.284, idec, 0.171, idec, 0.082, idec, \
                                                        0.037, idec, 0.012, idec, 0
kamp2                   	    linseg                  0, iatt, 0.000, iatt, 0.320, iatt, 0.882, iatt,      \
                                                        1.863, iatt, 4.175, iatt, 4.355, isus, -5.329, isus, -8.303, isus,   \
                                                        -1.480, isus, -0.472, idec, 1.819, idec, -0.135, idec, -0.082, idec, \
                                                        -0.170, idec, -0.065, idec, 0
kamp3                   	    linseg                  0, iatt, 1.000, iatt, 0.520, iatt, -0.303, iatt,     \
                                                        0.059, iatt, -4.103, iatt, -6.784, isus, 7.006, isus, 11, isus,      \
                                                        12.495, isus, -0.562, idec, -4.946, idec, -0.587, idec, 0.440, idec, \
                                                        0.174, idec, -0.027, idec, 0
iwt1                    	    =                       if29
iwt2                    	    =                       if30
iwt3                    	    =                       if31
inorm                   	    =                       27668.2
                                goto                    end
range3:                 	    ; for high mid-range tones
kamp1                   	    linseg                  0, iatt, 0.005, iatt, 0.000, iatt, -0.082, iatt,      \
                                                        0.36, iatt, 0.581, iatt, 0.416, isus, 1.073, isus, 0.000, isus,       \
                                                        0.356, isus, .86, idec, 0.532, idec, 0.162, idec, 0.076, idec, 0.064, \
                                                        idec, 0.031, idec, 0
kamp2                   	    linseg                  0, iatt, -0.005, iatt, 0.000, iatt, 0.205, iatt,      \
                                                        -0.284, iatt, -0.208, iatt, 0.326, isus, -0.401, isus, 1.540, isus,   \
                                                        0.589, isus, -0.486, idec, -0.016, idec, 0.141, idec, 0.105, idec,    \
                                                        -0.003, idec, -0.023, idec, 0
kamp3                   	    linseg                  0, iatt, 0.722, iatt, 1.500, iatt, 3.697, iatt,       \
                                                        0.080, iatt, -2.327, iatt, -0.684, isus, -2.638, isus, 0.000, isus,   \
                                                        1.347, isus, 0.485, idec, -0.419, idec, -.700, idec, -0.278, idec,    \
                                                        0.167, idec, -0.059, idec, 0
iwt1                    	    =                       if32
iwt2                    	    =                       if33
iwt3                    	    =                       if34
inorm                   	    =                       3775
                                goto                    end
range4:                                                 ; for high range tones
kamp1                   	    linseg                  0, iatt, 0.000, iatt, 0.000, iatt, 0.211, iatt,         \
                                                        0.526, iatt, 0.989, iatt, 1.216, isus, 1.727, isus, 1.881, isus,        \
                                                        1.462, isus, 1.28, idec, 0.75, idec, 0.34, idec, 0.154, idec, 0.122,    \
                                                        idec, 0.028, idec, 0
kamp2                   	    linseg                  0, iatt, 0.500, iatt, 0.000, iatt, 0.181, iatt,         \
                                                        0.859, iatt, -0.205, iatt, -0.430, isus, -0.725, isus, -0.544, isus,    \
                                                        -0.436, isus, -0.109, idec, -0.03, idec, -0.022, idec, -0.046, idec,    \
                                                        -0.071, idec, -0.019, idec, 0
kamp3                   	    linseg                  0, iatt, 0.000, iatt, 1.000, iatt, 0.426, iatt,         \
                                                        0.222, iatt, 0.175, iatt, -0.153, isus, 0.355, isus, 0.175, isus,       \
                                                        0.16, isus, -0.246, idec, -0.045, idec, -0.072, idec, 0.057, idec,      \
                                                        -0.024, idec, 0.002, idec, 0
iwt1                    	    =                       if35
iwt2                    	    =                       if36
iwt3                    	    =                       if37
inorm                   	    =                       4909.05
                                goto                    end
end:
kampr1                  	    randi                   .02*kamp1, 10, giseed                   ; up to 2% wavetable amplitude variation
giseed                  	    =                       frac(giseed*105.947)
kamp1                   	    =                       kamp1 + kampr1
kampr2                  	    randi                   .02*kamp2, 10, giseed                   ; up to 2% wavetable amplitude variation
giseed                  	    =                       frac(giseed*105.947)
kamp2                   	    =                       kamp2 + kampr2
kampr3                  	    randi                   .02*kamp3, 10, giseed                   ; up to 2% wavetable amplitude variation
giseed                  	    =                       frac(giseed*105.947)
kamp3                   	    =                       kamp3 + kampr3
awt1                    	    poscil                  kamp1, kfreq, iwt1, iphase              ; wavetable lookup
awt2                    	    poscil                  kamp2, kfreq, iwt2, iphase
awt3                    	    poscil                  kamp3, kfreq, iwt3, iphase
asig                    	    =                       awt1 + awt2 + awt3
asig                    	    =                       asig*(iampscale/inorm)
kcut                    	    linseg                  0, iattack, ifiltcut, isustain, ifiltcut, idecay, 0     ; lowpass filter for brightness control
afilt                   	    tone                    asig, kcut
asignal                    	    balance                 afilt, asig
iattack                         =                       0.005
isustain                        =                       p3
irelease                        =                       0.06
p3                              =                       isustain + iattack + irelease
adeclick                        linsegr                 0.0, iattack, 1.0, isustain, 1.0, irelease, 0.0
aoutleft, aoutright             pan2                    asignal * adeclick, i_pan
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                endin

                                //////////////////////////////////////////////
                                // OUTPUT INSTRUMENTS MUST GO BELOW HERE
                                //////////////////////////////////////////////

                                instr                   Reverberation
                                //////////////////////////////////////////////
                                // By Michael Gogins.
                                //////////////////////////////////////////////
ainleft                         inleta                  "inleft"
ainright                        inleta                  "inright"
if (gkReverberationEnabled == 0) then
aoutleft                        =                       ainleft
aoutright                       =                       ainright
kdry				            =			            1.0 - gkReverberationWet
else
awetleft, awetright             reverbsc                ainleft, ainright, gkReverberationDelay, 18000.0
aoutleft			            =			            ainleft *  kdry + awetleft  * gkReverberationWet
aoutright			            =			            ainright * kdry + awetright * gkReverberationWet
endif
                                outleta                 "outleft", aoutleft
                                outleta                 "outright", aoutright
                                endin

                                instr                   Compressor
                                //////////////////////////////////////////////
                                // By Michael Gogins.
                                //////////////////////////////////////////////
ainleft                         inleta                  "inleft"
ainright                        inleta                  "inright"
                                if (gkCompressorEnabled == 0) then
aoutleft                        =                       ainleft
aoutright                       =                       ainright
                                else
aoutleft                        compress                ainleft,        ainleft,  gkCompressorThreshold, 100 * gkCompressorLowKnee, 100 * gkCompressorHighKnee, 100 * gkCompressorRatio, gkCompressorAttack, gkCompressorRelease, .05
aoutright                       compress                ainright,       ainright, gkCompressorThreshold, 100 * gkCompressorLowKnee, 100 * gkCompressorHighKnee, 100 * gkCompressorRatio, gkCompressorAttack, gkCompressorRelease, .05
                                endif
                                outleta                 "outleft",      aoutleft
                                outleta                 "outright",     aoutright
                                endin

                                instr                   ParametricEq1
                                //////////////////////////////////////////////
                                // By Michael Gogins.
                                //////////////////////////////////////////////
ainleft                         inleta                  "inleft"
ainright                        inleta                  "inright"
                                if (gkParametricEq1Enabled == 0) then
aoutleft                        =                       ainleft
aoutright                       =                       ainright
                                else
                                if (gkParametricEq1Mode == 0) then
aoutleft                        pareq                   ainleft,        15000 * gkParametricEq1Frequency, 10 * gkParametricEq1Gain, giFlatQ + 10 * gkParametricEq1Q, 0
aoutright                       pareq                   ainright,       15000 * gkParametricEq1Frequency, 10 * gkParametricEq1Gain, giFlatQ + 10 * gkParametricEq1Q, 0
                                elseif  (gkParametricEq1Mode == 0.001) then
aoutleft                        pareq                   ainleft,        15000 * gkParametricEq1Frequency, 10 * gkParametricEq1Gain, giFlatQ + 10 * gkParametricEq1Q, 1
aoutright                       pareq                   ainright,       15000 * gkParametricEq1Frequency, 10 * gkParametricEq1Gain, giFlatQ + 10 * gkParametricEq1Q, 1
                                elseif  (gkParametricEq1Mode == 0.002) then
aoutleft                        pareq                   ainleft,        15000 * gkParametricEq1Frequency, 10 * gkParametricEq1Gain, giFlatQ + 10 * gkParametricEq1Q, 2
aoutright                       pareq                   ainright,       15000 * gkParametricEq1Frequency, 10 * gkParametricEq1Gain, giFlatQ + 10 * gkParametricEq1Q, 2
                                endif
                                endif
                                outleta                 "outleft",  aoutleft
                                outleta                 "outright", aoutright
                                endin

                                instr                   ParametricEq2
                                //////////////////////////////////////////////
                                // By Michael Gogins.
                                //////////////////////////////////////////////
ainleft                         inleta                  "inleft"
ainright                        inleta                  "inright"
                                if                      (gkParametricEq2Enabled == 0) then
aoutleft                        =                       ainleft
aoutright                       =                       ainright
                                else
                                if                      (gkParametricEq2Mode == 0) then
aoutleft                        pareq                   ainleft, 	15000 * gkParametricEq2Frequency, 10 * gkParametricEq2Gain, giFlatQ + 10 * gkParametricEq2Q, 0
aoutright                       pareq                   ainright,	15000 * gkParametricEq2Frequency, 10 * gkParametricEq2Gain, giFlatQ + 10 * gkParametricEq2Q, 0
                                elseif                  (gkParametricEq2Mode == 0.001) then
aoutleft                        pareq                   ainleft, 	15000 * gkParametricEq2Frequency, 10 * gkParametricEq2Gain, giFlatQ + 10 * gkParametricEq2Q, 1
aoutright                       pareq                   ainright,	15000 * gkParametricEq2Frequency, 10 * gkParametricEq2Gain, giFlatQ + 10 * gkParametricEq2Q, 1
                                elseif                  (gkParametricEq2Mode == 0.002) then
aoutleft                        pareq                   ainleft, 	15000 * gkParametricEq2Frequency, 10 * gkParametricEq2Gain, giFlatQ + 10 * gkParametricEq2Q, 2
aoutright                       pareq                   ainright,	15000 * gkParametricEq2Frequency, 10 * gkParametricEq2Gain, giFlatQ + 10 * gkParametricEq2Q, 2
                                endif
                                endif
                                outleta                 "outleft", 	aoutleft
                                outleta                 "outright", aoutright
                                endin

                                instr                   MasterOutput
                                //////////////////////////////////////////////
                                // By Michael Gogins.
                                //////////////////////////////////////////////
ainleft                         inleta                  "inleft"
ainright                        inleta                  "inright"
aoutleft                        =                       gkMasterLevel * ainleft
aoutright                       =                       gkMasterLevel * ainright
                                outs                    aoutleft, aoutright
                                endin

            </CsInstruments>
<CsScore>
i 14 0 8.177163 33 45 0 -0.6561414 0 0 1 1
i 14 5.929556 8.177163 41 45 0 0.6030155 0 0 1 1
i 16 10.00781 8.177163 90 47 0 0.843962 0 0 1 1
i 14 13.57773 8.177163 50 45 0 -0.5021387 0 0 1 1
i 16 13.85524 8.177163 46 47 0 -0.3452993 0 0 1 1
i 14 14.78251 8.177163 41 45 0 0.08499707 0 0 1 1
i 16 15.05422 8.177163 55 47 0 -0.5609124 0 0 1 1
i 14 15.65239 20.89508 31 45 0 0.8871863 0 0 1 1
i 4 15.86986 8.177163 37 45 0 0.8936304 0 0 1 1
i 16 19.05264 16.20762 63 47 0 0.8418509 0 0 1 1
i 14 23.65128 8.177163 58 45 0 0.4065101 0 0 1 1
i 14 23.74364 12.34076 50 45 0 0.8659974 0 0 1 1
i 16 24.33056 8.177163 54 47 0 -0.7022488 0 0 1 1
i 4 24.99226 8.177163 46 45 0 0.5365905 0 0 1 1
i 16 25.60085 15.44181 72 47 0 -0.365347 0 0 1 1
i 14 27.86917 15.88499 39 45 0 -0.8913897 0 0 1 1
i 14 31.78676 35.68714 40 45 0 -0.6975639 0 0 1 1
i 16 32.38226 8.177163 90 47 0 0.251574 0 0 1 1
i 57 35.13038 8.177163 86 40.75 0 0.6811752 0 0 1 1
i 14 35.32488 8.177163 58 45 0 0.00659282 0 0 1 1
i 4 36.75867 8.177163 54 45 0 0.5362715 0 0 1 1
i 14 37.20512 8.177163 67 45 0 -0.2496708 0 0 1 1
i 14 37.44119 8.177163 50 45 0 -0.5185362 0 0 1 1
i 16 37.84202 15.37762 63 47 0 0.3264472 0 0 1 1
i 4 38.61219 8.177163 46 45 0 -0.1822707 0 0 1 1
i 14 39.68616 33.83237 29 45 0 0.433165 0 0 1 1
i 4 39.95047 15.88499 35 45 0 -0.04543437 0 0 1 1
i 16 39.97043 8.177163 54 47 0 -0.1402422 0 0 1 1
i 14 40.10597 30.01449 59 45 0 -0.5870427 0 0 1 1
i 14 40.2285 8.177163 31 45 0 -0.3565564 0 0 1 1
i 13 40.28504 8.177163 41 33 0 0.5351038 0 0 1 1
i 16 41.50719 8.177163 44 47 0 -0.3302092 0 0 1 1
i 16 41.54605 8.177163 90 47 0 0.6703719 0 0 1 1
i 57 41.89138 8.177163 50 40.75 0 -0.6315948 0 0 1 1
i 16 42.59842 8.177163 71 47 0 0.8893233 0 0 1 1
i 14 44.87247 69.45042 48 45 0 0.5794259 0 0 1 1
i 16 46.80716 8.177163 53 47 0 -0.674671 0 0 1 1
i 57 47.24832 8.177163 59 40.75 0 0.47475 0 0 1 1
i 14 47.43519 8.177163 39 45 0 -0.01693973 0 0 1 1
i 16 50.35815 8.177163 44 47 0 0.2944899 0 0 1 1
i 14 50.61001 55.49624 67 45 0 -0.6733861 0 0 1 1
i 4 52.25645 8.177163 63 45 0 -0.5216237 0 0 1 1
i 4 52.39854 14.5827 54 45 0 -0.8078104 0 0 1 1
i 16 53.4884 8.177163 72 47 0 -0.8344057 0 0 1 1
i 16 53.82336 8.177163 90 47 0 -0.1642839 0 0 1 1
i 13 54.31949 8.177163 50 33 0 -0.07561952 0 0 1 1
i 16 55.51479 8.177163 62 47 0 -0.02237593 0 0 1 1
i 16 55.63823 8.177163 71 47 0 0.5291549 0 0 1 1
i 16 55.80141 15.53269 63 47 0 0.7575746 0 0 1 1
i 14 55.82496 8.177163 75 45 0 0.5535558 0 0 1 1
i 57 56.02138 8.177163 67 40.75 0 0.3703937 0 0 1 1
i 16 56.97078 33.30453 53 47 0 -0.8949268 0 0 1 1
i 57 58.00725 8.177163 59 40.75 0 0.379267 0 0 1 1
i 14 58.47188 8.177163 31 45 0 0.2591297 0 0 1 1
i 4 58.74551 38.27874 44 45 0 -0.07914092 0 0 1 1
i 4 59.51649 8.177163 35 45 0 0.4930508 0 0 1 1
i 14 62.24207 10.40415 38 45 0 0.1327584 0 0 1 1
i 16 62.5296 20.45448 90 47 0 0.6781633 0 0 1 1
i 16 63.22132 21.21698 72 47 0 0.5547159 0 0 1 1
i 16 63.975 8.177163 44 47 0 -0.868007 0 0 1 1
i 16 64.59142 8.177163 80 47 0 0.5782428 0 0 1 1
i 57 65.10688 8.177163 86 40.75 0 0.5775134 0 0 1 1
i 14 65.67857 8.177163 39 45 0 0.7921333 0 0 1 1
i 57 65.7543 8.177163 67 40.75 0 -0.1572003 0 0 1 1
i 14 66.55249 93.85367 57 45 0 -0.1383028 0 0 1 1
i 15 68.78871 8.177163 82 45 0 0.145722 0 0 1 1
i 4 70.21583 15.53269 63 45 0 -0.6154963 0 0 1 1
i 14 71.21789 14.48156 76 45 0 0.4711162 0 0 1 1
i 16 71.5905 8.177163 61 47 0 -0.4857191 0 0 1 1
i 14 72.21063 8.177163 59 45 0 0.5575222 0 0 1 1
i 13 72.42167 8.177163 59 33 0 0.8793389 0 0 1 1
i 4 73.10851 8.177163 71 45 0 -0.3015931 0 0 1 1
i 4 73.4717 8.177163 55 45 0 -0.3603029 0 0 1 1
i 14 74.10027 76.8234 38 45 0 -0.8756296 0 0 1 1
i 14 74.70934 17.05257 29 45 0 -0.5089719 0 0 1 1
i 13 75.27323 8.177163 50 33 0 0.7332565 0 0 1 1
i 14 76.43832 10.13125 40 45 0 0.627242 0 0 1 1
i 14 76.60426 8.177163 27 45 0 0.8190316 0 0 1 1
i 4 76.9255 8.177163 33 45 0 0.5020159 0 0 1 1
i 13 77.33213 8.177163 40 33 0 0.8774273 0 0 1 1
i 57 77.38419 8.177163 86 40.75 0 -0.7783283 0 0 1 1
i 4 77.75987 8.177163 35 45 0 0.5284756 0 0 1 1
i 11 77.84685 8.177163 46 43 0 0.1701064 0 0 1 1
i 16 78.75463 9.422409 63 47 0 0.4190377 0 0 1 1
i 57 78.79412 8.177163 67 40.75 0 0.3514192 0 0 1 1
i 57 82.06859 8.177163 59 40.75 0 0.3236756 0 0 1 1
i 16 83.32245 8.177163 80 47 0 -0.1938232 0 0 1 1
i 16 83.40103 24.62587 90 47 0 0.1108034 0 0 1 1
i 16 83.9659 8.177163 42 47 0 -0.5254775 0 0 1 1
i 57 84.43283 8.177163 48 40.75 0 0.04926863 0 0 1 1
i 16 84.70754 56.55669 72 47 0 -0.1724247 0 0 1 1
i 4 84.90443 8.177163 52 45 0 -0.2650277 0 0 1 1
i 15 85.0239 8.177163 54 45 0 0.167083 0 0 1 1
i 14 85.9687 117.9466 76 45 0 -0.2585787 0 0 1 1
i 14 87.13235 8.177163 47 45 0 0.8369395 0 0 1 1
i 4 87.42603 15.16543 63 45 0 -0.6220108 0 0 1 1
i 4 87.48087 33.88157 53 45 0 -0.1891652 0 0 1 1
i 14 88.46245 8.177163 27 45 0 -0.2028674 0 0 1 1
i 4 88.78369 8.177163 33 45 0 0.4085185 0 0 1 1
i 13 89.19032 8.177163 40 33 0 -0.2005743 0 0 1 1
i 16 92.90902 8.686203 62 47 0 0.7694871 0 0 1 1
i 14 92.95272 8.177163 29 45 0 -0.1149884 0 0 1 1
i 16 93.12817 8.177163 61 47 0 0.6528207 0 0 1 1
i 14 93.37679 8.177163 66 45 0 0.216648 0 0 1 1
i 57 93.38412 8.177163 67 40.75 0 -0.6848151 0 0 1 1
i 4 93.73141 8.177163 71 45 0 -0.05047776 0 0 1 1
i 57 93.8329 8.177163 86 40.75 0 -0.2876045 0 0 1 1
i 14 94.76581 8.177163 46 45 0 0.05371558 0 0 1 1
i 16 95.56281 8.177163 51 47 0 0.3889813 0 0 1 1
i 16 95.715 8.177163 53 47 0 0.8790829 0 0 1 1
i 14 95.74241 78.69709 47 45 0 0.3968882 0 0 1 1
i 57 96.099 8.177163 57 40.75 0 0.7426395 0 0 1 1
i 13 96.2644 8.177163 67 33 0 0.009897309 0 0 1 1
i 13 96.48301 8.177163 59 33 0 0.1048838 0 0 1 1
i 15 96.77772 8.177163 63 45 0 0.005742038 0 0 1 1
i 16 97.04215 8.177163 54 47 0 -0.06754643 0 0 1 1
i 16 97.58275 8.177163 42 47 0 0.08386553 0 0 1 1
i 14 97.83043 17.05257 27 45 0 -0.09434808 0 0 1 1
i 57 98.04969 8.177163 48 40.75 0 0.6380118 0 0 1 1
i 4 98.15167 8.177163 33 45 0 0.1876167 0 0 1 1
i 4 99.16409 14.41362 44 45 0 -0.002620465 0 0 1 1
i 11 99.43832 8.177163 54 43 0 0.8638661 0 0 1 1
i 16 100.792 15.70841 63 47 0 -0.8382288 0 0 1 1
i 4 102.1778 8.177163 71 45 0 0.8586037 0 0 1 1
i 14 102.8209 19.90422 68 45 0 -0.2462644 0 0 1 1
i 16 103.0801 8.177163 91 47 0 0.3231354 0 0 1 1
i 4 103.1478 8.177163 52 45 0 -0.2767799 0 0 1 1
i 14 103.6904 8.177163 65 45 0 0.6405752 0 0 1 1
i 14 104.2485 8.177163 94 41 0 -0.8188928 0 0 1 1
i 4 104.269 15.07543 63 45 0 0.2882151 0 0 1 1
i 16 105.0138 11.58099 80 47 0 0.4498938 0 0 1 1
i 57 105.4972 8.177163 86 40.75 0 -0.6606071 0 0 1 1
i 16 105.9488 8.177163 61 47 0 0.868249 0 0 1 1
i 13 106.2476 8.177163 48 33 0 -0.7283607 0 0 1 1
i 14 106.2632 8.177163 66 45 0 -0.3911881 0 0 1 1
i 13 106.3377 8.177163 59 33 0 0.5438006 0 0 1 1
i 57 106.4239 19.49336 67 40.75 0 -0.7603973 0 0 1 1
i 14 106.9418 15.97307 50 41 0 0.2292918 0 0 1 1
i 4 107.0271 8.177163 33 45 0 -0.885431 0 0 1 1
i 14 107.365 8.177163 29 45 0 0.3245167 0 0 1 1
i 13 107.4337 8.177163 40 33 0 0.06107959 0 0 1 1
i 16 107.714 8.177163 70 47 0 -0.1103997 0 0 1 1
i 14 107.781 14.6706 59 41 0 -0.5408078 0 0 1 1
i 57 108.2596 8.177163 76 40.75 0 -0.6515976 0 0 1 1
i 16 108.3401 8.177163 42 47 0 -0.2118007 0 0 1 1
i 14 108.352 8.177163 41 41 0 0.4723583 0 0 1 1
i 15 108.9501 8.177163 82 45 0 -0.827152 0 0 1 1
i 16 109.5026 8.177163 53 47 0 -0.4446792 0 0 1 1
i 15 109.8175 8.177163 63 45 0 0.00858779 0 0 1 1
i 14 110.5799 8.177163 68 41 0 0.5806889 0 0 1 1
i 16 111.3638 10.10634 90 47 0 0.8671016 0 0 1 1
i 4 111.6269 11.6033 42 45 0 0.5822197 0 0 1 1
i 16 112.9501 8.177163 62 47 0 -0.3567108 0 0 1 1
i 14 113.0092 22.58943 46 45 0 -0.8137003 0 0 1 1
i 4 113.494 68.73896 72 45 0 -0.4538743 0 0 1 1
i 16 113.8829 8.177163 78 47 0 0.07930098 0 0 1 1
i 14 114.6841 9.369574 36 45 0 0.697907 0 0 1 1
i 16 114.9557 8.177163 61 47 0 -0.2810888 0 0 1 1
i 14 115.1637 151.0827 76 41 0 -0.8401166 0 0 1 1
i 13 115.52 8.177163 48 33 0 -0.6068302 0 0 1 1
i 14 115.7019 8.177163 40 45 0 0.679255 0 0 1 1
i 57 115.8703 14.25279 86 40.75 0 -0.5214566 0 0 1 1
i 14 116.0738 29.99013 27 45 0 -0.4090445 0 0 1 1
i 14 116.2013 15.70841 67 41 0 -0.01360442 0 0 1 1
i 4 116.395 8.177163 33 45 0 -0.4978441 0 0 1 1
i 57 117.6367 8.177163 57 40.75 0 -0.01745731 0 0 1 1
i 14 118.2817 13.35843 48 45 0 0.8188981 0 0 1 1
i 16 119.5288 8.177163 91 47 0 0.2735436 0 0 1 1
i 14 119.9107 14.59181 95 41 0 0.4635065 0 0 1 1
i 14 119.9825 14.84983 65 45 0 -0.1165578 0 0 1 1
i 14 121.1861 10.51889 66 45 0 0.09518617 0 0 1 1
i 16 121.4625 8.177163 80 47 0 -0.8043254 0 0 1 1
i 5 121.8344 8.177163 90 41 0 -0.3194807 0 0 1 1
i 16 121.8871 8.177163 90 47 0 -0.1710335 0 0 1 1
i 4 121.9836 9.612716 61 45 0 0.7351805 0 0 1 1
i 4 122.1563 15.96235 53 45 0 0.556447 0 0 1 1
i 14 122.9742 10.42539 59 41 0 -0.4350674 0 0 1 1
i 14 123.0438 8.177163 55 45 0 -0.6662763 0 0 1 1
i 16 123.2135 8.859157 61 47 0 -0.01201167 0 0 1 1
i 16 123.8138 8.177163 51 47 0 -0.2186992 0 0 1 1
i 13 123.8942 8.177163 67 33 0 0.3932459 0 0 1 1
i 16 123.8955 8.177163 62 47 0 -0.381951 0 0 1 1
i 14 124.2716 8.177163 50 41 0 0.222184 0 0 1 1
i 13 124.491 15.40064 48 33 0 0.5613732 0 0 1 1
i 16 125.1662 8.177163 63 47 0 -0.3374856 0 0 1 1
i 14 125.2084 13.57264 40 41 0 -0.2057211 0 0 1 1
i 15 125.3989 8.177163 82 45 0 -0.2809264 0 0 1 1
i 5 125.4426 8.177163 46 41 0 0.5683843 0 0 1 1
i 14 125.6051 8.177163 68 45 0 0.3047132 0 0 1 1
i 14 126.2981 14.37283 49 41 0 -0.2170461 0 0 1 1
i 5 126.567 8.177163 55 41 0 -0.07470558 0 0 1 1
i 16 126.8212 8.177163 53 47 0 -0.3534949 0 0 1 1
i 4 126.9631 8.177163 63 45 0 0.739017 0 0 1 1
i 57 127.2349 8.177163 59 40.75 0 -0.02048811 0 0 1 1
i 11 127.2878 8.177163 63 43 0 0.4604218 0 0 1 1
i 14 128.7728 8.177163 68 41 0 -0.7054886 0 0 1 1
i 13 128.9032 8.177163 59 33 0 -0.1915383 0 0 1 1
i 4 129.8703 28.29598 42 45 0 0.6663362 0 0 1 1
i 14 130.0081 143.6398 57 41 0 -0.2504507 0 0 1 1
i 5 130.3169 15.70841 63 41 0 0.7508122 0 0 1 1
i 14 130.3367 14.66709 36 45 0 0.07884991 0 0 1 1
i 16 130.6269 8.177163 80 47 0 -0.6477411 0 0 1 1
i 4 130.8073 8.177163 33 45 0 -0.5402288 0 0 1 1
i 16 131.0429 72.15633 91 47 0 0.8080652 0 0 1 1
i 11 131.6748 8.177163 54 43 0 0.8821979 0 0 1 1
i 14 132.5023 73.36651 66 45 0 -0.4678633 0 0 1 1
i 14 133.3321 8.177163 26 45 0 -0.8702629 0 0 1 1
i 57 133.3547 8.177163 76 40.75 0 -0.2004927 0 0 1 1
i 57 133.46 8.177163 86 40.75 0 0.5034409 0 0 1 1
i 4 133.7226 8.177163 32 45 0 -0.04205147 0 0 1 1
i 14 133.8226 10.65864 67 41 0 0.07224841 0 0 1 1
i 14 133.9221 12.66104 59 41 0 -0.8667885 0 0 1 1
i 13 134.2168 8.177163 38 33 0 0.7203298 0 0 1 1
i 11 134.8424 8.177163 44 43 0 -0.5499083 0 0 1 1
i 14 134.9194 149.7097 95 41 0 0.6947966 0 0 1 1
i 16 134.9875 8.177163 62 47 0 -0.1057978 0 0 1 1
i 14 135.0504 8.177163 48 45 0 -0.6339078 0 0 1 1
i 13 135.2104 8.177163 67 33 0 -0.4688956 0 0 1 1
i 5 135.2667 8.177163 54 41 0 0.5393757 0 0 1 1
i 57 135.4626 8.177163 68 40.75 0 -0.04857289 0 0 1 1
i 14 135.595 8.177163 65 45 0 -0.7383183 0 0 1 1
i 14 135.6343 8.177163 50 45 0 0.260191 0 0 1 1
i 14 136.1034 8.177163 66 41 0 0.2395146 0 0 1 1
i 16 136.353 8.177163 53 47 0 0.151888 0 0 1 1
i 5 136.4581 14.99021 72 41 0 0.4079779 0 0 1 1
i 4 136.4736 8.177163 44 45 0 -0.2616514 0 0 1 1
i 14 138.3649 48.89498 48 41 0 0.324732 0 0 1 1
i 14 138.9046 8.177163 40 41 0 0.3731787 0 0 1 1
i 16 139.0291 12.64793 80 47 0 -0.6078087 0 0 1 1
i 16 139.6399 8.177163 70 47 0 -0.6592745 0 0 1 1
i 4 140.6838 11.30448 61 45 0 -0.09079907 0 0 1 1
i 16 141.4245 16.88894 72 47 0 -0.8243026 0 0 1 1
i 14 141.4529 8.177163 68 41 0 0.5352554 0 0 1 1
i 14 142.0256 8.177163 49 41 0 -0.5983996 0 0 1 1
i 4 142.134 14.45685 53 45 0 0.5961857 0 0 1 1
i 14 142.3017 10.88325 55 45 0 -0.3150656 0 0 1 1
i 14 142.4571 8.177163 85 41 0 0.2804038 0 0 1 1
i 5 142.8179 8.177163 90 41 0 -0.1436581 0 0 1 1
i 16 143.2068 9.336553 62 47 0 0.5074365 0 0 1 1
i 57 143.6523 8.177163 68 40.75 0 -0.6962534 0 0 1 1
i 57 143.9833 8.177163 86 40.75 0 0.8883625 0 0 1 1
i 57 144.5905 8.177163 67 40.75 0 -0.5731686 0 0 1 1
i 9 145.3952 8.177163 86 28 0 0.4732766 0 0 1 1
i 16 145.6956 9.397932 51 47 0 -0.4213428 0 0 1 1
i 57 146.1985 8.859157 57 40.75 0 -0.4605511 0 0 1 1
i 14 146.3942 20.2243 67 41 0 -0.7186707 0 0 1 1
i 13 146.4921 8.177163 57 33 0 -0.2802097 0 0 1 1
i 14 146.6585 9.090874 65 45 0 -0.3930708 0 0 1 1
i 15 146.835 8.177163 63 45 0 0.8435527 0 0 1 1
i 14 147.3564 8.177163 66 41 0 -0.5152091 0 0 1 1
i 15 147.4362 8.177163 82 45 0 0.1906708 0 0 1 1
i 5 147.9383 14.93001 63 41 0 -0.4922076 0 0 1 1
i 14 148.5322 46.98521 36 45 0 -0.5581701 0 0 1 1
i 14 148.6733 8.177163 59 41 0 -0.1771713 0 0 1 1
i 16 149.1864 8.177163 41 47 0 -0.1560769 0 0 1 1
i 14 149.2725 8.177163 27 45 0 -0.5937259 0 0 1 1
i 57 149.7539 8.177163 47 40.75 0 -0.3617067 0 0 1 1
i 5 149.9343 8.177163 55 41 0 0.2456434 0 0 1 1
i 13 149.9578 8.177163 48 33 0 -0.6271665 0 0 1 1
i 16 150.0639 8.177163 61 47 0 0.4846377 0 0 1 1
i 13 150.3715 8.177163 67 33 0 0.6911435 0 0 1 1
i 15 150.4723 8.177163 53 45 0 0.8265989 0 0 1 1
i 16 151.0173 8.177163 53 47 0 -0.8674807 0 0 1 1
i 14 151.0909 8.177163 38 41 0 0.1491235 0 0 1 1
i 5 151.3756 8.177163 44 41 0 0.4803374 0 0 1 1
i 16 151.3816 8.177163 59 47 0 -0.309964 0 0 1 1
i 5 151.412 8.177163 91 41 0 -0.6676809 0 0 1 1
i 14 151.5755 8.177163 26 45 0 -0.3744706 0 0 1 1
i 14 151.675 8.177163 40 41 0 0.6247974 0 0 1 1
i 9 151.7359 8.177163 50 28 0 -0.4846241 0 0 1 1
i 4 151.9659 8.177163 32 45 0 0.003553894 0 0 1 1
i 14 152.3713 9.048836 68 41 0 -0.8971238 0 0 1 1
i 5 152.399 8.177163 72 41 0 -0.8774302 0 0 1 1
i 13 152.4602 8.177163 38 33 0 -0.3841442 0 0 1 1
i 4 152.7509 20.15438 61 45 0 -0.7223417 0 0 1 1
i 14 152.8351 9.090874 38 45 0 -0.2074406 0 0 1 1
i 11 153.0857 8.177163 44 43 0 -0.2603724 0 0 1 1
i 57 155.4974 12.50514 86 40.75 0 0.3959477 0 0 1 1
i 14 155.5688 8.177163 85 41 0 -0.7821989 0 0 1 1
i 14 156.0192 8.177163 47 41 0 0.02494654 0 0 1 1
i 5 156.3461 8.177163 53 41 0 -0.136839 0 0 1 1
i 9 156.7598 8.177163 59 28 0 0.3416423 0 0 1 1
i 16 156.7826 8.177163 63 47 0 -0.4429024 0 0 1 1
i 14 156.8015 8.177163 48 45 0 -0.1772648 0 0 1 1
i 14 157.0749 49.96739 55 45 0 -0.4239422 0 0 1 1
i 16 157.5171 8.177163 62 47 0 0.7816511 0 0 1 1
i 16 159.372 9.356828 81 47 0 -0.04945457 0 0 1 1
i 14 159.3917 27.50511 38 41 0 0.1996492 0 0 1 1
i 5 159.6763 8.177163 44 41 0 -0.02087661 0 0 1 1
i 14 160.7637 12.57295 57 45 0 0.02613522 0 0 1 1
i 16 161.0665 11.35917 80 47 0 -0.4589654 0 0 1 1
i 13 161.1226 8.177163 67 33 0 -0.3005906 0 0 1 1
i 4 161.6634 9.679625 51 45 0 0.004252277 0 0 1 1
i 16 161.9043 9.419281 70 47 0 0.1729043 0 0 1 1
i 14 162.2794 8.533491 66 41 0 -0.3417192 0 0 1 1
i 57 162.416 12.73749 76 40.75 0 0.8333955 0 0 1 1
i 16 162.542 14.56667 72 47 0 -0.4816794 0 0 1 1
i 5 162.612 98.81692 72 41 0 0.4444276 0 0 1 1
i 14 162.7686 8.177163 45 45 0 -0.3194407 0 0 1 1
i 5 162.9261 72.00973 91 41 0 -0.3271821 0 0 1 1
i 15 163.0637 8.177163 82 45 0 0.8144497 0 0 1 1
i 14 163.3331 19.90422 68 41 0 0.06249788 0 0 1 1
i 13 163.6687 8.177163 57 33 0 0.6692493 0 0 1 1
i 14 164.1371 8.177163 56 41 0 0.7822848 0 0 1 1
i 11 164.3053 8.177163 63 43 0 0.3008053 0 0 1 1
i 4 164.4014 11.76789 42 45 0 -0.3785591 0 0 1 1
i 5 164.5124 8.177163 62 41 0 -0.782056 0 0 1 1
i 16 164.7282 8.177163 61 47 0 -0.4826005 0 0 1 1
i 5 164.7812 15.07543 63 41 0 0.3685027 0 0 1 1
i 9 164.9875 8.177163 68 28 0 -0.5488067 0 0 1 1
i 14 165.1726 8.177163 59 41 0 -0.1400069 0 0 1 1
i 16 165.4562 8.177163 60 47 0 0.7848991 0 0 1 1
i 14 165.551 20.40636 47 41 0 -0.644009 0 0 1 1
i 5 165.8779 46.33276 53 41 0 0.2577258 0 0 1 1
i 14 165.9878 8.177163 26 45 0 -0.04908215 0 0 1 1
i 57 166.0336 8.177163 66 40.75 0 -0.1135208 0 0 1 1
i 4 166.2404 8.177163 53 45 0 -0.4602246 0 0 1 1
i 4 166.3782 8.177163 32 45 0 0.01065495 0 0 1 1
i 15 166.7646 8.177163 72 45 0 0.7324994 0 0 1 1
i 9 166.8499 8.177163 59 28 0 -0.7622928 0 0 1 1
i 13 166.8724 8.177163 38 33 0 0.3850271 0 0 1 1
i 16 167.6898 8.177163 78 47 0 0.2109689 0 0 1 1
i 13 168.43 8.177163 48 33 0 -0.5596484 0 0 1 1
i 14 168.4712 8.177163 84 41 0 -0.1854327 0 0 1 1
i 14 168.8519 8.177163 59 45 0 -0.4002047 0 0 1 1
i 16 170.0571 8.654559 62 47 0 0.6445787 0 0 1 1
i 57 170.5027 8.177163 68 40.75 0 0.7360858 0 0 1 1
i 14 170.7527 10.55984 85 41 0 0.2111024 0 0 1 1
i 57 170.9235 8.177163 86 40.75 0 -0.5745263 0 0 1 1
i 14 171.3217 8.177163 40 41 0 0.0270427 0 0 1 1
i 14 171.4073 124.6803 66 41 0 0.5843371 0 0 1 1
i 5 172.4467 8.177163 44 41 0 -0.253216 0 0 1 1
i 16 172.5459 8.177163 51 47 0 0.5690106 0 0 1 1
i 14 172.6429 8.177163 75 41 0 -0.3719012 0 0 1 1
i 5 173.0248 8.177163 80 41 0 -0.5901879 0 0 1 1
i 57 173.0488 8.177163 57 40.75 0 -0.01297874 0 0 1 1
i 4 173.2216 8.177163 61 45 0 -0.5484154 0 0 1 1
i 16 173.3446 8.177163 80 47 0 0.6557535 0 0 1 1
i 9 173.5082 8.177163 86 28 0 -0.5324451 0 0 1 1
i 14 173.6488 9.364108 45 45 0 0.2092901 0 0 1 1
i 15 173.6853 8.177163 63 45 0 0.680491 0 0 1 1
i 9 174.1154 8.177163 67 28 0 -0.1935556 0 0 1 1
i 16 174.2986 8.177163 73 47 0 0.8038391 0 0 1 1
i 4 175.233 8.177163 51 45 0 -0.5999431 0 0 1 1
i 13 175.7358 8.177163 57 33 0 0.8493793 0 0 1 1
i 4 175.8381 8.177163 62 45 0 0.1151368 0 0 1 1
i 13 176.2837 8.177163 68 33 0 0.177064 0 0 1 1
i 14 176.3509 19.306 47 45 0 -0.6638007 0 0 1 1
i 16 176.9611 8.177163 82 47 0 0.1201993 0 0 1 1
i 14 177.3735 8.177163 26 45 0 0.09788449 0 0 1 1
i 14 177.6383 8.177163 57 45 0 -0.3411181 0 0 1 1
i 4 177.7639 8.177163 32 45 0 -0.6828319 0 0 1 1
i 4 178.0807 9.090874 42 45 0 0.4280235 0 0 1 1
i 4 179.171 8.177163 53 45 0 -0.4630974 0 0 1 1
i 11 179.3277 8.177163 53 43 0 0.5811465 0 0 1 1
i 11 179.4663 8.177163 63 43 0 -0.5661433 0 0 1 1
i 5 179.5888 8.177163 61 41 0 -0.3371815 0 0 1 1
i 16 181.4094 8.662907 81 47 0 0.6871057 0 0 1 1
i 57 181.8627 8.177163 87 40.75 0 0.3135833 0 0 1 1
i 14 182.2668 23.60333 85 41 0 0.4307356 0 0 1 1
i 16 182.413 9.7059 70 47 0 -0.7874222 0 0 1 1
i 14 182.6316 8.177163 65 41 0 -0.6631892 0 0 1 1
i 14 183.9127 8.177163 55 41 0 -0.7380848 0 0 1 1
i 4 184.2851 17.47493 61 45 0 -0.885034 0 0 1 1
i 14 184.3687 23.18323 45 45 0 -0.07828126 0 0 1 1
i 57 184.4534 8.177163 76 40.75 0 0.241746 0 0 1 1
i 16 184.6016 9.106853 80 47 0 0.05357339 0 0 1 1
i 4 184.6685 15.60607 72 45 0 -0.003835029 0 0 1 1
i 4 184.766 17.63917 51 45 0 -0.2955638 0 0 1 1
i 14 184.8594 17.05454 68 41 0 -0.444918 0 0 1 1
i 9 185.0223 8.177163 86 28 0 -0.4793842 0 0 1 1
i 14 185.0482 14.96652 84 41 0 -0.5883065 0 0 1 1
i 14 185.0529 8.177163 50 41 0 0.1727659 0 0 1 1
i 15 185.101 8.177163 82 45 0 0.4084077 0 0 1 1
i 16 185.1988 11.83069 62 47 0 0.6796316 0 0 1 1
i 14 185.84 14.26694 59 41 0 0.3728875 0 0 1 1
i 5 186.3075 9.344999 63 41 0 -0.4774219 0 0 1 1
i 9 186.3446 8.177163 67 28 0 -0.03960766 0 0 1 1
i 14 186.3682 8.177163 26 45 0 -0.8672188 0 0 1 1
i 14 186.6897 8.177163 57 45 0 0.4451962 0 0 1 1
i 13 186.7994 9.090874 57 33 0 0.2785024 0 0 1 1
i 13 186.8964 8.177163 68 33 0 -0.7594795 0 0 1 1
i 16 187.0701 8.177163 72 47 0 -0.04901562 0 0 1 1
i 5 187.0842 11.68432 80 41 0 0.8936551 0 0 1 1
i 16 187.2102 8.177163 51 47 0 0.05777554 0 0 1 1
i 13 187.6036 8.177163 47 33 0 0.7189848 0 0 1 1
i 57 187.7131 8.177163 57 40.75 0 0.4912217 0 0 1 1
i 14 188.1523 82.10388 47 41 0 -0.5332325 0 0 1 1
i 14 188.8082 17.73529 38 41 0 0.7327955 0 0 1 1
i 57 189.2979 8.177163 68 40.75 0 -0.2253374 0 0 1 1
i 9 189.4155 8.177163 59 28 0 0.62656 0 0 1 1
i 16 189.7272 8.177163 78 47 0 -0.3119414 0 0 1 1
i 14 190.6702 10.28157 48 41 0 -0.311874 0 0 1 1
i 14 190.8489 8.177163 36 41 0 0.2865791 0 0 1 1
i 5 191.1948 8.177163 42 41 0 0.156177 0 0 1 1
i 9 191.6327 8.177163 48 28 0 -0.8118302 0 0 1 1
i 5 192.0934 8.177163 44 41 0 0.4859382 0 0 1 1
i 16 192.1871 8.177163 54 47 0 -0.01484609 0 0 1 1
i 4 194.1415 8.177163 40 45 0 -0.02126877 0 0 1 1
i 16 194.1733 8.177163 80 47 0 -0.1275722 0 0 1 1
i 14 194.991 8.177163 74 41 0 -0.1894901 0 0 1 1
i 16 196.2199 8.177163 70 47 0 0.4976168 0 0 1 1
i 57 196.7315 8.177163 76 40.75 0 0.8545579 0 0 1 1
i 16 196.8355 8.177163 81 47 0 0.4473093 0 0 1 1
i 57 197.2889 8.177163 87 40.75 0 0.2421036 0 0 1 1
i 16 198.795 8.177163 51 47 0 -0.7700755 0 0 1 1
i 16 199.1173 9.106853 62 47 0 0.2400539 0 0 1 1
i 14 199.23 11.13399 56 41 0 0.05205714 0 0 1 1
i 14 199.3065 8.177163 34 45 0 -0.3668982 0 0 1 1
i 5 199.582 11.15732 62 41 0 -0.3144098 0 0 1 1
i 5 199.7875 8.177163 61 41 0 0.2360159 0 0 1 1
i 9 200.0276 8.177163 68 28 0 0.8901098 0 0 1 1
i 16 200.2267 8.177163 72 47 0 0.02888609 0 0 1 1
i 15 200.3862 8.177163 72 45 0 0.404017 0 0 1 1
i 9 200.4484 8.177163 86 28 0 -0.4579655 0 0 1 1
i 15 200.5272 8.177163 82 45 0 -0.3017896 0 0 1 1
i 14 200.7999 8.177163 67 41 0 0.4474964 0 0 1 1
i 14 200.9337 8.177163 84 41 0 0.3293397 0 0 1 1
i 14 201.6736 8.177163 45 41 0 0.3262885 0 0 1 1
i 5 202.0708 8.177163 51 41 0 -0.7580837 0 0 1 1
i 14 202.1971 8.177163 59 41 0 -0.07391468 0 0 1 1
i 14 202.2783 8.177163 65 41 0 0.3566643 0 0 1 1
i 9 202.5737 8.177163 57 28 0 -0.7778073 0 0 1 1
i 14 203.206 13.98768 48 41 0 -0.7755479 0 0 1 1
i 16 203.2102 8.177163 63 47 0 0.04867707 0 0 1 1
i 5 203.4582 8.177163 55 41 0 -0.4283122 0 0 1 1
i 14 203.6192 8.177163 36 41 0 -0.02992471 0 0 1 1
i 5 203.9652 8.177163 42 41 0 -0.6645502 0 0 1 1
i 9 204.4031 8.177163 48 28 0 -0.7300066 0 0 1 1
i 16 205.4302 17.63752 81 47 0 -0.683313 0 0 1 1
i 14 205.5269 20.06902 68 41 0 0.845707 0 0 1 1
i 16 205.8204 15.73599 91 47 0 0.461307 0 0 1 1
i 16 205.9196 17.80464 70 47 0 -0.4172825 0 0 1 1
i 5 206.975 15.24023 63 41 0 0.4048308 0 0 1 1
i 14 207.384 119.9401 85 41 0 -0.1516275 0 0 1 1
i 14 207.6459 8.177163 77 45 0 -0.04550688 0 0 1 1
i 57 207.9885 9.106853 76 40.75 0 -0.8002054 0 0 1 1
i 57 208.0872 8.177163 87 40.75 0 0.2047395 0 0 1 1
i 14 208.4549 8.177163 38 41 0 -0.01688547 0 0 1 1
i 57 208.8068 8.177163 66 40.75 0 0.7280548 0 0 1 1
i 14 208.8159 10.49159 66 45 0 0.7833857 0 0 1 1
i 14 208.9116 8.177163 74 41 0 0.7528979 0 0 1 1
i 4 209.1084 8.177163 72 45 0 -0.3531145 0 0 1 1
i 5 209.2935 17.2733 80 41 0 0.5221832 0 0 1 1
i 5 209.7732 8.177163 61 41 0 -0.4927907 0 0 1 1
i 14 210.4075 70.00694 55 41 0 -0.09188665 0 0 1 1
i 14 210.4498 11.759 56 45 0 0.01819604 0 0 1 1
i 14 210.5761 9.046645 75 41 0 -0.4848625 0 0 1 1
i 4 210.7799 10.17819 62 45 0 0.3813854 0 0 1 1
i 5 210.9343 8.662907 81 41 0 0.8720147 0 0 1 1
i 13 211.1978 8.177163 68 33 0 0.2793838 0 0 1 1
i 9 211.3876 8.177163 87 28 0 -0.4091994 0 0 1 1
i 5 211.8112 14.7432 62 41 0 -0.05577199 0 0 1 1
i 9 212.0213 10.1341 76 28 0 -0.8945978 0 0 1 1
i 9 212.2568 18.78989 68 28 0 -0.09643692 0 0 1 1
i 14 212.7414 11.92701 45 45 0 0.8476995 0 0 1 1
i 14 213.0624 8.177163 64 41 0 -0.3516057 0 0 1 1
i 4 213.114 10.95759 51 45 0 -0.5128037 0 0 1 1
i 5 213.4666 8.177163 70 41 0 -0.4543469 0 0 1 1
i 13 213.5856 9.730472 57 33 0 0.5516118 0 0 1 1
i 14 213.7078 17.73529 36 41 0 -0.02453202 0 0 1 1
i 5 214.0538 8.177163 42 41 0 -0.1664966 0 0 1 1
i 11 214.1826 8.177163 63 43 0 -0.5118673 0 0 1 1
i 16 214.6259 8.177163 82 47 0 0.07245376 0 0 1 1
i 5 215.1441 14.89334 53 41 0 0.4014436 0 0 1 1
i 16 215.4394 8.177163 63 47 0 -0.389351 0 0 1 1
i 16 215.459 8.177163 60 47 0 0.07715178 0 0 1 1
i 14 215.9673 10.83345 35 45 0 0.4606644 0 0 1 1
i 4 216.3878 10.38295 41 45 0 -0.3024409 0 0 1 1
i 13 216.92 9.812706 47 33 0 0.5431734 0 0 1 1
i 11 217.5937 9.090874 53 43 0 0.6427304 0 0 1 1
i 14 217.9558 8.177163 48 41 0 -0.8731949 0 0 1 1
i 14 218.0252 8.177163 56 41 0 0.6928968 0 0 1 1
i 14 218.4465 8.177163 59 45 0 0.1675565 0 0 1 1
i 14 219.2521 8.177163 78 41 0 -0.744809 0 0 1 1
i 5 219.4342 17.38515 61 41 0 0.7647767 0 0 1 1
i 14 220.0186 13.09075 74 41 0 0.7446952 0 0 1 1
i 14 220.5217 8.177163 24 45 0 0.8653622 0 0 1 1
i 4 220.9962 8.177163 30 45 0 0.7200398 0 0 1 1
i 9 221.1159 8.177163 86 28 0 0.8094863 0 0 1 1
i 13 221.5969 8.177163 36 33 0 0.7821395 0 0 1 1
i 11 222.3572 8.177163 42 43 0 -0.8850163 0 0 1 1
i 9 222.7725 8.177163 57 28 0 0.5000293 0 0 1 1
i 14 223.3197 8.177163 48 45 0 -0.853274 0 0 1 1
i 5 223.6119 8.177163 42 41 0 0.1581787 0 0 1 1
i 14 223.9758 8.177163 38 41 0 0.2696054 0 0 1 1
i 9 224.0498 8.177163 48 28 0 -0.3091744 0 0 1 1
i 4 224.5379 8.177163 54 45 0 -0.7000571 0 0 1 1
i 14 226.0023 8.177163 75 41 0 -0.6763092 0 0 1 1
i 5 226.3604 8.177163 81 41 0 0.8530471 0 0 1 1
i 9 226.8138 8.177163 87 28 0 0.05981305 0 0 1 1
i 14 227.6583 8.994648 68 41 0 0.2921405 0 0 1 1
i 14 228.2902 90.14018 56 41 0 0.3748637 0 0 1 1
i 5 228.5656 11.86684 51 41 0 -0.7762706 0 0 1 1
i 5 228.6422 9.77874 62 41 0 -0.52928 0 0 1 1
i 5 229.8339 8.177163 63 41 0 -0.1198266 0 0 1 1
i 16 230.0521 8.177163 82 47 0 -0.8798533 0 0 1 1
i 5 230.5763 8.177163 80 41 0 -0.7753275 0 0 1 1
i 16 230.6877 8.177163 91 47 0 -0.2824887 0 0 1 1
i 5 231.386 8.177163 53 41 0 -0.3379859 0 0 1 1
i 14 231.7085 9.422728 48 41 0 0.8869367 0 0 1 1
i 9 231.774 8.177163 59 28 0 -0.4156842 0 0 1 1
i 14 231.858 9.461297 45 41 0 0.6427192 0 0 1 1
i 16 232.3884 10.21318 81 47 0 -0.2599004 0 0 1 1
i 9 232.7581 8.177163 57 28 0 0.4441829 0 0 1 1
i 57 232.8136 8.177163 87 40.75 0 0.6207413 0 0 1 1
i 14 233.3545 31.66805 36 41 0 -0.5091505 0 0 1 1
i 5 233.7005 8.177163 42 41 0 0.8369139 0 0 1 1
i 14 234.597 41.20873 75 41 0 0.3819468 0 0 1 1
i 16 234.7633 11.00621 70 47 0 0.8688022 0 0 1 1
i 5 234.9551 24.62697 81 41 0 0.4678182 0 0 1 1
i 57 235.2432 9.757636 76 40.75 0 -0.5521313 0 0 1 1
i 5 235.3453 68.17907 91 41 0 -0.2653389 0 0 1 1
i 15 235.8506 8.177163 82 45 0 -0.5466594 0 0 1 1
i 9 237.5134 12.4403 76 28 0 0.5977223 0 0 1 1
i 14 237.5639 15.36311 74 41 0 0.8146706 0 0 1 1
i 9 237.6121 8.177163 87 28 0 0.4812661 0 0 1 1
i 16 238.0944 10.42153 60 47 0 0.6515439 0 0 1 1
i 57 238.6359 9.841308 66 40.75 0 -0.07980527 0 0 1 1
i 5 239.0447 16.97282 62 41 0 -0.3011691 0 0 1 1
i 5 239.2183 15.86247 80 41 0 0.6088679 0 0 1 1
i 15 239.3214 9.106853 72 45 0 0.09775582 0 0 1 1
i 9 239.4902 16.7376 68 28 0 -0.682442 0 0 1 1
i 14 239.6638 8.177163 59 41 0 -0.5180693 0 0 1 1
i 5 239.7189 11.86608 70 41 0 -0.01576934 0 0 1 1
i 5 239.9049 8.177163 61 41 0 -0.1626214 0 0 1 1
i 16 240.1891 8.177163 78 47 0 0.4671207 0 0 1 1
i 5 240.3253 8.177163 53 41 0 -0.2572844 0 0 1 1
i 14 240.8607 15.3809 64 41 0 0.4892149 0 0 1 1
i 14 242.1255 13.12131 68 41 0 0.8667727 0 0 1 1
i 9 242.4192 15.95629 57 28 0 0.6230911 0 0 1 1
i 16 242.7834 8.177163 49 47 0 0.6023189 0 0 1 1
i 57 243.3946 8.177163 56 40.75 0 -0.2390499 0 0 1 1
i 14 243.619 8.177163 77 41 0 -0.8088733 0 0 1 1
i 15 244.1682 8.177163 62 45 0 0.8560109 0 0 1 1
i 16 245.1475 8.177163 68 47 0 0.6166607 0 0 1 1
i 16 245.4312 8.177163 72 47 0 0.6650287 0 0 1 1
i 16 246.3871 8.177163 74 47 0 -0.7641028 0 0 1 1
i 9 247.4812 8.177163 87 28 0 -0.8417231 0 0 1 1
i 5 248.2123 34.23316 51 41 0 0.2050773 0 0 1 1
i 5 248.2889 13.07203 61 41 0 0.4242245 0 0 1 1
i 14 248.7145 15.16631 45 41 0 0.20795 0 0 1 1
i 5 249.2214 8.177163 42 41 0 0.2927341 0 0 1 1
i 16 250.1557 8.177163 63 47 0 0.812276 0 0 1 1
i 16 250.7195 8.177163 82 47 0 -0.4214025 0 0 1 1
i 14 251.9404 8.177163 35 41 0 0.7548943 0 0 1 1
i 5 252.3609 8.177163 41 41 0 -0.6868862 0 0 1 1
i 9 252.8931 8.177163 47 28 0 -0.3364817 0 0 1 1
i 16 253.5668 8.177163 53 47 0 0.6698077 0 0 1 1
i 9 253.9632 8.177163 76 28 0 -0.4794413 0 0 1 1
i 5 254.078 9.422728 53 41 0 0.5545053 0 0 1 1
i 14 254.3773 37.53033 74 41 0 0.1017572 0 0 1 1
i 14 254.4196 8.177163 59 41 0 0.7984662 0 0 1 1
i 57 255.2412 8.177163 69 40.75 0 0.006666233 0 0 1 1
i 14 258.1268 8.177163 68 41 0 0.2995503 0 0 1 1
i 9 258.2796 12.23609 87 28 0 -0.6246617 0 0 1 1
i 14 258.7245 8.177163 96 41 0 0.8518118 0 0 1 1
i 5 259.4849 8.177163 63 41 0 0.2407804 0 0 1 1
i 5 259.8576 21.17251 70 41 0 -0.5784304 0 0 1 1
i 5 259.9356 63.4677 81 41 0 -0.766543 0 0 1 1
i 5 260.1738 20.3853 62 41 0 -0.009045395 0 0 1 1
i 14 261.6 12.0574 64 41 0 0.5147638 0 0 1 1
i 14 263.3268 17.38683 77 41 0 -0.06357647 0 0 1 1
i 9 264.7681 13.69956 76 28 0 0.689457 0 0 1 1
i 5 264.8862 14.16947 72 41 0 0.265264 0 0 1 1
i 16 265.3755 8.177163 82 47 0 0.7258337 0 0 1 1
i 9 266.1127 10.22531 66 28 0 -0.8892475 0 0 1 1
i 5 266.9365 9.422728 61 41 0 0.3484113 0 0 1 1
i 14 267.1915 8.177163 54 41 0 0.1531614 0 0 1 1
i 5 267.6193 8.177163 60 41 0 -0.7103497 0 0 1 1
i 14 268.3097 28.7614 45 41 0 0.8825758 0 0 1 1
i 16 268.8463 8.177163 72 47 0 -0.4860144 0 0 1 1
i 14 269.1069 8.177163 36 41 0 -0.3007659 0 0 1 1
i 14 269.3191 13.3774 76 41 0 0.3408891 0 0 1 1
i 14 269.714 8.177163 78 41 0 0.8305573 0 0 1 1
i 9 269.845 13.07203 57 28 0 -0.4921216 0 0 1 1
i 14 271.5872 8.177163 35 41 0 0.7636345 0 0 1 1
i 14 271.6981 10.40672 47 41 0 0.6772431 0 0 1 1
i 5 272.0076 8.177163 41 41 0 -0.1228598 0 0 1 1
i 9 272.352 8.177163 68 28 0 -0.05240266 0 0 1 1
i 9 272.5399 8.177163 47 28 0 0.1117845 0 0 1 1
i 9 272.7468 8.177163 87 28 0 0.6077077 0 0 1 1
i 16 273.2136 8.177163 53 47 0 -0.633044 0 0 1 1
i 14 275.0977 10.29462 57 41 0 0.2507007 0 0 1 1
i 16 275.3368 8.177163 63 47 0 0.8920686 0 0 1 1
i 5 275.912 8.177163 74 41 0 0.2509094 0 0 1 1
i 14 277.5096 53.18203 64 41 0 -0.108867 0 0 1 1
i 14 278.8825 8.177163 68 41 0 -0.848901 0 0 1 1
i 14 279.1336 50.29432 75 41 0 -0.7503284 0 0 1 1
i 14 279.3919 44.82258 96 41 0 0.05466093 0 0 1 1
i 5 281.3888 12.53986 70 41 0 0.5076027 0 0 1 1
i 14 281.8563 10.40672 55 41 0 0.4259614 0 0 1 1
i 9 281.8687 11.744 76 28 0 -0.5577103 0 0 1 1
i 5 282.4511 9.795199 60 41 0 -0.4287109 0 0 1 1
i 14 282.6635 11.94068 77 41 0 -0.08595942 0 0 1 1
i 9 283.0059 8.177163 87 28 0 -0.07195161 0 0 1 1
i 14 283.6413 8.177163 54 41 0 0.3125469 0 0 1 1
i 9 284.6106 8.177163 66 28 0 -0.5814845 0 0 1 1
i 16 285.2961 8.177163 72 47 0 -0.5619045 0 0 1 1
i 5 285.3996 12.0441 51 41 0 -0.7263237 0 0 1 1
i 16 286.0429 8.177163 82 47 0 -0.3170257 0 0 1 1
i 5 286.1346 11.60357 62 41 0 -0.5934123 0 0 1 1
i 14 286.8581 8.177163 76 41 0 -0.8554787 0 0 1 1
i 14 287.1081 8.177163 35 41 0 0.5951574 0 0 1 1
i 5 287.5285 8.177163 41 41 0 -0.4593089 0 0 1 1
i 5 287.8895 8.177163 72 41 0 -0.644337 0 0 1 1
i 9 288.0608 8.177163 47 28 0 -0.6202045 0 0 1 1
i 9 288.4926 9.422728 57 28 0 -0.3545807 0 0 1 1
i 9 289.9789 8.177163 68 28 0 -0.8395698 0 0 1 1
i 14 290.1925 8.177163 68 41 0 -0.1632791 0 0 1 1
i 14 290.3815 8.177163 78 41 0 0.5096226 0 0 1 1
i 5 294.8983 30.10469 70 41 0 -0.1019306 0 0 1 1
i 14 295.3584 9.455412 54 41 0 -0.8685212 0 0 1 1
i 9 296.9504 9.422728 76 28 0 -0.3855881 0 0 1 1
i 5 297.0644 8.177163 60 41 0 0.8125141 0 0 1 1
i 9 297.4731 8.177163 87 28 0 -0.591633 0 0 1 1
i 14 297.5373 20.04226 66 41 0 -0.7305303 0 0 1 1
i 9 297.606 8.177163 66 28 0 -0.4069589 0 0 1 1
i 14 297.6877 20.59246 45 41 0 -0.5375449 0 0 1 1
i 14 298.7657 8.177163 77 41 0 0.4556094 0 0 1 1
i 5 298.8856 10.40672 51 41 0 0.541583 0 0 1 1
i 5 299.1879 10.29462 62 41 0 0.1549515 0 0 1 1
i 14 299.3696 8.177163 35 41 0 -0.005409865 0 0 1 1
i 5 299.79 8.177163 41 41 0 0.1637518 0 0 1 1
i 5 300.2283 8.177163 72 41 0 0.1238247 0 0 1 1
i 16 300.3778 9.422728 72 47 0 -0.5668236 0 0 1 1
i 16 300.5101 8.177163 83 47 0 0.722982 0 0 1 1
i 16 301.4741 8.177163 61 47 0 -0.8559681 0 0 1 1
i 5 305.4743 15.26609 91 41 0 -0.4241223 0 0 1 1
i 14 306.9029 24.33754 54 41 0 0.5085275 0 0 1 1
i 5 307.3308 18.36701 60 41 0 0.1794638 0 0 1 1
i 14 307.4029 8.177163 77 41 0 0.1011878 0 0 1 1
i 9 307.5076 10.29462 76 28 0 -0.7262411 0 0 1 1
i 9 307.6002 8.177163 87 28 0 0.7682167 0 0 1 1
i 9 308.275 10.40672 66 28 0 -0.1828956 0 0 1 1
i 14 309.0562 8.177163 35 41 0 -0.8281747 0 0 1 1
i 9 310.3866 8.177163 55 28 0 0.1532102 0 0 1 1
i 5 317.4274 8.177163 49 41 0 -0.1562131 0 0 1 1
i 14 322.9897 8.177163 43 41 0 -0.6668399 0 0 1 1
i 14 327.4 8.177163 96 41 0 0.1960902 0 0 1 1
i 14 328.5165 11.63124 85 41 0 -0.8002685 0 0 1 1
i 5 328.7957 8.177163 92 41 0 0.6269324 0 0 1 1
i 14 330.0756 13.82464 75 41 0 -0.08628495 0 0 1 1
i 5 330.3906 11.33218 81 41 0 -0.02697412 0 0 1 1
i 9 330.7894 8.177163 87 28 0 -0.7785957 0 0 1 1
i 14 332.2624 14.76231 64 41 0 -0.5021894 0 0 1 1
i 5 332.6179 13.0599 71 41 0 0.2598565 0 0 1 1
i 9 333.0679 10.90495 77 28 0 0.8564973 0 0 1 1
i 16 333.6376 8.177163 83 47 0 0.8088162 0 0 1 1
i 14 335.3407 14.33292 54 41 0 0.4256505 0 0 1 1
i 5 335.7419 13.28892 60 41 0 -0.1424635 0 0 1 1
i 9 336.2498 11.96742 66 28 0 0.3343209 0 0 1 1
i 16 336.8927 10.29462 72 47 0 0.335189 0 0 1 1
i 14 337.7064 8.177163 78 41 0 -0.4837207 0 0 1 1
i 14 339.6866 12.28334 43 41 0 0.8905946 0 0 1 1
i 5 340.1394 11.79819 49 41 0 -0.1159325 0 0 1 1
i 9 340.7126 11.18408 56 28 0 0.4388267 0 0 1 1
i 16 341.4381 10.40672 62 47 0 -0.291509 0 0 1 1
i 14 342.3565 9.422728 68 41 0 -0.2467516 0 0 1 1
i 5 343.5191 8.177163 74 41 0 0.8817899 0 0 1 1
i 14 345.8369 8.177163 33 41 0 0.4051409 0 0 1 1
i 5 346.3479 8.177163 39 41 0 0.607512 0 0 1 1
i 9 346.9948 8.177163 45 28 0 -0.1835362 0 0 1 1
i 16 347.8136 8.177163 51 47 0 0.6330523 0 0 1 1
i 14 348.8501 8.177163 57 41 0 -0.5911148 0 0 1 1
i 5 350.1621 8.177163 63 41 0 0.8053097 0 0 1 1
i 9 351.8228 8.177163 69 28 0 -0.268386 0 0 1 1

e     5.000

</CsScore>
</CsoundSynthesizer>
