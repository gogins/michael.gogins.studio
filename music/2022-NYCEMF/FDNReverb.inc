gk_FdnReverb_revTime chnexport "gk_FdnReverb_revTime", 3
gk_FdnReverb_fadeInTime chnexport "gk_FdnReverb_fadeInTime", 3
gi_FdnReverb_delayLength chnexport "gi_FdnReverb_delayLength", 3
gk_FdnReverb_highCutoff chnexport "gk_FdnReverb_highCutoff", 3
gk_FdnReverb_highQ chnexport "gk_FdnReverb_highQf", 3
gk_FdnReverb_highGain chnexport "gk_FdnReverb_highGain", 3
gk_FdnReverb_lowCutoff chnexport "gk_FdnReverb_lowCutoff", 3
gk_FdnReverb_lowQ chnexport "gk_FdnReverb_lowQ", 3
gk_FdnReverb_lowGain chnexport "gk_FdnReverb_lowGain", 3
gk_FdnReverb_wet chnexport "gk_FdnReverb_wet", 3

gk_FdnReverb_revTime init 0.01
gk_FdnReverb_fadeInTime init 0.01
gi_FdnReverb_delayLength init 0.8
gk_FdnReverb_highCutoff init 0.8
gk_FdnReverb_highQ init 0.0
gk_FdnReverb_highGain init 0.5
gk_FdnReverb_lowCutoff init 0.01
gk_FdnReverb_lowQ init 0.0
gk_FdnReverb_lowGain init 0.01
gk_FdnReverb_wet init 0.5

instr FdnReverb
gk_FdnReverb_dry = 1.0 - gk_FdnReverb_wet
ab_in[] init 4
ab_out[] init 4
ab_in, inletv "ab_in"
// It is the Ambisonic B-format signal that is reverberated.
ab_out[0],  ab_out[1], ab_out[2], ab_out[3] vst3audio gi_iem_fdn_reverb, ab_out[0], ab_out[1], ab_out[2], ab_out[3] 
outletv "ab_out", ab_out
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin
