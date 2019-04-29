#include <cmath>
#include <csound.hpp>
#include <cstring>
#include <string>
#include <Silence.hpp>

int main(int argc, const char **argv)
{
    csound::MusicModel model;
    model.setAuthor("Michael Gogins");
    model.setTitle("Diachronic");
    model.setCsoundOrchestra(R"(
    
sr = 48000
ksmps = 64
nchnls = 2 
0dbfs =  1

gi_aeolus aeolus_init "/home/mkg/stops-0.3.0", "Aeolus", "waves", 0, 10

instr 1 
print p1, p2, p3, p4, p5
aeolus_note gi_aeolus, p1, p4, p5
endin

instr 2 
print p1, p2, p3, p4, p5
aeolus_note gi_aeolus, p1, p4, p5
endin

instr 3 
print p1, p2, p3, p4, p5
aeolus_note gi_aeolus, p1, p4, p5
endin

instr 4 
print p1, p2, p3, p4, p5
aeolus_note gi_aeolus, p1, p4, p5
endin

instr 5
print p1, p2, p3, p4, p5
aeolus_note gi_aeolus, p1, p4, p5
endin

instr 6
print p1, p2, p3, p4, p5
aeolus_note gi_aeolus, p1, p4, p5
endin

alwayson "aeolus_out"

; Send audio from the Aeolus to the output.
instr aeolus_out 
print p1, p2, p3
aeolus_preset gi_aeolus, 1, 1, "~/.aeolus-presets"
a_out[] init 2
a_out aeolus_out gi_aeolus
out a_out
endin
)");
    ///csound::ScoreNode scoreNode;
    ///scoreNode.importFilename = "/home/mkg/Downloads/mahler_symphony_10_1_(c)boot.mid";
    csound::ChordLindenmayer lindenmayer;
    //lindenmayer.tieNotes = false;
    lindenmayer.range = 24;
    lindenmayer.axiom = "C [ =s0,1 =s1,0 =s2,0 =s3,0 F ] [ =s0,0 =s1,1 =s2,0 =s3,0  F ] [ =s0,0 =s1,0 =s2,1 =s3,0 F ] [ =s0,0 =s1,0 =s2,0 =s3,1 F ]";
    lindenmayer.iterationCount = 4;
    ///lindenmayer.timeStep = 0.125;
    lindenmayer.rules["F"] = "F F F";
    lindenmayer.rules["C"] = "C C";
    model.addChild(&lindenmayer);
    model.arrange( 0,  4,  0.00); 
    model.arrange( 1,  1,  0.00); 
    model.arrange( 2,  2,  0.00);
    model.arrange( 3,  3,  0.00); 
    model.arrange( 4,  4,  0.00); 
    model.arrange( 5,  1,  0.00);
    model.arrange( 6,  2,  0.00);
    model.arrange( 7,  3,  0.00);
    model.arrange( 8,  4,  0.00);
    model.arrange( 9,  1,  0.00);
    model.arrange(10,  2,  0.00);
    model.arrange(11,  3,  0.00); 
    model.arrange(12,  4,  0.00);
    model.arrange(13,  1,  0.00);
    model.arrange(14,  2,  0.00);
    model.arrange(15,  3,  0.00);
    model.arrange(16,  4,  0.00);
    model.processArgv(argc, argv);
    return 0;
}
