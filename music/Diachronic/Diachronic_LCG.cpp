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

gi_aeolus aeolus_init "/home/mkg/stops-0.3.0", "Aeolus", "waves", 0, 12

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
    csound::Generator lcg;
    int iterations;
    lcg.callable = [](csound::Score &score) {
        int a = 122;
        int c = 311;
        int m = 601;
        int x = 11;
        int x1 = 5;
        double timestep = 0.125;
        double duration = .5;
        for (int i = 0; i < 400; ++i) {
            x = ((a * x1) + c) % m;
            x1 = x;
            score.append(double(i * timestep), duration, 144., 1., double(x), 70., .5);
        }
    };
    csound::Rescale rescale;
    //rescale.setRescale(csound::Event::TIME, true, true, 1., 240.);
    //rescale.setRescale(csound::Event::DURATION, true, true, .2, 0.);
    rescale.setRescale(csound::Event::INSTRUMENT, true, true, 1., 0.);
    rescale.setRescale(csound::Event::KEY, true, true, 38., 60.);
    rescale.setRescale(csound::Event::VELOCITY, true, true, 60., 6.);
    csound::CellRepeat a;
    a.repeat(10, 2 *.125, false, 0, 46, 3);
    a.addChild(&lcg);
    rescale.addChild(&a);
    csound::CellRepeat b;
    b.repeat( 9, 2 *.125, false, 1, 40, 3);
    b.addChild(&lcg);
    rescale.addChild(&b);
    csound::CellRepeat c;
    c.repeat( 8, 2 *.125, false, 2, 54, 3);
    c.addChild(&lcg);
    rescale.addChild(&c);
    model.addChild(&rescale);
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
