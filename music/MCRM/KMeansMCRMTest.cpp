#include <Composition.hpp>
#include <MCRM.hpp>
#include <eigen3/Eigen/Dense>
#include <functional>
#include <memory>
#include <MusicModel.hpp>
#include <random>
#include <ScoreNode.hpp>
#include <VoiceleadingNode.hpp>
#include <vector>

/**
 * All composition and synthesis code is defined in the main function.
 * There is no need for any of this code to be in a separate file.
 */
int main(int argc, const char **argv)
{
    csound::MusicModel model;
    // These fields determine output filenames and ID 3 tags.
    model.setAuthor("Michael Gogins");
    model.setTitle("KMeansMCRMTest");
    model.setAlbum("Silence");
    model.setYear("2018");
    model.setPerformanceRightsOrganization("Irreducible Productions, ASCAP");
    csound::KMeansMCRM mcrm;
    mcrm.sample_count = 20000000;
    mcrm.means_count =       200;
    mcrm.algorithm_type = csound::KMeansMCRM::DETERMINISTIC;
    mcrm.setDepth(9);
    mcrm.resize(3);
    mcrm.setTransformationElement(0, csound::Event::TIME,   csound::Event::TIME,            0.5);
    mcrm.setTransformationElement(0, csound::Event::KEY,    csound::Event::KEY,             0.5);
    mcrm.setTransformationElement(0, csound::Event::TIME,   csound::Event::HOMOGENEITY,   -1);
    mcrm.setTransformationElement(1, csound::Event::TIME,   csound::Event::TIME,            0.5);
    mcrm.setTransformationElement(1, csound::Event::KEY,    csound::Event::KEY,             0.5);
    mcrm.setTransformationElement(1, csound::Event::KEY,    csound::Event::HOMOGENEITY,    1.);
    mcrm.setTransformationElement(2, csound::Event::TIME,   csound::Event::TIME,            0.5);
    mcrm.setTransformationElement(2, csound::Event::KEY,    csound::Event::KEY,             0.5);
    mcrm.setTransformationElement(2, csound::Event::TIME,   csound::Event::HOMOGENEITY,    1.);
    //mcrm.setTransformationElement(2, csound::Event::KEY, csound::Event::HOMOGENEITY, 0.);
    csound::Rescale rescale;
    rescale.setRescale(csound::Event::TIME, true, true, 1., 100.);
    rescale.setRescale(csound::Event::DURATION, true, true, .2, 0.);
    rescale.setRescale(csound::Event::INSTRUMENT, true, true, 1., 0.);
    rescale.setRescale(csound::Event::KEY, true, true, 36., 72.);
    rescale.setRescale(csound::Event::VELOCITY, true, true, 60., 6.);
    rescale.addChild(&mcrm);
    model.addChild(&rescale);
    model.setTieOverlappingNotes(true);
    model.setDuration(240.);
    model.setCsoundOrchestra(R"(
sr = 48000
ksmps = 64
nchnls = 2 
0dbfs = 1

gi_aeolus aeolus_init "/home/mkg/stops-0.3.0", "Aeolus", "waves", 0, 3

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
    model.processArgv(argc, argv);
}

