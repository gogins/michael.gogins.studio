import copy
import CsoundAC
import math
import os
import string
import sys
import types 
import time
from reaper_python import *
for p in sys.path:
    RPR_ShowConsoleMsg(p + '\n')
import ac_reaper
import CsoundAC
import GeneralizedContextualGroup
 
model = CsoundAC.MusicModel()
gcg = GeneralizedContextualGroup.GeneralizedContextualGroup()
gcg.avoidParallelFifths = True 
 
gcg.setAxiom('section1 dd dd dd T+2 V-13 O.2 section2 T+5 V-7 section3 D/2.5 ') 
gcg.addRule( 'section1', 'B15  R63 P(0,4,7) V+9 I0=1 I1=2 I2=3 V+11 a3 dd a4 dd ') 
# gcg.addRule( 'section1', 'B15  R63 P(0,4,7) V+9 I0=1 I1=2 I2=3 V+11 WV a3 dd a4 dd ') 
gcg.addRule( 'section2', 'B24  R45 P(0,3,5,10) V+36 L/12 D*2.25 I0=5 I1=2 I2=5 I3=5 I4=2 I5=5 T+2 V+27 a3x a5 D/2.225 V-7 L*12 ')
gcg.addRule( 'section3', 'B12   R70 P(0,4,7) V+109 O.15 I0=3 I1=2 I2=1 L*12 arp1 dd K D*1.875 L/4 a4x a6 K V-24 L*16 B0 arp2 arp2 B3 dd dd T+6 sV-2 D*2 WV WC WC WC WC WC ') 
     
gcg.addRule('a5',   'D*1.875 a3 D/1.875 ')   
gcg.addRule('a3',   'V+11 a3k a3q a3 a3 WV ')   
gcg.addRule('a3k',  'K arp WV WC ')    
gcg.addRule('a3q',  'Q7 WV K D/1.245 WV arp1 D/1.25 WC a4q D*1.225 V+2 D*1.25 V+4 WC WC ') 
  
gcg.addRule('a6',   'xV-9 L*2 a4k a4q D/1.128 a6 arp a3 D/1.125 a3k K T2   a6 D*1.125 D*1.125 L/2 WC ')
gcg.addRule('a4',   'xV-9 L*2 a4k a4q D/1.128 a4 arp a3 D/1.125 a3k a4 D*1.125 D*1.125 L/2 WC ')
gcg.addRule('a4k',  'K  WV ')
gcg.addRule('a4q',  'Q3 WV K V+1 WC') 

gcg.addRule('arp',  'T+6 V+17 WC V+5 WC V-16 WC V-9 T-7 ') 
gcg.addRule('arp1', 'L/2 D/2.12 Q5 WV Q5 WV  Q5 WV Q5 WV D*2.12 L*2 O2 ') 
gcg.addRule('arp2', 'L/2.5 D/2.125 Q5  WV Q5 WV Q5 WV Q5 WV Q5 WV Q5 WV D*2.125 L*2.51 ') 
gcg.addRule('dd',   'WV WV V-1 WC V1')   
 
gcg.setIterationCount(6) 
gcg.debug = True 
gcg.generate() 

rescale = CsoundAC.Rescale()
rescale.setRescale( CsoundAC.Event.TIME,       True, False, (1.0 / 40.0),   60     )
rescale.setRescale( CsoundAC.Event.KEY,        True, False, 20,             48     )
rescale.setRescale( CsoundAC.Event.VELOCITY,    True, True, 55,             30     )
rescale.setRescale( CsoundAC.Event.PAN,        True, True,   0.1,            0.8   )
rescale.addChild(gcg) 

model.addChild(rescale)
model.setAuthor("CsoundAC Tutorial");
model.setTitle("reasript-duo-dualities");
model.generateAllNames()
model.generate()
model.getScore().tieOverlappingNotes(True)
model.getScore().setDuration(7.5*60) 
# The ac_reaper module score_to_midiitem function can translate both scores as 
# raw Python arrays of notes, or scores as CsoundAC Scores.
ac_reaper.score_to_midiitem(model.getScore())
