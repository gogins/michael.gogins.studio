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
 
gcg.setAxiom('section1 section2 section3 ')
gcg.addRule( 'section1', 'P(0,4,7) V+9 R66 I0=1 I1=2 I2=3 WC V+11 1 WC xV-2 WC a3 dd a4 dd ') 
gcg.addRule( 'section2', 'P(0,3,7) T+2 V+36 L*2 D*2.25 R54 I0=1 I1=1 I2=1 I3=1 I4=1 I5=1 T+2 V+35 a3 D/2.225 ') 
# gcg.addRule( 'section2', 'P(0,3,7) T+2 L* 2 D*2.25 R54 I0=1 I1=1 I2=1 I3=1 I4=1 I5=1 T+2 V+35 a5 D/2.225 ') 
gcg.addRule( 'section3', 'P(0,4,7) R69 V+110 O.15 I0=3 I1=2 I2=1 L/2 arp1 dd K D*1.875 L/4 a4 arp2 dd dd T-17 V-2 WC WC WC WC WC WC ') 
  
# gcg.setAxiom('pcs1 R66 I0=1 I1 =2 I2=3 WC xV+11 WC xV-2 WC a3 dd a4 dd L*2 D*3.2 R54 I0=1 I1=1 I2=1 I3=1 42=1 pcs(0,4,7,11,14) T+2 zV+17 a3 D/3.175 R66 O.15 I0=3 I1=2 I2=1 L/2 arp1 dd K D*1.875 L/4 a4 arp2 dd dd')
 
gcg.addRule('a3',   'V+11 a3k a3q a3 a3 WV ')  
gcg.addRule('a3k',  'K  arp WV WC ') 
gcg.addRule('a3q',  'Q7     WV K D/1.245 WV arp1 D/1.25 WC a4q D*1.25 V+2 D*1.25 V+4 WC WC ')
 
gcg.addRule('a4',   'xV-9  L*2 a4k a4q D/1.28 a4 arp a3 D/1.25 a3k a4 D*1.25 D*1.25 L/2 WC ')
# gcg.addRule('a5',   'xV-9  L*2 a4k a4q D/1.28 a4 arp2 a3 D/1.25 a3k a5 D*1.25 D*1.25 L/2 WC ')
gcg.addRule('a4k',  'K  WV ')
gcg.addRule('a4q',  'Q3 WV K V+1 WC')

gcg.addRule('arp',  'T+6 V+17 WC V+5 WC V-16 WC V-9 T-7 ') 
gcg.addRule('arp1', 'L/2 D/2.125 Q5 WV Q5 WV  Q5 WV Q5 WV D*2.125 L*2 O2 ') 
gcg.addRule('arp2', 'L/2.5 D/2.125 Q5  WV Q5 WV Q5 WV Q5 WV Q5 WV Q5 WV D*2.125 L*2.51 ') 
gcg.addRule('dd',   'WV WV ')   
 
gcg.setIterationCount(6) 
gcg.debug = True 
gcg.generate()

rescale = CsoundAC.Rescale()
rescale.setRescale( CsoundAC.Event.TIME,       True, False, (1.0 / 40.0),   60     )
rescale.setRescale( CsoundAC.Event.KEY,        True, False, 33,             48     )
rescale.setRescale( CsoundAC.Event.VELOCITY,   True, True,  50,             40     )
rescale.setRescale( CsoundAC.Event.PAN,        True, True,   0.1,            0.8   )
rescale.addChild(gcg) 

model.addChild(rescale)
model.setAuthor("CsoundAC Tutorial");
model.setTitle("reasript-duo-dualities");
model.generateAllNames()
model.generate()
model.getScore().tieOverlappingNotes(True)
model.getScore().setDuration(8*60) 
# The ac_reaper module score_to_midiitem function can translate both scores as 
# raw Python arrays of notes, or scores as CsoundAC Scores.
ac_reaper.score_to_midiitem(model.getScore())
