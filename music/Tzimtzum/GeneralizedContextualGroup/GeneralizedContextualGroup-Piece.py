'''
A   S I L E N C E   C O M P O S I T I O N
Copyright (C) 2008 by Michael Gogins.
All rights reserved.
This software is licensed under the terms of the
GNU Lesser General Public License.
'''
print(__doc__)
print('IMPORTING REQUIRED MODULES...')
print
import csnd6
import CsoundAC
import math
import os
import random
import signal
import string
import sys
import time
import traceback
import numpy
sys.path.append('/home/mkg/Dropbox/studio')
import GeneralizedContextualGroup

'''
N/A /home/mkg/Dropbox/2009-ICMC-MKG/figure-1.py:27
N/A /home/mkg/Dropbox/2009-ICMC-MKG/figure-2.py:27
N/A /home/mkg/Dropbox/2009-ICMC-MKG/figure-3.py:27
x   /home/mkg/Dropbox/2009-ICMC-MKG/mkg-2009-02-03-b.py:27
x   /home/mkg/Dropbox/2009-ICMC-MKG/mkg-2009-02-03-c.py:28
x   /home/mkg/Dropbox/2009-ICMC-MKG/mkg-2009-02-03-d.py:28
x   /home/mkg/Dropbox/2010-ICMC-MKG/60x60/mkg-2009-12-30-1.py:707
x   /home/mkg/Dropbox/2010-ICMC-MKG/60x60/mkg-2009-12-30-10.py:674
    /home/mkg/Dropbox/2010-ICMC-MKG/60x60/mkg-2009-12-30-11.py:674
    /home/mkg/Dropbox/2010-ICMC-MKG/60x60/mkg-2009-12-30-12.py:674
    /home/mkg/Dropbox/2010-ICMC-MKG/60x60/mkg-2009-12-30-2.py:707
    /home/mkg/Dropbox/2010-ICMC-MKG/60x60/mkg-2009-12-30-3.py:707
    /home/mkg/Dropbox/2010-ICMC-MKG/60x60/mkg-2009-12-30-4.py:720
    /home/mkg/Dropbox/2010-ICMC-MKG/60x60/mkg-2009-12-30-5.py:720
    /home/mkg/Dropbox/2010-ICMC-MKG/60x60/mkg-2009-12-30-6.py:674
    /home/mkg/Dropbox/2010-ICMC-MKG/60x60/mkg-2009-12-30-7.py:674
    /home/mkg/Dropbox/2010-ICMC-MKG/60x60/mkg-2009-12-30-8.py:707
    /home/mkg/Dropbox/2010-ICMC-MKG/60x60/mkg-2009-12-30-9.py:674
    /home/mkg/Dropbox/2010-ICMC-MKG/60x60/mkg-2010-06-07-1.py:674
x   /home/mkg/Dropbox/2010-ICMC-MKG/mkg-2009-09-14-o/mkg-2009-09-14-o-1.py:707
    /home/mkg/Dropbox/2010-ICMC-MKG/mkg-2009-09-14-o/mkg-2009-09-14-o.py:707
    /home/mkg/Dropbox/2014-Vanderbilt_Planetarium-MKG/mkg-2009-09-14-o-1.py:707
x   /home/mkg/Dropbox/music/mkg-2010-03-16-e/mkg-2010-03-16-e-1.py:451
    /home/mkg/Dropbox/music/mkg-2010-03-16-e/mkg-2010-03-16-e.py:384
    /home/mkg/Dropbox/music/mkg-2010-06-07-1/mkg-2010-06-07-1.py:786
x   /home/mkg/Dropbox/studio/mkg-2009-09-14-o.py:707
x   /home/mkg/Dropbox/studio/mkg-2009-09-14-r.py:707
x   /home/mkg/Dropbox/studio/mkg-2009-09-14-s.py:728
    /home/mkg/Dropbox/studio/01_workshop/mkg-2008-12-14-a.py:27
    /home/mkg/Dropbox/studio/01_workshop/mkg-2008-12-14-b.py:27
    /home/mkg/Dropbox/studio/01_workshop/mkg-2008-12-17-a.py:27
    /home/mkg/Dropbox/studio/01_workshop/mkg-2008-12-24-a.py:27
    /home/mkg/Dropbox/studio/01_workshop/mkg-2008-12-25-a.py:27
x   /home/mkg/Dropbox/studio/01_workshop/mkg-2008-12-25-b.py:27
    /home/mkg/Dropbox/studio/01_workshop/mkg-2009-01-01-a.py:27
    /home/mkg/Dropbox/studio/01_workshop/mkg-2009-01-10-a.py:27
    /home/mkg/Dropbox/studio/01_workshop/mkg-2009-01-10-b.py:27
x   /home/mkg/Dropbox/studio/01_workshop/mkg-2009-01-10-c.py:27
N/A /home/mkg/michael.gogins.studio/music/Tzimtzum/GeneralizedContextualGroup/GeneralizedContextualGroup-Piece.py:28
N/A /home/mkg/michael.gogins.studio/music/Tzimtzum/GeneralizedContextualGroup/mkg-2009-01-10-b-1.py:28
x   /home/mkg/michael.gogins.studio/music/Tzimtzum/GeneralizedContextualGroup/mkg-2009-01-10        # -b.py:27
'''

class Composition(object):
    def generate(self):
        # mkg-2009-09-14-s        
        self.sequence = CsoundAC.Sequence()
        self.model.addChild(self.sequence)
        self.generate14()
        #~ self.generate13()
        #~ self.generate12()
        #~ self.generate11()
        #~ self.generate10()
        #~ self.generate9()
        #~ self.generate8()
        #~ self.generate7()
        #~ self.generate6()
        #~ self.generate5()
        #~ self.generate4()
        #~ self.generate3()
        #~ self.generate2()
        #~ self.generate1()
        print
        
    def generate14(self):
        # mkg-2009-01-10-b
        gcg = GeneralizedContextualGroup.GeneralizedContextualGroup()
        gcg.thisown = 0
        gcg.setAxiom('S(0,3,7,11,14) R36 WV A')
        gcg.addRule('A', 'V+8 K WV Q7 K D-1.5 WC V+9 WC V-7 WC V+11 A D-2 B D+2 D+1.5 L+ K WV A L- Q7 WV A')
        gcg.addRule('B', 'V+8 WV Q3 V+8 WV B')
        gcg.setIterationCount(4)
        gcg.debug = True
        gcg.generate()
        random = CsoundAC.Random()
        random.thisown = 0
        random.createDistribution("uniform_real")
        random.setElement(7, 11, 1)
        rescale = CsoundAC.Rescale()
        rescale.thisown = 0
        ### rescale.setRescale( CsoundAC.Event.TIME,       True,  False,  1,     120    )
        rescale.setRescale( CsoundAC.Event.TIME,       True,  False,  4,     120    )
        rescale.setRescale( CsoundAC.Event.DURATION,   False, False,  0.25,    1.0  )
        rescale.setRescale( CsoundAC.Event.INSTRUMENT, True,  False,  1,       0    )
        rescale.setRescale( CsoundAC.Event.KEY,        True,  False, 60,       36   )
        rescale.setRescale( CsoundAC.Event.VELOCITY,   True,  True,  15,       12   )
        rescale.setRescale( CsoundAC.Event.PAN,        True,  True,  -0.875,   1.75 )
        # random.addChild(lindenmayer)
        rescale.addChild(gcg)
        self.sequence.addChild(rescale)
        print
        
    def generate13(self):
        # mkg-2008-12-25-b
        gcg = GeneralizedContextualGroup.GeneralizedContextualGroup()
        gcg.thisown = 0
        gcg.setAxiom('S(0,3,7,10,14) R36 WC A')
        gcg.addRule('A', 'K WC Q7 K WC A')
        gcg.setIterationCount(9)
        gcg.debug = True
        gcg.generate()
        random = CsoundAC.Random()
        random.thisown = 0
        random.createDistribution("uniform_real")
        random.setElement(7, 11, 1)
        rescale = CsoundAC.Rescale()
        rescale.thisown = 0
        ### rescale.setRescale( CsoundAC.Event.TIME,       1, 0,  1,     120    )
        rescale.setRescale( CsoundAC.Event.TIME,       True,  False,  4,     120    )
        rescale.setRescale( CsoundAC.Event.DURATION,   False, False,  0.25,    1.0  )
        rescale.setRescale( CsoundAC.Event.INSTRUMENT, True,  False,  1,       0    )
        rescale.setRescale( CsoundAC.Event.KEY,        True,  False, 60,       36   )
        rescale.setRescale( CsoundAC.Event.VELOCITY,   True,  True,  30,       12   )
        rescale.setRescale( CsoundAC.Event.PAN,        True,  True,  -0.875,   1.75 )
        # random.addChild(lindenmayer)
        rescale.addChild(gcg)
        self.sequence.addChild(rescale)
        print

    def generate12(self):
        gcg = GeneralizedContextualGroup.GeneralizedContextualGroup()
        gcg.thisown = 0
        gcg.avoidParallelFifths=True
        gcg.setAxiom('pcs1 V+47 WC R48 a3 seq a4 seq a3 seq a4 seq a3 ')
        gcg.addRule('pcs1', 'P(0,4,7,11,14)')

        gcg.addRule('a3',   'a3k a3q a3 a3')
        gcg.addRule('a3k',  'K  WV')
        gcg.addRule('a3q',  'Q2 K D/1.25 WV Q7 D*1.25 WC')

        gcg.addRule('a4',   'L*2 a4k a4q D/1.25 a4 D/1.25 a4 D*1.25 D*1.25 L/2')
        gcg.addRule('a4k',  'K  WV')
        gcg.addRule('a4q',  'Q4 WV Q4 K V+4 WC')

        gcg.addRule('seq',  'L/5 D/2 Q-1 WV Q-1 WV Q-1 WV Q-1 L*5 D*2')

        gcg.setIterationCount(7)
        gcg.debug = True
        gcg.generate()
        rescale = CsoundAC.Rescale()
        rescale.thisown = 0
        ### rescale.setRescale( CsoundAC.Event.TIME,       True, False, (1.0 / 40.0), 120    )
        rescale.setRescale( CsoundAC.Event.TIME,       True, False,  4,           120    )
        rescale.setRescale( CsoundAC.Event.INSTRUMENT, True, True,   1,             0    )
        rescale.setRescale( CsoundAC.Event.KEY,        True, False, 42,            36    )
        rescale.setRescale( CsoundAC.Event.VELOCITY,   True, True,  43,            17    )
        rescale.setRescale( CsoundAC.Event.PAN,        True, True,   0.05,          0.9  )
        rescale.addChild(gcg)
        self.sequence.addChild(rescale)
        print
        
    def generate11(self):
        # mkg-2009-09-14-r
        gcg = GeneralizedContextualGroup.GeneralizedContextualGroup()
        gcg.thisown = 0
        
        gcg.avoidParallelFifths=True
        # Ends on Cm9.
        gcg.setAxiom('pcs1 V+47 WC R45 a3 seq R50 a4 seq R55 a3 seq R60 a4 seq R65 a3 ')
        gcg.addRule('pcs1', 'P(0,4,7,11,14)')

        gcg.addRule('a3',   'a3k a3q a3 a3')
        gcg.addRule('a3k',  'K  WV')
        gcg.addRule('a3q',  'Q2 K D/1.25 WV Q7 D*1.25 WC')

        gcg.addRule('a4',   'L*2 a4k a4q D/1.25 a4 D/1.25 a4 D*1.25 D*1.25 L/2')
        gcg.addRule('a4k',  'K  WV')
        gcg.addRule('a4q',  'Q4 WV Q4 K V+4 WC')

        gcg.addRule('seq',  'L/5 D/6 Q-1 WV Q-1 WV Q-1 WV Q-1 L*5 D*6')

        gcg.setIterationCount(2)
        gcg.debug = True
        gcg.generate()
        rescale = CsoundAC.Rescale()
        rescale.thisown = 0
        ### rescale.setRescale( CsoundAC.Event.TIME,       True, False, (1.0 / 40.0), 120    )
        rescale.setRescale( CsoundAC.Event.TIME,       True, False,  4,           120    )
        rescale.setRescale( CsoundAC.Event.INSTRUMENT, True, False,  1,             3.99 )
        rescale.setRescale( CsoundAC.Event.KEY,        True, False, 37,            36    )
        rescale.setRescale( CsoundAC.Event.VELOCITY,   True, True,  43,            17    )
        rescale.setRescale( CsoundAC.Event.PAN,        True, True,   0.05,          0.9  )
        rescale.addChild(gcg)
        self.sequence.addChild(rescale)
        print
        
    def generate10(self):
        # mkg-2009-09-14-o
        gcg = GeneralizedContextualGroup.GeneralizedContextualGroup()
        gcg.thisown = 0
        # Ends on Cm9.
        gcg.setAxiom('pcs1 V+47 WC R45 a3 Q5 R50 a4 R55 a3 R60 a4 R65 a3 ')
        gcg.addRule('pcs1', 'P(0,4,7,11,14)')

        gcg.addRule('a3',   'a3k a3q a3 a3')
        gcg.addRule('a3k',  'K  WV')
        gcg.addRule('a3q',  'Q3 K D/1.25 WV Q3 V+1 D*1.25 WC')

        gcg.addRule('a4',   'L*2 a4k a4q D/1.25 a4 D/1.25  a4 D*1.25 D*1.25 L/2')
        gcg.addRule('a4k',  'K  WV')
        gcg.addRule('a4q',  'Q4 WV Q4 K V+4 WC')

        ### gcg.setIterationCount(2)
        gcg.setIterationCount(4)
        gcg.debug = True
        gcg.generate()
        rescale = CsoundAC.Rescale()
        rescale.thisown = 0
        ### rescale.setRescale( CsoundAC.Event.TIME,       True, False, (1.0 / 40.0), 120    )
        rescale.setRescale( CsoundAC.Event.TIME,       True, False,  1,           120    )
        rescale.setRescale( CsoundAC.Event.INSTRUMENT, True, False,  1,             3.99 )
        rescale.setRescale( CsoundAC.Event.KEY,        True, False, 37,            36    )
        rescale.setRescale( CsoundAC.Event.VELOCITY,   True, True,  43,            17    )
        rescale.setRescale( CsoundAC.Event.PAN,        True, True,   0.05,          0.9  )
        rescale.addChild(gcg)
        self.sequence.addChild(rescale)
        print
            
    def generate9(self):
        # mkg-2009-01-10-c
        gcg = GeneralizedContextualGroup.GeneralizedContextualGroup()
        gcg.thisown = 0
        gcg.setAxiom('S(0,3,7,11,14) R48 WV B')
        gcg.addRule('A', 'V+8 K WV Q7 K D-1.5 WC V+9 WC V-7 WC V+11 A D-3 B D+3 D+1.5 L+ K WV A L- Q7 WV A')
        gcg.addRule('B', 'V+8 WC V-7 WC V+8 WC V-7 WC V+8 WC V-7 WC B B B K WV V+8 WC V-7 WC B B B Q7 WV V+8 WC V-7 WC B B B')
        gcg.setIterationCount(3)
        gcg.debug = True
        gcg.generate()
        random = CsoundAC.Random()
        random.thisown = 0
        random.createDistribution("uniform_real")
        random.setElement(7, 11, 1)
        rescale = CsoundAC.Rescale()
        rescale.thisown = 0
        # rescale.setRescale( CsoundAC.Event.TIME,       True, False,  1,     120    )
        rescale.setRescale( CsoundAC.Event.TIME,       True,  False,  4,     120    )
        rescale.setRescale( CsoundAC.Event.DURATION,   False, False,  0.25,    1.0  )
        rescale.setRescale( CsoundAC.Event.INSTRUMENT, True,  False,  1,       0    )
        rescale.setRescale( CsoundAC.Event.KEY,        True,  False, 48,       36   )
        rescale.setRescale( CsoundAC.Event.VELOCITY,   True,  True,  15,       12   )
        rescale.setRescale( CsoundAC.Event.PAN,        True,  True,  -0.875,   1.75 )
        # random.addChild(lindenmayer)
        rescale.addChild(gcg)
        self.sequence.addChild(rescale)
        print

    def generate8(self):
        # mkg-2009-12-30-10
        gcg = GeneralizedContextualGroup.GeneralizedContextualGroup()
        gcg.avoidParallelFifths = True

        gcg.setAxiom('pcs1 R66 WC V+11 WC V-2 WC a3 dd a4 dd L*2 a3 L/2 arp1 dd K D*1.875 L/4 a4 arp2 dd dd')
        gcg.addRule( 'pcs1', 'P(0,4,7,14)')

        gcg.addRule('a3',   'a3k a3q a3 a3 ') 
        gcg.addRule('a3k',  'K  arp WV WC ')
        gcg.addRule('a3q',  'Q7     WV K D/1.245 WV arp1 V+2 D*1.25 V+5 WC WC ')

        gcg.addRule('a4',   'L*2 a4k a4q D/1.28 a4 arp a3 D/1.25 a3k a4 D*1.25 D*1.25 L/2 WC ')
        gcg.addRule('a4k',  'K  WV ')
        gcg.addRule('a4q',  'Q3 WV K V+1 WC')
        
        gcg.addRule('arp',  'V+18 WC V-21 WC ') 
        gcg.addRule('arp1', 'L/2 D/2.125 Q5 WV Q5 WV Q5 WV Q5 WV D*2.125 L*2') 
        gcg.addRule('arp2', 'L/2 D/2.125 Q5 WV Q5 WV Q5 WV Q5 WV Q5 WV Q5 WV D*2.125 L*2') 
        gcg.addRule('dd',   'WV WV ') 

        gcg.setIterationCount(3)
        gcg.debug = True
        gcg.generate()
        rescale = CsoundAC.Rescale()
        ### rescale.setRescale( CsoundAC.Event.TIME,       True, False, (1.0 / 40.0), 120     )
        rescale.setRescale( CsoundAC.Event.TIME,       True, False, 4,            120     )
        rescale.setRescale( CsoundAC.Event.INSTRUMENT, True, True,  1,              0     )
        rescale.setRescale( CsoundAC.Event.KEY,        True, False, 37,            36     )
        rescale.setRescale( CsoundAC.Event.VELOCITY,   True, True,  25,            40     )
        rescale.setRescale( CsoundAC.Event.PAN,        True, True,  -0.9,           1.8   )
        rescale.addChild(gcg)
        self.sequence.addChild(rescale)
        print

    def generate7(self):
        # mkg-2009-12-30-1
        gcg = GeneralizedContextualGroup.GeneralizedContextualGroup()
        gcg.thisown = 0

        gcg.setAxiom('pcs1 R72 WC V+29 WC V-2 WC a3 V+5 WC V+5 WC Q5 a4 R55 a3 a4 a3 arp WV WV WV WV')
        gcg.addRule('pcs1', 'P(0,3,7,10)')

        gcg.addRule('a3',   'a3k a3q a3 a3 WC ')
        gcg.addRule('a3k',  'K  arp WV')
        gcg.addRule('a3q',  'Q2 WV K D/1.245 WV arp V+2 D*1.25 V+5 WC ')

        gcg.addRule('a4',   'L*2 a4k a4q D/1.27 a4 arp a3 D/1.25  a3k a4 D*1.25 arp D*1.25 L/2 WC ')
        gcg.addRule('a4k',  'K  WV')
        gcg.addRule('a4q',  'Q5 WV K V+4 WC')
        
        gcg.addRule('arp',  'V+12 WC V+2 VC arp V-13 VC ') 

        gcg.setIterationCount(3)
        gcg.debug = True
        gcg.generate()
        rescale = CsoundAC.Rescale()
        rescale.thisown = 0
        rescale.setRescale( CsoundAC.Event.TIME,       True, False, (1.0 / 40.0), 120    )
        rescale.setRescale( CsoundAC.Event.INSTRUMENT, True, False,  1,             3.99 )
        rescale.setRescale( CsoundAC.Event.KEY,        True, False, 37,            36    )
        rescale.setRescale( CsoundAC.Event.VELOCITY,   True, True,  43,            17    )
        rescale.setRescale( CsoundAC.Event.PAN,        True, True,   0.05,          0.9  )
        rescale.addChild(gcg)
        self.model.addChild(rescale)
        print

    def generate6(self):
        # mkg-2009-02-03-d
        gcg = GeneralizedContextualGroup.GeneralizedContextualGroup()
        gcg.thisown = 0
        gcg.setAxiom('R48 D/4 pcs1 ar1 a1 c dD+ pcs2 ar2 a2 c dD- pcs3 ar3 a3 c pcs4 ar4 a4 c a4 pcs3 c dD- ar3 a3 c pcs2 ar2 a2 c pcs1 dD+ ar1 a1 c c')
        #gcg.addRule('dD+',  'D*1.01 dD+')
        #gcg.addRule('dD-',  'D/1.01 dD-')
        gcg.addRule('dL+',  'L*1.02 dL+')
        gcg.addRule('dL-',  'L/1.02 dL-')
        gcg.addRule('ar1',  'I0=0 I1=1  I2=10 I3=11 I4=4 I5=5')
        gcg.addRule('ar2',  'I0=0 I1=1  I2=2  I3=3  I4=4 I5=5')
        gcg.addRule('ar3',  'I0=2 I1=3  I2=12 I3=13 I4=4 I5=5')
        gcg.addRule('ar4',  'I0=2 I1=12 I2=4  I3=15 I4=3 I5=3')
        gcg.addRule('c',    'd d')
        # Quartal
        gcg.addRule('pcs1', 'P(0,5,10,15)')
        # Major
        gcg.addRule('pcs2', 'P(0,4,7,11)')
        # Minor
        gcg.addRule('pcs3', 'P(0,3,7,10)')
        # Major
        gcg.addRule('pcs4', 'P(0,4,7,11)')
        gcg.addRule('d',    '      WV      WV      WV      WV      WV      WV      WV      WV ')
        gcg.addRule('a1',   'ck1 dD- cq1 a1 a1')
        gcg.addRule('ck1',  'K     WV      WV      WV      WV      WV      WV      WV      WV ')
        gcg.addRule('cq1',  'Q7 K  WV      WV      WV      WV      WV      WV      WV      WV ')
        gcg.addRule('a2',   'dL+ ck2 dL- cq2 a2 a2')
        gcg.addRule('ck2',  'K     WV      WV      WV dD+  WV V+8  WC      WC      WC      WC ')
        gcg.addRule('cq2',  'Q1 K  WV      WV      WV dD-  WV V+8  WV      WV      WV      WV ')
        gcg.addRule('a3',   'ck3 dL+ cq3 dL- a3 a3 ')
        gcg.addRule('ck3',  'K     WV      WV V+5  WC      WC V+1  WC dL-  WC V+5  WC      WC ')
        gcg.addRule('cq3',  'Q5 K  WV      WV V+5  WC      WC Q7   WV dL+  WV V+5  WC      WC ')
        gcg.addRule('a4',   'ck4 dD- cq4 a4 dD+ a4')
        gcg.addRule('ck4',  'K     WV V+1  WC V+1  WC V+1  WC V+1  WC V+1  WC V+1  WC V+1  WC ')
        gcg.addRule('cq4',  'Q3 K  WV V+1  WV V+1  WC      WC V+1  WC V+1  WC V+1  WC V+1  WC ')
        gcg.setIterationCount(5)
        gcg.debug = True
        gcg.generate()
        random = CsoundAC.Random()
        random.thisown = 0
        random.createDistribution("uniform_real")
        random.setElement(7, 11, 1)
        rescale = CsoundAC.Rescale()
        rescale.thisown = 0
        ### rescale.setRescale( CsoundAC.Event.TIME,       1, 0,  1,     120    )
        rescale.setRescale( CsoundAC.Event.TIME,       True,  False,  4,     120    )
        rescale.setRescale( CsoundAC.Event.DURATION,   False, False,  0.25,    1.0  )
        rescale.setRescale( CsoundAC.Event.INSTRUMENT, True,  False,  0,       0    )
        rescale.setRescale( CsoundAC.Event.KEY,        True,  False, 39,       36   )
        rescale.setRescale( CsoundAC.Event.VELOCITY,   True,  True,  43,       14   )
        rescale.setRescale( CsoundAC.Event.PAN,        True,  True,  -0.95,  1.90   )
        # random.addChild(lindenmayer)
        rescale.addChild(gcg)
        self.sequence.addChild(rescale)
        print

    def generate5(self):
        # mkg-2009-02-03-c
        gcg = GeneralizedContextualGroup.GeneralizedContextualGroup()
        gcg.thisown = 0
        gcg.setAxiom('R48 D/4 pcs1 ar1 a1 c dD+ pcs2 ar2 a2 c dD- pcs3 ar3 a3 c pcs4 ar4 a4 c a4 pcs3 c dD- ar3 a3 c pcs2 ar2 a2 c pcs1 dD+ ar1 a1 c c')
        #gcg.addRule('dD+',  'D*1.01 dD+')
        #gcg.addRule('dD-',  'D/1.01 dD-')
        gcg.addRule('dL+',  'L*1.02 dL+')
        gcg.addRule('dL-',  'L/1.02 dL-')
        gcg.addRule('ar1',  'I0=0 I1=1  I2=10 I3=11 I4=4 I5=5')
        gcg.addRule('ar2',  'I0=0 I1=1  I2=2  I3=3  I4=4 I5=5')
        gcg.addRule('ar3',  'I0=2 I1=3  I2=12 I3=13 I4=4 I5=5')
        gcg.addRule('ar4',  'I0=2 I1=12 I2=4  I3=15 I4=3 I5=3')
        gcg.addRule('c',    'd d')
        # Quartal
        gcg.addRule('pcs1', 'P(0,5,10,15)')
        # Major
        gcg.addRule('pcs2', 'P(0,4,7,11)')
        # Minor
        gcg.addRule('pcs3', 'P(0,3,7,10)')
        # Major
        gcg.addRule('pcs4', 'P(0,4,7,11)')
        gcg.addRule('d',    '      WV      WV      WV      WV      WV      WV      WV      WV ')
        gcg.addRule('a1',   'ck1 dD- cq1 a1 a1')
        gcg.addRule('ck1',  'K     WV      WV      WV      WV      WV      WV      WV      WV ')
        gcg.addRule('cq1',  'Q7 K  WV      WV      WV      WV      WV      WV      WV      WV ')
        gcg.addRule('a2',   'dL+ ck2 dL- cq2 a2 a2')
        gcg.addRule('ck2',  'K     WV      WV      WV dD+ WV V+8  WC      WC      WC      WC ')
        gcg.addRule('cq2',  'Q1 K  WV      WV      WV dD- WV V+8  WV      WV      WV      WV ')
        gcg.addRule('a3',   'ck3 dL+ cq3 dL- a3 a3 ')
        gcg.addRule('ck3',  'K     WV      WV V+5  WC      WC V+4  WC dL- WC V+5  WC      WC ')
        gcg.addRule('cq3',  'Q5 K  WV      WV V+5  WC      WC Q7   WV dL+ WV V+5  WC      WC ')
        gcg.addRule('a4',   'ck4 dD- cq4 a4 dD+ a4')
        gcg.addRule('ck4',  'K     WV V+1  WC V+1  WC V+1  WC V+1  WC V+1  WC V+1  WC V+1  WC ')
        gcg.addRule('cq4',  'Q3 K  WV V+1  WV V+1  WC V+1  WC V+1  WC V+1  WC V+1  WC V+1  WC ')
        gcg.setIterationCount(5)
        gcg.debug = True
        gcg.generate()
        random = CsoundAC.Random()
        random.thisown = 0
        random.createDistribution("uniform_real")
        random.setElement(7, 11, 1)
        rescale = CsoundAC.Rescale()
        rescale.thisown = 0
        ### rescale.setRescale( CsoundAC.Event.TIME,       True,  False,  1,     120    )
        rescale.setRescale( CsoundAC.Event.TIME,       True,  False,  4,     120    )
        rescale.setRescale( CsoundAC.Event.DURATION,   False, False,  0.25,    1.0  )
        rescale.setRescale( CsoundAC.Event.INSTRUMENT, True,  False,  0,       0    )
        rescale.setRescale( CsoundAC.Event.KEY,        True,  False, 39,       36   )
        rescale.setRescale( CsoundAC.Event.VELOCITY,   True,  True,  46,       14   )
        rescale.setRescale( CsoundAC.Event.PAN,        True,  True,  -0.95,  1.90   )
        # random.addChild(lindenmayer)
        rescale.addChild(gcg)
        self.sequence.addChild(rescale)

    def generate4(self):
        # mkg-2009-02-03-b
        gcg = GeneralizedContextualGroup.GeneralizedContextualGroup()
        gcg.thisown = 0
        gcg.setAxiom('R48 D/4 pcs1 ar1 a1 c dD+ pcs2 ar2 a2 c dD- pcs3 ar3 a3 c pcs4 ar4 a4 c a4 pcs3 c dD- ar3 a3 c pcs2 ar2 a2 c pcs1 dD+ ar1 a1 c c')
        #gcg.addRule('dD+',  'D*1.01 dD+')
        #gcg.addRule('dD-',  'D/1.01 dD-')
        gcg.addRule('dL+',  'L*1.02 dL+')
        gcg.addRule('dL-',  'L/1.02 dL-')
        gcg.addRule('ar1',  'I0=0 I1=1  I2=10 I3=11 I4=4 I5=5')
        gcg.addRule('ar2',  'I0=0 I1=1  I2=2  I3=3  I4=4 I5=5')
        gcg.addRule('ar3',  'I0=2 I1=3  I2=12 I3=13 I4=4 I5=5')
        gcg.addRule('ar4',  'I0=2 I1=12 I2=4  I3=14 I4=3 I5=3')
        gcg.addRule('c',    'd d')
        # Quartal
        gcg.addRule('pcs1', 'P(0,5,10,15)')
        # Major
        gcg.addRule('pcs2', 'P(0,4,7,11)')
        # Minor
        gcg.addRule('pcs3', 'P(0,3,7,10)')
        # Major
        gcg.addRule('pcs4', 'P(0,4,7,11)')
        gcg.addRule('d',    '      WV      WV      WV      WV      WV      WV      WV      WV ')
        gcg.addRule('a1',   'ck1 dD- cq1 a1 a1')
        gcg.addRule('ck1',  'K     WV      WV      WV      WV      WV      WV      WV      WV ')
        gcg.addRule('cq1',  'Q7 K  WV      WV      WV      WV      WV      WV      WV      WV ')
        gcg.addRule('a2',   'dL+ ck2 dL- cq2 a2 a2')
        gcg.addRule('ck2',  'K     WV      WV      WV dD+ WV V+8  WC      WC      WC      WC ')
        gcg.addRule('cq2',  'Q1 K  WV      WV      WV dD- WV V+8  WV      WV      WV      WV ')
        gcg.addRule('a3',   'ck3 dL+ cq3 dL- a3 a3 ')
        gcg.addRule('ck3',  'K     WV      WV V+5  WC      WC V+4  WC dL- WC V+5  WC      WC ')
        gcg.addRule('cq3',  'Q5 K  WV      WV V+5  WC      WC Q7   WV dL+ WV V+5  WC      WC ')
        gcg.addRule('a4',   'ck4 dD- cq4 a4 dD+ a4')
        gcg.addRule('ck4',  'K     WV V+1  WC V+1  WC V+1  WC V+1  WC V+1  WC V+1  WC V+2  WC ')
        gcg.addRule('cq4',  'Q3 K  WV V+1  WV V+1  WC V+1  WC V+1  WC V+1  WC V+1  WC V+1  WC ')
        gcg.setIterationCount(5)
        gcg.debug = True
        gcg.generate()
        random = CsoundAC.Random()
        random.thisown = 0
        random.createDistribution("uniform_real")
        random.setElement(7, 11, 1)
        rescale = CsoundAC.Rescale()
        rescale.thisown = 0
        rescale.setRescale( CsoundAC.Event.TIME,       True,  False,  4,     120    )
        rescale.setRescale( CsoundAC.Event.DURATION,   False, False,  0.25,    1.0  )
        rescale.setRescale( CsoundAC.Event.INSTRUMENT, True,  False,  0,       0    )
        rescale.setRescale( CsoundAC.Event.KEY,        True,  False, 39,       36   )
        rescale.setRescale( CsoundAC.Event.VELOCITY,   True,  True,  46,       14   )
        rescale.setRescale( CsoundAC.Event.PAN,        True,  True,  -0.95,  1.90   )
        # random.addChild(lindenmayer)
        self.sequence.addChild(rescale)
        print        
        
    def generate1(self):
        print( 'CREATING MUSIC MODEL...')
        ##sys.path.append('d:/utah/home/mkg/projects/icmc2009-mkg')
        # Original.
        gcg = GeneralizedContextualGroup.GeneralizedContextualGroup()
        gcg.thisown = 0
        gcg.setAxiom('S(0,3,7,11,14) R60 WV A')
        gcg.addRule('A', 'V+8 K WV Q7 K D-1.5 WC V+9 WC V-7 WC V+11 A D-2 B D+2 D+1.5 L+ K WV A L- Q7 WV A')
        gcg.addRule('B', 'V+8 WV Q3 V+8 WV B')
        gcg.setIterationCount(4)
        gcg.debug = True 
        gcg.generate()
        random = CsoundAC.Random()
        random.thisown = 0
        random.createDistribution("uniform_real")
        random.setElement(7, 11, 1)
        rescale = CsoundAC.Rescale()
        rescale.thisown = 0
        rescale.setRescale( CsoundAC.Event.TIME,       True,  False,  4,     120    )
        rescale.setRescale( CsoundAC.Event.DURATION,   False, False,  0.25,    1.0  )
        rescale.setRescale( CsoundAC.Event.INSTRUMENT, True,  False,  1,       1.99 )
        rescale.setRescale( CsoundAC.Event.KEY,        True,  False, 36,       60   )
        rescale.setRescale( CsoundAC.Event.VELOCITY,   True,  True,  15,       12   )
        rescale.setRescale( CsoundAC.Event.PAN,        True,  True,  -0.875,   1.75 )
        # random.addChild(lindenmayer)
        rescale.addChild(gcg)
        self.sequence.addChild(rescale)
        print
        
    def generate2(self):
        # mkg-2010-03-16-e-1
        gcg = GeneralizedContextualGroup.GeneralizedContextualGroup()
        gcg.avoidParallelFifths = True
        gcg.thisown = 0

        gcg.setAxiom('pcs1 R65 WC V+1 WC V-20 WC a3 a3 dd a4 a4 dd L*2 a3 a3 L/2 arp1 dd K L/4 a4 a3 arp2 dd dd')
        gcg.addRule( 'pcs1', 'P(0,4,7,14)')

        gcg.addRule('a3',   'a3k a3q a3 a3 ')
        gcg.addRule('a3k',  'K  arp WV WC ')
        gcg.addRule('a3q',  'Q7 WV K D/1.125 WV arp1 Q3 arp1 arp1 V+2 D*1.12 V+5 WC WC ')

        gcg.addRule('a4',   'L*2 a4k a4q a4 arp a3 D/1.012 a3k a4 D*1.0125 L/2 WC ')
        gcg.addRule('a4k',  'K  WV ')
        gcg.addRule('a4q',  'Q3 WV K V+1 WC')
        
        gcg.addRule('arp',  'V-2 WC V-1 WC ') 
        gcg.addRule('arp1', 'L/2 D/1.0125 Q5 WV Q5 WV Q5 WV Q5 WV D*1.0125 L*2') 
        gcg.addRule('arp2', 'L/2 D/1.125 Q5 WV Q5 WV Q5 WV Q5 WV Q5 WV Q5 WV D*1.125 L*2') 
        gcg.addRule('dd',   'WV WV V+3 WV WV V+3') 

        gcg.setIterationCount(4)
        gcg.debug = True
        gcg.generate()
        rescale = CsoundAC.Rescale()
        rescale.thisown = 0
        ### rescale.setRescale( CsoundAC.Event.TIME,       True, False, (1.0 / 40.0), 120     )
        rescale.setRescale( CsoundAC.Event.TIME,       True, False, 4,            120     )
        rescale.setRescale( CsoundAC.Event.INSTRUMENT, True, True,  1,              0     )
        rescale.setRescale( CsoundAC.Event.KEY,        True, False, 41,            36     )
        rescale.setRescale( CsoundAC.Event.VELOCITY,   True, True,  25,            10     )
        rescale.setRescale( CsoundAC.Event.PAN,        True, True,  -0.9,           1.8   )
        rescale.addChild(gcg)
        self.sequence.addChild(rescale)
        print
        
    def generate3(self):
        # mkg-2009-09-14-o-1
        gcg = GeneralizedContextualGroup.GeneralizedContextualGroup()
        gcg.thisown = 0
        # Ends on Cm9.
        gcg.setAxiom('pcs1 V+47 WC R45 a3 Q5 R50 a4 R55 a3 R60 a4 R65 a3 ')
        gcg.addRule('pcs1', 'P(0,4,7,11,14)')

        gcg.addRule('a3',   'a3k a3q a3 a3')
        gcg.addRule('a3k',  'K  WV')
        gcg.addRule('a3q',  'Q3 K D/1.25 WV Q3 V+1 D*1.25 WC')

        gcg.addRule('a4',   'L*2 a4k a4q D/1.25 a4 D/1.25  a4 D*1.25 D*1.25 L/2')
        gcg.addRule('a4k',  'K  WV')
        gcg.addRule('a4q',  'Q4 WV Q4 K V+4 WC')

        ### gcg.setIterationCount(2)
        gcg.setIterationCount(6)
        gcg.debug = True
        gcg.generate()
        rescale = CsoundAC.Rescale()
        rescale.thisown = 0
        ### rescale.setRescale( CsoundAC.Event.TIME,       True, False, (1.0 / 40.0), 120    )
        rescale.setRescale( CsoundAC.Event.TIME,       True, False,  4,           120    )
        rescale.setRescale( CsoundAC.Event.INSTRUMENT, True, True,   1,             3.99 )
        rescale.setRescale( CsoundAC.Event.KEY,        True, False, 37,            36    )
        rescale.setRescale( CsoundAC.Event.VELOCITY,   True, True,  43,            17    )
        rescale.setRescale( CsoundAC.Event.PAN,        True, True,   0.05,          0.9  )
        rescale.addChild(gcg)
        self.sequence.addChild(rescale)
        print

    def arrange(self):
        print('CREATING CSOUND ARRANGEMENT...')
        print
        
    def orchestra(self):
        print('CREATING CSOUND ORCHESTRA...')
        print
        ##print
        self.csoundOrchestra = '''
sr = 96000
ksmps = 128
nchnls = 2
0dbfs = 1

alwayson "aeolus_output"

gi_aeolus aeolus_init "stops-0.3.0", "Aeolus", "waves", 0, 15, 0

instr aeolus_note, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16
prints "Aeolus note: i: %9.4f time: %9.4f duration: %9.4f key: %9.4f velocity: %9.4f\\n", p1, p2, p3, p4, p5
;;; aeolus_note gi_aeolus, p1, p4, p5
aeolus_note gi_aeolus, 1, p4, p5
endin

instr aeolus_preset
prints "Aeolus preset: i: %9.4f time: %9.4f duration: %9.4f bank:%9.4f preset:%9.4f\\n", p1, p2, p3, p4, p5
aeolus_preset gi_aeolus, p4, p5
endin

instr aeolus_stop
prints "aeolus_stop: i: %9.4f time: %9.4f duration: %9.4f stop: %9.4f\\n", p1, p2, p3, p4
i_stop init p4
aeolus_stop gi_aeolus, i_stop
endin

instr aeolus_output
aeolus_preset gi_aeolus, 0, 0
// All our stops are in the Pedal division, MIDI channel 1.
aeolus_group_mode gi_aeolus, 0, 3
a_out[] init 2
a_out aeolus_out gi_aeolus
// Use reverbsc instead of the built-in Aeolus reverb.
i_reverb_feedback init .80
i_reverb_highpass init 9000
i_wet init .33334
i_dry init 1 - i_wet
a_left, a_right reverbsc a_out[0], a_out[1], i_reverb_feedback, i_reverb_highpass
outs a_out[0] * i_dry + a_left * i_wet, a_out[1] * i_dry + a_right * i_wet
endin
        '''
        self.model.setCsoundOrchestra(self.csoundOrchestra)
        print
        
    def render(self):
        print('RENDERING...')
        print
        self.model.setCsoundCommand(self.csoundCommand)
        self.model.generate()
        ### self.score.setDuration(9. * 60.)
        print(self.model.getScore().getCsoundScore())
        self.model.perform()
        self.score.save(self.midiFilename)
        print('FINISHED...')
        print
        self.ended = time.clock()
        self.elapsed = self.ended - self.began
        print('At:                                  %s' % time.strftime('%Y-%b-%d %A %H:%M:%S'))
        print('Elapsed time:                        %-9.2f seconds.' % self.elapsed)
        print
        if self.playback and (self.rendering != 'audio'):
            print('OPENING SOUNDFILE FOR PLAYBACK...')
            print
            print('Soundfile:                           %s' % self.soundfileName)
            os.spawnl(os.P_NOWAIT, self.soundfilePlayer, self.soundfilePlayer, self.soundfileName)
            print
            
    def run(self):
        self.orchestra()
        self.generate()
        self.arrange()
        self.render()
        
    def __init__(self, author='Composer', rendering='audio', instrumentLibrary=r'/home/mkg/csound-extended/test-examples/csound-vst/CsoundAC.csd', soundfilePlayer=r'D:\utah\opt\audacity\audacity.exe'):
        print('SETTING RENDERING AND PLAYBACK OPTIONS...')
        print
        print('Set "rendering" to:      "cd", "preview" (default), "master", or "audio".')
        print('Set "playback" to:       True (default) or False.')
        print
        self.rendering = 'preview'
        self.playback = True
        print('Rendering option:        %s' % self.rendering)
        print('Play after rendering:    %s' % self.playback)
        print
        print('CREATING FILENAMES...')
        print
        self.began = time.clock()
        self.author = author
        print('Author:                  %s' % self.author)
        self.scriptFilename = os.path.realpath(sys.argv[0])
        print('Full Python script:      %s' % self.scriptFilename)
        self.title, ext = os.path.splitext(os.path.basename(self.scriptFilename))
        print('Base Python script:      %s' % self.title)
        self.directory = os.path.dirname(self.scriptFilename)
        if len(self.directory):
            os.chdir(self.directory)
        print('Working directory:       %s' % self.directory)
        self.orcFilename = self.title + '.orc'
        self.instrumentLibrary = instrumentLibrary
        print('Instrument library:      %s' % self.instrumentLibrary)
        print('Csound orchestra:        %s' % self.orcFilename)
        self.scoFilename = self.title + '.sco'
        print('Csound score:            %s' % self.scoFilename)
        self.midiFilename = self.title + '.mid'
        print('MIDI filename:           %s' % self.midiFilename)
        self.soundfileName = self.title + '.wav'
        print('Soundfile name:          %s' % self.soundfileName)
        self.dacName = 'dac'
        print('Audio output name:       %s' % self.dacName)
        self.soundfilePlayer = soundfilePlayer
        print('Soundfile player:        %s' % self.soundfilePlayer)
        commandsForRendering = {
            'audio':    'csound --messagelevel=1  --noheader                  --nodisplays --sample-rate=44100 --control-rate=100   --midi-key=4 --midi-velocity=5                                                                  --output=%s %s %s' % (self.dacName,                                             self.orcFilename, self.scoFilename),
            'preview':  'csound --messagelevel=99 -W -f --rewrite  -K --nodisplays --sample-rate=44100 --control-rate=441   --midi-key=4 --midi-velocity=5 -+id_artist=%s -+id_copyright=Copyright_2007_by_%s -+id_title=%s --output=%s %s %s' % (self.author, self.author, self.title, self.soundfileName, self.orcFilename, self.scoFilename),
            'cd':       'csound --messagelevel=99 -W -f --rewrite  -K --nodisplays --sample-rate=44100 --control-rate=44100 --midi-key=4 --midi-velocity=5 -+id_artist=%s -+id_copyright=Copyright_2007_by_%s -+id_title=%s --output=%s %s %s' % (self.author, self.author, self.title, self.soundfileName, self.orcFilename, self.scoFilename),
            'master':   'csound --messagelevel=99 -W -f --rewrite  -K --nodisplays --sample-rate=88200 --control-rate=88200 --midi-key=4 --midi-velocity=5 -+id_artist=%s -+id_copyright=Copyright_2007_by_%s -+id_title=%s --output=%s %s %s' % (self.author, self.author, self.title, self.soundfileName, self.orcFilename, self.scoFilename)
        }    
        self.csoundCommand = commandsForRendering[self.rendering]
        print('Csound command line:     %s' % self.csoundCommand)
        print
        print('CREATING GLOBAL OBJECTS...')
        print
        self.model = CsoundAC.MusicModel()
        ## self.csound = self.model.getCppSound()
        ## self.csound.setPythonMessageCallback()
        self.score = self.model.getScore()
           
composition = Composition(author='Michael_Gogins')
composition.run()
