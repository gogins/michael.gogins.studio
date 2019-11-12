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

class Composition(object):
    def generate(self):
        print( 'CREATING MUSIC MODEL...')
        ##sys.path.append('d:/utah/home/mkg/projects/icmc2009-mkg')
        sys.path.append('/home/mkg/Dropbox/studio')
        import GeneralizedContextualGroup
        '''
        # Original.
        gcg = GeneralizedContextualGroup.GeneralizedContextualGroup()
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
        rescale.setRescale( CsoundAC.Event.TIME,       True,  False,  1,     120    )
        rescale.setRescale( CsoundAC.Event.DURATION,   False, False,  0.25,    1.0  )
        rescale.setRescale( CsoundAC.Event.INSTRUMENT, True,  False,  1,       1.99 )
        rescale.setRescale( CsoundAC.Event.KEY,        True,  False, 36,       60   )
        rescale.setRescale( CsoundAC.Event.VELOCITY,   True,  True,  15,       12   )
        rescale.setRescale( CsoundAC.Event.PAN,        True,  True,  -0.875,   1.75 )
        # random.addChild(lindenmayer)
        rescale.addChild(gcg)
        self.model.addChild(rescale)
        print
        
        '''
        # mkg-2010-03-16-e-1
        self.gcg = GeneralizedContextualGroup.GeneralizedContextualGroup()
        self.gcg.avoidParallelFifths = True

        self.gcg.setAxiom('pcs1 R65 WC V+1 WC V-20 WC a3 a3 dd a4 a4 dd L*2 a3 a3 L/2 arp1 dd K L/4 a4 a3 arp2 dd dd')
        self.gcg.addRule( 'pcs1', 'P(0,4,7,14)')

        self.gcg.addRule('a3',   'a3k a3q a3 a3 ')
        self.gcg.addRule('a3k',  'K  arp WV WC ')
        self.gcg.addRule('a3q',  'Q7 WV K D/1.125 WV arp1 Q3 arp1 arp1 V+2 D*1.12 V+5 WC WC ')

        self.gcg.addRule('a4',   'L*2 a4k a4q a4 arp a3 D/1.012 a3k a4 D*1.0125 L/2 WC ')
        self.gcg.addRule('a4k',  'K  WV ')
        self.gcg.addRule('a4q',  'Q3 WV K V+1 WC')
        
        self.gcg.addRule('arp',  'V-2 WC V-1 WC ') 
        self.gcg.addRule('arp1', 'L/2 D/1.0125 Q5 WV Q5 WV Q5 WV Q5 WV D*1.0125 L*2') 
        self.gcg.addRule('arp2', 'L/2 D/1.125 Q5 WV Q5 WV Q5 WV Q5 WV Q5 WV Q5 WV D*1.125 L*2') 
        self.gcg.addRule('dd',   'WV WV V+3 WV WV V+3') 

        self.gcg.setIterationCount(4)
        self.gcg.debug = True
        self.gcg.generate()
        self.rescale = CsoundAC.Rescale()
        self.rescale.setRescale( CsoundAC.Event.TIME,       True, False, (1.0 / 40.0), 120     )
        self.rescale.setRescale( CsoundAC.Event.INSTRUMENT, True, True,  1,              0     )
        self.rescale.setRescale( CsoundAC.Event.KEY,        True, False, 41,            36     )
        self.rescale.setRescale( CsoundAC.Event.VELOCITY,   True, True,  25,            10     )
        self.rescale.setRescale( CsoundAC.Event.PAN,        True, True,  -0.9,           1.8   )
        self.rescale.addChild(self.gcg)
        self.model.addChild(self.rescale)
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

instr aeolus_note
prints "Aeolus note: i: %9.4f time: %9.4f duration: %9.4f key: %9.4f velocity: %9.4f\\n", p1, p2, p3, p4, p5
aeolus_note gi_aeolus, p1, p4, p5
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
        self.score.setDuration(360)
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
