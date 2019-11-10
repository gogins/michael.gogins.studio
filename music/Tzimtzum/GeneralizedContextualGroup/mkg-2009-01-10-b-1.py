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
        rescale.setRescale( CsoundAC.Event.INSTRUMENT, True,  False,  1,       0    )
        rescale.setRescale( CsoundAC.Event.KEY,        True,  False, 36,      60   )
        rescale.setRescale( CsoundAC.Event.VELOCITY,   True,  True, 15,       12   )
        rescale.setRescale( CsoundAC.Event.PAN,        True,  True, -0.875,   1.75 )
        # random.addChild(lindenmayer)
        rescale.addChild(gcg)
        self.model.addChild(rescale)
        print
        
    def arrange(self):
        print('CREATING CSOUND ARRANGEMENT...')
        print
        #                    CsoundAC,   Csound,                                                           level (+-dB),    pan (-1.0 through +1.0)
        self.model.arrange(  0,          'Pianoteq',                                              0.0,            +0.5 )
        self.model.arrange(  1,          'Pianoteq',                                              0.0,             0.2 )
        self.model.arrange(  2,          'Pianoteq',                                              0.0,            -0.2 )
        self.model.arrange(  3,          'Pianoteq',                                              0.0,            +0.7 )
        self.model.arrange(  4,          'Pianoteq',                                              0.0,            -0.7 )
        self.model.arrange(  5,          'Pianoteq',                                              0.0,            +0.7 )
        self.model.arrange(  6,          'Pianoteq',                                              0.0,             0.0 )
        print
        
    def orchestra(self):
        print('CREATING CSOUND ORCHESTRA...')
        print
        ## self.csoundFile =  csnd.CsoundFile()
        self.model.cppsoundLoad(self.instrumentLibrary)
        ##instruments = self.csoundFile.getInstrumentNames()
        ##for number, name in instruments.items():
        ##    print('Instr %4d: %s' % (number, name))
        ##print
        self.csoundOrchestra = self.model.getCsoundOrchestra()
        
        self.csoundScoreHeader = \
        '''
        ; EFFECTS MATRIX

            ; Chorus to Reverb
            i 1 0 0 200 210 0.05
            ; Leslie to Reverb
            ; i 1 0 0 201 210 0.0
            ; Chorus to Output
            i 1 0 0 200 220 0.05
            ; Reverb to Output
            i 1 0 0 210 220 0.25

        ; SOUNDFONTS OUTPUT

            ; Insno     Start   Dur     Key 	Amplitude
            i 190 	    0       -1      0	    73.

        ; PIANOTEQ OUTPUT

            ; Insno     Start   Dur     Key 	Amplitude
            i 191 	    0       -1      0	    1.


        ; MASTER EFFECT CONTROLS

            ; Chorus.
            ; Insno     Start   Dur     Delay   Divisor of Delay
            i 200       0       -1      10      30

            ; Reverb.
            ; Insno     Start   Dur     Delay   Pitch mod   Cutoff
            i 210       0       -1      0.95    0.002       10000

            ; Master output.
            ; Insno     Start   Dur     Fadein  Fadeout
            i 220       0       -1      0.0     0.1
        '''
        self.model.setCsoundOrchestra(self.csoundOrchestra)
        self.model.setCsoundScoreHeader(self.csoundScoreHeader)
        print
        
    def render(self):
        print('RENDERING...')
        print
        self.model.setCsoundCommand(self.csoundCommand)
        self.model.generate()
        self.score.setDuration(360)
        self.model.createCsoundScore(self.csoundScoreHeader)
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
            'preview':  'csound --messagelevel=99 -W -f --rewrite --dither -K --nodisplays --sample-rate=44100 --control-rate=441   --midi-key=4 --midi-velocity=5 -+id_artist=%s -+id_copyright=Copyright_2007_by_%s -+id_title=%s --output=%s %s %s' % (self.author, self.author, self.title, self.soundfileName, self.orcFilename, self.scoFilename),
            'cd':       'csound --messagelevel=99 -W -f --rewrite --dither -K --nodisplays --sample-rate=44100 --control-rate=44100 --midi-key=4 --midi-velocity=5 -+id_artist=%s -+id_copyright=Copyright_2007_by_%s -+id_title=%s --output=%s %s %s' % (self.author, self.author, self.title, self.soundfileName, self.orcFilename, self.scoFilename),
            'master':   'csound --messagelevel=99 -W -f --rewrite --dither -K --nodisplays --sample-rate=88200 --control-rate=88200 --midi-key=4 --midi-velocity=5 -+id_artist=%s -+id_copyright=Copyright_2007_by_%s -+id_title=%s --output=%s %s %s' % (self.author, self.author, self.title, self.soundfileName, self.orcFilename, self.scoFilename)
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
