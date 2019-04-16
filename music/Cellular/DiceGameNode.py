'''
MUSIKALISCHES WUERFELSPIEL OF 1787 AS A NODE
Implemented by Michael Gogins
6 November 2004
This code is in the public domain
'''
import CsoundAC
import random

## Inherit Python DiceGameNode class
## from CsoundAC.Node C++ class.

class DiceGameNode(CsoundAC.Node):
    def __init__(self):
        print 'DiceGameNode.__init__()...'
        CsoundAC.Node.__init__(self)
        self.minuetTable = {}
        self.minuetTable[ 2] = { 1: 96,  2: 22,  3:141,  4: 41,  5:105,  6:122,  7: 11,  8: 30,  9: 70, 10:121, 11: 26, 12:  9, 13:112, 14: 49, 15:109, 16: 14}
        self.minuetTable[ 3] = { 1: 32,  2:  6,  3:128,  4: 63,  5:146,  6: 46,  7:134,  8: 81,  9:117, 10: 39, 11:126, 12: 56, 13:174, 14: 18, 15:116, 16: 83}
        self.minuetTable[ 4] = { 1: 69,  2: 95,  3:158,  4: 13,  5:153,  6: 55,  7:110,  8: 24,  9: 66, 10:139, 11: 15, 12:132, 13: 73, 14: 58, 15:145, 16: 79}
        self.minuetTable[ 5] = { 1: 40,  2: 17,  3:113,  4: 85,  5:161,  6:  2,  7:159,  8:100,  9: 90, 10:176, 11:  7, 12: 34, 13: 67, 14:160, 15: 52, 16:170}
        self.minuetTable[ 6] = { 1:148,  2: 74,  3:163,  4: 45,  5: 80,  6: 97,  7: 36,  8:107,  9: 25, 10:143, 11: 64, 12:125, 13: 76, 14:136, 15:  1, 16: 93}
        self.minuetTable[ 7] = { 1:104,  2:157,  3: 27,  4:167,  5:154,  6: 68,  7:118,  8: 91,  9:138, 10: 71, 11:150, 12: 29, 13:101, 14:162, 15: 23, 16:151}
        self.minuetTable[ 8] = { 1:152,  2: 60,  3:171,  4: 53,  5: 99,  6:133,  7: 21,  8:127,  9: 16, 10:155, 11: 57, 12:175, 13: 43, 14:168, 15: 89, 16:172}
        self.minuetTable[ 9] = { 1:119,  2: 84,  3:114,  4: 50,  5:140,  6: 86,  7:169,  8: 94,  9:120, 10: 88, 11: 48, 12:166, 13: 51, 14:115, 15: 72, 16:111}
        self.minuetTable[10] = { 1: 98,  2:142,  3: 42,  4:156,  5: 75,  6:129,  7: 62,  8:123,  9: 65, 10: 77, 11: 19, 12: 82, 13:137, 14: 38, 15:149, 16:  8}
        self.minuetTable[11] = { 1:  3,  2: 87,  3:165,  4: 61,  5:135,  6: 47,  7:147,  8: 33,  9:102, 10:  4, 11: 31, 12:164, 13:144, 14: 59, 15:173, 16: 78}
        self.minuetTable[12] = { 1: 54,  2:130,  3: 10,  4:103,  5: 28,  6: 37,  7:106,  8:  5,  9: 35, 10: 20, 11:108, 12: 92, 13: 12, 14:124, 15: 44, 16:131}

        self.trioTable = {}
        self.trioTable[   1] = {17: 72, 18:  6, 19: 59, 20: 25, 21: 81, 22: 41, 23: 89, 24: 13, 25: 36, 26:  5, 27: 46, 28: 79, 29: 30, 30: 95, 31: 19, 32: 66}
        self.trioTable[   2] = {17: 56, 18: 82, 19: 42, 20: 74, 21: 14, 22:  7, 23: 26, 24: 71, 25: 76, 26: 20, 27: 64, 28: 84, 29:  8, 30: 35, 31: 47, 32: 88}
        self.trioTable[   3] = {17: 75, 18: 39, 19: 54, 20:  1, 21: 65, 22: 43, 23: 15, 24: 80, 25:  9, 26: 34, 27: 93, 28: 48, 29: 69, 30: 58, 31: 90, 32: 21}
        self.trioTable[   4] = {17: 40, 18: 73, 19: 16, 20: 68, 21: 29, 22: 55, 23:  2, 24: 61, 25: 22, 26: 67, 27: 49, 28: 77, 29: 57, 30: 87, 31: 33, 32: 10}
        self.trioTable[   5] = {17: 83, 18:  3, 19: 28, 20: 53, 21: 37, 22: 17, 23: 44, 24: 70, 25: 63, 26: 85, 27: 32, 28: 96, 29: 12, 30: 23, 31: 50, 32: 91}
        self.trioTable[   6] = {17: 18, 18: 45, 19: 62, 20: 38, 21:  4, 22: 27, 23: 52, 24: 94, 25: 11, 26: 92, 27: 24, 28: 86, 29: 51, 30: 60, 31: 78, 32: 31}

    def readMeasure(self, section, number):
        scoreNode = CsoundAC.ScoreNode()
        scoreNode.thisown = 0
        filename = section + str(number) + '.mid'
        print 'Reading: %s' % filename
        scoreNode.getScore().load(filename)
        return scoreNode, filename

    def rollDice(self, throws):
        diceroll = 0
        for i in range(throws):
            diceroll += random.randint(1, 6)
        return diceroll

    def generate(self):
        print 'BEGAN generate...'
        print 'First, select random measures for minuet and trio by dice roll and bar number.'
        random.seed()
        measuresChosen = {}
        for bar in xrange( 1, 17):
            measuresChosen[bar] = self.minuetTable[self.rollDice(2)][bar]
        for bar in xrange(17, 33):
            measuresChosen[bar] = self.trioTable  [self.rollDice(1)][bar]
        print 'Next, assemble the selected measures with appropriate repeats:'
        cumulativeTime = 0
        for repeat in xrange(1, 3):
            for barNumber in xrange(1, 17):
                measure, filename = self.readMeasure('M', measuresChosen[barNumber])
                notes = len(measure.getScore())
                duration = measure.getScore().getDuration()
                print 'Repeat %d: Minuet bar %d measure M%d %d notes at %f seconds' % (repeat, barNumber, measuresChosen[barNumber], notes, cumulativeTime)
                print measure.getScore().getCsoundScore()
                cumulativeTime += duration
                barTime = CsoundAC.Rescale()
                barTime.setRescale(0, 1, 0,  cumulativeTime, 0)
                barTime.setRescale(5, 1, 1,  70, 10)
                barTime.thisown=0
                barTime.addChild(measure)
                self.addChild(barTime)
            for barNumber in xrange(17, 33):
                measure, filename = self.readMeasure('T', measuresChosen[barNumber])
                notes = len(measure.getScore())
                duration = measure.getScore().getDuration()
                print 'Repeat %d: Trio   bar %d measure T%d %d notes at %f seconds' % (repeat, barNumber, measuresChosen[barNumber], notes, cumulativeTime)
                print measure.getScore().getCsoundScore()
                cumulativeTime += duration
                barTime = CsoundAC.Rescale()
                barTime.setRescale(0, 1, 0,  cumulativeTime, 0)
                barTime.setRescale(5, 1, 1,  70, 10)
                barTime.thisown=0
                barTime.addChild(measure)
                self.addChild(barTime)
        print 'ENDED generate.'





