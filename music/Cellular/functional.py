import CsoundAC
import string
import sys
import traceback

print('''
Functional (Pseudo-Functional?) Harmony by Transformations in Chord Space

Copyright (C) 2020 by Michael Gogins

These are the observed/permitted moves in Tymoczko's graph of local functional 
harmony in the major scale. Any number of descending thirds moving away from I 
towards V: I to vi to IV to ii to viio to V. Or:

I to vi
I to IV
I to ii
I to viio
I to V
vi to IV
vi to ii
vi to viio
vi to V
IV to ii
IV to viio
IV to V
ii to viio
ii to V
viio to V

Otherwise, moving back towards I:

vi to I6
IV to I
V to I
V to vi
V to IV6
viio to I

Brutally oversimplifying, use I for I6, and IV for IV6. If we use smooth 
voice-leading, the 6 chords are of course what we get.

Even more brutally, use vii for viio, though vii modulates.

Assuming that we obtain the required transformations in chord space by 
composing K and T, what is the irreducible set of these transformations?
By composing K and Q?

And what happens if one of this irreducible set is applied "out of order?"

Also keep in mind, I'm not committed to this picture, other things 
happening are just fine if and only if they sound good.

Obviously, for this to really reproduce the graph, the transformations 
have to know what key they are in. But this is still useful to me if I 
end up with some tonal sounding atonal thing, or some modulating thing.

NOTA BENE: 

K respects order of pitches! Normalize chords to OP before using K.
In fact, normalize ALL chords to OP after each operation, for consistency.
''')

I = CsoundAC.chordForName("CM")
vi = CsoundAC.chordForName("Am")
IV = CsoundAC.chordForName("FM")
ii = CsoundAC.chordForName("Dm")
vii = CsoundAC.chordForName("Bm")
viio = CsoundAC.chordForName("Bo")
V = CsoundAC.chordForName("GM")
iii = CsoundAC.chordForName("Em")

print("GOING AWAY FROM I TOWARDS V...")
I_to_vi = I.K().eOP()
print("I_to_vi: \n" + I_to_vi.information())
I_to_IV = I.T(5).eOP()
print("I_to_IV: \n" + I_to_IV.information())
I_to_ii = I.K().eOP().T(5).eOP()
print("I_to_ii: \n" + I_to_ii.information())
I_to_vii = I.K().eOP().T(2).eOP()
print("I_to_vii: \n" + I_to_vii.information())
I_to_V = I.T(7).eOP()
print("I_to_V: \n" + I_to_V.information())
vi_to_IV = vi.K().eOP()
print("vi_to_IV: \n" + vi_to_IV.information())
vi_to_ii = vi.T(5).eOP()
print("vi_to_ii: \n" + vi_to_ii.information())
vi_to_vii = vi.T(2).eOP()
print("vi_to_vii: \n" + vi_to_vii.information())
vi_to_V = vi.K().eOP().T(2).eOP()
print("vi_to_V: \n" + vi_to_V.information())
IV_to_ii = IV.K().eOP().T(5).eOP()
print("IV_to_ii: \n" + IV_to_ii.information())
IV_to_vii = IV.K().eOP().T(2).eOP()
print("IV_to_vii: \n" + IV_to_vii.information())
IV_to_V = IV.T(2).eOP()
print("IV_to_V: \n" + IV_to_V.information())
ii_to_vii = ii.T(9).eOP()
print("ii_to_vii: \n" + ii_to_vii.information())
ii_to_V = ii.K().eOP().T(5).eOP()
print("ii_to_V: \n" + ii_to_V.information())
vii_to_V = vii.K().eOP()
print("vii_to_V: \n" + vii_to_V.information())
print("GOING BACK TOWARDS I...")
ii_to_I = ii.K().eOP().T(10).eOP()
print("ii_to_I: \n" + ii_to_I.information())
vi_to_I6 = vi.K().eOP().T(7).eOP()
print("vi_to_I6: \n" + vi_to_I6.information())
IV_to_I = IV.T(7).eOP()
print("IV_to_I: \n" + IV_to_I.information())
V_to_I = V.T(5).eOP()
print("V_to_I: \n" + V_to_I.information())
V_to_vi = V.K().eOP().T(10).eOP()
print("V_to_vi: \n" + V_to_vi.information())
V_to_IV6 = V.T(10).eOP()
print("V_to_IV6: \n" + V_to_IV6.information())
vii_to_I = vii.K().eOP().T(5).eOP()
print("vii_to_I: \n" + vii_to_I.information())
print('''
Hence the lambdas for this irreducible set of transformations are
(think about weighting these):  
''')
t2  = lambda chord : chord.T(2).eOP()
t5  = lambda chord : chord.T(5).eOP()
t7  = lambda chord : chord.T(7).eOP()
t9  = lambda chord : chord.T(9).eOP()
t10 = lambda chord : chord.T(10).eOP()
k0  = lambda chord : chord.K().eOP()
k2  = lambda chord : chord.K().eOP().T(2).eOP()
k5  = lambda chord : chord.K().eOP().T(5).eOP()
k7  = lambda chord : chord.K().eOP().T(7).eOP()
k10 = lambda chord : chord.K().eOP().T(10).eOP()
'''
Let's try to extend this to the ninth chords of the major scale:

C-E-G-B-D = 1,3,5,7,9 = major ninth chord
D-F-A-C-E = 1,b3,5,b7,9 = minor ninth chord
E-G-B-D-F = 1,b3,5,b7,b9 = minor seven flat nine chord
F-A-C-E-G = 1,3,5,7,9 = major ninth chord
G-B-D-F-A = 1,3,5,b7,9 = dominant ninth chord
A-C-E-G-B = 1,b3,5,b7,9 = minor ninth chord
B-D-F-A-C = 1,b3,b5,b7,b9 = half-diminished seven flat nine chord
'''
IM9 = CsoundAC.chordForName("CM9")
print(IM9.information())
iim9 = CsoundAC.chordForName("Dm9")
print(iim9.information())
iiim7b9 = CsoundAC.chordForName("Em7b9")
print(iiim7b9.information())
IVM9 = CsoundAC.chordForName("FM9")
print(IVM9.information())
V9 = CsoundAC.chordForName("G9")
print(V9.information())
vim9 = CsoundAC.chordForName("Am9")
print(vim9.information())
viim7b9b5 = CsoundAC.chordForName("Bm7b9b5")
print(viim7b9b5.information())

print
print("NINTH CHORDS GOING AWAY FROM I TOWARDS V...")
I_to_vi = IM9.K().eOP().T(5).eOP()
print("I_to_vi: \n" + I_to_vi.information())
I_to_IV = IM9.T(5).eOP()
print("I_to_IV: \n" + I_to_IV.information())
I_to_ii = IM9.I().eOP().T(10).eOP()
print("I_to_ii: \n" + I_to_ii.information())
I_to_vii = IM9.K().eOP().T(7).eOP()
print("I_to_vii: \n" + I_to_vii.information())
I_to_V = IM9.T(7).eOP()
print("I_to_V: \n" + I_to_V.information())
vi_to_IV = vi.K().eOP()
print("vi_to_IV: \n" + vi_to_IV.information())
vi_to_ii = vi.T(5).eOP()
print("vi_to_ii: \n" + vi_to_ii.information())
vi_to_vii = vi.T(2).eOP()
print("vi_to_vii: \n" + vi_to_vii.information())
vi_to_V = vi.K().eOP().T(2).eOP()
print("vi_to_V: \n" + vi_to_V.information())
IV_to_ii = IV.K().eOP().T(5).eOP()
print("IV_to_ii: \n" + IV_to_ii.information())
IV_to_vii = IV.K().eOP().T(2).eOP()
print("IV_to_vii: \n" + IV_to_vii.information())
IV_to_V = IV.T(2).eOP()
print("IV_to_V: \n" + IV_to_V.information())
ii_to_vii = ii.T(9).eOP()
print("ii_to_vii: \n" + ii_to_vii.information())
ii_to_V = ii.K().eOP().T(5).eOP()
print("ii_to_V: \n" + ii_to_V.information())
vii_to_V = vii.K().eOP()
print("vii_to_V: \n" + vii_to_V.information())
print("NINTH CHORDS GOING BACK TOWARDS I...")
ii_to_I = ii.K().eOP().T(10).eOP()
print("ii_to_I: \n" + ii_to_I.information())
vi_to_I6 = vi.K().eOP().T(7).eOP()
print("vi_to_I6: \n" + vi_to_I6.information())
IV_to_I = IV.T(7).eOP()
print("IV_to_I: \n" + IV_to_I.information())
V_to_I = V.T(5).eOP()
print("V_to_I: \n" + V_to_I.information())
V_to_vi = V.K().eOP().T(10).eOP()
print("V_to_vi: \n" + V_to_vi.information())
V_to_IV6 = V.T(10).eOP()
print("V_to_IV6: \n" + V_to_IV6.information())
vii_to_I = vii.K().eOP().T(5).eOP()
print("vii_to_I: \n" + vii_to_I.information())

print('''
First order Markov transition matrix, from row to column:
    
     I VI IV II VII  V III
I    0  7 18  14  9 53   1
VI   7  0 15  48  9 20   2
IV  34  0  0  14 14 37   0
II   1  1  0   0 19 77   1
VII 82  1  2   0  0 15   1
V   84  8  5   0  2  0   0 

''')
C_major = CsoundAC.chordForName("C major")
print(C_major.information())
D_Dorian = CsoundAC.chordForName("D Dorian")
print(D_Dorian.information())



