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
I_to_vi = I.K()
print("I_to_vi: \n" + I_to_vi.information())
I_to_IV = I.T(5)
print("I_to_IV: \n" + I_to_IV.information())
I_to_ii = I.K().T(5)
print("I_to_ii: \n" + I_to_ii.information())
I_to_vii = I.K().T(2)
print("I_to_vii: \n" + I_to_vii.information())
I_to_V = I.T(7)
print("I_to_vii: \n" + I_to_vii.information())
vi_to_IV = vi.K()
print("vi_to_IV: \n" + vi_to_IV.information())
vi_to_ii = vi.T(5)
print("vi_to_ii: \n" + vi_to_ii.information())
vi_to_vii = vi.T(2)
print("vi_to_vii: \n" + vi_to_vii.information())
vi_to_V = vi.K().T(2)
print("vi_to_V: \n" + vi_to_V.information())
IV_to_ii = IV.K().T(5)
print("IV_to_ii: \n" + IV_to_ii.information())
IV_to_vii = IV.K().T(2)
print("IV_to_vii: \n" + IV_to_vii.information())
IV_to_V = IV.T(2)
print("IV_to_V: \n" + IV_to_V.information())
ii_to_vii = ii.T(9)
print("ii_to_vii: \n" + ii_to_vii.information())
ii_to_V = ii.K().T(5)
print("ii_to_V: \n" + ii_to_V.information())
vii_to_V = vii.K()
print("vii_to_V: \n" + vii_to_V.information())
print("GOING BACK TOWARDS I...")
vi_to_I6 = vi.K().T(7)
print("vi_to_I6: \n" + vi_to_I6.information())
IV_to_I = IV.T(7)
print("IV_to_I: \n" + IV_to_I.information())
V_to_I = V.T(5)
print("V_to_I: \n" + V_to_I.information())
V_to_vi = V.K().T(10)
print("V_to_vi: \n" + V_to_vi.information())
V_to_IV6 = V.T(10)
print("V_to_IV6: \n" + V_to_IV6.information())
vii_to_I = vii.K().T(5)
print("vii_to_I: \n" + vii_to_I.information())
print('''
Hence the lambdas for this irreducible set of transformations are:
''')
t2  = lambda chord : chord.T(2)
t5  = lambda chord : chord.T(5)
t7  = lambda chord : chord.T(7)
t9  = lambda chord : chord.T(9)
t10 = lambda chord : chord.T(10)
k0  = lambda chord : chord.K()
k2  = lambda chord : chord.K().T(2)
k5  = lambda chord : chord.K().T(5)
k7  = lambda chord : chord.K().T(7)
k10 = lambda chord : chord.K().T(10)





