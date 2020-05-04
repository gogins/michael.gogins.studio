'''
Functional (Pseudo-Functional?) Harmony by Transformations in Chord Space

Copyright (C) 2020 by Michael Gogins
'''
import CsoundAC
import string
import sys
import traceback

# The question is, how many of these moves can be used freely in more than one place and still make sense?

tonic = CsoundAC.chordForName("CM")
print("tonic:\n{}".format(tonic.information()))
tonic_to_tonic_minor = tonic.K()
print("tonic_to_tonic_minor:\n{}".format(tonic_to_tonic_minor.information()))
tonic_to_subdominant_minor = tonic.K().T(5)
print("tonic_to_subdominant_minor:\n{}".format(tonic_to_subdominant_minor.information()))
tonic_to_subdominant = tonic.T(5)
print("tonic_to_subdominant:\n{}".format(tonic_to_subdominant.information()))
subdominant_to_dominant = tonic_to_subdominant.T(2)
print("subdominant_to_dominant:\n{}".format(subdominant_to_dominant.information()))
subdominant_minor_to_dominant = tonic_to_subdominant_minor.K().T(2)
print("subdominant_minor_to_dominant:\n{}".format(subdominant_minor_to_dominant.information()))
subdominant_minor_to_subtonic = tonic_to_subdominant_minor.T(9)
print("subdominant_minor_to_subtonic:\n{}".format(subdominant_minor_to_subtonic.information()))
subdominant_to_subtonic = tonic_to_subdominant.K().T(9)
print("subdominant_to_subtonic:\n{}".format(subdominant_to_subtonic.information()))
dominant_to_tonic = subdominant_to_dominant.T(5)
print("dominant_to_tonic:\n{}".format(dominant_to_tonic.information()))
subtonic_to_tonic = subdominant_to_subtonic.K().T(10)
print("subtonic_to_tonic:\n{}".format(subtonic_to_tonic.information()))

# The irreducible set of unique moves is then:

tonic.T(2)
tonic.T(5)
tonic.T(9)
tonic.K()
tonic.K().T(2)
tonic.K().T(5)
tonic.K().T(9)
tonic.K().T(10)

# Make some lambdas...

t2 =  lambda chord : chord.T(2)
t5 =  lambda chord : chord.T(5)
t9 =  lambda chord : chord.T(9)
k =   lambda chord : chord.K()
k2 =  lambda chord : chord.K().T(2)
k5 =  lambda chord : chord.K().T(5)
k9 =  lambda chord : chord.K().T(9)
k10 = lambda chord : chord.K().T(10)


# I'm fudging this with a minor "subtonic," instead of the actual diminished subtonic....

# If we break up K().T(x) we have even less:

tonic.K()
tonic.T(2)
tonic.T(5)
tonic.T(9)
tonic.T(10)

# Keep in mind, K simply takes a chord to its relative minor or major!

# Also, 2, 5, and 9 are Jeff Stone's "strong" root progressions.

# What happens with minor triads, major and minor sevenths, major and minor ninths? Minor triads break down,
# but not completely. Major sevenths break down because K fails. Major ninths break down only on the supertonic.

# My hunch is that using one of the irreducible set of unique moves "out of order" simply induces a modulation that more or less makes sense.
# Well, I will try that.

# Might have to complete this for the minor scale as well.