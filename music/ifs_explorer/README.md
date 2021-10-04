# IFS Explorer

Michael Gogins<br>
https://github.com/gogins<br>
http://michaelgogins.tumblr.com

This directory contains code, using the new Clang embedded C++ compiler 
opcodes, to demonstrate the effects of changing fundamental transformations 
in the Hutchinson operator of an iterated function system (IFS), also known 
as the multiple copy reducing machine (MCRM).

A four transformation Hutchinson operator is the starting point, mapping the 
unit square to each of its quarters. The iterations are carried to some depth 
and rendered using phase-synchronous cosine grains, and then studied as 
sonographs.

A sequence of simple changes to the Hutchinson operator is made, and the 
sonograph of the resulting soundfile is compared to that of the original unit 
sqaure.

The phase-synchronous grain is implemented in C++. The idea is that each point 
in the final iteration of the IFS is a single cosine grain. All grains of the 
same frequency will have the same phase. Grains of the same frequency can thus 
overlap without artifacts caused by differing phases.



