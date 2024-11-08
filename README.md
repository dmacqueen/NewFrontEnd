# NewFrontEnd

Repository dmacqueen/NewFrontEnd  
Created by David MacQueen, 2024.09.14 

# A repository for the development of a new front end for (Standard/Successor) ML.

The compiler front end includes lexical analysis, parsing, and "elaboration", where elaboration consists of variable binding analysis and
type checking, taken to include the static analysis and static representation of the module system.

The resulting front end is not expected to conform completely with the Definition of Standard ML (Revised), and so may
be considered a front end for a "Successor" ML.  As a working name for the input source language, we are using simply "ML"
(any suggestions?).  The front end will process ML source files into a typed abstract syntax, with no commitments regarding
the "middle end" or further stages of compilation (and thus no use of concepts or types dependent on, say, the FLINT middle end).

The plan is for the new front end to consist of a set of libraries (in roughly the CM sense). These libraries should be compatible
with a future "library-ized" version of SML/NJ.  It is likely that CM, or a revision thereof, will be required to compile these
libraries [compile and build infrastructure].

This is essentially a fresh, "from scratch" implementation, but it is expected to borrow extensively from the current SML/NJ front end.
