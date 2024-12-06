# NewFrontEnd

Repository dmacqueen/NewFrontEnd  
Created by David MacQueen, 2024.09.14 

# A new front end for (Standard/Successor) ML.

An SML compiler front end (following the pattern of SML/NJ) includes lexical analysis, parsing, and "elaboration", where elaboration consists of variable binding analysis and type checking, taken to include the static analysis and static representation of the module system.

The resulting front end is not expected to conform completely with the Definition of Standard ML (Revised), and so may be considered a front end for a "Successor" ML.  As a working name for the input source language, we are using simply "ML" (any suggestions?).  The front end will process ML source files into a typed abstract syntax, with no commitments regarding the "middle end" or further stages of compilation (and thus no use of concepts or types dependent on, say, the FLINT middle end).

The plan is for the new front end to consist of a set of libraries (in roughly the CM sense). These libraries should be compatible with a future "library-ized" version of SML/NJ.  It is likely that CM, or a revision thereof, will be required to compile these libraries [compile and build infrastructure].

This is essentially a fresh, "from scratch" implementation, but it is expected to borrow extensively from the current SML/NJ front end, and from the MacQueen/Kuan model of the internal representation and elaboration of modules.

This NewFrontEnd repository may at some point be moved to the smlnj github project.

# Contents

## Top (directory .)

- README.md: this file

## Proposals (directory ./proposals/)

- language.txt: proposed micro-design alterations to (Standard) ML, under the title MsML
  (MacQueen's (own version of) Successor ML).
  
- variables-etc.txt: design-engineering note concerning symbols, variables, static environments, etc.

## Discussion (directory ./discussion/)

- rwh1.txt: Bob Harper's initial response to proposals/language.txt (originally rh-response.txt)

- dbm1.txt: DBM's response to discussion/rw1.txt (originally dbm-on-rwh1.txt).

- rwh2.txt: RWH's response to discussion/dbm1.txt (originally rwh-on-dbm-on-rwh1.txt).

- dbm2.txt: DBM's response to discussion/rwh2.txt (originally dbm-rwh-2.txt)

- rwh3.txt: RWH's background discussion of SML module semantics and design (originally rwh-background.txt).

- dbm3.txt: DBM's comments on discussion/rwh3.txt.
