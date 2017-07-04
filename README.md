Psyscript is a sequence processing language; it is a solver aided DSL.

It isn't done!

This is a project that takes high-level constraints, translates them into DIMACS CNF (using a Tseytin transformation!) to feed through a SAT solver, and then translates the results back into something intelligible at a high-level. 


Here's what works so far:

- Generating valid DIMACS CNF files which produce correct (exhaustively tested) constraints for multibit adders.
- The popcount algorithm, which generates a DIMACS CNF file to represent "k of N" bits are set. (Also tested exhaustively!)

Here's a high-level todo list:

- Generating constraints for "k < N" bits are set.
- Handling encodings for data which at a high-level is not binary; in practice this is categorical data with >2 categories.
- Explicit support for more types of constraints.


## Useage info!

Grabbing [stack](https://docs.haskellstack.org/en/stable/README/) is the easiest way to run this code.

Grab the repo:
`git clone https://github.com/anniecherk/pyschocnf`

Let stack handle all the dependencies:
`stack install`

Build it:
`stack build`

There isn't yet a stable way to run this code, more on this later!


