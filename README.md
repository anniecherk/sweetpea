SweetPea is a SAT-sampler aided language that addresses problems scientists face when creating randomized experimental designs; this prototype that is targeted at psychology and neuroscience experiments.

An experimental design is a description of experimental factors and how to map those factors onto a sequence of trials such that researchers can draw statistically valid conclusions. SweetPea provides a high-level interface to declaratively describe an experimental design, and a low-level synthesizer to generate unbiased sequences of trials given satisfiable constraints. SweetPea samples sequences of trials by compiling experimental designs into Boolean logic, which are then passed to a SAT-sampler. The SAT-sampler Unigen provides statistical guarantees that the solutions it finds are approximately uniformly probable in the space of all valid solutions. This means that while producing sequences of trials that are perfectly unbiased is intractable, we do the next best thing-- produce sequences that are approximately unbiased.


## Useage info!

Grab [stack](https://docs.haskellstack.org/en/stable/README/) if you don't already have it.

Grab the repo:
`git clone https://github.com/anniecherk/sweetpea`
`cd sweetpea`

Let stack handle all the dependencies:
`stack install`

Build it:
`stack build`

## Run an example

`stack exec end-to-end > ex.cnf`

`cryptominisat5_simple --verb=0 ex.cnf > ex.sol`

`stack exec decode`

`>> ex.sol -- type in the file name of the solution file` 

