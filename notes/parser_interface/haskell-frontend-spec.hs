Keywords


----------- Streams --------------

data Stream = Stream [String] | Stream [Stream]


--------- Transitions --------------

data Transitions = Transitions [Streams] | Transitions [Paths]

children :: Stream -> Transitions
leaves   :: Stream -> Transitions
~ infix  :: Stream -> String -> Stream(?)

"Path" ?

subtract?

cross :: Stream -> Stream -> Stream

---------- Constraints -------------

data Constraints = Constraint ParserConstraint | Constraints [ParserConstraint]

-- Stream & Transition need to be in a type-class
max-in-a-row :: Int -> Stream -> ParseConstraint
count :: (Int, Int) ->



globalConstraints = Constraint (balanceTransitions light-dark-transitions)

# two possible interpetations
topBlockConstraints = Constraints [[max-in-a-row 3 levels | levels <- [blue-pink-transitions, shape-transitions]],    # "allOf"
                                   count (4,8) [lightColors ~ 'pink', lightColors ~ 'blue']]                          # "anyOf"

bottomBlockConstraints = Constraints (...)

initializationConstraints = Constraints (avoid lightColors)    # avoid is count(0)

terminationConstraints = Constraints (include darkColors)      # include is count(1,)

noConstraints = []



---------- Design ------------

design = cross [color, shape]


----------- Blocks ------------    # fully crossed constraints not good enough!!!
# Block <what's in it?>  <what are the constraints>

initBlock = Block (sample 2 design) initialConstraints                # sample (w.replacement) from all possible trials in the full crossing

termBlock = Block (sample 2 design) terminalConstraints               # avoid / include can ONLY be used with sample

myBlock = repeat 4 (Block (fully-crossed design) topBlockConstraints) noConstraints  # x-matrix

otherBlock = exactly (lightColors ~ 'blue', shape ~ 'circle')

middleBlock = mix [myBloc, otherBlock] someConstraints


# this shuffles order of blocks
bottomBlock = randomSequence [myBlock, otherBlock] blockBoundaryAndBeyondConstraints


----------- notes ------
# random w. out replacement, permute, random w/out replacement

# block combinators:
#  sequence combinators:
#    sequence / randomWithReplacement / permutations
#    repeat
#  mix
#  optional # at random add this block, or don't    # null blocks

# block filler : what's in it?
#  sample / fully cross

# block builder :
#  exactly


# implicit randomization (use exactly + sequence if you don't want that)
# what are all block ordering constraints? is random order the only one?

-------- Experiment ---------

# session contains experiments
myGreatExperiment = experiment (sequence [initBlock, myBlock, termBlock] globalConstraints)



# experiment is a function that takes a block
