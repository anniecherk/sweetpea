


----------- Streams --------------

data Stream = Stream [String] | Stream [Stream]


--------- Transitions --------------

data Transitions = Transitions [Streams] | Transitions [Paths]

children :: Stream -> Transitions
leaves   :: Stream -> Transitions
tilda    :: Stream -> String -> Stream -- or Path(?)
--  ~ infix

--"Path" ?

-- subtract?

cross :: Stream -> Stream -> Stream

---------- Constraints -------------

data Constraints = Constraint ParserConstraint | Constraints [ParserConstraint]

-- Stream & Transition need to be in a type-class
maxInARow :: Int -> Stream -> ParserConstraint
count :: (Count, Count) -> Stream -> ParserConstraint
avoid :: Stream -> ParserConstraint
include :: Stream -> ParserConstraint

noConstraints :: ParserConstraints
noConstraints = []

data Count = Count Int | Count None



---------- Design ------------


----------- Blocks ------------    # fully crossed constraints not good enough!!!

data Block = Block BlockContents Constraints

data BlockContents = BlockContents

sample :: Int -> Stream -> BlockContents
fullyCross :: Stream -> BlockContents
exactly :: Stream -> Block




sequence :: [Blocks] -> Constraints -> Block
randomWithReplacement :: [Blocks] -> Constraints ->  Block
permutations :: [Blocks] -> Constraints ->  Block
repeat :: Int -> Block -> Constraints -> Block
mix :: [Blocks] -> Constraints ->  Block



-------- Experiment ---------

-- maybe this is is the "run" function? Not clear yet.
experiment :: Block
