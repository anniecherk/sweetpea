-- TODO: make all booleanCNF functions stateful
-- careful! interface is NOT consistent! Currently only aDoubleImpliesList is stateful

module DataStructures
( Count, Var, Index, CNF, SATResult(..), CountingConstraint(..), Trial(..)
, emptyState, initState, getFresh, getNFresh, putFresh, appendCNF, zeroOut, setToOne, setToZero
, distribute, xNorCNF, xorCNF, nAndCNF, andCNF, aDoubleImpliesList, doubleImplies, aDoubleImpliesBandC )
where

import Control.Monad.Trans.State
import Control.Monad (replicateM)

-- AND of ORs
type Count = Int
type Var = Int
type Index = Int
type CNF = [[Var]]

-- It seems repetitive to have each operation hold on to the args
-- given that all args are the same, but if the functionality changes in
-- the future, this will be a lot less painful to refactor
data CountingConstraint = Exactly Int [Var] | GreaterThan Int [Var] | LessThan Int [Var] deriving(Show)

data Trial = Trial { numFields :: Int -- fields are one-hot encoded levels
                   , fieldVars :: [Var]
                   , numStates :: Int -- states are for enforcing constraints to ensure fully-crossed
                   , stateVars :: [Var]
                    } deriving (Show, Eq)


-- Trial ::
-- # states       # fields         # fieldVars     # stateVars
-- (4 fully x'd) (for constraints)
--
-- 1 lookup table from fieldName to index into the fieldVar list
-- function to getFieldByName

-- for testing!
data SATResult = Correct | Unsatisfiable | WrongResult Int Int | ParseError deriving(Show)



data Logic = And [Logic] | Or [Logic] | Not Logic | Var Int

-- ignoring the last three cases: https://www.cs.jhu.edu/~jason/tutorials/convert-to-CNF.html
toCNF :: Logic -> Logic
toCNF (Var x) = Var x
toCNF (And ps) = And $ map toCNF ps -- I think this is the right interpretation here
toCNF (Not (Var x)) = Var (-1*x)
toCNF (Not (Not x)) = toCNF x -- double negation
-- If φ has the form ~(P ^ Q), then return CONVERT(~P v ~Q).
toCNF (Not (And ps)) = toCNF $ Or $ map Not ps
-- If φ has the form ~(P v Q), then return CONVERT(~P ^ ~Q).
toCNF (Not (Or ps)) = toCNF $ And $ map Not ps
toCNF (Or [p]) = And [Or [p]]
toCNF (Or (p:ps)) = undefined -- slightly complicated bc the result isn't always an *explicit* and of ors-- will need to fix this
  where convertedHead = toCNF p
        convertedClauses = map toCNF ps



-- If φ has the form P v Q, then:
--    CONVERT(P) must have the form P1 ^ P2 ^ ... ^ Pm, and
--    CONVERT(Q) must have the form Q1 ^ Q2 ^ ... ^ Qn,
--    where all the Pi and Qi are dijunctions of literals.
--    So we need a CNF formula equivalent to
--       (P1 ^ P2 ^ ... ^ Pm) v (Q1 ^ Q2 ^ ... ^ Qn).
--    So return (P1 v Q1) ^ (P1 v Q2) ^ ... ^ (P1 v Qn)
--            ^ (P2 v Q1) ^ (P2 v Q2) ^ ... ^ (P2 v Qn)
--              ...
--            ^ (Pm v Q1) ^ (Pm v Q2) ^ ... ^ (Pm v Qn)



------------------------------------------------------------------
---------- Helpful State Abstractions ----------------------------

emptyState :: (Count, CNF)
emptyState = (0, [])

-- if we need to start with variables 1-maxVar
initState :: Int -> (Count, CNF)
initState maxVar = (maxVar, [])

getFresh :: State (Count, CNF) Count
getFresh =  do (numVars, x) <- get
               put (numVars + 1, x)
               return (numVars+1)

getNFresh :: Int -> State (Count, CNF) [Count]
getNFresh n = replicateM n getFresh

putFresh :: Int -> State (Count, CNF) ()
putFresh n =  do (numVars, x) <- get
                 put (n, x)


appendCNF :: CNF -> State (Count, CNF) ()
appendCNF newEntry = do (x, accum) <- get
                        put (x, newEntry ++ accum)
                        return ()

zeroOut :: [Var] -> State (Count, CNF) ()
zeroOut inList = appendCNF $ map (\x -> [-x]) inList

setToOne :: Var -> State (Count, CNF) ()
setToOne x = appendCNF [[x]]

setToZero :: Var -> State (Count, CNF) ()
setToZero x = appendCNF [[-x]]


------------------------------------------------------------------
---------- Helper Functions --------------------------------------

-- (a or ~b) and (~a or b)
doubleImplies :: Int -> Int -> CNF
doubleImplies a b = [[a, -b], [-a, b]]

-- a <=> (b and c)  = (-a v b) ^ (-a v c) ^ (a v -b v -c)
-- Thanks wolfram alpha :heart: (see note: wolfram_doubleimplies.txt)
aDoubleImpliesBandC :: Var -> Var -> Var -> CNF
aDoubleImpliesBandC a b c = [[-a, b], [-a, c], [a, -b, -c]]

-- the double-implication generalization
-- https://www.wolframalpha.com/input/?i=CNF+A++%3C%3D%3E+(B+%26%26+C+%26%26+D+%26%26+E)
-- CNF A  <=> (B && C && D)
-- (-a or b)
-- (-a or c)
-- (-a or d)
-- (a or -b or -c or -d)
aDoubleImpliesList :: Var -> [Var] -> State (Count, CNF) ()
        -- makes the list of [-b, -c, .., a] : makes the lists [-a, b], [-a, c], ...
aDoubleImpliesList a inList = appendCNF result
  where result = (map ((-1) *) inList ++ [a]) : map (\x -> [-a, x]) inList


-- wraps in an extra layer: this is just for readabilty
andCNF :: [Int] -> CNF
andCNF = return --(: [])

-- not A or not B
nAndCNF :: Int -> Int -> CNF
nAndCNF a b = [[-a, -b]]

-- A xor B
-- (A or B) and (-A or -B)
xorCNF :: Int -> Int -> CNF
xorCNF a b = [[a, b], [-a, -b]]

xNorCNF :: Int -> Int -> CNF
xNorCNF a b = [[a, -b], [-a, b]]

distribute :: Int -> CNF -> CNF
distribute inputID = map (\orClause -> inputID : orClause)
