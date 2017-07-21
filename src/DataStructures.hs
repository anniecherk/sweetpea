module DataStructures
( Count, Var, Index, CNF, SATResult(..), CountingConstraint(..)
, emptyState, initState, getFresh, getNFresh, appendCNF, zeroOut, setToOne, setToZero )
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


-- for testing!
data SATResult = Correct | Unsatisfiable | WrongResult Int Int | ParseError deriving(Show)


------------------------------------------------------------------
---------- Helpful State Abstractions ----------------------------

emptyState :: (Count, CNF)
emptyState = (1, [])

-- if we need to start with variables 1-maxVar
initState :: Int -> (Count, CNF)
initState maxVar = (maxVar, [])

getFresh :: State (Count, CNF) Count
getFresh =  do (numVars, x) <- get
               put (numVars + 1, x)
               return (numVars + 1)

getNFresh :: Int -> State (Count, CNF) [Count]
getNFresh n = replicateM n getFresh


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
