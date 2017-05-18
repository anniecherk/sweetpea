module StatefulCompiler
(CNF )
where

import Control.Monad.Trans.State
import System.Random

-- AND of ORs
type CNF = [[Int]]




getFresh :: State (Int, CNF) Int
getFresh =  do (numVars, x) <- get
               put (numVars + 1, x)
               return (numVars + 1)

appendCNF :: CNF -> State (Int, CNF) ()
appendCNF newEntry = do (x, accum) <- get
                        put (x, newEntry ++ accum)
                        return ()


halfAdder :: Int -> Int -> State (Int, CNF) (Int, Int)
halfAdder a b = do c <- getFresh
                   s <- getFresh
                   appendCNF $ computeC c a b
                   appendCNF $ computeS s a b
                   return (c, s)
                where computeC :: Int -> Int -> Int -> CNF
                      computeC c a b = cImpliescVal ++ cValImpliesC
                         where cVal = andCNF [a, b]
                               cNegVal = nAndCNF a b
                               cImpliescVal = distribute (-c) cVal
                               cValImpliesC = distribute c cNegVal
                      computeS :: Int -> Int -> Int -> CNF
                      computeS s a b = sImpliescVal ++ sValImpliesS
                         where sVal = xorCNF a b
                               sNegVal = xNorCNF a b
                               sImpliescVal = distribute (-s) sVal
                               sValImpliesS = distribute s sNegVal










-------------------------------
-- BACKEND

-- (a or ~b) and (~a or b)
doubleImplies :: Int -> Int -> CNF
doubleImplies a b = [[a, -b], [-a, b]]

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
------------
