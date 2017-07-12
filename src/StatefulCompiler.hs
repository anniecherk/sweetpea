module StatefulCompiler
(CNF )
where

import Control.Monad.Trans.State
import System.Random

-- AND of ORs
type Count = Int
type Var = Int
type CNF = [[Var]]



-------------
getFresh :: State (Count, CNF) Count
getFresh =  do (numVars, x) <- get
               put (numVars + 1, x)
               return (numVars + 1)

appendCNF :: CNF -> State (Count, CNF) ()
appendCNF newEntry = do (x, accum) <- get
                        put (x, newEntry ++ accum)
                        return ()

-------------

halfAdder :: Var -> Var -> State (Count, CNF) (Var, Var)
halfAdder a b = do c <- getFresh
                   s <- getFresh
                   appendCNF $ computeC c a b
                   appendCNF $ computeS s a b
                   return (c, s)
                where computeC :: Var -> Var -> Var -> CNF
                      computeC c a b = cImpliescVal ++ cValImpliesC
                         where cVal = andCNF [a, b]
                               cNegVal = nAndCNF a b
                               cImpliescVal = distribute (-c) cVal
                               cValImpliesC = distribute c cNegVal
                      computeS :: Var -> Var -> Var -> CNF
                      computeS s a b = sImpliescVal ++ sValImpliesS
                         where sVal = xorCNF a b
                               sNegVal = xNorCNF a b
                               sImpliescVal = distribute (-s) sVal
                               sValImpliesS = distribute s sNegVal


-- http://www.dsm.fordham.edu/~moniot/Classes/CompOrganization/binary-adder/node6.html
-- creates 2 new variables & 16 clauses.
       --     a      b     cin       (nVars  accum)  c    s
fullAdder :: Var -> Var -> Var -> State (Count, CNF) (Var, Var)
fullAdder a b cin = do cout <- getFresh
                       s <- getFresh
                       appendCNF $ computeC cout a b cin
                       appendCNF $ computeS s    a b cin
                       return (cout, s)
                    where computeC :: Var -> Var -> Var -> Var -> CNF
                          computeC c x y z = cImpliescVal ++ cValImpliesC
                            where cVal = [[x, y], [x, z], [y, z]]  -- see adder-notes for derivations
                                  cNegVal = [[-x, -y], [-x, -z], [-y, -z]]
                                  cImpliescVal = distribute (-c) cVal
                                  cValImpliesC = distribute c cNegVal
                          computeS :: Var -> Var -> Var -> Var -> CNF
                          computeS s x y z = sImpliescVal ++ sValImpliesC
                            where sVal = [[-x, -y, z], [-x, y, -z], [x, -y, -z], [x, y, z]] -- see adder-notes for derivations
                                  sNegVal = [[-x, -y, -z], [-x, y, z], [x, -y, z], [x, y, -z]]
                                  sImpliescVal = distribute (-s) sVal
                                  sValImpliesC = distribute s sNegVal

-- http://www.dsm.fordham.edu/~moniot/Classes/CompOrganization/binary-adder/node7.html
-- Ripple Carry! Wooh!
-- Just a recursive function to tie the c's together correctly
          --    a's      b's    cin          nVars  accum   c's    s's
rippleCarry :: [Var] -> [Var] -> Var -> State (Count, CNF) ([Var], [Var])
rippleCarry as bs cin = rippleCarryWorker (zip (reverse as) (reverse bs)) cin [] []

                 --   [as, bs]       cin    cAccum   sAccum          nVars        cAccum sAccum
rippleCarryWorker :: [(Var, Var)] -> Var -> [Var] -> [Var] -> State (Count, CNF) ([Var], [Var])
rippleCarryWorker []   _   cAccum sAccum = return (cAccum, sAccum)
rippleCarryWorker asbs cin cAccum sAccum =  do
                             let a = fst $ head asbs
                             let b = snd $ head asbs
                             (c, s) <- fullAdder a b cin
                             let newCs = cAccum ++ [c]
                             let newSs = sAccum ++ [s]
                             rippleCarryWorker (tail asbs) c newCs newSs







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
