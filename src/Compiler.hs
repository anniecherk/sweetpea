module Compiler
( CNF, ClauseSummary, showCNF
)
where

import Data.List






-- AND of ORs
type CNF = [[Int]]
data ClauseSummary = State CNF Int deriving (Eq)
instance Show ClauseSummary where
  show (State cnf numVars) = "p cnf " ++ show numVars ++ " " ++ show (length cnf)
    ++ "\n" ++ showCNF cnf


showCNF :: CNF -> String
showCNF cnf = foldl(\acc andClause -> acc ++ -- bizarre head/tail splitting because want no leading space
  (foldl (\acc or1 -> acc ++ " " ++ show or1) (show $ head andClause) (tail andClause)) ++ " 0\n") "" cnf

-- wraps in an extra layer: this is just for readabilty
andCNF :: [Int] -> CNF
andCNF input = map (\x -> [x]) input

-- A xor B
-- (A or B) and (-A or -B)
xorCNF :: Int -> Int -> CNF
xorCNF a b = [[a, b], [-a, -b]]

       --     a      b     accum
halfAdder :: (Int -> Int -> CNF) -> CNF
halfAdder a b accum =
  where numVars = (length . nub . concat) accum
        cID  = numVars + 1
        sID  = numVars + 1
        cVal = andCNF [a, b]
        sVal = xorCNF a b


halfAdder _ = []
--     cID = numVars + 1 -- "make" two more vars
--     sID = numVars + 2
--     cVal = And [Var a, Var b]
--     sVal = xOr a b
--     cTrue = [ Var cID, cVal ]
--     cFalse = [ Var (-1*cID), negateSAT cVal ]
--     newConstraints = constraints : negate cTrue : negate cFalse
--                                  : negate sTrue : negate sFalse




-- data Tree a =  Leaf a  |  Tree a :^: Tree a
-- the derived instance of Show is equivalent to
--
-- instance (Show a) => Show (Tree a) where
--
--        showsPrec d (Leaf m) = showParen (d > app_prec) $
--             showString "Leaf " . showsPrec (app_prec+1) m
--          where app_prec = 10
--
--        showsPrec d (u :^: v) = showParen (d > up_prec) $
--             showsPrec (up_prec+1) u .
--             showString " :^: "      .
--             showsPrec (up_prec+1) v
--          where up_prec = 5


-- negateSAT :: CNF -> CNF
-- negateSAT [] = []
-- negateSAT [[]] = []
-- negateSAT [x:[]] = [[-1*x]]-- negateSAT (And xs) = Or $ map negateSAT xs
-- negateSAT [xs] = _ -- ??   -- negateSAT (Or xs) = And $ map negateSAT xs
--
--




-- And [SAT] | Or [SAT] | Var Int deriving (Show, Eq) -- | Not SAT
-- data ConstraintState = State [SAT] Int deriving (Show, Eq)-- but actually only ORs (bc CNF is an AND or ORs)
-- -- Int is the number of vars
--
-- negateSAT :: SAT -> SAT
-- negateSAT (Var x) = Var $ -1*x
-- negateSAT (And xs) = Or $ map negateSAT xs
-- negateSAT (Or xs) = And $ map negateSAT xs
--
-- -- but only takes vars- is this the wrong design?
-- halfAdder :: SAT -> SAT -> ConstraintState -> ConstraintState
-- halfAdder (Var a) (Var b) (State constraints numVars) = (State newConstraints $ numVars + 2)
--     where
--     cID = numVars + 1 -- "make" two more vars
--     sID = numVars + 2
--     cVal = And [Var a, Var b]
--     sVal = xOr a b
--     cTrue = [ Var cID, cVal ]
--     cFalse = [ Var (-1*cID), negateSAT cVal ]
--     newConstraints = constraints : negate cTrue : negate cFalse
--                                  : negate sTrue : negate sFalse
-- halfAdder _ _ _ = error "can't put non-vars into halfAdder"
--
-- xOr :: Int -> Int -> SAT
-- xOr a b = Or [(And [a, b]), (And [-1*a, -1*b])]
-- -- (A or B) and (-A or -B)
