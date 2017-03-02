module Compiler
( SAT, negate
)
where

data SAT = And [SAT] | Or [SAT] | Var Int deriving (Show, Eq) -- | Not SAT
data ConstraintState = State [SAT] Int deriving (Show, Eq)-- but actually only ORs (bc CNF is an AND or ORs)
-- Int is the number of vars

negateSAT :: SAT -> SAT
negateSAT (Var x) = Var $ -1*x
negateSAT (And xs) = Or $ map negateSAT xs
negateSAT (Or xs) = And $ map negateSAT xs

-- but only takes vars- is this the wrong design?
halfAdder :: SAT -> SAT -> ConstraintState -> ConstraintState
halfAdder (Var a) (Var b) (State constraints numVars) = (State newConstraints $ numVars + 2)
    where
    cID = numVars + 1 -- "make" two more vars
    sID = numVars + 2
    cVal = And [Var a, Var b]
    sVal = xOr a b
    cTrue = [ Var cID, cVal ]
    cFalse = [ Var (-1*cID), negateSAT cVal ]
    newConstraints = constraints : negate cTrue : negate cFalse
                                 : negate sTrue : negate sFalse
halfAdder _ _ _ = error "can't put non-vars into halfAdder"

xOr :: Int -> Int -> SAT
xOr a b = Or [(And [a, b]), (And [-1*a, -1*b])]
-- (A or B) and (-A or -B)
