module Compiler
( CNF, showCNF
)
where








-- AND of ORs
type CNF = [[Int]]
data ClauseSummary = State CNF Int deriving (Show, Eq)

showCNF :: CNF -> String
showCNF cnf = foldl(\acc andClause -> acc ++ (foldl (\acc or1 -> acc ++ " " ++ show or1) "" andClause) ++ "\n") "" cnf



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
-- halfAdder :: CNF -> CNF -> ClauseSummary -> ClauseSummary
-- halfAdder [] = []
-- halfAdder [[]] = error "matched on list of lists"



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
