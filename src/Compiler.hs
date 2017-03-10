module Compiler
( CNF, showDIMACS, showCNF, halfAdder, parseResult,
andCNF, testHalfAdderDIMACS -- "exploration"
)
where

import Data.List















-- (a, b, c) = halfAdder 1 2 2 []
-- putStr $ showDIMACS a 4


-- putStr $ showDIMACS (halfAdder 1 2 2 []) 3
-- result <- readFile "generated_cnfs/sample_output.cnf"
-- parseResult result












-- AND of ORs
type CNF = [[Int]]

              -- accum, number variables
data AdderState = State CNF Int


showDIMACS :: CNF -> Int -> String
showDIMACS cnf nVars = "p cnf " ++ show nVars ++ " " ++ show (length cnf)
  ++ "\n" ++ showCNF cnf

showCNF :: CNF -> String
showCNF = foldl(\acc andClause -> acc ++ -- bizarre head/tail splitting because want no leading space
  foldl (\acc or1 -> acc ++ " " ++ show or1) (show $ head andClause) (tail andClause) ++ " 0\n") ""

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



-- http://www.dsm.fordham.edu/~moniot/Classes/CompOrganization/binary-adder/node6.html

        --     a      b    cin    nVars  accum
--fullAdder :: Int -> Int -> Int -> Int -> CNF -> CNF
--fullAdder a b cin nVars accum =


        --     a      b    nVars  accum : accum  c    s
halfAdder :: Int -> Int -> Int -> CNF -> (CNF, Int, Int)
halfAdder a b numVars accum =
    (accum ++
    computeC numVars a b ++
    computeS (numVars+1) a b, numVars+1, numVars+2)



    --   numVars    a      b
computeC :: Int -> Int -> Int -> CNF
computeC numVars a b = cImpliescVal ++ cValImpliesC
  where c  = numVars + 1
        cVal = andCNF [a, b]
        cNegVal = nAndCNF a b
        cImpliescVal = distribute c cVal
        cValImpliesC = distribute (-c) cNegVal

    --   numVars    a      b
computeS :: Int -> Int -> Int -> CNF
computeS numVars a b = sImpliescVal ++ sValImpliesS
  where s  = numVars + 1
        sVal = xorCNF a b
        sNegVal = xNorCNF a b
        sImpliescVal = distribute s sVal
        sValImpliesS = distribute (-s) sNegVal



-- how to test a half adder:
-- generate the constraint: (halfAdder 1 2 2 [])
-- append each possible combo to the CNF (1 0 \n 2 0 \n)
-- get the DIMACS, run it through the solver
-- check if c & s are what we expect


testHalfAdderConstraints :: [CNF]
testHalfAdderConstraints = map (\x-> adderConstraints ++ andCNF [head x] ++ andCNF [(head . tail) x]) allInputs
  where (adderConstraints, _, _) = halfAdder 1 2 2 []
        allInputs = sequence [[1, -1], [2, -2]] -- 0+0, 0+1, 1+0, 1+1


testHalfAdderDIMACS :: [String]
testHalfAdderDIMACS = map (`showDIMACS` 4) testHalfAdderConstraints







parseResult :: String -> CNF
parseResult result = map (map read . init . words . tail) $ lines result
-- parseResult result = map (map (\y -> read y ::Int ) . (init . words . tail)) $ lines result


        -- sID  = numVars + 1
        -- sVal = xorCNF a b


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
