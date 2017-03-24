module Compiler
( CNF, showDIMACS, showCNF, halfAdder, parseResult,
andCNF, testHalfAdderDIMACS, testFullAdderDIMACS -- "exploration"
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
-- creates 2 new variables & 16 clauses.
        --     a      b    cin    nVars  accum : accum  c    s
fullAdder :: Int -> Int -> Int -> Int -> CNF -> (CNF, Int, Int)
fullAdder a b cin nVars accum = (sAccum++cAccum, nVars+1, nVars+2) --c is computed first
  where cAccum = computeFullC nVars a b cin
        sAccum = computeFullS (nVars+1) a b cin


        --   numVars    x      y      z
computeFullC :: Int -> Int -> Int -> Int -> CNF
computeFullC nVars x y z = cImpliescVal ++ cValImpliesC
  where c = nVars + 1
        cVal = [[x, y], [x, z], [y, z]]  -- see adder-notes for derivations
        cNegVal = [[x, y], [x, z], [y, z]]
        cImpliescVal = distribute (-c) cVal
        cValImpliesC = distribute c cNegVal


        --   numVars    x      y      z
computeFullS :: Int -> Int -> Int -> Int -> CNF
computeFullS nVars x y z = sImpliescVal ++ sValImpliesC
  where s = nVars + 1
        sVal = [[-x, -y, z], [-x, y, -z], [x, -y, -z], [x, y, z]] -- see adder-notes for derivations
        sNegVal = [[-x, -y, -z], [-x, y, z], [x, -y, z], [x, y, -z]]
        sImpliescVal = distribute (-s) sVal
        sValImpliesC = distribute s sNegVal

---------------


-- creates 2 new variables & 6 clauses...
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
        cImpliescVal = distribute (-c) cVal
        cValImpliesC = distribute c cNegVal


    --   numVars    a      b
computeS :: Int -> Int -> Int -> CNF
computeS numVars a b = sImpliescVal ++ sValImpliesS
  where s  = numVars + 1
        sVal = xorCNF a b
        sNegVal = xNorCNF a b
        sImpliescVal = distribute (-s) sVal
        sValImpliesS = distribute s sNegVal

--------- Testing ! ------------------------------------------------------------
testHalfAdderConstraints :: [CNF]
testHalfAdderConstraints = map (\x-> adderConstraints ++ andCNF [head x] ++ andCNF [(head . tail) x]) allInputs
  where (adderConstraints, _, _) = halfAdder 1 2 2 []
        allInputs = sequence [[1, -1], [2, -2]] -- 0+0, 0+1, 1+0, 1+1

testHalfAdderDIMACS :: [String]
testHalfAdderDIMACS = map (`showDIMACS` 4) testHalfAdderConstraints
----

testFullAdderConstraints :: [CNF]
testFullAdderConstraints = map (\x-> adderConstraints ++ andCNF (fstX x) ++ andCNF (sndX x) ++ andCNF (thdX x)) allInputs
  where (adderConstraints, _, _) = fullAdder 1 2 3 3 [] -- a b c #vars accum
        allInputs = sequence [[1, -1], [2, -2], [3, -3]] -- generates all 8 input combos (in counting order)
        fstX x = [head x] -- these tease apart the above tuples so we can "and" them as assertions
        sndX x = [x !! 1]
        thdX x = [x !! 2] -- now easier by index :)

testFullAdderDIMACS :: [String]
testFullAdderDIMACS = map (`showDIMACS` 4) testFullAdderConstraints


solnFullAdder :: [String]
solnFullAdder = ["not implemented"]


-- sum is positive iff (a+b+c) is odd
-- carry is positive iff (a+b+c) > 2
--                  [a, b, c] -> "v a b c_in carry sum 0"
computeSolnFullAdder :: [Int] -> String
computeSolnFullAdder incoming = "not implemented"


--------- Testing ! ------------------------------------------------------------

parseResult :: String -> CNF
parseResult result = map (map read . init . words . tail) $ lines result
-- parseResult result = map (map (\y -> read y ::Int ) . (init . words . tail)) $ lines result
