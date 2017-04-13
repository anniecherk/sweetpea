module Compiler
( CNF, showDIMACS, showCNF, halfAdder, parseResult, testRippleCarryDIMACS, solnRippleCarry, rippleCarryAsBsCin, rippleCarryAsBsCinList
, andCNF, testHalfAdderDIMACS, testFullAdderDIMACS, solnFullAdder, computeSolnFullAdder, rippleCarry
, popCountCompute, popCountLayer, popCount-- "exploration"
)
where

import Data.List -- for zip4
import Data.Tuple.Select








-- numDigs = 1
-- numVars = 1 + (4*numDigs)
-- mapM (\x -> [x, -x]) [1..numVars]





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


-- setting up popcount!
-- track state in rec calls
popCount :: [Int] -> (CNF, [Int]) -- or CNF idk which is better
popCount [] = ([[]], [])
popCount inList = popCountLayer bitList nVars accum
  where nearestLargestPow = ceiling $ logBase 2 $ fromIntegral $ length inList --pad out with 0's to a power of 2
        auxList = [(length inList + 1).. 2^nearestLargestPow]
        bitList = map (: []) (inList ++ auxList)
        accum = map (\x -> [-x]) auxList
        nVars = length bitList


popCountLayer :: [[Int]] -> Int -> CNF -> (CNF, [Int])
popCountLayer [] _ res = ([], [])
popCountLayer [x] _ cnf = (cnf, x)
popCountLayer bitList nVars accum = popCountLayer var_list newNVars $ layerRes ++ accum -- pass in empty accum b.c pCC uses the accum for it's own recursive call; this is the init call
  where halfWay = quot (length bitList) 2
        firstHalf = take halfWay bitList
        secondHalf = drop halfWay bitList
        (layerRes, var_list) = popCountCompute firstHalf secondHalf nVars (accum, [])
        newNVars = maximum $ concat var_list
        --binaryResult = head cs : ss

-- EXPECTS TWO LISTS OF THE SAME LENGTH!
popCountCompute :: [[Int]] -> [[Int]] -> Int -> (CNF, [[Int]]) -> (CNF, [[Int]])
popCountCompute [] [] nVars accum = accum
popCountCompute (a:as) (b:bs) nVars (accum, res_vars_in) = popCountCompute as bs newNVars (res, formattedResult : res_vars_in)
  where c_in = nVars + 1
        (res, cs, ss) = rippleCarry a b c_in c_in accum
        newNVars = nVars + length cs + length ss
        formattedResult = head cs : ss




-- bind up the result as the FINAL c with the s's and shove these in a list

-- On how to pad out odd length lists:
-- [1, 2, 3]
-- generate a fresh variable: 4
-- [1, 2, 3, 4]
-- AND concat a condition that is -4 (that is, the fresh var is 0)

-- find the length of the list, get it up to an even number

-- [[1], [2], [3], [4], [5], [6], [7], [8], [9]]

-- 1) make it even: get it up to 10
-- [[1], [2], [3], [4], [5], [6], [7], [8], [9], [10]] && -10
-- 1b) pair up & count with 1 bit adders

-- 2) make it even: get it up to 6
-- [[1+2 1+2], [3+4 3+4], [5+6 5+6], [7+8 7+8], [9+10 9+10]]
-- [[A, B], [C, D], [E, F], [G, H], [I, J]] -- these are 2 bit values!!
-- [[A, B], [C, D], [E, F], [G, H], [I, J], [K, L]]  && -K && -L
-- 2b) pair up & count with 2 bit adders

-- these are fresh var names because naming is hard
-- 3) make it even: up to 4
-- [[A, B, C], [D, E, F], [G, H, I]]  -- 3 bit values!!
-- [[A, B, C], [D, E, F], [G, H, I], [J, K, L]] && -J && -K && -L
-- 3b) pair up & count with 4 bit adders

-- these are fresh var names because naming is hard
-- [[A, B, C, D], [E, F, G, H]]  -- 4 bit values!!
-- 4) final add! 5 bit value: [A B C D E] is the result!
-- Note! B C D E are SUM bits while A is the FINAL C bit!!




-- popCount inList = go inList [0] terminLen (length inList) [] --0 is a hack but should show up as an error
--   where terminLen = truncate $ logBase 2 $ fromIntegral $ length inList
--         go :: [Int] -> [Int] -> Int -> Int -> CNF -> (CNF, [Int], [Int])
--         go inputList cs terminationLen nVars accum
--         -- BUG!!!! update this <= to something better
--           | length inputList <= terminationLen = (accum, cs, inputList)
--           -- the new list is the s's out of the ripple carry
--           | otherwise = go ss cs terminationLen nVars res
--           where
--             halfWay = quot (length inList) 2
--             firstHalf = take halfWay inputList
--             secondHalf = drop halfWay inputList
--             (res, cs, ss) = rippleCarry firstHalf secondHalf 0 nVars accum





-- http://www.dsm.fordham.edu/~moniot/Classes/CompOrganization/binary-adder/node7.html
-- Ripple Carry! Wooh!
-- Just a recursive function to tie the c's together correctly
          --    a's      b's    cin    nVars  accum : accum  c's    s's
rippleCarry :: [Int] -> [Int] -> Int -> Int -> CNF -> (CNF, [Int], [Int])
rippleCarry as bs cin nVars accum = go (zip as bs) cin nVars (accum, [], [])
  where go :: [(Int, Int)] -> Int -> Int -> (CNF, [Int], [Int]) -> (CNF, [Int], [Int])
        go []    _    _     resAccum               = resAccum
        go asbs cin' nVar' (cnfList, cList, sList) = go (tail asbs) cRes (nVar' + 2) (newCnfList, newCs, newSs)
          where a = fst $ head asbs
                b = snd $ head asbs
                (cnfRes, cRes, sRes) = fullAdder a b cin' nVar' cnfList
                newCnfList = cnfList ++ cnfRes
                newCs = cList ++ [cRes]
                newSs = sList ++ [sRes]



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
                cNegVal = [[-x, -y], [-x, -z], [-y, -z]]
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
    where
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

testHalfAdderDIMACS :: [String]
testHalfAdderDIMACS = map (`showDIMACS` 4) testHalfAdderConstraints
  where testHalfAdderConstraints :: [CNF]
        testHalfAdderConstraints = map (\x-> adderConstraints ++ andCNF [head x] ++ andCNF [(head . tail) x]) allInputs
          where (adderConstraints, _, _) = halfAdder 1 2 2 []
                allInputs = sequence [[1, -1], [2, -2]] -- 0+0, 0+1, 1+0, 1+1
----



testFullAdderDIMACS :: [String]
testFullAdderDIMACS = map (`showDIMACS` 5) testFullAdderConstraints
  where testFullAdderConstraints :: [CNF]
        testFullAdderConstraints = map (\x-> adderConstraints ++ andCNF (fstX x) ++ andCNF (sndX x) ++ andCNF (thdX x)) allInputs
          where (adderConstraints, _, _) = fullAdder 1 2 3 3 [] -- a b c #vars accum
                allInputs = sequence [[1, -1], [2, -2], [3, -3]] -- generates all 8 input combos (in counting order)
                fstX x = [head x] -- these tease apart the above tuples so we can "and" them as assertions
                sndX x = [x !! 1]
                thdX x = [x !! 2] -- now easier by index :)



-- s SATISFIABLE
-- v a b c_in c s 0
solnFullAdder :: [String]                        -- this formats the list to be space sep'd
solnFullAdder = map (\x -> "s SATISFIABLE\nv " ++ tail (foldl (\acc x-> acc ++ " " ++ show x) "" (computeSolnFullAdder x 4 5)) ++ " 0\n") allInputs
  where allInputs = sequence [[1, -1], [2, -2], [3, -3]] -- generates all 8 input combos (in counting order)


-- sum is positive iff (a+b+c) is odd
-- carry is positive iff (a+b+c) > 2
--                  [a, b, c] -> "a b c_in carry sum"
computeSolnFullAdder :: [Int] -> Int -> Int -> [Int]
computeSolnFullAdder incoming cindex sindex = incoming ++ [c] ++ [s]
  where total = sum $ map (\x -> if x < 0 then 0 else 1) incoming
        c = if total > 1 then cindex else (- cindex)
        s = if odd total then sindex else (- sindex)
-----------

-- RIPPLE CARRY !
testRippleCarryDIMACS :: Int -> [String]
testRippleCarryDIMACS numDigs = map (`showDIMACS` numVars) testRippleCarryConstraints
  where numVars = 1 + (4*numDigs)
        testRippleCarryConstraints :: [CNF]
        testRippleCarryConstraints = map (foldl (\acc y -> acc ++ andCNF [y]) adderConstraints) allInputs
          where (as, bs, cin) = rippleCarryAsBsCin numDigs
                (adderConstraints, _, _) = rippleCarry as bs cin cin [] -- [as] [bs] cin #vars accum
                allInputs = mapM (\x -> [x, -x]) [1..cin] -- generates all input combos (in counting order)


solnRippleCarry :: Int -> [String]            -- tail gets rid of a leading space
solnRippleCarry numDigs = map (\x -> "s SATISFIABLE\nv " ++ tail (foldl (\acc y-> acc ++ " " ++ show y) "" x) ++ " 0\n") result
  where numVars = 1 + (4*numDigs)
        allInputs = map rippleCarryAsBsCinList $ mapM (\x -> [x, -x]) [1..(2*numDigs + 1)]
        (as_in, bs_in, cin_in) = rippleCarryAsBsCin numDigs
        (_, cs, ss) = rippleCarry as_in bs_in cin_in cin_in [] -- [as] [bs] cin #vars accum
        cases = map (\(as, bs, cin) -> zip5 as bs (repeat cin) cs ss) allInputs
        result = map (\x -> go x (sel3 $ head x) []) cases
        -- dont ask questions :(      -- this is the initial cin, need this bc chaining
        go :: [(Int, Int, Int, Int, Int)] -> Int -> [Int] -> [Int]
        go []    _      accum = sortBy (\x y -> compare (abs x) (abs y)) accum
        go todo cin accum = go (tail todo) cout_val $ nub (accum ++ res)
          where (a, b, _, cout_index, s_index) = head todo
                res = computeSolnFullAdder [a, b, cin] cout_index s_index
                cout_val = res !! 3


rippleCarryAsBsCin :: Int -> ([Int], [Int], Int)
rippleCarryAsBsCin numDigs = (as, bs, cin)
  where as = take numDigs [1..]
        bs = drop numDigs $ take (2*numDigs) [1..]
        cin = 2*numDigs + 1


rippleCarryAsBsCinList :: [Int] -> ([Int], [Int], Int)
rippleCarryAsBsCinList inputList = (as, bs, cin)
  where numDigs = quot (length inputList) 2
        as = take numDigs inputList
        bs = drop numDigs $ take (2*numDigs) inputList
        cin = head $ drop (2*numDigs) inputList


----------------
-- Pop Count!
-- popCountDIMACSInstance :: Int -> Int -> [String]
-- popCountDIMACSInstance numDigs numTrue =

  -- map (`showDIMACS` numVars) popCountConstraints
  -- where numVars = 1 + (4*numDigs)
  --       popCountConstraints :: [CNF]
  --       popCountConstraints = map (foldl (\acc y -> acc ++ andCNF [y]) adderConstraints) allInputs
  --         where allInputs = mapM (\x -> [x, -x]) [1..numVars] -- generates all input combos (in counting order)
  --               --popCount
  --               (as, bs, cin) = rippleCarryAsBsCin numDigs
  --               (adderConstraints, _, _) = rippleCarry as bs cin cin [] -- [as] [bs] cin #vars accum



--------- Testing ! ------------------------------------------------------------

parseResult :: String -> CNF
parseResult result = map (map read . init . words . tail) $ lines result
