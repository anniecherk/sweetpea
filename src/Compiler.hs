module Compiler
( CNF, showDIMACS, showCNF, halfAdder, parseResult, testRippleCarryDIMACS, solnRippleCarry, rippleCarryAsBsCin, rippleCarryAsBsCinList
, andCNF, testHalfAdderDIMACS, testFullAdderDIMACS, solnFullAdder, computeSolnFullAdder, rippleCarry -- "exploration"
)
where

import Data.List -- for zip4


-- :m + Data.List
-- numDigs = 2
-- numVars = 1 + (4*numDigs)
-- allInputs = map rippleCarryAsBsCinList $ mapM (\x -> [x, -x]) [1..(2*numDigs + 1)]
-- (as_in, bs_in, cin_in) = rippleCarryAsBsCin numDigs
-- (_, cs, ss) = rippleCarry as_in bs_in cin_in cin_in []
--
-- cases = map (\(as, bs, cin) -> zip5 as bs (repeat cin) cs ss) allInputs
-- res = map (\x -> concatMap (\(a, b, cin, cindex, sindex) -> computeSolnFullAdder [a, b, cin] cindex sindex) x) cases


-- :m + Data.List
-- numDigs = 2
-- numVars = 1 + (4*numDigs)
-- allInputs = map rippleCarryAsBsCinList $ mapM (\x -> [x, -x]) [1..(2*numDigs + 1)]
-- (as_in, bs_in, cin_in) = rippleCarryAsBsCin numDigs
-- (_, cs, ss) = rippleCarry as_in bs_in cin_in cin_in [] -- [as] [bs] cin #vars accum
-- cases = map (\(as, bs, cin) -> zip5 as bs (repeat cin) cs ss) allInputs
-- result = map (\x -> sort $ concatMap (\(a, b, cin, cindex, sindex) -> computeSolnFullAdder [a, b, cin] cindex sindex) x) cases




-- (as, bs, cin) = head allInputs
-- cs = [6, 8]
-- ss = [7, 9]
-- map (\(a, b, cindex, sindex) -> computeSolnFullAdder [a, b, cin] cindex sindex) $ zip4 as bs cs ss












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
popCount :: [Int] -> (CNF, [Int], [Int]) -- or CNF idk which is better
popCount inList = go inList [0] terminLen (length inList) [] --0 is a hack but should show up as an error
  where terminLen = truncate $ logBase 2 $ fromIntegral $ length inList
        go :: [Int] -> [Int] -> Int -> Int -> CNF -> (CNF, [Int], [Int])
        go inputList cs terminationLen nVars accum
          | length inputList == terminationLen = (accum, cs, inputList)
          -- the new list is the s's out of the ripple carry
          | otherwise = go ss cs terminationLen nVars res
          where
            halfWay = quot (length inList) 2
            firstHalf = take halfWay inputList
            secondHalf = drop halfWay inputList
            (res, cs, ss) = rippleCarry firstHalf secondHalf 0 nVars accum





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
-- TODO: generating the as bs input lists might be tricky.. does it matter?
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
        result = map (\x -> sortBy (\x y -> compare (abs x) (abs y)) $ concatMap (\(a, b, cin, cindex, sindex) -> computeSolnFullAdder [a, b, cin] cindex sindex) x) cases
        --result = map (\(as, bs, cin) -> map (\(a, b, cindex, sindex) -> computeSolnFullAdder [a, b, cin] cindex sindex) $ zip4 as bs cs ss) allInputs
                -- we get in a list of numbers that's pos/ neg
                -- we have a key to which numbers are a's, which are b's
                -- match up the correct a's & b's and total them, produce the right c's & s's, then append


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


--------- Testing ! ------------------------------------------------------------

parseResult :: String -> CNF
parseResult result = map (map read . init . words . tail) $ lines result
-- parseResult result = map (map (\y -> read y ::Int ) . (init . words . tail)) $ lines result
