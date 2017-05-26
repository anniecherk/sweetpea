module Compiler
( CNF, halfAdder, fullAdder, andCNF, rippleCarry, popCountCompute, popCountLayer, popCount,
assertKofN, toNegTwosComp, assertKofwhichN, assertKlessthanN, subtract', nAndCNF
, doubleImplies )
where

-- AND of ORs
type CNF = [[Int]]

-------------------------------
-- FRONTEND












-- inputFactors = [[1, 2, 3], [4, 5]]
-- factorToBalance = 1
-- trialLength = length $ concat inputFactors
-- inList = [1.. (trialLength*(4+1))] -- because 4 for each one to show up once, and +1 because trans = trials - 1. This is the SMALLEST example with 2 levels O_o
-- nVars = 25
--
-- level1 = head $ inputFactors !! factorToBalance
-- level2 = head $ tail $ inputFactors !! factorToBalance
--
--
--
-- --need to "wind back" by trialLength because 0..n-1 not 1..n. Janky janky janky
-- circle = filter (\x -> rem x trialLength == (rem level1 trialLength)) inList
-- square = filter (\x -> rem x trialLength == (rem level2 trialLength)) inList
--
-- -- THESE ARE THE POSITIVE VALUES TODO that is definitely wrong, and is [[x], [y]] brain is melting
-- circleCircle = map (\(x, y) -> [x, y]) $ zip circle (tail circle)
-- squareSquare = map (\(x, y) -> [x, y]) $ zip square (tail square)
-- circleSquare = map (\(x, y) -> [x, y]) $ zip circle (tail square)
-- squareCircle = map (\(x, y) -> [x, y]) $ zip circle (tail square)
--
-- -- THESE ARE THE NEGATIVE VALUES
-- -- map (\(x, y) -> nAndCNF x y) $ zip circle (tail circle)
--
-- CREATE NEW VARS
--
-- -- this is horrible
-- as = [(nVars+1).. (nVars + (length circleCircle))]
-- bs = [(maximum as)+1.. (maximum as) + (length circleCircle)]
-- cs = [(maximum bs)+1.. (maximum bs) + (length circleCircle)]
-- ds = [(maximum cs)+1.. (maximum cs) + (length circleCircle)]

-- -- we're going to get through this, now let's do values

-- -- okay hang in there, now doubleImplies with the values



-- TODO: handle not even multiples of transitions (+/- 1 perhaps)  --> wont that always be true
-- TODO:
-- inputFactors is the key of how to read the relations in the inputList
-- ie, color: red, blue, green | shape: circle, square comes in as
--    [[1, 2, 3], [4, 5]]
-- factorToBalance is the index, ie 0 for color, 1 for shape
-- inList is the flattened list of ALL trials, ie [1.. 5*40] has 40 trials
-- transitionConstraint :: [[Int]] -> Int -> [Int]
-- transitionConstraint inputFactors factorToBalance




-- TODO: possible off-by-one, careful!
assertKofwhichN :: Int -> Int -> Int -> [Int] -> CNF
assertKofwhichN numFactors whichFactor k inList =
  assertKofN k $ filter (\x -> rem x numFactors == whichFactor) inList

-- asserts that the total multibit "sum" value out of popcount
-- IS K (in binary), this requires left padding w. 0's for correct comparison
-- (assertion made by adding those double implications to CNF accumulator)
assertKofN :: Int -> [Int] -> CNF
assertKofN k inList = map (:[]) assertion -- ++ accum
  where (accum, sumBits) = popCount inList
        inBinary = toBinary k []
        leftPadded = reverse $ take (length sumBits) (reverse inBinary ++ repeat (-1))
        assertion = zipWith (*) leftPadded sumBits
        toBinary :: Int -> [Int] -> [Int]
        toBinary input acc
          | input == 0 = acc
          | even input = toBinary (quot input 2) ((-1):acc)
          | otherwise  = toBinary (quot input 2) (1:acc)


------------------------------------------------
-- k < n  === k + (-n) < 0
-- k is the desiredCount
-- n is the output of popcount of the inList
assertKlessthanN :: Int -> [Int] -> CNF
assertKlessthanN desiredCount inList = [[]]


subtract' :: [Int] -> [Int] -> Int -> (CNF, [Int])
subtract' k n nVars = ([[]], [])
  where (cnf, twosCompN) = toNegTwosComp n nVars
        twosCompK = 3 -- NEED TO PASS OUT NVARS FROM TONEGTWOSCOMP <-- it's high time to refactor


-- https://courses.cs.vt.edu/csonline/NumberSystems/Lessons/SubtractionWithTwosComplement/index.html
-- prepend a "1" to make it negative, flip the bits, & add one
-- input is the binary rep of the number to negate
-- nVars is the fresh variable store to thread into the adder (to add one)
-- result is cnf for the adder and new variables, "ss" is the final neg 2's comp variable
toNegTwosComp :: [Int] -> Int -> (CNF, [Int])
toNegTwosComp input nVars = (cnf, ss)
  -- one more var than before so we can set the high bit
  where flippedBitsVars = [(nVars+1).. (length input + nVars + 1)]
        -- "prepend a 1" by "anding" it on the the CNF
        leadingOneCNF = [[head flippedBitsVars]]
        -- flip the bits, ie assert freshVar_i iff ~inputVar_i
        flippedBitsCNF = concat $ zipWith doubleImplies (tail flippedBitsVars) input
        -- make a zero padded one
        oneVars = [(length input + nVars + 2).. ((2*length input) + nVars + 2)] --TODO: aaaand it's time for the state monad
        cin = 1 + maximum oneVars
      -- accum c's s's                     a's      b's    cin nVars       accum
        (cnf, _, ss) = rippleCarry flippedBitsVars oneVars cin cin (leadingOneCNF ++ flippedBitsCNF)



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


-- See! notes/how_to_zeropad_on_popcount.txt for algorithm
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
popCountLayer bitList nVars accum = popCountLayer var_list newNVars layerRes -- pass in empty accum b.c pCC uses the accum for it's own recursive call; this is the init call
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
  where c_in = nVars + 1           -- assert that c_in is 0
        (res, cs, ss) = rippleCarry a b c_in c_in ([-c_in] : accum)
        newNVars = nVars + length cs + length ss + 1 -- 1 for the c_in
        formattedResult = maximum cs :  (reverse ss)



-- http://www.dsm.fordham.edu/~moniot/Classes/CompOrganization/binary-adder/node7.html
-- Ripple Carry! Wooh!
-- Just a recursive function to tie the c's together correctly
          --    a's      b's    cin    nVars  accum : accum  c's    s's
rippleCarry :: [Int] -> [Int] -> Int -> Int -> CNF -> (CNF, [Int], [Int])
rippleCarry as bs cin nVars accum = go (zip (reverse as) (reverse bs)) cin nVars (accum, [], [])
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
