module Compiler
( CNF, halfAdder, fullAdder, andCNF, rippleCarry, popCountCompute, popCountLayer, popCount,
assertKofN
, doubleImplies )
where















-- input = [1, 2, 3, 4]
-- nVars = 4
-- freshVars = [(nVars+1).. ((length input) + nVars + 1)]






-- AND of ORs
type CNF = [[Int]]

-------------------------------
-- FRONTEND

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


subtract :: [Int] -> [Int] -> (CNF, [Int])
subtract k n = ([[]], [])

-- https://courses.cs.vt.edu/csonline/NumberSystems/Lessons/SubtractionWithTwosComplement/index.html
-- prepend a "1" to make it negative, flip the bits, & add one
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




-- (a or ~b) and (~a or b)
doubleImplies :: Int -> Int -> CNF
doubleImplies a b = [[a, -b], [-a, b]]




-- inList = [1.. 9]
-- k = 4
-- (accum, sumBits) = popCount inList




-------------------------------
-- BACKEND

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
