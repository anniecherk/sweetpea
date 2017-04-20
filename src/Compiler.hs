module Compiler
( CNF, halfAdder, fullAdder, andCNF, rippleCarry, popCountCompute, popCountLayer, popCount )
where



-- AND of ORs
type CNF = [[Int]]

-------------------------------

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




-- inList = [1.. 3]
-- nearestLargestPow = ceiling $ logBase 2 $ fromIntegral $ length inList --pad out with 0's to a power of 2
-- auxList = [(length inList + 1).. 2^nearestLargestPow]
-- bitList = map (: []) (inList ++ auxList)
-- accum = map (\x -> [-x]) auxList
-- nVars = length bitList




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
        formattedResult = maximum cs : ss



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
