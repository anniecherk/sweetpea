module StatefulCompiler
( emptyState, initState
-- debugging vv
, getFresh
-- debugging ^^
, assertKofN, kLessThanN, kGreaterThanN
, halfAdder, fullAdder, rippleCarry, popCount
, andCNF )
where

import Control.Monad.Trans.State
import Control.Monad (replicateM)
import System.Random

import DataStructures

-- these are defined in DataStructures.hs, just here for reference
-- -- AND of ORs
-- type Count = Int
-- type Var = Int
-- type CNF = [[Var]]

------------------------------------------------------------------
---------- Helpful State Abstractions ----------------------------

emptyState :: (Count, CNF)
emptyState = (1, [])

-- if we need to start with variables 1-maxVar
initState :: Int -> (Count, CNF)
initState maxVar = (maxVar, [])

getFresh :: State (Count, CNF) Count
getFresh =  do (numVars, x) <- get
               put (numVars + 1, x)
               return (numVars + 1)

appendCNF :: CNF -> State (Count, CNF) ()
appendCNF newEntry = do (x, accum) <- get
                        put (x, newEntry ++ accum)
                        return ()

zeroOut :: [Var] -> State (Count, CNF) ()
zeroOut inList = appendCNF $ map (\x -> [-x]) inList

setToOne :: Var -> State (Count, CNF) ()
setToOne x = appendCNF [[x]]

setToZero :: Var -> State (Count, CNF) ()
setToZero x = appendCNF [[-x]]


------------------------------------------------------------------
---------- High Level Functions (Assert ==, <, >) ----------------

-- return the number "in binary" where True is 1 and False is -1
-- ie, 2 => [1, -1]
-- 8 => [1, -1, -1, -1]
toBinary :: Int -> [Int] -> [Int]
toBinary input acc
  | input == 0 = acc
  | even input = toBinary (quot input 2) ((-1):acc)
  | otherwise  = toBinary (quot input 2) (1:acc)



-- asserts that the total multibit "sum" value out of popcount
-- IS K (in binary), this requires left padding w. 0's for correct comparison
-- (assertion made by adding those double implications to CNF accumulator)
assertKofN :: Int -> [Var] -> State (Count, CNF) ()
assertKofN k inList = do sumBits <- popCount inList
                         let inBinary = toBinary k [] -- tricky! right-pad w. -1's, then reverse, take the right amount, reverse
                         let leftPadded = reverse $ take (length sumBits) (reverse inBinary ++ repeat (-1))
                         let assertion = zipWith (*) leftPadded sumBits
                         appendCNF $ map (:[]) assertion

kLessThanN :: Int -> [Var] -> State (Count, CNF) ()
kLessThanN = inequality True

kGreaterThanN :: Int -> [Var] -> State (Count, CNF) ()
kGreaterThanN = inequality False

-- TODO: use LL parser DS here instead of String
inequality :: Bool -> Int -> [Var] -> State (Count, CNF) ()
inequality isLessThan kInt inList = do
    popCountSum <- popCount inList
    kVars <- replicateM kInt getFresh
    appendCNF $ map (:[]) $ zipWith (*) kVars $ toBinary kInt []
    if isLessThan
    then subtract' kVars popCountSum
    else subtract' popCountSum kVars

 -- -- k < n  === k + (-n) < 0
 -- -- k is the desiredCount
 -- -- n is the output of popcount of the inList
subtract' :: [Var] -> [Var] -> State (Count, CNF) ()
subtract' k n = do
  twosCompN <- toNegTwosComp n
  -- zero pad twosCompK until it's the same size as twosCompN
  zeroPadding <- replicateM (length twosCompN - length k) getFresh
  zeroOut zeroPadding -- this updates our CNF constraints to make sure those vars are actually 0
  let twosCompK = zeroPadding ++ k --prepend zeropadding
  -- add em up
  (cs, _) <- rippleCarry twosCompN twosCompK
  -- assert that the top bit (the top carry out) is a 1 (meaning the number is negative in 2's comp)
  setToOne $ maximum cs --TODO:OVERFLOW IN TWOS COMP: the top carry bit? not top sum bit?



 -- https://courses.cs.vt.edu/csonline/NumberSystems/Lessons/SubtractionWithTwosComplement/index.html
 -- prepend a "1" to make it negative, flip the bits, & add one
 -- inList is the a list of the bits of the number in binary to take the 2's comp form of
 -- "ss" is the final neg 2's comp variable
toNegTwosComp :: [Int] -> State (Count, CNF) [Int]
toNegTwosComp inList = do  ----- NEGATE & FLIP THE BITS --------
      -- get new vars, 1 more than inList so we can set the top bit
      flippedBitsVars <- replicateM (1 + length inList) getFresh
      setToOne $ head flippedBitsVars -- sets top bit to "one" to negate
      -- flip the bits, ie assert freshVar_i iff ~inputVar_i
      appendCNF $ concat $ zipWith doubleImplies (tail flippedBitsVars) (map (\x -> -x) inList)
      ------- GET THE "ONE" TO ADD ----------------
      -- make a zero padded one (for the addition) of the right dimensions
      oneVars <- replicateM (1 + length inList) getFresh
      -- set all the top bits to 0, the bottom bit to 1, ie 0001
      zeroOut $ init oneVars
      setToOne $ last oneVars
      ------- ADD EM ------------------------------
      (_, ss) <- rippleCarry flippedBitsVars oneVars
      return ss



------------------------------------------------------------------
---------- Adders ----------------------------
halfAdder :: Var -> Var -> State (Count, CNF) (Var, Var)
halfAdder a b = do c <- getFresh
                   s <- getFresh
                   appendCNF $ computeC c a b
                   appendCNF $ computeS s a b
                   return (c, s)
                where computeC :: Var -> Var -> Var -> CNF
                      computeC c a b = cImpliescVal ++ cValImpliesC
                         where cVal = andCNF [a, b]
                               cNegVal = nAndCNF a b
                               cImpliescVal = distribute (-c) cVal
                               cValImpliesC = distribute c cNegVal
                      computeS :: Var -> Var -> Var -> CNF
                      computeS s a b = sImpliescVal ++ sValImpliesS
                         where sVal = xorCNF a b
                               sNegVal = xNorCNF a b
                               sImpliescVal = distribute (-s) sVal
                               sValImpliesS = distribute s sNegVal


-- http://www.dsm.fordham.edu/~moniot/Classes/CompOrganization/binary-adder/node6.html
-- creates 2 new variables & 16 clauses.
       --     a      b     cin       (nVars  accum)  c    s
fullAdder :: Var -> Var -> Var -> State (Count, CNF) (Var, Var)
fullAdder a b cin = do cout <- getFresh
                       s <- getFresh
                       appendCNF $ computeC cout a b cin
                       appendCNF $ computeS s    a b cin
                       return (cout, s)
                    where computeC :: Var -> Var -> Var -> Var -> CNF
                          computeC c x y z = cImpliescVal ++ cValImpliesC
                            where cVal = [[x, y], [x, z], [y, z]]  -- see adder-notes for derivations
                                  cNegVal = [[-x, -y], [-x, -z], [-y, -z]]
                                  cImpliescVal = distribute (-c) cVal
                                  cValImpliesC = distribute c cNegVal
                          computeS :: Var -> Var -> Var -> Var -> CNF
                          computeS s x y z = sImpliescVal ++ sValImpliesC
                            where sVal = [[-x, -y, z], [-x, y, -z], [x, -y, -z], [x, y, z]] -- see adder-notes for derivations
                                  sNegVal = [[-x, -y, -z], [-x, y, z], [x, -y, z], [x, y, -z]]
                                  sImpliescVal = distribute (-s) sVal
                                  sValImpliesC = distribute s sNegVal

-- http://www.dsm.fordham.edu/~moniot/Classes/CompOrganization/binary-adder/node7.html
-- Ripple Carry! Wooh!
-- Just a recursive function to tie the c's together correctly
          --    a's      b's           nVars  accum   c's    s's
rippleCarry :: [Var] -> [Var] -> State (Count, CNF) ([Var], [Var])
rippleCarry as bs = do cin <- getFresh
                       setToZero cin
                       rippleCarryWorker (zip (reverse as) (reverse bs)) cin [] []
                           --   [as, bs]       cin    cAccum   sAccum          nVars        cAccum sAccum
    where rippleCarryWorker :: [(Var, Var)] -> Var -> [Var] -> [Var] -> State (Count, CNF) ([Var], [Var])
          rippleCarryWorker []   _   cAccum sAccum = return (cAccum, sAccum)
          rippleCarryWorker asbs cin cAccum sAccum =  do
                                     let a = fst $ head asbs
                                     let b = snd $ head asbs
                                     (c, s) <- fullAdder a b cin
                                     let newCs = cAccum ++ [c]
                                     let newSs = sAccum ++ [s]
                                     rippleCarryWorker (tail asbs) c newCs newSs


-- -- See! notes/how_to_zeropad_on_popcount.txt for algorithm
-- returns the list that represents the bits of the "sum" variable in binary
popCount :: [Var] -> State (Count, CNF) [Var]
popCount [] = error "Why did you call popcount with an empty list?"
popCount inList = do let nearestLargestPow = ceiling $ logBase 2 $ fromIntegral $ length inList --pad out with 0's to a power of 2
                     auxList <- replicateM (2^nearestLargestPow - length inList) getFresh -- grab that many fresh vars
                     zeroOut auxList -- make sure we add all the fake 0'd out aux vars to the cnf...
                     popCountLayer $ map (: []) (inList ++ auxList)


-- performs a "layer" of popcount (see example in notes/how_to_zeropad_on_popcount.txt)
popCountLayer :: [[Var]] -> State (Count, CNF) [Int]
popCountLayer [] = error "Why did you call popcountlayer with an empty list?"
popCountLayer [x] = return x
popCountLayer bitList = do let halfWay = quot (length bitList) 2
                           let firstHalf = take halfWay bitList
                           let secondHalf = drop halfWay bitList
                           varList <- popCountCompute firstHalf secondHalf []
                           popCountLayer varList


-- -- EXPECTS TWO LISTS OF THE SAME LENGTH!
-- takes in the two "numbers" in binary to add together
-- returns lists of the output variables
popCountCompute :: [[Var]] -> [[Var]] -> [[Var]]-> State (Count, CNF) [[Var]]
popCountCompute [] [] accum = return accum
popCountCompute (a:as) (b:bs) resultVars = do (cs, ss) <- rippleCarry a b
                                              let formattedResult = formatSum cs ss
                                              popCountCompute as bs (formattedResult : resultVars)


formatSum :: [Var] -> [Var] -> [Var]
formatSum cs ss = maximum cs : reverse ss


------------------------------------------------------------------
---------- Helper Functions --------------------------------------

-- (a or ~b) and (~a or b)
doubleImplies :: Int -> Int -> CNF
doubleImplies a b = [[a, -b], [-a, b]]

-- a <=> (b and c)  = (-a v b) ^ (-a v c) ^ (a v -b v -c)
-- Thanks wolfram alpha :heart: (see note: wolfram_doubleimplies.txt)
aDoubleImpliesBandC :: Int -> Int -> Int -> CNF
aDoubleImpliesBandC a b c = [[-a, b], [-a, c], [a, -b, -c]]

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
