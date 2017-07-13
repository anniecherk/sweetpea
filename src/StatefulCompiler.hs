module StatefulCompiler
(CNF )
where

import Control.Monad.Trans.State
import Control.Monad (replicateM)
import System.Random

-- AND of ORs
type Count = Int
type Var = Int
type CNF = [[Var]]


-- High Level Functions (Assert ==, <, >)

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



 -- -- k < n  === k + (-n) < 0
 -- -- k is the desiredCount
 -- -- n is the output of popcount of the inList

subtract' :: [Int] -> [Int] -> State (Count, CNF) ()
subtract' k n = error "too busy for subtraction"

 -- subtract' :: [Int] -> [Int] -> Int -> CNF
 -- subtract' k n nVars = subtractionCnf ++ assertion
 --   where (nCnf, twosCompN) = toNegTwosComp n nVars
 --         newNVars = maximum twosCompN -- we're not going to talk about how kludgy this is
 --         -- zero pad twosCompK until it's the same size as twosCompN
 --         zeroPadding = [(newNVars+1)..(length twosCompN - length k + newNVars)]
 --         twosCompK = zeroPadding ++ k --prepend zeropadding
 --         kCnf = map (\x -> [-x]) zeroPadding --"and" the new vars in their negative form
 --
 --         rcCin = 1 + maximum zeroPadding
 --         rcCNF = [[-rcCin]]
 --
 --         finalNumberNVars = rcCin + 1 --this is the max after everything else is done, in need of refactor
 --         -- add them with a ripple carry
 --         -- accum c's s's                 a's      b's    cin     nVars            accum
 --         (subtractionCnf, cs, ss) = rippleCarry twosCompN twosCompK rcCin finalNumberNVars (nCnf ++ kCnf ++ rcCNF)
 --         -- assert that the top bit (the top carry out) is a 1 (meaning the number is negative in 2's comp)
 --         assertion = [[-1 * maximum cs]]
 --
 --


 -- https://courses.cs.vt.edu/csonline/NumberSystems/Lessons/SubtractionWithTwosComplement/index.html
 -- prepend a "1" to make it negative, flip the bits, & add one
 -- inList is the a list of the bits of the number in binary to take the 2's comp form of
 -- "ss" is the final neg 2's comp variable
toNegTwosComp :: [Int] -> State (Count, CNF) [Int]
toNegTwosComp inList = do  ----- NEGATE & FLIP THE BITS --------
      -- get new vars, 1 more than inList so we can set the top bit
      flippedBitsVars <- replicateM (1 + length inList) getFresh
      appendCNF [[head flippedBitsVars]] -- sets top bit to "one" to negate
      -- flip the bits, ie assert freshVar_i iff ~inputVar_i
      appendCNF $ concat $ zipWith doubleImplies (tail flippedBitsVars) (map (\x -> -x) inList)
      ------- GET THE "ONE" TO ADD ----------------
      -- make a zero padded one (for the addition) of the right dimensions
      oneVars <- replicateM (1 + length inList) getFresh
      -- set all the top bits to 0, the bottom bit to 1, ie 0001
      appendCNF $ map (\x -> [-x]) (init oneVars) ++ [[last oneVars]]
      ------- ADD EM ------------------------------
      (_, ss) <- rippleCarry flippedBitsVars oneVars
      return ss




 -- toNegTwosComp :: [Int] -> Int -> (CNF, [Int])
 -- toNegTwosComp input nVars = (cnf, ss)
 --   -- one more var than before so we can set the high bit
 --   where flippedBitsVars = [(nVars+1).. (length input + nVars + 1)]
 --         -- "prepend a 1" by "anding" it on the the CNF
 --         leadingOneCNF = [[head flippedBitsVars]]
 --         -- flip the bits, ie assert freshVar_i iff ~inputVar_i
 --         flippedBitsCNF = concat $ zipWith doubleImplies (tail flippedBitsVars) input
 --         -- make a zero padded one
 --         oneVars = [(length input + nVars + 2).. ((2*length input) + nVars + 2)] --TODO: aaaand it's time for the state monad
 --         cin = 1 + maximum oneVars
 --       -- accum c's s's                     a's      b's    cin nVars       accum
 --         (cnf, _, ss) = rippleCarry flippedBitsVars oneVars cin cin (leadingOneCNF ++ flippedBitsCNF ++ [[-cin]])






-------------
getFresh :: State (Count, CNF) Count
getFresh =  do (numVars, x) <- get
               put (numVars + 1, x)
               return (numVars + 1)

appendCNF :: CNF -> State (Count, CNF) ()
appendCNF newEntry = do (x, accum) <- get
                        put (x, newEntry ++ accum)
                        return ()

-------------

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
                       appendCNF [[-cin]]
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
                     appendCNF $ map (\x -> [-x]) auxList -- make sure we add all the fake 0'd out aux vars to the cnf...
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
                                              let formattedResult = maximum cs : reverse ss
                                              popCountCompute as bs (formattedResult : resultVars)




-------------------------------
-- BACKEND

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
------------
