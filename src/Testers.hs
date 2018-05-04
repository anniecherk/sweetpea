-- This is an old module left over from an earlier testing framework
-- Leaving it here so we can still test the adders
-- The new tests are in Validator

module Testers
(
  testHalfAdderDIMACS, testFullAdderDIMACS, solnFullAdder
  --- debugging vv
  , exhaust
  --- debugging ^^
  , rippleCarryDIMACS, popCountDIMACS
  , assertKofNDIMACS, assertAllKofNDIMACS
  , popCountKlessthanDIMACS, popCountAllKlessthanNDIMACS
  , testResultPopCount
  , testResultKandN
)
where

import Data.List -- for zip4
import Data.Tuple.Select
import Text.Read (readMaybe)
import Control.Monad
import Control.Monad.Trans.State

import DataStructures
import SweetPeaCore
import CodeGen





--------- Testing ! ------------------------------------------------------------


testHalfAdderDIMACS :: [String]
testHalfAdderDIMACS = map (`showDIMACS` 4) testHalfAdderConstraints
  where testHalfAdderConstraints :: [CNF]
        testHalfAdderConstraints = map (\x-> adderConstraints ++ andCNF [head x] ++ andCNF [(head . tail) x]) allInputs
          where adderConstraints = snd $ execState (halfAdder 1 2) $ initState 2
                allInputs = sequence [[1, -1], [2, -2]] -- 0+0, 0+1, 1+0, 1+1
----

--
--
testFullAdderDIMACS :: [String]
testFullAdderDIMACS = map (`showDIMACS` 5) testFullAdderConstraints
  where testFullAdderConstraints :: [CNF]
        testFullAdderConstraints = map (\x-> adderConstraints ++ andCNF (fstX x) ++ andCNF (sndX x) ++ andCNF (thdX x)) allInputs
          where adderConstraints = snd $ execState (fullAdder 1 2 3) $ initState 3
                allInputs = sequence [[1, -1], [2, -2], [3, -3]] -- generates all 8 input combos (in counting order)
                fstX x = [head x] -- these tease apart the above tuples so we can "and" them as assertions
                sndX x = [x !! 1]
                thdX x = [x !! 2] -- now easier by index :)
--
--
--
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

------------------

------------------

rippleCarryDIMACS :: Int -> [String]
rippleCarryDIMACS numDigs = map ((\ x -> showDIMACS (x ++ cnf) finalNVars) . map (: [])) allInputs
  where (finalNVars, cnf) = execState (rippleCarry [1.. numDigs] [numDigs+1.. numDigs*2]) $ initState numDigs
        allInputs = exhaust [1.. numDigs]

-- Pop Count!
popCountDIMACS :: Int -> [String]
popCountDIMACS numDigs = map ((\ x -> showDIMACS (x ++ cnf) finalNVars) . map (: [])) allInputs
  where (finalNVars, cnf) = execState (popCount [1.. numDigs]) $ initState numDigs
        allInputs = exhaust [1.. numDigs]

-- This generates all possible *assignments* (ie 1 or -1) to the input list
-- so given [1, 2] --> [[1, 2], [1, -2], [-1, 2], [-1, -2]]
exhaust :: [Int] -> [[Int]]
exhaust [] = []
exhaust [x] = [[x], [-x]]
exhaust (x:xs) = concatMap (\ys -> [x:ys, (-x):ys]) (exhaust xs)
--
--
-- EXACTLY k of n need to be true vvv
---
-- this version is for a particular k
-- (ie ask the solver to find an assignmnet where k of n are true)
-- note: there are many soln's to this! the solver just needs to find *a* solution
assertKofNDIMACS :: Int -> Int -> String
assertKofNDIMACS numDigs k = showDIMACS cnf finalNVars
  where (finalNVars, cnf) = execState (assertKofN k [1.. numDigs]) $ initState numDigs

-- this is the exhaustive version
-- asserts every k from 0 to n
assertAllKofNDIMACS :: Int -> [String]
assertAllKofNDIMACS numDigs = map (assertKofNDIMACS numDigs) [0.. numDigs]
--
--
--- AT LEAST k of n need to be true vvv
-- this version is for a particular k
-- (ie ask the solver to find an assignmnet where k of n are true)
-- note: there are many soln's to this! the solver just needs to find *a* solution
popCountKlessthanDIMACS :: Int -> Int -> String
popCountKlessthanDIMACS numDigs k = showDIMACS cnf finalNVars
  where (finalNVars, cnf) = execState (kLessThanN k [1.. numDigs]) $ initState numDigs


-- and this is exhaustive, less tahn
-- asserts every k from 0 to n
popCountAllKlessthanNDIMACS :: Int -> [String] -- NOTE: this doesn't check that fewer than 0 are true (duh)
popCountAllKlessthanNDIMACS numDigs = map (popCountKlessthanDIMACS numDigs) [1.. numDigs]
--


------ System Testing ----------------------------------------------------------

testResultPopCount :: String -> Int -> SATResult
testResultPopCount result setVars
  | numLines == 1 = Unsatisfiable
  | otherwise = correct
  where numLines = length $ lines result
        resVars = evalState (popCount [1.. setVars]) $ initState setVars
        inList = mapM readMaybe . init . concatMap (words . tail) . tail . lines $ result :: Maybe [Int]
        correct = case inList of
          Nothing -> ParseError
          Just x -> popCountCorrectHuh x setVars resVars

popCountCorrectHuh :: [Int] -> Int -> [Int] -> SATResult
popCountCorrectHuh inList setVars resVars
  | nSetBits == resSetBits = Correct
  | otherwise = WrongResult nSetBits resSetBits
  where nSetBits = sum $ map (\x -> if x < 0 then 0 else 1) $ take setVars inList
        resBools = map ((> 0) . (inList !!) . subtract 1) resVars
        resSetBits = foldl (\acc bit -> if bit then acc*2+1 else acc*2) 0 resBools



-------------
testResultKandN :: String -> Int -> Int -> (Int -> Int -> Bool) -> SATResult
testResultKandN result k nSetVars eqOrLessThan
  | numLines == 1 = Unsatisfiable
  | otherwise = correct
  where numLines = length $ lines result
        inList = mapM readMaybe . init . concatMap (words . tail) . tail . lines $ result :: Maybe [Int]
        correct = case inList of
          Nothing -> ParseError
          Just x -> kAndNCorrectHuh x k nSetVars eqOrLessThan

kAndNCorrectHuh :: [Int] -> Int -> Int -> (Int -> Int -> Bool) -> SATResult
kAndNCorrectHuh inList k nSetVars eqOrLessThan
  | eqOrLessThan nSetBits k = Correct
  | otherwise = WrongResult k nSetBits
  where nSetBits = sum $ map (\x -> if x < 0 then 0 else 1) $ take nSetVars inList
