module Main where

import Compiler
import Testers
import System.Environment
import System.Directory
import Data.Char

doTest :: String -> Int -> IO String
doTest file nSetVars= do
--doTest file = do
  result <- readFile file
  -- need to parse the file name! yeesh
--  let nSetVars = (digitToInt . head) file
  return $ validate $ testPopCountResult result nSetVars--1


validate :: SATResult -> String
validate Correct = "Correct!"
validate Unsatisfiable = "oh no, Unsatisfiable!"
validate (WrongResult x y) = "expected " ++ show x ++ " but got " ++ show y
validate ParseError = "oh no, parse error!"


splitOnArgs :: [String] -> IO ()
splitOnArgs args
-- mean to be run with the gen_popcount_results.sh script: it generates files, runs them, reads them back in to be validated
-- example useage: gen_popcount_results.sh 6
-- 6 specifies the length of the sequence we're exhausitvely validating
-- defaults to 2
-- NOTE: popcount is tested by setting the inputs, and validating what they add to!
  | head args == "generatePopCount"
  = do
      let popCountLength = if length args == 2
                           then read (head (tail args)) :: Int
                           else 2
      mapM_ (\(i, x) -> writeFile ("popCountTests/" ++ show popCountLength ++ "_popCounter" ++ "_" ++ show i ++ ".cnf") x) $ zip [0..] $ popCountDIMACS popCountLength
      putStrLn "Done generating tests"
  | head args == "testPopCount"
  = do
      fileList <- getDirectoryContents "./popCountResults"
      -- tail . tail gets rid of . & .. directories
      results <- mapM (\x -> doTest ("./popCountResults/" ++ x) $ (digitToInt . head) x) $ drop 2 fileList
    --  results <- mapM (doTest . ("./popCountResults/" ++)) $ drop 2 fileList
      -- if
      mapM_ putStrLn results
      putStrLn "Done testing"

------------------------------------------------------------------------------------------------------------------------
  | head args == "halfAdder" --HALF ADDER (they dont have sol's: I checked these by hand early on)
  = mapM_ (\(i, x) -> writeFile ("generated_tests/halfAdder_" ++ show i ++ ".cnf") x) $ zip [0..] testHalfAdderDIMACS

------------------------------------------------------------------------------------------------------------------------
-- meant to be run from the run_tests.sh script: it generates all the files, and then diffs them against the SAT result
-- useage: ./run_tests.sh --fulladder
  | head args == "fullAdder" --tests ALL inputs to the full adder
  = do
    mapM_ (\(i, x) -> writeFile ("generated_tests/fullAdder_" ++ show i ++ ".cnf") x) $ zip [0..] testFullAdderDIMACS
    mapM_ (\(i, x) -> writeFile ("generated_tests/fullAdder_" ++ show i ++ ".sol") x) $ zip [0..] solnFullAdder

------------------------------------------------------------------------------------------------------------------------
-- meant to be run from the run_tests.sh script: it generates all the files, and then diffs them against the SAT result
-- useage: ./rippleCarry.sh --ripplecarry 6 // where 6 specifies the length of the sequenece we're exhaustively testing
-- defaults to size 2.. not sure if this is the best design but...
  | head args == "rippleCarry" --tests ALL inputs to the full adder
  = do
    let rippleSize = if length args == 2
                     then read (head (tail args)) :: Int
                     else 2
    mapM_ (\(i, x) -> writeFile ("generated_tests/rippleAdder" ++ show rippleSize ++ "_" ++ show i ++ ".cnf") x) $ zip [0..] $ testRippleCarryDIMACS rippleSize
    mapM_ (\(i, x) -> writeFile ("generated_tests/rippleAdder" ++ show rippleSize ++ "_" ++ show i ++ ".sol") x) $ zip [0..] $ solnRippleCarry rippleSize


------------------------------------------------------------------------------------------------------------------------
  | otherwise = putStrLn $ "I don't know what you want from me! \n You said: " ++
  show args ++ "\nTry one of these cmdln args:\n\
  \generatePopCount [n], testPopCount, halfAdder, fullAdder, rippleCarry [n] \n"


main :: IO ()                                                 -- zipping index for file names
main = do
    args <- getArgs
    if length args < 1
    then putStrLn "use commandline arg <generate n> to generate popCount tests of strings of length n \n or  commandline arg <test> to test all files in popCountResults directory."
    else splitOnArgs args


  -- TODO: switch these on command line args



  -- -- RIPPLE CARRY SIZE 1 : same as a full adder
  -- let rippleSize = 1
  -- mapM_ (\(i, x) -> writeFile ("generated_tests/rippleAdder" ++ show rippleSize ++ "_" ++ show i ++ ".cnf") x) $ zip [0..] $ testRippleCarryDIMACS rippleSize
  -- mapM_ (\(i, x) -> writeFile ("generated_tests/rippleAdder" ++ show rippleSize ++ "_" ++ show i ++ ".sol") x) $ zip [0..] $ solnRippleCarry rippleSize
  -- -- RIPPLE CARRY SIZE 2 : 2 bit adder
  -- let rippleSize = 2
  -- mapM_ (\(i, x) -> writeFile ("generated_tests/rippleAdder" ++ show rippleSize ++ "_" ++ show i ++ ".cnf") x) $ zip [0..] $ testRippleCarryDIMACS rippleSize
  -- mapM_ (\(i, x) -> writeFile ("generated_tests/rippleAdder" ++ show rippleSize ++ "_" ++ show i ++ ".sol") x) $ zip [0..] $ solnRippleCarry rippleSize
  -- -- RIPPLE CARRY SIZE 3 : 3 bit adder!!!
  -- let rippleSize = 3
  -- mapM_ (\(i, x) -> writeFile ("generated_tests/rippleAdder" ++ show rippleSize ++ "_" ++ show i ++ ".cnf") x) $ zip [0..] $ testRippleCarryDIMACS rippleSize
  -- mapM_ (\(i, x) -> writeFile ("generated_tests/rippleAdder" ++ show rippleSize ++ "_" ++ show i ++ ".sol") x) $ zip [0..] $ solnRippleCarry rippleSize


  -- let popCountLength = 2
  -- mapM_ (\(i, x) -> writeFile ("popCountTests/popCounter" ++ show popCountLength ++ "_" ++ show i ++ ".cnf") x) $ zip [0..] $ popCountDIMACS popCountLength
  -- let popCountLength = 3
  -- mapM_ (\(i, x) -> writeFile ("popCountTests/popCounter" ++ show popCountLength ++ "_" ++ show i ++ ".cnf") x) $ zip [0..] $ popCountDIMACS popCountLength
  -- let popCountLength = 4
  -- mapM_ (\(i, x) -> writeFile ("popCountTests/popCounter" ++ show popCountLength ++ "_" ++ show i ++ ".cnf") x) $ zip [0..] $ popCountDIMACS popCountLength
  -- let popCountLength = 5
  -- mapM_ (\(i, x) -> writeFile ("popCountTests/popCounter" ++ show popCountLength ++ "_" ++ show i ++ ".cnf") x) $ zip [0..] $ popCountDIMACS popCountLength



--  return ()


  --writeFile "generated_cnfs/testFile.annie" "hello world\n"



  --putStr $ showCNF [[1, 2, 3], [4, 5, 6]]
