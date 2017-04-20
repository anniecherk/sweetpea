module Main where

import Compiler
import Testers
import System.Environment
import System.Directory

-- doTest :: String -> SATResult
-- doTest file = do
--   result <- readFile file
--   testResult result 1
--
--
-- validate :: SATResult -> String
-- validate Correct = ""
-- validate Unsatisfiable = "oh no, Unsatisfiable!"
-- validate WrongResult x y = "expected " ++ show x ++ " but got " ++ show y
-- validate ParseError = "oh no, parse error!"



main :: IO ()                                                 -- zipping index for file names
main = do
    args <- getArgs
    if length args < 1
    then putStrLn "use commandline arg <generate n> to generate popCount tests of strings of length n \n or  commandline arg <test> to test all files in popCountResults directory."
    else
      if head args == "generate"
      then do
        let popCountLength = head (tail args)
        mapM_ (\(i, x) -> writeFile ("popCountTests/" ++ popCountLength ++ "_popCounter" ++ "_" ++ show i ++ ".cnf") x) $ zip [0..] $ popCountDIMACS (read popCountLength ::Int)
        putStrLn "Done generating tests"
      else do
        fileList <- getDirectoryContents "./popCountResults"
        -- tail . tail gets rid of . & ..
      --  map (doTest . ("./popCountResults/" ++)) $ (tail . tail) fileList
        putStrLn "Done testing"


  -- TODO: switch these on command line args

  -- --HALF ADDER (they dont have sol's)
  -- mapM_ (\(i, x) -> writeFile ("generated_tests/halfAdder_" ++ show i ++ ".cnf") x) $ zip [0..] testHalfAdderDIMACS
  -- -- FULL ADDER
  -- mapM_ (\(i, x) -> writeFile ("generated_tests/fullAdder_" ++ show i ++ ".cnf") x) $ zip [0..] testFullAdderDIMACS
  -- mapM_ (\(i, x) -> writeFile ("generated_tests/fullAdder_" ++ show i ++ ".sol") x) $ zip [0..] solnFullAdder
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
