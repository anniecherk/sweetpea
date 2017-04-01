module Main where

import Compiler

main :: IO ()                                                 -- zipping index for file names
main = do --HALF ADDER (they dont have sol's)
  mapM_ (\(i, x) -> writeFile ("generated_tests/halfAdder_" ++ show i ++ ".cnf") x) $ zip [0..] testHalfAdderDIMACS
  -- FULL ADDER
  mapM_ (\(i, x) -> writeFile ("generated_tests/fullAdder_" ++ show i ++ ".cnf") x) $ zip [0..] testFullAdderDIMACS
  mapM_ (\(i, x) -> writeFile ("generated_tests/fullAdder_" ++ show i ++ ".sol") x) $ zip [0..] solnFullAdder
  -- RIPPLE CARRY SIZE 1 : same as a full adder
  let rippleSize = 1
  mapM_ (\(i, x) -> writeFile ("generated_tests/rippleAdder" ++ show rippleSize ++ "_" ++ show i ++ ".cnf") x) $ zip [0..] $ testRippleCarryDIMACS rippleSize
  mapM_ (\(i, x) -> writeFile ("generated_tests/rippleAdder" ++ show rippleSize ++ "_" ++ show i ++ ".sol") x) $ zip [0..] $ solnRippleCarry rippleSize
  -- RIPPLE CARRY SIZE 2 : 2 bit adder
  let rippleSize = 2
  mapM_ (\(i, x) -> writeFile ("generated_tests/rippleAdder" ++ show rippleSize ++ "_" ++ show i ++ ".cnf") x) $ zip [0..] $ testRippleCarryDIMACS rippleSize
  mapM_ (\(i, x) -> writeFile ("generated_tests/rippleAdder" ++ show rippleSize ++ "_" ++ show i ++ ".sol") x) $ zip [0..] $ solnRippleCarry rippleSize
  -- RIPPLE CARRY SIZE 3 : 3 bit adder!!!
  let rippleSize = 3
  mapM_ (\(i, x) -> writeFile ("generated_tests/rippleAdder" ++ show rippleSize ++ "_" ++ show i ++ ".cnf") x) $ zip [0..] $ testRippleCarryDIMACS rippleSize
  mapM_ (\(i, x) -> writeFile ("generated_tests/rippleAdder" ++ show rippleSize ++ "_" ++ show i ++ ".sol") x) $ zip [0..] $ solnRippleCarry rippleSize

  return ()


  --writeFile "generated_cnfs/testFile.annie" "hello world\n"



  --putStr $ showCNF [[1, 2, 3], [4, 5, 6]]
