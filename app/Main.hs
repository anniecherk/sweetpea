module Main where

import Compiler

main :: IO [()]                     -- zipping index for file names
main = mapM (\(i, x) -> writeFile ("generated_halfadder_tests/test_" ++ show i) x) $ zip [0..] testHalfAdderDIMACS


  --writeFile "generated_cnfs/testFile.annie" "hello world\n"



  --putStr $ showCNF [[1, 2, 3], [4, 5, 6]]
