module Main where

import Compiler

main :: IO ()
main = putStr $ showCNF [[1, 2, 3], [4, 5, 6]]
