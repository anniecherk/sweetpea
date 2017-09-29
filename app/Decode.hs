module Main where

import FrontEnd

main :: IO ()
main = do let color = NTNode "color" [LeafNode "red", LeafNode "blue"]
          let shape = NTNode "shape" [LeafNode "circle", LeafNode "square"]
          let design = [color, shape]
          fileName <- getLine
          contents <- readFile fileName
          putStrLn $ decode contents design
