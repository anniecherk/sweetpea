module Main where

import FrontEnd

main :: IO ()
main = do let color = Factor "color" [Level "red", Level "blue"]
          let shape = Factor "shape" [Level "circle", Level "square"]
          let design = [color, shape]
          fileName <- getLine
          contents <- readFile fileName
          putStrLn $ decode contents design
