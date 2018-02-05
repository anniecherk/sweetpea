module Main where

import FrontEnd
import FrontEndSugar

main :: IO ()
main = do let color = Factor "color" [Level "red", Level "blue"]
          let text  = Factor "text"  [Level "red", Level "blue"]

          --conLevel  = DerivedLevel  "con" (Derivation (==) color text)
          --incLevel  = DerivedLevel  "inc" (Derivation (/=) color text)
          let conLevel  = DerivedLevel  "con" (equal color text)
          let incLevel  = DerivedLevel  "inc" (notEq color text)
          let conFactor = Factor "congruent?"  [conLevel, incLevel]

          let nTrials = 4

          let design = [color, text, conFactor]
          contents <- readFile "ex.res"
        --  fileName <- getLine
        --  contents <- readFile fileName
          putStrLn $ decode contents design nTrials








          -- let color = Factor "color" [Level "red", Level "blue", Level "green"]
          -- let shape = Factor "shape" [Level "circle", Level "square"]
          -- let size  = Factor "size"  [Level "small",  Level "big"]
          --
          -- let nTrials = 6
          --
          -- let design = [color, shape, size]
