module Main where

import FrontEnd
import FrontEndSugar

main :: IO ()
main = do let color = Factor "color" [Level "red  |", Level "blue |"]
          let text  = Factor "text"  [Level "red  |", Level "blue |"]
          let width = 2
          let stride = 1
          let conLevel  = DerivedLevel "con" width stride (Derivation (==) (color,0) (text,0))
          let incLevel  = DerivedLevel "inc" width stride (Derivation (/=) (color,0) (text,0))
          -- conLevel  = DerivedLevel  "con" (equal color text)
          -- incLevel  = DerivedLevel  "inc" (notEq color text)
          let conFactor = Factor "congruent?"  [conLevel, incLevel]

          let design       = [color, text, conFactor]

          let nTrials = 4

          contents <- readFile "ex.res"
        --  fileName <- getLine
        --  contents <- readFile fileName
          putStrLn $ "\n" ++ decode contents design nTrials




          -- main = do let color = Factor "color"   [Level "red  ", Level "blue "]
          --           let text  = Factor "| text"  [Level "red  ", Level "blue "]
          --           let width = 2
          --           let stride = 1
          --           let colorTransitions = Factor "| transitions"
          --                               [DerivedLevel "repeat" width stride $ Derivation (==) (color, 0) (color, 1),
          --                                DerivedLevel "switch" width stride $ Derivation (/=) (color, 0) (color, 1)]
          --
          --
          --
          --           let design       = [color, text, colorTransitions]
          --
          --           let nTrials = 4
          -- TODO for the love of god subprocess calls



          -- let color = Factor "color" [Level "red", Level "blue", Level "green"]
          -- let shape = Factor "shape" [Level "circle", Level "square"]
          -- let size  = Factor "size"  [Level "small",  Level "big"]
          --
          -- let nTrials = 6
          --
          -- let design = [color, shape, size]
