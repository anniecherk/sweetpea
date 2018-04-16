module Main where

import FrontEnd
import FrontEndSugar

main :: IO ()
main = do let task         = Factor "task" [Level "color-task", Level "motion-task"]
          let response     = Factor "response" [Level "left", Level "right"]
          let color        = Factor "color" [Level "blue", Level "red"]
          let motion       = Factor "motion" [Level "up", Level "down"]
          let correlation  = Factor "correlation" [Level "correlated", Level "uncorrelated"]
          let congruency   = Factor "congruency" [Level "incongruent", Level "congruent"]
          let irrelevantFeatureTransition = Factor "irrelevant-feature-transition" [Level "low", Level "high"]
    -- miniblock_size = Factor("miniblock_size", [4, 5, 6])

          let responseTransition = Factor "response-transition"
                          [ transition "repeat" $ Derivation (==) (response, 0) (response, 1)
                          , transition "switch" $ Derivation (/=) (response, 0) (response, 1)]


          let taskTransition = Factor "task_transition"
                          [ transition "repeat" $ Derivation (==) (task, 0) (task, 1)
                          , transition "switch" $ Derivation (/=) (task, 0) (task, 1)]


          let design = [ task, taskTransition
                   , response, responseTransition
                   , correlation, congruency
                   , irrelevantFeatureTransition
                   , color, motion
                   ]


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
