module Main where

import FrontEnd
import FrontEndSugar
import CodeGen
import DataStructures



main :: IO ()
main = putStrLn "temp2" -- (showDIMACS cnf nVars)
  where  -- TODO: miniblock size
    task         = Factor "task"        [Level "color-task", Level "motion-task"]
    color        = Factor "color"       [Level "blue", Level "red"]
    motion       = Factor "motion"      [Level "up", Level "down"]
    correlation  = Factor "correlation" [Level "correlated", Level "uncorrelated"]

--
--     responseTransition = Factor "response-transition"
--                           [ transition "repeat" $ Derivation (==) (response, 0) (response, 1)
--                           , transition "switch" $ Derivation (/=) (response, 0) (response, 1)]
--
--     taskTransition = Factor "task_transition"
--                           [ transition "repeat" $ Derivation (==) (task, 0) (task, 1)
--                           , transition "switch" $ Derivation (/=) (task, 0) (task, 1)]
--
-- --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--     -- stimulus is color + motion
--     -- RESPONSE: left or right
--     -- if color task, then response is left if color is blue, right if color is red
--     -- if motion tsk, then response is left if motion is up,  right if motion is down
--
--
--     responseLeft task color motion = (task == "color-task" && (color=="blue")) ||
--                                      (task == "motion-task" && (motion=="up"))
--
--     responseRight = ! responseLeft
--
--     response = Factor "response" [ withinTrial "left" $
--                                     Derivation responseLeft  (task, 0) (color, 0) (motion, 0)
--                                  , withinTrial "right" $
--                                     Derivation responseRight (task, 0) (color, 0) (motion, 0)
--                                  ]
-- --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--     -- CONGRUENCY
--     -- congruent if (motion is up && color is blue) or (motion is down && color is red)
--     -- incngruent if (motion is up && color is red) or (motion is down && color is blue)
--     congruent color motion = (motion == "up" && color == "blue") or (motion == "down" && color == "red")
--     incongruent = ! congruent
--
--     congruency = Factor "congruency" [ quality "cngrnt" $
--                                         Derivation congruent   (color, 0) (motion, 0)
--                                      , quality "incong" $
--                                         Derivation incongruent (color, 0) (motion, 0)
--                                      ]
-- --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--     -- Irrelvant feature transition : can't be counterbalanced (shouldn't be able to)
--     -- repeat if ((task == color) && motion[1] == motion[0])
--     --        or ((task == motion) && color[1] == color[0])
--     -- switch is the opposite
--     repeatIFT task color0 color1 motion0 motion1 = if task == "color"
--                                                    then motion0 == motion1
--                                                    else color0  == color1
--     switchIFT = ! repeatIFT
--
--     irr_feat_trans = Factor "irr_feat_trans"
--             [ transition "repeat" $
--               Derivation repeatIFT (task, 0) (color, 0) (color, 1) (motion, 0) (motion, 1)
--             , transition "switch" $
--               Derivation switchIFT (task, 0) (color, 0) (color, 1) (motion, 0) (motion, 1)
--             ]
-- --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
--
--     design       = [ task, taskTransition
--                    , response, responseTransition
--                    , correlation, congruency
--                    , irrelevantFeatureTransition
--                    , color, motion
--                    ]
-- -- no more than 7 in a row: task & also response
--     constraints = [ NoMoreThanKInARow 7 ["task", "color-task"]
--                   , NoMoreThanKInARow 7 ["task", "motion-task"] ]
--
--     crossing     = [0, 1, 2, 3, 4, 5, 6]
--     block        = fullyCrossedBlock design crossing [] --constraints
--     experiment   = [block]
--     (nVars, cnf) = synthesizeTrials experiment
--
--













    --    response     = Factor "response" [Level "left", Level "right"]
    --    congruency   = Factor "congruency" [Level "incongruent", Level "congruent"]
    --    irrelevantFeatureTransition = Factor "irrelevant-feature-transition" [Level "repeat", Level "switch"]
    --    miniblock_size = Factor("miniblock_size", [4, 5, 6])
