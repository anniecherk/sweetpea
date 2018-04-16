module Main where

import FrontEnd
import FrontEndSugar
import CodeGen
import DataStructures



main :: IO ()
main = putStrLn (showDIMACS cnf nVars)
  where
    response     = Factor "response" [Level "left", Level "right"]
    task         = Factor "task" [Level "color-task", Level "motion-task"]
    color        = Factor "color" [Level "blue", Level "red"]
    motion       = Factor "motion" [Level "up", Level "down"]
    correlation  = Factor "correlation" [Level "correlated", Level "uncorrelated"]
    congruency   = Factor "congruency" [Level "incongruent", Level "congruent"]
    irrelevantFeatureTransition = Factor "irrelevant-feature-transition"
                                    [Level "repeat", Level "switch"]
    -- miniblock_size = Factor("miniblock_size", [4, 5, 6])

    -- stimulus is color + motion
    -- RESPONSE
    -- if color task, then response is left if color is blue, right if color is red
    -- if motion tsk, then response is left if motion is up,  right if motion is down

    -- CONGRUENCY
    -- congruent if (motion is up && color is blue) or (motion is down && color is red)
    -- incngruent if (motion is up && color is red) or (motion is down && color is blue)

    -- Irrelvant feature transition : can't be counterbalanced (shouldn't be able to)
    -- repeat if ((task == color) && motion[1] == motion[0])
    --        or ((task == motion) && color[1] == color[0])
    -- switch is the opposite


    responseTransition = Factor "response-transition"
                          [ transition "repeat" $ Derivation (==) (response, 0) (response, 1)
                          , transition "switch" $ Derivation (/=) (response, 0) (response, 1)]

    taskTransition = Factor "task_transition"
                          [ transition "repeat" $ Derivation (==) (task, 0) (task, 1)
                          , transition "switch" $ Derivation (/=) (task, 0) (task, 1)]


    design       = [ task, taskTransition
                   , response, responseTransition
                   , correlation, congruency
                   , irrelevantFeatureTransition
                   , color, motion
                   ]
-- no more than 7 in a row: task & also response
    constraints = [ NoMoreThanKInARow 7 ["task", "color-task"]
                  , NoMoreThanKInARow 7 ["task", "motion-task"] ]

                  -- crossing 4 factors (0 through 3) takes about 30 seconds
                  -- crossing 5 factors (0 through 4) takes about

    crossing     = [0, 1, 2, 3, 4, 5, 6]
    block        = fullyCrossedBlock design crossing [] -- constraints
    experiment   = [block]
    (nVars, cnf) = synthesizeTrials experiment
