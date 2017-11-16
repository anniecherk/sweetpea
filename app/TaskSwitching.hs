module Main where

import FrontEnd
import FrontEndSugar
import CodeGen
import DataStructures
import Data.List (delete)


main :: IO ()
main = putStrLn $ show resultingSeqs
  where
    task               = Feature "task"                [Level "color task",  Level "motion task"]
    response           = Feature "response"            [Level "left",        Level "right"]
    featureCorrelation = Feature "feature_correlation" [Level "uncorrelated",Level "correlated"]
    congruency         = Feature "congruency"          [Level "incongruent", Level "congruent"]
    irrelevantFeature  = Feature "irrelevant_feature"  [Level "low",         Level "high"]
    color              = Feature "color"               [Level "blue",        Level "red"]
    motion             = Feature "motion"              [Level "up",          Level "down"]

    taskTransition     = Transition task
    responseTransition = Transition response

    design = [task, response,
              taskTransition, responseTransition,
              featureCorrelation, congruency, irrelevantFeature,
              color, motion]

    fullyCrossedFactors = remove [color, motion] design

    constraints = (allLevels task     $ NoMoreThanKInARow 7) ++
                  (allLevels response $ NoMoreThanKInARow 3)

    block = fullyCrossedBlock fullyCrossedFactors design constraints
    experiment = [block]
    resultingSeqs = runExperiment experiment



-- main = putStrLn (showDIMACS cnf nVars)
    -- (nVars, cnf) = runExperiment ast



-- Take 1:
-- hardcoding some things:
-- implictely makes a NTNode* "taskTrans" [LeafNode "c/c", LeafNode "m/m", LeafNode "c/m", LeafNode "m/c"]
-- with a window width of 2, stride of 1.
-- add "transition-bound" contraints between "c/c" and "c_i & c_{i+1}" etc
-- balancing transitions should happen automatically during fully-crossing EXCEPT for the off by oneHotConstraints
-- so, for now:
-- when fully crossing, fully-cross all normal factors, and cross "within 1" for transition factors ||=> oh shit that's not going to work because we're counting states
-- *: notice that the assumption that we have 4 possible transitions is only true b/c task has 2 leaves


-- for the n-1 option we would have to:

--    miniblock_size = NTNode "miniblock_size" [LeafNode 4, LeafNode 5, LeafNode 6])




-- balance task & response transitions TODO
-- bind transitions like we bound the states: should actually be super easy
