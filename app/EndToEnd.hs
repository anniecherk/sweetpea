module Main where

import FrontEnd
import FrontEndSugar
import CodeGen
import DataStructures


main :: IO ()
main = putStrLn (showDIMACS cnf nVars)
  where

    color = Factor "color" [Level "red", Level "blue"]
    text  = Factor "text"  [Level "red", Level "blue"]

    conLevel  = DerivedLevel  "con" (Equal [color, text])
    incLevel  = DerivedLevel  "inc" (NotEq [color, text])
    conFactor = Factor "congruent?"  [conLevel, incLevel]

    k = 1
    constraints = [NoMoreThanKInARow k ["congruent?", "con"]]

    design       = [color, text, conFactor]
    crossing     = [0, 1]
    block        = fullyCrossedBlock design crossing constraints
    experiment   = [block]
    (nVars, cnf) = synthesizeTrials experiment



    -- color = Factor "color" [Level "red", Level "blue", Level "green"]
    -- shape = Factor "shape" [Level "circle", Level "square"]
    -- size  = Factor "size"  [Level "small",  Level "big"]


-- transitions
-- colorTransitions = Transition color
-- constraints  = [Balance colorTransitions]

-- # block combinators:
-- #  sequence combinators:
-- #    sequence / randomWithReplacement / permutations
-- #    repeat
-- #  mix
-- #  optional # at random add this block, or don't    # null blocks
--
-- # block filler : what's in it?
-- #  sample / fully cross
--
-- # block builder :
-- #  exactly






-- SMALLER EXAMPLE
          -- let color = NTNode "color" [LeafNode "red", LeafNode "blue"]
          --           let design = [color]
          --           let block = makeBlock (fullyCrossSize design) [color] [FullyCross]
          --           let ast = [block]
          --           let (nVars, cnf) = runExperiment ast
          --           putStrLn $ showDIMACS cnf nVars
