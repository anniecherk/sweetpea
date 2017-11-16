module Main where

import FrontEnd
import FrontEndSugar
import CodeGen
import DataStructures


main :: IO ()
main = putStrLn (showDIMACS cnf nVars)
  where

    color = Factor "color" [Level "red",    Level "blue"]
    shape = Factor "shape" [Level "circle", Level "square"]
    size  = Factor "size"  [Level "small",  Level "big"]

    -- colorTransitions = Transition color

    -- constraints  = [Balance colorTransitions]

    design       = [color, shape, size]
    crossing     = design `remove` [size] --NOTE to self: assumes crossing is in the same ORDER as design
    block        = fullyCrossedBlock design crossing [] -- constraints
    experiment   = [block]
    (nVars, cnf) = synthesizeTrials experiment



-- transitions

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
