module Main where

import FrontEnd
import FrontEndSugar
import CodeGen
import DataStructures


main :: IO ()
main = putStrLn (showDIMACS cnf nVars)
  where
    color = NTNode "color" [LeafNode "red", LeafNode "blue"]
    shape = NTNode "shape" [LeafNode "circle", LeafNode "square"]
    design = [color, shape]
    -- numTrials = fullyCrossSize design -- function to gen fully crossed block: hide this inside
    -- constraints = [FullyCross]        -- function to write multifullycroseed block
    -- block = makeBlock numTrials design constraints

    constraints = [AtLeastKeveryJ 1 4 ["color", "red"]]
    block = fullyCrossedBlock design constraints
    ast = [block]
    (nVars, cnf) = runExperiment ast


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
