module Main where

import FrontEnd
import CodeGen
import DataStructures


main :: IO ()
main = do let color = NTNode "color" [LeafNode "red", LeafNode "blue"]
          let shape = NTNode "shape" [LeafNode "circle", LeafNode "square"]
          let design = [color, shape]
          let numTrials = (fullyCrossSize design)
        --  let constraints = [NoMoreInARow 2 ["color", "red"]]
          let block = makeBlock numTrials design [FullyCross]
          let ast = [block]
          let (nVars, cnf) = runExperiment ast
          putStrLn $ showDIMACS cnf nVars


-- SMALLER EXAMPLE
          -- let color = NTNode "color" [LeafNode "red", LeafNode "blue"]
          --           let design = [color]
          --           let block = makeBlock (fullyCrossSize design) [color] [FullyCross]
          --           let ast = [block]
          --           let (nVars, cnf) = runExperiment ast
          --           putStrLn $ showDIMACS cnf nVars
