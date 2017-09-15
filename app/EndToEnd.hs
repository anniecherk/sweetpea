module Main where
  
import FrontEnd
import CodeGen
import DataStructures


main :: IO ()
main = do let color = NTNode "color" [LeafNode "red", LeafNode "blue"]
          let shape = NTNode "shape" [LeafNode "circle", LeafNode "square"]
          let design = [color, shape]
          let block = makeBlock (fullyCrossSize design) [color, shape] [FullyCross]
          let ast = [block]
          let (nVars, cnf) = runExperiment ast
          putStrLn $ showDIMACS cnf nVars
