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

    width = 2
    stride = 1
    conLevel  = DerivedLevel "con" width stride (Derivation (==) (color,0) (text,0))
    incLevel  = DerivedLevel "inc" width stride (Derivation (/=) (color,0) (text,0))
    -- conLevel  = DerivedLevel  "con" (equal color text)
    -- incLevel  = DerivedLevel  "inc" (notEq color text)
    conFactor = Factor "congruent?"  [conLevel, incLevel]

    design       = [color, text, conFactor]

    k = 1
    constraints = [NoMoreThanKInARow k ["congruent?", "con"]]

    crossing     = [0, 1]
    block        = fullyCrossedBlock design crossing constraints
    experiment   = [block]
    (nVars, cnf) = synthesizeTrials experiment
