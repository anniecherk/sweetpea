module CodeGen
(showDIMACS, showCNF)
where

import DataStructures

hello :: String
hello = "hey there!"


showDIMACS :: CNF -> Int -> String
showDIMACS cnf nVars = "p cnf " ++ show nVars ++ " " ++ show (length cnf)
  ++ "\n" ++ showCNF cnf

showCNF :: CNF -> String
showCNF = foldl(\acc andClause -> acc ++ -- bizarre head/tail splitting because want no leading space
  foldl (\acc or1 -> acc ++ " " ++ show or1) (show $ head andClause) (tail andClause) ++ " 0\n") ""
