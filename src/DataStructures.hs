module DataStructures
( CNF, Var, Count, SATResult(..) )
where


-- AND of ORs
type Count = Int
type Var = Int
type CNF = [[Var]]


-- for testing!
data SATResult = Correct | Unsatisfiable | WrongResult Int Int | ParseError deriving(Show)
