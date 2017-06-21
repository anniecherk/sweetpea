module HL_to_IL
-- (Variables)
-- ( decodeHL_IR, decodeFactorPaths, decodeRawConstraint
--   , FactorPath, FactorPaths, FullyCross(..), RawConstraint(..), HL_IR(..) )
where

import Parser
import qualified Data.Map as M


-- get something like

-- HL_IR
--   [["color","darkColors","black"],["color","darkColors","pink"],
--   ["color","lightColors","white"],["color","lightColors","pink"],["shape","circle"],
--   ["shape","square"],["shape","triangle"]]
--
--   (FullyCross [["shape"],["color"]] 2)
--
--   [NoMoreThanKOutOfJ [["color","lightColors","pink"]] 3 4
--   , AtLeastKOutOfJ [["shape","circle"]] 2 7,
--   NoMoreThanKInARow [["color","lightColors","pink"]] 3]

-- need to make a list of [cnf] variables of the right length

-- use fully cross to construct the cells in FactorPaths
-- "allocate" new variables & store them in lookup
-- fill in levelSizes and objectSize

-- grab *all* the level-specific



type Variable = Int
type Variables = [Variable]

type LevelCollection = [Variable]

-- maybe this is in the next step?
-- note: lists of strings is a bad key urgh
type CellToVariableDict = M.Map FactorPaths Variable
type LevelToVariablesDict = M.Map FactorPaths Variables

data IL_Constraint =
  IL_AssertCorrectAmount    Variables Int | --this is for like "we have 3 red balls"
  IL_NoMoreThanKOutOfJ  Variables Int Int | -- rep "max in a row" here too
  IL_AtLeastKOutOfJ     Variables Int Int | -- rep "min in a row" here too
  IL_BalanceTransitions Variables Variable deriving (Show, Eq)

data IL_IR = IL_IR { cellLookup :: CellToVariableDict -- this is how the NAME of a cell translates to a variable
                   , importantVars :: Variables -- important in that these are the ones we'll care about interpretting after SAT
                   , levelSizes :: [Int]
                   , objectSize :: Int -- sum of level sizes
                   , levelLookup :: LevelToVariablesDict
                   , constraints :: [IL_Constraint]
                   } deriving (Show, Eq)



getLevelSizes :: FactorPaths -> FullyCross -> [Int]
getLevelSizes allPaths whatToCross = [0]

getObjectSize :: [Int] -> Int
getObjectSize = sum

getImportantVars :: Int -> Variables
getImportantVars objectSize = [1..objectSize]

getCellLookUp :: FactorPaths -> Variables -> CellToVariableDict
getCellLookUp allPaths importantVars = M.empty

--TODO: Maybe this should take paths ref'd in HL_constraints?
getLevelLookUp :: FactorPath -> Variables -> LevelToVariablesDict
getLevelLookUp allPaths importantVars = M.empty

--This is the heavy lifting
getIL_Constraint :: RawConstraint -> LevelToVariablesDict -> IL_Constraint
getIL_Constraint rawConstraint levelLookUp = IL_AssertCorrectAmount [0] 0
