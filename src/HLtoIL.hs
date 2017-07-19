module HLtoIL
-- (Variables)
-- ( decodeHL_IR, decodeFactorPaths, decodeRawConstraint
--   , FactorPath, FactorPaths, FullyCross(..), RawConstraint(..), HL_IR(..) )
where

import ParserDS
import qualified Data.Map as M
import Data.Maybe

import DataStructures




-- experiment (Block (fully-crossed design) theConstraints)
--        where
--         color  = Stream "color" ["red", "blue"]
--         shape  = Stream "shape" ["circle", "square"]
--         noConstraints = Constraints NoConstraints
--         design = cross [color, shape]











-- get something like

-- HL_IR
--   [["color","darkColors","black"],["color","darkColors","pink"],
--   ["color","lightColors","white"],["color","lightColors","pink"],["shape","circle"],
--   ["shape","square"],["shape","triangle"]]
--
--   (FullyCross [["shape"],["color"]] 2)
--
  -- [NoMoreThanKOutOfJ [["color","lightColors","pink"]] 3 4
  -- , AtLeastKOutOfJ [["shape","circle"]] 2 7,
  -- NoMoreThanKInARow [["color","lightColors","pink"]] 3]

-- need to make a list of [cnf] variables of the right length

-- use fully cross to construct the cells in FactorPaths
-- "allocate" new variables & store them in lookup
-- fill in levelSizes and objectSize

-- grab *all* the level-specific

--
--
-- type Variable = Int
-- type Variables = [Variable]
--
-- type LevelCollection = [Variable]
--
-- -- maybe this is in the next step?
-- -- note: lists of strings is a bad key urgh
-- type CellToVariableDict = M.Map FactorPath Variable
-- type LevelToVariablesDict = M.Map FactorPath Variables
--
-- data ILConstraint =
--   ILAssertCorrectAmount    Variables Int | --this is for like "we have 3 red balls"
--   ILNoMoreThanKOutOfJ  Variables Int Int | -- rep "max in a row" here too
--   ILAtLeastKOutOfJ     Variables Int Int | -- rep "min in a row" here too
--   ILBalanceTransitions Variables Variable deriving (Show, Eq)
-- --
-- -- data IL_IR = IL_IR { cellLookup :: CellToVariableDict -- this is how the NAME of a cell translates to a variable
-- --                    , importantVars :: Variables -- important in that these are the ones we'll care about interpretting after SAT
-- --                    , levelSizes :: [Int]
-- --                    , objectSize :: Int -- sum of level sizes
-- --                    , levelLookup :: LevelToVariablesDict
-- --                    , constraints :: [IL_Constraint]
-- --                    } deriving (Show, Eq)
--
-- type ILIR = [ILConstraint]
--
--
-- -- given a fully formed HLIR it produces a fully formed ILIR
-- translateHLtoIL :: HLIR -> ILIR
-- translateHLtoIL (HLIR factorPaths fullyCross rawConstraints) =
--   concatMap (`getILConstraint` levelLookUp) rawConstraints
--   where levelSizes = getLevelSizes factorPaths fullyCross
--         objectSize = getObjectSize levelSizes
--         importantVars = getImportantVars objectSize
--         cellLookUp = getCellLookUp factorPaths importantVars
--         numObjects = totalNumberObjects objectSize fullyCross
--         -- CellToVariableDict -> LevelToVariablesDict
--         levelLookUp = getLevelLookUp factorPaths numObjects importantVars
--         --TODO!!! also "assert correct number" constraints!!
--
--
--         -- getLevelLookUp :: FactorPaths -> Int -> CellToVariableDict -> LevelToVariablesDict
--         -- getLevelLookUp allPaths numObjects importantVars = M.empty
--         --
--         -- --This is the heavy lifting
--         -- getIL_Constraint :: RawConstraint -> LevelToVariablesDict -> IL_Constraint
--
-- -- for fully crossing
-- -- will we ever fully cross on anything other than the factor level?
-- -- assuming no- but
-- -- TODO: the fullycrossed datastructure is set up for "yes"
-- getLevelSizes :: FactorPaths -> FullyCross -> [Int]
-- getLevelSizes allPaths (FullyCross toCross _) =
--   map (\y -> length $ filter id $ map (\x -> head x == head y) allPaths) toCross
-- -- for unit testing:
-- -- allPaths = [["color","darkColors","black"],["color","darkColors","pink"], ["color","lightColors","white"],["color","lightColors","pink"],["shape","circle"], ["shape","square"],["shape","triangle"]]
-- -- toCross = [["shape"],["color"]]
--
-- -- allPaths2 = [["color","darkColors","black"],["color","darkColors","pink"],["color","lightColors","white"],["color","lightColors","pink"],["shape","circle"],["shape","square"],["shape","triangle"],["temperature","warm"],["temperature","cold"]]
--
-- -- this defines a "unit"
-- getObjectSize :: [Int] -> Int
-- getObjectSize = sum
--
-- -- important in that these are the ones we'll care about interpretting after SAT
-- getImportantVars :: Int -> Variables
-- getImportantVars objectSize = [1..objectSize]
--
-- totalNumberObjects :: Int -> FullyCross -> Int
-- totalNumberObjects objectSize (FullyCross _ numReps) = objectSize * numReps
--
-- -- this is how the NAME of a cell translates to a variable
-- -- TODO: partial paths? for later
-- -- TODO: either use in getLevelLookup or cut this function
-- getCellLookUp :: FactorPaths -> Variables -> CellToVariableDict
-- getCellLookUp allPaths importantVars = M.fromList $ zip allPaths importantVars
--
--
--
-- -- allPaths = [["color","darkColors","black"],["color","darkColors","pink"], ["color","lightColors","white"],["color","lightColors","pink"],["shape","circle"], ["shape","square"],["shape","triangle"]]
-- -- importantVars = [1..7]
--
--
-- -- a dictionary of all partial paths to all the variables they map to
-- -- value is a LIST of variables (as opposed to getCellLookup where value is a VARIABLE)
-- -- TODO: partial paths: I think what we actually mean is an "OR" so wait on it
-- -- switch to CellToVariableDict for partial paths?
-- getLevelLookUp :: FactorPaths -> Int -> Variables -> LevelToVariablesDict
-- getLevelLookUp allPaths numObjects importantVars = M.fromList $ zip allPaths levels
--   where objectSize = length importantVars
--         levels = map (\x -> [x,(x+objectSize)..(numObjects*objectSize)]) importantVars
--
-- --HACK: good god fix this
-- unwrapLevel :: FactorPath -> LevelToVariablesDict -> Variables
-- unwrapLevel applyPath levelLookUp = fromMaybe [] ( M.lookup applyPath levelLookUp)
--
--   -- case M.lookup applyPath levelLookUp of
--   -- Just vars -> vars
--   -- Nothing -> []
--
--
--
-- --This is the heavy lifting
-- -- [NoMoreThanKOutOfJ [["color","lightColors","pink"]] 3 4
-- -- , AtLeastKOutOfJ [["shape","circle"]] 2 7,
-- -- NoMoreThanKInARow [["color","lightColors","pink"]] 3]
-- -- need to return IL_Constraint *List* because we allow shorthand of applying constraints over many levels
-- getILConstraint :: RawConstraint -> LevelToVariablesDict -> [ILConstraint]
-- getILConstraint (NoMoreThanKInARow applyPaths k) levelLookUp
--   = map (\x-> ILNoMoreThanKOutOfJ (unwrapLevel x levelLookUp) k k) applyPaths
-- getILConstraint (AtLeastKInARow     applyPaths k) levelLookUp
--   = map (\x-> ILAtLeastKOutOfJ (unwrapLevel x levelLookUp) k k) applyPaths
-- getILConstraint (NoMoreThanKOutOfJ  applyPaths k j) levelLookUp
--   = map (\x-> ILNoMoreThanKOutOfJ (unwrapLevel x levelLookUp) k j) applyPaths
-- getILConstraint (AtLeastKOutOfJ     applyPaths k j) levelLookUp
--   = map (\x-> ILAtLeastKOutOfJ (unwrapLevel x levelLookUp) k j) applyPaths
-- -- getIL_Constraint (BalanceTransitions applyPath) levelLookUp = IL_NoMoreThanKOutOfJ [0] 0 0 TODO: transitions are hard
--
--
-- --
-- -- data IL_Constraint =
-- --   IL_AssertCorrectAmount    Variables Int | --this is for like "we have 3 red balls"
-- --   IL_NoMoreThanKOutOfJ  Variables Int Int | -- rep "max in a row" here too
-- --   IL_AtLeastKOutOfJ     Variables Int Int | -- rep "min in a row" here too
-- --   IL_AtLeastKOutOfJ Variables Variable deriving (Show, Eq)
--
-- -- -- resolving the ints...
-- -- data RawConstraint =
-- --   NoMoreThanKInARow  FactorPaths Int |
-- --   AtLeastKInARow     FactorPaths Int |
-- --   NoMoreThanKOutOfJ  FactorPaths Int Int |
-- --   AtLeastKOutOfJ     FactorPaths Int Int |
-- --   BalanceTransitions FactorPaths
