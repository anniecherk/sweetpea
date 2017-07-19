module ILtoLL
-- ( decodeHL_IR, decodeFactorPaths, decodeRawConstraint
--   , FactorPath, FactorPaths, FullyCross(..), RawConstraint(..), HL_IR(..) )
where

import qualified Data.Map as M
import Data.List (tails)

import ParserDS
import HLtoIL
import DataStructures




-- type Var = Int
-- data LLIR = [LLConstraints]
-- type CompilerFunc = Int -> [Var] -> State (Count, CNF) ()
-- data LLConstraints = (CompilerFunc, Int, [Var])
--
-- -- evalState
-- initState :: Int -> (Count, CNF)
-- initState nVars = (nVars, [])
--















-- Translate from:
--
-- IL_IR { cellLookup :: CellToVariableDict -- this is how the NAME of a cell translates to a variable
--                    , importantVars :: Variables -- important in that these are the ones we'll care about interpretting after SAT
--                    , levelSizes :: [Int]
--                    , objectSize :: Int -- sum of level sizes
--                    , levelLookup :: LevelToVariablesDict
--                    , constraints :: [IL_Constraints]
--                    }
--
-- to exactly what the compiler needs by desugaring the constraints. HL->IL does most of the heavy lifting
-- use the k & n values to get concrete windows to apply constraints to




-- data CompilerOp = LessThan Int | GreaterThan Int | Exactly Int
-- type LLConstraint = (CompilerOp, Variables)
-- type LLIR =[LLConstraint]
--
--
-- -- data IL_Constraint =
-- --   IL_AssertCorrectAmount    Variables Int | --this is for like "we have 3 red balls"
-- --   IL_NoMoreThanKOutOfJ  Variables Int Int | -- rep "max in a row" here too
-- --   IL_AtLeastKOutOfJ     Variables Int Int | -- rep "min in a row" here too
-- --   IL_BalanceTransitions Variables Variable deriving (Show, Eq)
-- -- --
-- -- TODO: handle balance transitions bc idk what to do now
-- desugarConstraints :: ILConstraint -> LLIR
-- desugarConstraints (ILAssertCorrectAmount vars k) = [(Exactly k, vars)]
-- desugarConstraints (ILNoMoreThanKOutOfJ vars k j) =
--   map (\x-> (GreaterThan k, x)) $ gather j vars
-- desugarConstraints (ILAtLeastKOutOfJ vars k j)    =
--   map (\x-> (LessThan k, x)) $ gather j vars
--
--
-- ilToLl :: ILIR -> LLIR
-- ilToLl = concatMap desugarConstraints
--
-- -- thanks stackoverflow
-- -- https://stackoverflow.com/questions/24599875/is-there-a-built-in-function-to-get-all-consecutive-subsequences-of-size-n-of-a
-- gather :: Int -> Variables -> [Variables]
-- gather n inputList = takeWhile ((== n) . length) $ map (take n) $ tails inputList
