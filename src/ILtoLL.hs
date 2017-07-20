module ILtoLL
( generateCNF, CountingConstraint(..), LLIR(..))
-- ( decodeHL_IR, decodeFactorPaths, decodeRawConstraint
--   , FactorPath, FactorPaths, FullyCross(..), RawConstraint(..), HL_IR(..) )
where

import Control.Monad.Trans.State
import qualified Data.Map as M
import Data.List (tails)

import ParserDS
import HLtoIL
import DataStructures
import StatefulCompiler


-- useage
-- inList = [1..4]
-- constraints = [GreaterThan 1 inList, LessThan 3 inList]
-- ir = LLIR constraints 4
-- generateCNF ir


-- all the constraints w/ their args & blocks; total # of vars
data LLIR = LLIR [CountingConstraint] Int deriving(Show)


generateCNF :: LLIR -> CNF
generateCNF (LLIR constraints globalVarCount) = snd $ execState (generateCNFWorker constraints) (initState globalVarCount)
    where generateCNFWorker :: [CountingConstraint] -> State (Count, CNF) ()
          generateCNFWorker [] = return ()
          generateCNFWorker (Exactly k block     : xs) = assertKofN    k block >> generateCNFWorker xs
          generateCNFWorker (GreaterThan k block : xs) = kGreaterThanN k block >> generateCNFWorker xs
          generateCNFWorker (LessThan k block    : xs) = kLessThanN    k block >> generateCNFWorker xs





-- processConstraints

-- generateCNF (Exactly k block):xs










-- type CompilerFunc = Int -> [Var] -> State (Count, CNF) ()
-- data LLConstraint = LLConstraint CompilerFunc Int [Var]




-- -- evalState
-- initState :: Int -> (Count, CNF)
-- initState nVars = (nVars, [])









-- data CompilerOp = LessThan Int | GreaterThan Int | Exactly Int
-- type LLConstraint = (CompilerOp, Variables)
-- type LLIR =[LLConstraint]













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
