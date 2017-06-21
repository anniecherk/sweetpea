module IL_to_LL
-- ( decodeHL_IR, decodeFactorPaths, decodeRawConstraint
--   , FactorPath, FactorPaths, FullyCross(..), RawConstraint(..), HL_IR(..) )
where

import Parser
import HL_to_IL
import qualified Data.Map as M


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




data CompilerOp = LessThan | GreaterThan | Exactly
type LL_Constraint = (CompilerOp, Variables)
type LL_IR =[LL_Constraint]

desugarConstraints :: IL_Constraint -> LL_IR
desugarConstraints il_constraint = [(LessThan, [0])]
