module FrontEnd
( Design(..), HLAST(..), HLLabelTree(..), HLConstraint(..)
-- for testing
, enforceOneHot, HLBlock(..), ILBlock(..), LLConstraint(..), LLAST(..)
, countLeaves, totalLeavesInDesign, allocateVars, ilBlockToLLBlocks, desugarConstraint
, llfullyCross, entangleFC, chunkify, trialConsistency, getTrialVars, getShapedLevels
--
, makeBlock, hlToIl, ilToll, buildCNF, fullyCrossSize, runExperiment )
where

import Data.List (transpose, nub)
import DataStructures
import Control.Monad.Trans.State

-- At the "High Level" blocks are non-overlapping SPANS. They know how many trials they contain
-- and what the "design" of the trial is- the design is a tree which you
-- can use to find out things like how many levels there are and how big the trials are
-- They also know what constraints they have applied to them at a high level
data HLBlock = HLBlock { hlnumTrials :: Int
                       , hldesign :: Design
                       , hlconstraints :: [HLConstraint]
                       } deriving(Show, Eq)

-- HLLabelTrees are just factors, and they're used to interpret trials
--     ie [color, [red, blue]]
-- A design is just a list of factors
type Design = [HLLabelTree]
data HLLabelTree = NTNode String [HLLabelTree] | LeafNode String deriving(Show, Eq)

-- The "High Level" AST is just a list of blocks
-- In the future this might also include cross-block constraints
type HLAST = [HLBlock]

-- These are closely related to the constraints that are exposed to the user
data HLConstraint =  HLAssertEq Int [String] --TODO: the list of string rep, what does that mean?
                   | HLAssertLt Int [String]
                   | HLAssertGt Int [String]
                   | Consistency
                   | FullyCross deriving(Show, Eq)

-------------------------
-- The "Intermediate Level" representation just gets variables allocated
-- Transformation time!
type ILAST = [ILBlock]

-- so a block that occupies: 0, 1, 2, 3 the startAddr is 0 & the endAddr is 3
data ILBlock = ILBlock { ilnumTrials :: Int
                      , ilstartAddr :: Int
                      , ilendAddr :: Int
                      , ildesign :: Design
                      , ilconstraints :: [HLConstraint]
                      } deriving(Show, Eq)

-------------------------
-- The "Low Level" representation turns constraints into actionable commands
data LLConstraint = AssertEq Int [Var] | AssertLt Int [Var]
                 | AssertGt Int [Var] | OneHot [Var]
                 | Entangle Var [Var] deriving(Show, Eq)


-- the low level AST is just a list of "commands" to execute
type LLAST = [LLConstraint]

-------------------------------
-- I would have called it go!!! but bang isn't a valid identifier character
runExperiment :: HLAST -> (Int, CNF)
runExperiment ast = execState (hlToIl ast >>= ilToll >>= buildCNF) emptyState

-------------------------------

-- constructor which gloms on consistency constraint to HLBlocks
makeBlock :: Int -> Design -> [HLConstraint] -> HLBlock
makeBlock numTs des consts = HLBlock numTs des (Consistency : consts)

-- reports number of leaf nodes in tree
-- ie, countLeaves (color, (ltcolor, (red, blue)), (dkcolor, (blue, blue))) == 4
countLeaves :: HLLabelTree -> Int
countLeaves (NTNode _ children) = foldl (\acc x -> acc + countLeaves x) 0 children
countLeaves (LeafNode _) = 1

-- reports number of leaf nodes in full design (just a list of trees)
totalLeavesInDesign :: Design -> Int
totalLeavesInDesign = foldl (\acc x -> acc + countLeaves x) 0

-- only tells you the NUMBER of TRIALS (which is the product of all the factor sizes)
fullyCrossSize :: Design -> Int
fullyCrossSize factors = numStates
  where numStates = foldl (\acc x -> acc * countLeaves x) 1 factors


-- Transformation time!
-- HL -> IL is variable allocation
hlToIl :: HLAST -> State (Int, CNF) ILAST
hlToIl = mapM allocateVars

-- updates the State monad to match the number of vars spanned by that block
allocateVars :: HLBlock ->  State (Int, CNF) ILBlock
allocateVars (HLBlock numTrials design constraints) = do
      startAddr <- getFresh
      let trialSize = totalLeavesInDesign design
      let endAddr = startAddr + (numTrials * trialSize) - 1
      putFresh endAddr
      return $ ILBlock numTrials startAddr endAddr design constraints


-- Transformation time!
-- IL -> LL is (HL constraints) -> (LL commands)
ilToll :: ILAST -> State (Count, CNF) LLAST
ilToll = concatMapM ilBlockToLLBlocks

ilBlockToLLBlocks :: ILBlock -> State (Count, CNF) LLAST
ilBlockToLLBlocks block@(ILBlock _ _ _ _ constraints) = concatMapM (`desugarConstraint` block) constraints



-- TODO: desugar the other constraints from HLConstraints to LLConstraints using Design
desugarConstraint :: HLConstraint -> ILBlock -> State (Count, CNF) [LLConstraint]
desugarConstraint Consistency inBlock = return $ trialConsistency inBlock
desugarConstraint FullyCross  inBlock = llfullyCross inBlock
desugarConstraint _ inBlock = error "desugar const not implemented yet"

-- 1. Generate Intermediate Vars
-- 2. Entangle them w/ block vars
-- 3. 1 hot the *states* ie, 1 red circle, etc
llfullyCross :: ILBlock -> State (Count, CNF) [LLConstraint]
llfullyCross block@(ILBlock numTrials start end design _) = do
  stateVars <- getNFresh (numTrials * numTrials) -- #1
  let states = chunkify stateVars numTrials
  let transposedStates = transpose states -- transpose so that we 1-hot each of "the same" state
  let levels = getShapedLevels block
  let entanglements = concatMap (uncurry entangleFC) $ zip states levels
  let entangleConstraints = map (uncurry Entangle) entanglements -- #2
  let oneHotConstraints = map OneHot transposedStates -- #3
  return $ oneHotConstraints ++ entangleConstraints

-- matches up states and levels for binding with iff relationship
-- ie, states = [5, 6, 7, 8]
--     levels = [[1, 2], [3, 4]]
-- entangleFC states newLevels =>
-- [(5,[1,3]),(6,[1,4]),(7,[2,3]),(8,[2,4])]
entangleFC :: [Int] -> [[Int]] -> [(Int, [Int])]
entangleFC states levels = zip states (sequence levels)


-- chunks a list into chunkSize sized chunks
chunkify :: [Int] -> Int -> [[Int]]
chunkify [] _ = []
chunkify inList chunkSize = take chunkSize inList : chunkify (drop chunkSize inList) chunkSize

-- make sure exactly 1 choice of every level is true
trialConsistency :: ILBlock -> [LLConstraint]
trialConsistency block = map OneHot allLevelPairs
  where allLevelPairs = concat $ getShapedLevels block

-- helper for getting shaped levels
-- given a starting index, and a list that says how many levels in each factor, returns one trial
-- ie getTrialVars 1 [2, 2] => [[1, 2], [3, 4]]
getTrialVars :: Int -> [Int] -> [[Int]]
getTrialVars start trialShape = reverse $ snd $ foldl (\(count, acc) x-> (count+x, [count..(count+x-1)]:acc)) (start, []) trialShape

-- returns the level vars, nested by factor then by trial
-- ie if the block has color=r, b & shape=c, s
-- then a nesting might be
-- [ [[1, 2], [3, 4]], [[5, 6], [7, 8]] ]
getShapedLevels :: ILBlock -> [[[Int]]]
getShapedLevels (ILBlock numTrials start end design _) = levelGroups
  where trialSize = totalLeavesInDesign design
        trialShape = map countLeaves design
        levelGroups = map (`getTrialVars` trialShape) [start, (trialSize+1).. end]


-- Transformation time!
-- This actually runs the commands we constructed in the low level
buildCNF :: LLAST -> State (Int, CNF) ()
buildCNF = mapM_ buildCommand

buildCommand :: LLConstraint -> State (Int, CNF) ()
buildCommand (OneHot todo) = enforceOneHot todo
buildCommand (Entangle state levels) = aDoubleImpliesList state levels
buildCommand _ = error "other commands not implemented"


-- mutually exclusive fields actually
-- asserts that only one of the list can be true
-- (a and -b and -c) or (-a and b and c) or (-a and -b and c)... but in CNF
-- which is: (a or b or c) and (-a or -b) and (-b or -c) and (-a or -c)
enforceOneHot :: [Var] -> State (Count, CNF) ()
enforceOneHot inList = do appendCNF [inList] -- this appends (a or b or c)
                          appendCNF $ not_pairs inList -- appends the (-a or -b) and (-b or -c) etc pairs
  where not_pairs xs = nub [[-x, -y] | x <- xs, y <- xs, x < y]


-- why isn't this part of control.monad? No one knows
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM func args = do result <- mapM func args
                          return $ concat result
