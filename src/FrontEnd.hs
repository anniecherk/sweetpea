module FrontEnd
( Design(..), HLAST(..), HLLabelTree(..), HLConstraint(..)
, makeBlock, hlToIl, ilToll, buildCNF, produceCNF, fullyCrossSize, runExperiment )
where

import Data.List (transpose, nub)
import DataStructures
import Control.Monad.Trans.State


data HLBlock = HLBlock { hlnumTrials :: Int
                       , hldesign :: Design
                       , hlconstraints :: [HLConstraint]
                       } deriving(Show)

type Design = [HLLabelTree]
data HLLabelTree = NTNode String [HLLabelTree] | LeafNode String deriving(Show)

type HLAST = [HLBlock]

data HLConstraint =  HLAssertEq Int [String] --TODO: the list of string rep, what does that mean?
                   | HLAssertLt Int [String]
                   | HLAssertGt Int [String]
                   | Consistency
                   | FullyCross deriving(Show)

-- list of IL blocks
type ILAST = [ILBlock]

-- so a block that occupies: 0, 1, 2, 3 the startAddr is 0 & the endAddr is 3
data ILBlock = ILBlock { ilnumTrials :: Int
                      , ilstartAddr :: Int
                      , ilendAddr :: Int
                      , ildesign :: Design
                      , ilconstraints :: [HLConstraint]
                      } deriving(Show)


--- Transformation time!
data LLConstraint = AssertEq Int [Var] | AssertLt Int [Var]
                 | AssertGt Int [Var] | OneHot [Var]
                 | Entangle Var [Var] deriving(Show)

-- list of IL blocks : get the total num allocated by looking at last block
type LLAST = [LLConstraint]






-- complicatedColor = NTNode "color" [(NTNode "darkcolor" [(LeafNode "black"), (LeafNode "blue")]), (NTNode "lightcolor" [(LeafNode "blue"), (LeafNode "pink")])]
-- complicatedShape = NTNode "shape" [(LeafNode "circle"), (LeafNode "square"), (LeafNode "triangle")]

-- color = NTNode "color" [(LeafNode "red"), (LeafNode "blue")]
-- shape = NTNode "shape" [(LeafNode "circle"), (LeafNode "square")]
-- design = [color, shape]
-- block = makeBlock (fullyCrossSize design) [color, shape] [FullyCross]
-- ast = [block]
-- produceCNF ast
--
-- execState (hlToIl ast >>= ilToll >>= buildCNF) emptyState
-- runState (hlToIl ast >>= ilToll) emptyState
-- runState (hlToIl ast) emptyState
--
--
-- ilBlock = ILBlock {ilnumTrials = 4, ilstartAddr = 1, ilendAddr = 16, ildesign = [NTNode "color" [LeafNode "red",LeafNode "blue"],NTNode "shape" [LeafNode "circle",LeafNode "square"]], ilconstraints = [Consistency,FullyCross]}
-- numTrials = 4
-- states = transpose $ chunkify [17..32] 4
-- levels = getShapedLevels ilBlock


runExperiment :: HLAST -> (Int, CNF)
runExperiment ast = execState (hlToIl ast >>= ilToll >>= buildCNF) emptyState

produceCNF :: HLAST -> CNF
produceCNF ast = snd $ runExperiment ast

makeBlock :: Int -> Design -> [HLConstraint] -> HLBlock
makeBlock numTs des consts = HLBlock numTs des (Consistency : consts)

-- tests
-- countLeaves color == 4
-- countLeaves shape == 3
-- countLeaves (LeafNode "a") == 1
countLeaves :: HLLabelTree -> Int
countLeaves (NTNode _ children) = foldl (\acc x -> acc + countLeaves x) 0 children
countLeaves (LeafNode _) = 1

totalLeavesInDesign :: Design -> Int
totalLeavesInDesign = foldl (\acc x -> acc + countLeaves x) 0

-- only tells you the NUMBER of TRIALS
fullyCrossSize :: Design -> Int
fullyCrossSize factors = numStates
  where numStates = foldl (\acc x -> acc * countLeaves x) 1 factors


-- Transformation time!
hlToIl :: HLAST -> State (Int, CNF) ILAST
hlToIl = mapM allocateVars


allocateVars :: HLBlock ->  State (Int, CNF) ILBlock
allocateVars (HLBlock numTrials design constraints) = do
      startAddr <- getFresh
      let trialSize = totalLeavesInDesign design
      let endAddr = startAddr + (numTrials * trialSize) - 1
      putFresh endAddr
      return $ ILBlock numTrials startAddr endAddr design constraints


-- Transformation time!
ilToll :: ILAST -> State (Count, CNF) LLAST
ilToll ilast = do llblocks <- mapM ilBlockToLLBlocks ilast
                  return $ concat llblocks


ilBlockToLLBlocks :: ILBlock -> State (Count, CNF) LLAST
ilBlockToLLBlocks block@(ILBlock _ _ _ _ constraints) = do
  llblocks <- mapM (`desugarConstraint` block) constraints
  return $ concat llblocks


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
  let states = transpose $ chunkify stateVars numTrials
  let levels = getShapedLevels block
  let entanglements = concatMap (uncurry entangleFC) $ zip states levels
  let entangleConstraints = map (uncurry Entangle) entanglements -- #2
  let oneHotConstraints = map OneHot states -- #3
  return $ oneHotConstraints ++ entangleConstraints

entangleFC :: [Int] -> [[Int]] -> [(Int, [Int])]
entangleFC states levels = zip states (sequence levels)




-- chunks a list into chunkSize sized chunks
chunkify :: [Int] -> Int -> [[Int]]
chunkify [] _ = []
chunkify inList chunkSize = take chunkSize inList : chunkify (drop chunkSize inList) chunkSize


trialConsistency :: ILBlock -> [LLConstraint]
trialConsistency block = map OneHot allLevelPairs
  where allLevelPairs = concatMap sequence $ getShapedLevels block

getShapeVars :: Int -> [Int] -> [[Int]]
getShapeVars start trialShape = reverse $ snd $ foldl (\(count, acc) x-> (count+x, [count..(count+x-1)]:acc)) (start, []) trialShape

getShapedLevels :: ILBlock -> [[[Int]]]
getShapedLevels (ILBlock numTrials start end design _) = levelGroups
  where trialSize = totalLeavesInDesign design
        trialShape = map countLeaves design
        levelGroups = map (`getShapeVars` trialShape) [start, (trialSize+1).. end]

-- Transformation time!
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
