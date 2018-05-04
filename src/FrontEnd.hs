{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiWayIf #-}

module FrontEnd
( Design(..), HLAST(..), HLLabelTree(..), HLConstraint(..), Derivation(..)
-- for testing
, enforceOneHot, HLBlock(..), ILBlock(..), LLConstraint(..), LLAST(..)
-- , HLSet(..)
, crossedFactors, cross
, getLeafNames
, iffDerivations
, iffWithDNFList
, getVarsByName, slidingWindow
, countLeaves, totalLeavesInDesign, allocateVars, ilBlockToLLBlocks, desugarConstraint
, llfullyCross, entangleFC, chunkify, trialConsistency, getTrialVars, getShapedLevels
--
, makeBlock, hlToIl, ilToll, buildCNF, fullyCrossSize
-- multiFullyCrossSize
, leafNamesInDesign, indexOfLevel, getMatchIdxs
, makeHLDerivation, processDerivations
, chunkByWidthAndStride
, synthesizeTrials, decode
)
where

import Data.List (transpose, nub, find, sortBy, groupBy, tails, foldl')
import DataStructures
import Control.Monad.Trans.State
import Text.Read (readMaybe)
import Control.Monad
import SweetPeaCore

-- import Data.Logic.Propositional
-- import Data.Logic.Propositional.NormalForms



-- At the "High Level" blocks are non-overlapping SPANS. They know how many trials they contain
-- and what the "design" of the trial is- the design is a tree which you
-- can use to find out things like how many levels there are and how big the trials are
-- They also know what constraints they have applied to them at a high level
data HLBlock = HLBlock { hlnumTrials   :: Int
                       , hldesign      :: Design
                       , crossingIdxs  :: [Int] -- crossing will either be == hldesign or a subset ordered in the same way
                       , hlconstraints :: [HLConstraint]
                       } deriving(Show, Eq)

-- HLLabelTrees are just factors, and they're used to interpret trials
--     ie [color, [red, blue]]
-- A design is just a list of factors
type Design = [HLLabelTree]
data HLLabelTree = Ignore -- this is a dummy variable for sequence specifications
                 | Factor String [HLLabelTree]
                 | Level String
                 | DerivedLevel String Int Int Derivation deriving (Show, Eq)
                           -- name, width, stride, derivation


-- user defined function between all combos of the levels of the two factors
data Derivation = Derivation (String -> String -> Bool) (HLLabelTree, Int) (HLLabelTree, Int)
instance Show Derivation where
    show (Derivation func factA factB) = "Derivation" ++ show factA ++ show factB
instance Eq Derivation where
    Derivation {} == Derivation {} = False --TODO: might want to fix this, but for now, don't do equality over derived factors

-- The "High Level" AST is just a list of blocks
-- In the future this might also include cross-block constraints
type HLAST = [HLBlock]



-- These are closely related to the constraints that are exposed to the user
data HLConstraint =  NoMoreThanKInARow Int [String]  --HLSet
                   | NoMoreThanKeveryJ Int Int [String]  --HLSet
                   | AtLeastKInARow Int [String]  --HLSet
                   | AtLeastKeveryJ Int Int [String]  --HLSet
                   | ExactlyKInARow Int [String]  --HLSet
                   | ExactlyKeveryJ Int Int [String]  --HLSet
                   | Balance HLLabelTree
                   | Consistency
                   | HLDerivation Int Int [[Int]] Int -- width, stride, indices of the dependent levels & own index
                   | HLTransition [[Int]] Int
                   | FullyCross deriving(Show, Eq)
-- "MultiFullyCross" is fully specified by just having a block with rep * sizefullycross trials
-- we can rederive # reps by looking at numTrials & the design

-- These are for applying constraints to sets
-- data HLSet = Level [String] | SampleWithReplacement [[String]] | SampleWithOutReplacement [[String]] deriving(Show, Eq)

-------------------------
-- The "Intermediate Level" representation just gets variables allocated
-- Transformation time!
type ILAST = [ILBlock]

-- so a block that occupies: 0, 1, 2, 3 the startAddr is 0 & the endAddr is 3
data ILBlock = ILBlock { ilnumTrials   :: Int
                       , ilstartAddr   :: Int
                       , ilendAddr     :: Int
                       , ildesign      :: Design
                       , ilcrossingIdxs:: [Int]
                       , ilconstraints :: [HLConstraint]
                       } deriving(Show, Eq)

-------------------------
-- The "Low Level" representation turns constraints into actionable commands
data LLConstraint = AssertEq Int [DataStructures.Var] | AssertLt Int [DataStructures.Var]
                  | AssertGt Int [DataStructures.Var] | OneHot [DataStructures.Var]
                  | Entangle DataStructures.Var [DataStructures.Var] deriving(Show, Eq)


-- the low level AST is just a list of "commands" to execute
type LLAST = [LLConstraint]

-------------------------------------------------------------
-- I would have called it go!!! but bang isn't a valid identifier character
synthesizeTrials :: HLAST -> (Int, CNF)
synthesizeTrials ast = execState (hlToIl ast >>= ilToll >>= buildCNF) emptyState

-------------------------------------------------------------
-- reports number of leaf nodes in tree
-- ie, countLeaves (color, (ltcolor, (red, blue)), (dkcolor, (blue, blue))) == 4
countLeaves :: HLLabelTree -> Int
countLeaves (Factor _ children) = foldl' (\acc x -> acc + countLeaves x) 0 children
countLeaves (Level _) = 1
countLeaves DerivedLevel{} = 1 --idk...
countLeaves  Ignore = 0

-- reports number of leaf nodes in full design (just a list of trees)
totalLeavesInDesign :: Design -> Int
totalLeavesInDesign = foldl' (\acc x -> acc + countLeaves x) 0


-- flattened version:
-- ["color red", "color blue", "shape circle", "shape square"]
leafNamesInDesignFlat :: Design -> [String]
leafNamesInDesignFlat design = map unwords $ leafNamesInDesign design


-- leafNamesInDesign design
-- [["color", "red"],["color", "blue"],["shape", "circle"],["shape", "square"]]
leafNamesInDesign :: Design -> [[String]]
leafNamesInDesign = concatMap getLeafNames

getLeafNames :: HLLabelTree -> [[String]]
getLeafNames (Factor name children) = foldl' (\acc x -> acc ++ [name : head (getLeafNames x)]) [] children
getLeafNames (Level name) = [[name]]
getLeafNames (DerivedLevel name _ _ _) = [[name]]
getLeafNames  Ignore = []
-- getLeafNames (Transition factor) = undefined -- map (("abc"++) . (concat)) [["def", "asd"], ["bgr"]]


-- goes through and pulls out HL constraints for any derived factors, if any exist
processDerivations :: Design -> [Int] -> [HLConstraint]
processDerivations design crossing = concatMap (\x -> makeHLDerivation x design crossing) design

-- processTransitions :: Design -> [HLConstraint]
-- processTransitions design = undefined



-- for derivations we need to group by level name
--   HLDerivation   [[Int]] Int -- indices of the dependent levels & own index
-- here's a long :( example
-- color = Factor "color" [Level "red", Level "blue"]
-- text  = Factor "text"  [Level "red", Level "blue"]
-- conLevel  = DerivedLevel  "con" (Derivation (==) color text)
-- incLevel  = DerivedLevel  "inc" (Derivation (/=) color text)
-- conFactor = Factor "congruent?"  [conLevel, incLevel]
-- design       = [color, text, conFactor]
-- makeHLDerivation color design  @?=  []
-- makeHLDerivation conFactor design  @?=  [HLDerivation [[0,2],[1,3]] 4,HLDerivation [[0,3],[1,2]] 5]
makeHLDerivation :: HLLabelTree -> Design -> [Int] -> [HLConstraint]
makeHLDerivation = go []
  where go :: [String] -> HLLabelTree -> Design -> [Int] -> [HLConstraint]
  -- case: top-level factor, start off the name train
        go [] (Factor name children) design c = concatMap (\x -> go [name] x design c) children
  -- case: nested factor, just add a name & keep going
        go parName (Factor name children) design c = concatMap (\x -> go (parName ++ [name]) x design c) children
  -- don't care about non-derived levels & ignores
        go _ (Level _) _ _ = []
        go _ Ignore    _ _ = []
        go parName (DerivedLevel name width stride (Derivation func factA factB)) design crossing =
          -- this first call filters the crossing of levels of those two factors to those which satisfy func
          -- the second call gets the index of the derivedLevel
          -- set stride = 1, width = 1 for derived transitions
          [HLDerivation width stride (getMatchIdxs width design crossing factA factB func)
                                     (indexOfLevel (parName ++ [name]) design)]


cross :: [a] -> [a] -> [(a,a)]
cross a b = [(x,y) | (x:ys) <- tails a, y <- b]

-- this essentially "applies the derivation"
-- Here's an unfortunantly long running example
-- color = Factor "color" [Level "red", Level "blue"]
-- text  = Factor "text"  [Level "red", Level "blue"]
-- conLevel  = DerivedLevel  "con" (Derivation (==) color text)
-- incLevel  = DerivedLevel  "inc" (Derivation (/=) color text)
-- conFactor = Factor "congruent?"  [conLevel, incLevel]
-- design       = [color, text, conFactor]
--
-- factA = Factor "color" [Level "red",Level "blue"]
-- factB = Factor "text" [Level "red",Level "blue"]
-- getMatchIdxs 1 design factA factB (==)  @?=  [[0,2],[1,3]]
-- getMatchIdxs 1 design factA factB (/=)  @?=  [[0,3],[1,2]]
getMatchIdxs :: Int -> Design -> [Int] -> (HLLabelTree, Int) -> (HLLabelTree, Int) -> (String -> String -> Bool) -> [[Int]]
getMatchIdxs width design crossing (factA, idxA) (factB, idxB) func = map (\x -> [head x + idxA*trialSize, (x!!1) + idxB*trialSize]) unshiftedIdxs
  where combos = cross (getLeafNames factA) (getLeafNames factB)
        matches = filter (\(x,y) -> func (last x) (last y)) combos
        unshiftedIdxs = map (\(x,y) -> [indexOfLevel x design, indexOfLevel y design]) matches
        trialSize = totalLeavesInDesign design





-- a helper for getMatchIdxs, it's a problem of not having "pointers" into the design
-- so the derivedFactor defines the factors it's derived over, but we need to know their
-- indexes in the overall design. That's what this is figuring out.
-- idxsOfLevelInFullDesign :: String -> Design -> [Int]
-- idxsOfLevelInFullDesign factors design = map (`indexOfLevel` design) (leafNamesInDesign factors)

-- TODO: why is the interface not ["color", "red"]
-- returns the index of a name
-- ie, if the design is ["color", ["red", "blue"]], ["shape" ["circle", "square"]]
-- then the queries / responses are:
-- ["color, red"]    => 0
-- ["color, blue"]   => 1
-- ["shape, circle"] => 2
-- ["shape, square"] => 3
indexOfLevel :: [String] -> Design -> Int
indexOfLevel target design = case result of
                                Just value -> snd value
                                Nothing    -> error $ "That isn't a valid level! target: " ++ show target
    where result = find (\x-> fst x == target) $ zip (leafNamesInDesign design) [0..]

-- returns the indices of all variables that match a particular level
-- ie ["color", "red"] in the testExample is [1,5,9,13]
getVarsByName :: [String] -> ILBlock -> [Int]
getVarsByName target (ILBlock _ start end design cross _) = map (!! indexOfLevel target design) allVars
  where allVars = chunkify [start..end] (totalLeavesInDesign design)
-------------------------------------------------------------

crossedFactors :: Design -> [Int] -> Design
crossedFactors factors = map (\x -> factors !! x)


-- constructor which gloms on consistency constraint to HLBlocks
-- also checks for Derived Levels & adds correct derivation constraints
makeBlock :: Int -> Design -> [Int] -> [HLConstraint] -> HLBlock
makeBlock numTs des crossidxs consts = HLBlock numTs des crossidxs constraints
  where derivationConstraints = processDerivations des crossidxs
        -- transitionConstraints = processTransitions des
        constraints = derivationConstraints ++ (Consistency : consts)


-- only tells you the NUMBER of TRIALS (which is the product of all the factor sizes)
fullyCrossSize :: Design -> [Int] -> Int
fullyCrossSize factors crossingIdxs = numStates
  where numStates = foldl' (\acc x -> acc * countLeaves x) 1 $ crossedFactors factors crossingIdxs

-- HACK
-- only tells you the NUMBER of TRIALS (which is the product of all the factor sizes)
-- multiFullyCrossSize :: Design -> Int -> Int
-- multiFullyCrossSize factors rep = rep * fullyCrossSize factors

-------------------------------------------------------------

-- Transformation time!
-- HL -> IL is variable allocation
hlToIl :: HLAST -> State (Int, CNF) ILAST
hlToIl = mapM allocateVars

-- updates the State monad to match the number of vars spanned by that block
allocateVars :: HLBlock ->  State (Int, CNF) ILBlock
allocateVars (HLBlock numTrials design crossing constraints) = do
      startAddr <- getFresh
      let trialSize = totalLeavesInDesign design
      let endAddr = startAddr + (numTrials * trialSize) - 1
      putFresh endAddr
      return $ ILBlock numTrials startAddr endAddr design crossing constraints

-------------------------------------------------------------
-- Transformation time!
-- IL -> LL is (HL constraints) -> (LL commands)
ilToll :: ILAST -> State (Count, CNF) LLAST
ilToll = concatMapM ilBlockToLLBlocks

ilBlockToLLBlocks :: ILBlock -> State (Count, CNF) LLAST
ilBlockToLLBlocks block@(ILBlock _ _ _ _ _ constraints) = concatMapM (`desugarConstraint` block) constraints


desugarConstraint :: HLConstraint -> ILBlock -> State (Count, CNF) [LLConstraint]
desugarConstraint (HLDerivation width stride toBind index) inBlock = iffDerivations width stride toBind index inBlock
desugarConstraint Consistency inBlock = return $ trialConsistency inBlock
desugarConstraint FullyCross  inBlock = llfullyCross inBlock
desugarConstraint (NoMoreThanKInARow k level) inBlock = return $ noMoreThanInRange k (k+1) level inBlock
desugarConstraint (NoMoreThanKeveryJ k j level) inBlock = return $ noMoreThanInRange k j level inBlock
desugarConstraint (AtLeastKInARow k level) inBlock = return $ noFewerThanInRange k k level inBlock
desugarConstraint (AtLeastKeveryJ k j level) inBlock = return $ noFewerThanInRange k j level inBlock
desugarConstraint (ExactlyKInARow k level) inBlock = return $ exactlyInRange k k level inBlock
desugarConstraint (ExactlyKeveryJ k j level) inBlock = return $ exactlyInRange k j level inBlock
desugarConstraint (Balance factor) inBlock = undefined --TODO clearly
-- desugarConstraint _ inBlock = error "desugar const not implemented yet"


exactlyInRange :: Int -> Int -> [String] -> ILBlock -> [LLConstraint]
exactlyInRange k range level inBlock = map (AssertEq k) levels
  where levels = levelsInRange k range level inBlock

-- no more than 2 in a row, means lt 3 in a row
-- "nomore" is lteq
noMoreThanInRange :: Int -> Int -> [String] -> ILBlock -> [LLConstraint]
noMoreThanInRange k range level inBlock = map (AssertLt (k+1)) levels
  where levels = levelsInRange k range level inBlock

-- no fewer than 2 in a row, means 2+ in a row
-- "nofewer" is gteq
noFewerThanInRange :: Int -> Int -> [String] -> ILBlock -> [LLConstraint]
noFewerThanInRange k range level inBlock = map (AssertGt (k-1)) levels
  where levels = levelsInRange k range level inBlock

-- TODO: error checking on bounds of lt/eq/gt

-- helper function for the above 3 which makes the lists they act on
levelsInRange :: Int -> Int -> [String] -> ILBlock -> [[Int]]
levelsInRange k range level inBlock = slidingWindow range allVarsForLevel
  where allVarsForLevel = getVarsByName level inBlock


-- 1. Generate Intermediate Vars
-- 2. Entangle them w/ block vars
-- 3. 1 hot the *states* ie, 1 red circle, etc
llfullyCross :: ILBlock -> State (Count, CNF) [LLConstraint]
llfullyCross block@(ILBlock numTrials _ _ design crossingIdxs _) = do
  let crossed = crossedFactors design crossingIdxs
  let numStates = fullyCrossSize design crossingIdxs -- updated
  let numReps = div numTrials numStates -- TODO reversing how many reps were in MultiFullyCross; for single fullycross numReps = 1
  stateVars <- getNFresh (numTrials * numStates) -- #1 --debug: stateVars = [1 ..numTrials*numStates]
  let states = chunkify stateVars numStates
  let transposedStates = transpose states -- transpose so that we 1-hot each of "the same" state
  let levels = getShapedOnlyCrossedLevels block --BUG: THIS ISN'T CORRECT WHEN DESIGN != CROSSING
  let entanglements = concatMap (uncurry entangleFC) $ zip states levels
  let entangleConstraints = map (uncurry Entangle) entanglements -- #2
--  let oneHotConstraints = map OneHot transposedStates -- #3
  let oneHotConstraints = map (AssertEq numReps) transposedStates -- #3
  return $ oneHotConstraints ++ entangleConstraints

-- matches up states and levels for binding with iff relationship
-- ie, states = [5, 6, 7, 8]
--     levels = [[1, 2], [3, 4]]
-- entangleFC states newLevels =>
-- [(5,[1,3]),(6,[1,4]),(7,[2,3]),(8,[2,4])]
entangleFC :: [Int] -> [[Int]] -> [(Int, [Int])]
entangleFC states levels = zip states (sequence levels)


-- make sure exactly 1 choice of every level is true
trialConsistency :: ILBlock -> [LLConstraint]
trialConsistency block = map OneHot allLevelPairs
  where allLevelPairs = concat $ getShapedLevels block


-- BUG: WARNING: THIS MUST BE CHANGED TO A GENERAL TOCNF FUNCTION! THIS GENERALIZATION IS WRONG
--     THE TEST CASE FOR THIS BUG IS FULLY CROSSING W/ 3+ LEVELS
-- this is the IL => LL translation for "derived" factors
-- sample the block at the toBind indicies, and entangle each of those w/ index
-- example:
-- color = Factor "color" [Level "red", Level "blue"]
-- text  = Factor "text"  [Level "red", Level "blue"]
-- conLevel  = DerivedLevel  "con" (Derivation (==) color text)
-- incLevel  = DerivedLevel  "inc" (Derivation (/=) color text)
-- conFactor = Factor "congruent?"  [conLevel, incLevel]
-- design       = [color, text, conFactor]
-- inBlock = (ILBlock 3 8 25 design [0, 1, 2] [])
-- iffDerivations [[0, 2], [1, 3]] 4 inBlock  @?=
--    Entangle 12 [8,10],Entangle 12 [9,11],Entangle 18 [14,16],Entangle 18 [15,17],Entangle 24 [20,22],Entangle 24 [21,23]]
iffDerivations :: Int -> Int -> [[Int]] -> Int -> ILBlock -> State (Count, CNF) [LLConstraint]
iffDerivations width stride toBind index inBlock = do -- State Monad
        let allVars = map concat $ getShapedLevels inBlock
        let chunkedVars = chunkByWidthAndStride width stride allVars
        let iteration = do -- list monad!!
                      currTrial <- chunkedVars -- grab a single trial from the list of trial vars
                      let derivedVar = currTrial !! index
                      let allDependencies = do -- this is equivalent to `map (\is -> map (currTrial !!) is) toBind`
                                              is <- toBind -- grab a single index from the list of indices to bind
                                              return $ map (currTrial !!) is
                      -- return (Biconditional (Variable $ Var derivedVar))
                      return (derivedVar, allDependencies)
        iffWithDNFList iteration
        return []


-- shapedVars = [[8,9,10,11,12,13],[14,15,16,17,18,19],[20,21,22,23,24,25]]
-- chunkByWidthAndStride 1 1 shapedVars @?=
--     [[8,9,10,11,12,13],[14,15,16,17,18,19],[20,21,22,23,24,25]]
-- chunkByWidthAndStride 1 2 shapedVars @?=
--     [[8,9,10,11,12,13],[20,21,22,23,24,25]]
-- chunkByWidthAndStride 2 1 shapedVars @?=
--     [[8,9,10,11,12,13,14,15,16,17,18,19],[14,15,16,17,18,19,20,21,22,23,24,25]]
chunkByWidthAndStride :: Int -> Int -> [[Int]] -> [[Int]]
chunkByWidthAndStride width stride [] = []
chunkByWidthAndStride width stride shapedVars
  | length shapedVars < width = []
  | otherwise                 = concat (take width shapedVars) : chunkByWidthAndStride width stride (drop stride shapedVars)



-- This takes a list of tuples of the form (a, xs) where xs is a DNF (OR of ANDS) formatted list
--   and produces <a iff xs> in CNF form
-- This is needed to synth the derivedFactor bindings, if using it for something else, buyer-beware.
-- This call will directly put the CNF into the State instead of going through the compiler.
iffWithDNFList :: [(Int, [[Int]])] -> State (Count, CNF) ()
iffWithDNFList todo = do --state monad
     let possibleValidCombos = do --list monad
                      (index, dnfBindings) <- todo
                      return $ map (\x -> negate index : x) $ sequence dnfBindings
     let possibleInvalidCombos = do -- list monad
                      (index, dnfBindings) <- todo
                      return $ map (\x -> index : map negate x) dnfBindings
     mapM_ appendCNF possibleValidCombos
     mapM_ appendCNF possibleInvalidCombos


-- helper for getting shaped levels
-- given a starting index, and a list that says how many levels in each factor, returns one trial
-- ie getTrialVars 1 [2, 2] => [[1, 2], [3, 4]]
-- getTrialVars 8 [3, 2, 2] => [[8,9,10],[11,12],[13,14]]
getTrialVars :: Int -> [Int] -> [[Int]]
getTrialVars start trialShape = reverse $ snd $ foldl' (\(count, acc) x-> (count+x, [count..(count+x-1)]:acc)) (start, []) trialShape

-- TODO: todo: move to unit tests...
-- NOTE: THIS VERSION WORKS FOR **ALL** FACTORS, NOT JUST THE ONES IN THE CROSSING
-- returns the level vars, nested by factor then by trial
-- ie if the block has color=r, b & shape=c, s
-- then a nesting might be
-- design = [(Factor "color" [(Level "red"), (Level "blue")]), (Factor "shape" [(Level "circle"), (Level "square")])]
-- getShapedLevels (ILBlock 8 1 8 design design []) @?= [ [[1, 2], [3, 4]], [[5, 6], [7, 8]] ]
--
-- design2 = [(Factor "color" [(Level "red"), (Level "blue")]), (Factor "shape" [(Level "circle"), (Level "square")]), (Factor "size" [(Level "big"), (Level "small")])]
-- crossing = remove design2 [(Factor "size" [(Level "big"), (Level "small")])]
-- getShapedLevels (ILBlock 8 1 48 design crossing []) @?=
-- [[[1,2],[3,4],[5,6]],[[7,8],[9,10],[11,12]],[[13,14],[15,16],[17,18]],[[19,20],[21,22],[23,24]],[[25,26],[27,28],[29,30]],[[31,32],[33,34],[35,36]],[[37,38],[39,40],[41,42]],[[43,44],[45,46],[47,48]]]
--
-- design = [Factor "color" [Level "red", Level "blue", Level "green"], Factor "size" [Level "big", Level "small"], Factor "shape" [Level "circle", Level "square"]]
-- getShapedLevels (ILBlock 2 8 21 design [0, 1, 2] []) @?=
-- [[[8,9,10],[11,12],[13,14]],[[15,16,17],[18,19],[20,21]]]
getShapedLevels :: ILBlock -> [[[Int]]]
getShapedLevels (ILBlock numTrials start end design _ _) = levelGroups
  where trialSize   = totalLeavesInDesign design -- :: Int
        trialShape  = map countLeaves design     -- :: [Int]
        levelGroups = map (`getTrialVars` trialShape) [start, (start+trialSize).. end]
                         -- getTrialVars 8 [3, 2, 2] => [[8,9,10],[11,12],[13,14]]


-- A modified version that uses only the subset that's crossed
-- This method only gets called to bind the crossing constraints
-- EXAMPLE
-- design = [Factor "color" [Level "red", Level "blue", Level "green"], Factor "size" [Level "big", Level "small"], Factor "shape" [Level "circle", Level "square"]]
-- getShapedLevels (ILBlock 2 8 21 design _ _) @?=
-- [[[8,9,10],[13,14]],[[15,16,17],[20,21]]]
getShapedOnlyCrossedLevels :: ILBlock -> [[[Int]]]
getShapedOnlyCrossedLevels block@(ILBlock _ _ _ _ crossingIdxs _) = crossedVars
  where allVars = getShapedLevels block
        crossedVars = map (\sublist -> map (\idx -> sublist !! idx) crossingIdxs) allVars




-------------------------------------------------------------
-- Transformation time!
-- This actually runs the commands we constructed in the low level
buildCNF :: LLAST -> State (Int, CNF) ()
buildCNF = mapM_ buildCommand

buildCommand :: LLConstraint -> State (Int, CNF) ()
buildCommand (OneHot todo) = enforceOneHot todo
buildCommand (Entangle state levels) = aDoubleImpliesList state levels
buildCommand (AssertLt k vars) = kLessThanN k vars
buildCommand (AssertGt k vars) = kGreaterThanN k vars
buildCommand (AssertEq k vars) = assertKofN k vars
-- buildCommand _ = error "other commands not implemented"

-------------------------------------------------------------
-- Decoding it back:
--    need to take a nTrials arg in cases where the full design isn't fully-crossed
decode :: String -> Design -> Int -> String
decode result design nTrials
  | numLines == 1 = "Oh no! Something was unsatisifiable, no sequence was found!"
  | otherwise = labelling
  where numLines = length $ lines result
        parsedList = mapM readMaybe . init . concatMap (words . tail) . tail . lines $ result :: Maybe [Int]
        labelling = case parsedList of
          Nothing -> "Hmm the result file seems to be misformatted..."
          Just vars -> label vars design nTrials

label :: [Int] -> Design -> Int ->  String
label inList design nTrials = unlines $ map unwords $ chunkify selectedVarNames (length design)
--where relevantVars = take (totalLeavesInDesign design * fullyCrossSize design) inList --HACK: actually need probably the AST instead of design
  where relevantVars = take (totalLeavesInDesign design * nTrials) inList --HACK: actually need probably the AST instead of design
        names = take (length relevantVars) (cycle $ leafNamesInDesignFlat design)
        varOn = map (> 0) relevantVars
        selectedVarNames = map snd $ filter fst $ zip varOn names
-------------------------------------------------------------

-- mutually exclusive fields actually
-- asserts that only one of the list can be true
-- (a and -b and -c) or (-a and b and c) or (-a and -b and c)... but in CNF
-- which is: (a or b or c) and (-a or -b) and (-b or -c) and (-a or -c)
enforceOneHot :: [DataStructures.Var] -> State (Count, CNF) ()
enforceOneHot inList = do appendCNF [inList] -- this appends (a or b or c)
                          appendCNF $ not_pairs inList -- appends the (-a or -b) and (-b or -c) etc pairs
  where not_pairs xs = nub [[-x, -y] | x <- xs, y <- xs, x < y]


-- why isn't this part of control.monad? No one knows
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM func args = do result <- mapM func args
                          return $ concat result


-- chunks a list into chunkSize sized chunks
chunkify :: [a] -> Int -> [[a]]
chunkify [] _ = []
chunkify inList chunkSize = take chunkSize inList : chunkify (drop chunkSize inList) chunkSize

-- slidingWindow 3 [1..6] => [[1,2,3],[2,3,4],[3,4,5],[4,5,6]]
slidingWindow :: Int -> [a] -> [[a]]
slidingWindow wsize todo
  | length todo  < wsize = error "Tried to take a sliding window that was too big"
  | length todo == wsize = [todo]
  | otherwise = take wsize todo : slidingWindow wsize (tail todo)
