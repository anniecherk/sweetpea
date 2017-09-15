-- TODO omg tests
-- type Builder = State (Int, CNF)

-- hlToIl = undefined :: HLAST -> State (Int, CNF) ILAST
-- ilToLL = undefined :: ILAST -> State (Int, CNF) LLAST
-- buildCNF = undefined :: LLAST -> State (Int, CNF) ()
-- execState (hlToIl undefined >>= ilToLL >>= buildCNF) emptyState -- or evalState?
----- DELETE THESE IMPORTS!! ------------
import Control.Monad.Trans.State
import Control.Monad (replicateM)
-------------------------------------------
import Data.List (transpose, nub)


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
-- execState (hlToIl ast >>= ilToll >>= buildCNF) emptyState
-- runState (hlToIl ast >>= ilToll) emptyState
-- runState (hlToIl ast) emptyState
--
--
-- ilBlock = ILBlock {ilnumTrials = 4, ilstartAddr = 1, ilendAddr = 16, ildesign = [NTNode "color" [LeafNode "red",LeafNode "blue"],NTNode "shape" [LeafNode "circle",LeafNode "square"]], ilconstraints = [Consistency,FullyCross]}
-- numTrials = 4
-- states = transpose $ chunkify [17..32] 4
-- levels = getShapedLevels ilBlock


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
  llblocks <- mapM (\x -> desugarConstraint x block) constraints
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
  let entanglements = concatMap (\(s, l) -> entangleFC s l) $ zip states levels
  let entangleConstraints = map (\(s, l) -> Entangle s l) entanglements -- #2
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
        levelGroups = map (\x -> getShapeVars x trialShape) [start, (trialSize+1).. end]

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




---------------
-- COPY PASTED FOR NOW
type CNF = [[Var]]
emptyState :: (Count, CNF)
emptyState = (0, [])

-- if we need to start with variables 1-maxVar
initState :: Int -> (Count, CNF)
initState maxVar = (maxVar, [])

getFresh :: State (Count, CNF) Count
getFresh =  do (numVars, x) <- get
               put (numVars + 1, x)
               return (numVars+1)

putFresh :: Int -> State (Count, CNF) ()
putFresh n =  do (numVars, x) <- get
                 put (n, x)


getNFresh :: Int -> State (Count, CNF) [Count]
getNFresh n = replicateM n getFresh


appendCNF :: CNF -> State (Count, CNF) ()
appendCNF newEntry = do (x, accum) <- get
                        put (x, newEntry ++ accum)
                        return ()


-- the double-implication generalization
-- https://www.wolframalpha.com/input/?i=CNF+A++%3C%3D%3E+(B+%26%26+C+%26%26+D+%26%26+E)
-- CNF A  <=> (B && C && D)
-- (-a or b)
-- (-a or c)
-- (-a or d)
-- (a or -b or -c or -d)
aDoubleImpliesList :: Var -> [Var] -> State (Count, CNF) ()
        -- makes the list of [-b, -c, .., a] : makes the lists [-a, b], [-a, c], ...
aDoubleImpliesList a inList = appendCNF result
  where result = (map ((-1) *) inList ++ [a]) : map (\x -> [-a, x]) inList

type Count = Int
type Var = Int

--}
