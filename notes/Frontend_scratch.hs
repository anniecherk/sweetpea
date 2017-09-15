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


                  --  data HLBlock = HLBlock { hlnumTrials :: Int
                  --                         , hldesign :: Design
                  --                         , hlconstraints :: [HLConstraint]
                  --                         } deriving(Show)

makeBlock :: Int -> Design -> [HLConstraint] -> HLBlock
makeBlock numTs des consts = HLBlock numTs des (Consistency : consts)




-- complicatedColor = NTNode "color" [(NTNode "darkcolor" [(LeafNode "black"), (LeafNode "blue")]), (NTNode "lightcolor" [(LeafNode "blue"), (LeafNode "pink")])]
-- complicatedShape = NTNode "shape" [(LeafNode "circle"), (LeafNode "square"), (LeafNode "triangle")]

-- color = NTNode "color" [(LeafNode "red"), (LeafNode "blue")]
-- shape = NTNode "shape" [(LeafNode "circle"), (LeafNode "square")]
-- design = [color, shape]
-- block = makeBlock (hlFullyCross design) [color, shape] [FullyCross]
-- ast = [block, block]
-- hlToIl ast

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
hlFullyCross :: Design -> Int
hlFullyCross factors = numStates
  where numStates = foldl (\acc x -> acc * countLeaves x) 1 factors


type Count = Int
type Var = Int

-- Transformation time!
-- hlToIl :: HLAST -> ILAST
hlToIl :: HLAST -> State (Int, CNF) ILAST
hlToIl = mapM allocateVars


allocateVars :: HLBlock ->  State (Int, CNF) ILBlock
allocateVars (HLBlock numTrials design constraints) = do
      startAddr <- getFresh
      let trialSize = totalLeavesInDesign design
      let endAddr = startAddr + (numTrials * trialSize) - 1
      putFresh endAddr
      return $ ILBlock numTrials startAddr endAddr design constraints



-- list of IL blocks : get the total num allocated by looking at last block
type ILAST = [ILBlock]

-- so a block that occupies: 0, 1, 2, 3 the startAddr is 0 & the endAddr is 3
data ILBlock = ILBlock { ilnumTrials :: Int
                       , ilstartAddr :: Int
                       , ilendAddr :: Int
                       , ildesign :: Design
                       , ilconstraints :: [HLConstraint]
                       } deriving(Show)


--- Transformation time!
-- TODO: It's constraint time. How are constraints going to be represented?
-- 1. Start up the State(Int, CNF)
-- 2. Variable alloc for FC & fully cross => equate states with variable combos
-- 3.
data LLConstraint = AssertEq Int [Var] | AssertLt Int [Var]
                  | AssertGt Int [Var] | OneHot [Var]
                  | Entangle Var [Var] deriving(Show)

-- list of IL blocks : get the total num allocated by looking at last block
type LLBlock = [LLConstraint]
type LLAST = [LLBlock]

-- data LLBlock = LLBlock { llconstraints :: [LLConstraint]
--                        } deriving(Show)

-- concatMap sequence [ [[1, 2], [3, 4]], [[5, 6], [7, 8]] ]


-- TODO: desugar the other constraints from HLConstraints to LLConstraints using Design
ilToll :: ILAST -> State (Count, CNF) LLAST
ilToll ilast = do llblocks <- mapM ilBlockToLLBlocks ilast
                  return $ concat llblocks


ilBlockToLLBlocks :: ILBlock -> State (Count, CNF) [LLBlock]
ilBlockToLLBlocks block@(ILBlock _ _ _ _ constraints) = mapM (\x -> desugarConstraint x block) constraints


-- ILBlock { ilnumTrials :: Int
--                        , ilstartAddr :: Int
--                        , ilendAddr :: Int
--                        , ildesign :: Design
--                        , ilconstraints

desugarConstraint :: HLConstraint -> ILBlock -> State (Count, CNF) LLBlock
desugarConstraint Consistency inBlock = return $ trialConsistency inBlock
desugarConstraint FullyCross  inBlock = return [Entangle 0 [0,0]] --TODO
desugarConstraint _ inBlock = error "desugar const not implemented yet"

-- 1. Generate Intermediate Vars
-- 2. Entangle them w/ block vars
-- 3. 1 hot the *states* ie, 1 red circle, etc
llfullyCross :: ILBlock -> [LLConstraint]
llfullyCross (ILBlock numTrials start end design _) = []


trialConsistency :: ILBlock -> [LLConstraint]
trialConsistency (ILBlock numTrials start end design _) = map (\x -> OneHot x) allLevelPairs
  where trialSize = totalLeavesInDesign design
        trialShape = map countLeaves design
        levelGroups = map (\x -> getShapeVars x trialShape) [start, (trialSize+1).. end]
        allLevelPairs = concatMap sequence levelGroups

getShapeVars :: Int -> [Int] -> [[Int]]
getShapeVars start trialShape = reverse $ snd $ foldl (\(count, acc) x-> (count+x, [count..(count+x-1)]:acc)) (start, []) trialShape


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

--}
