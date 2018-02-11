{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.Trans.State
import Control.Monad (replicateM)



import FrontEnd
import FrontEndSugar
import DataStructures


main = defaultMain tests --putStrLn "not set up"




tests :: TestTree
tests = testGroup "Tests" [frontEndTests, dataStructureTests]

frontEndTests :: TestTree
frontEndTests = testGroup "FrontEnd Tests" [oneHotTests, hlTreeTests, hltoilTests, iltollTests, iffDerivationsTests]

dataStructureTests :: TestTree
dataStructureTests = testGroup "DataStructure Tests" [helperTests]

---------------------------------------------------------------------------------------------------------------
-- FRONT END TESTS

color = Factor "color" [Level "red", Level "blue"]
text  = Factor "text"  [Level "red", Level "blue"]
conLevel  = DerivedLevel  "con" (Derivation (==) color text)
incLevel  = DerivedLevel  "inc" (Derivation (/=) color text)
conFactor = Factor "congruent?"  [conLevel, incLevel]
design    = [color, text, conFactor]

hlDerivationTests = testGroup "hl derivation test"
  [ testCase "running derivation example" $
      makeHLDerivation color design  @?=  []
  , testCase "non-derivation example" $
      makeHLDerivation conFactor design  @?=  [HLDerivation [[0,2],[1,3]] 4,HLDerivation [[0,3],[1,2]] 5]
  ]

oneHotTests = testGroup "one hot tests"
  [ testCase "empty edgecase" $
      execState (enforceOneHot []) emptyState
        @?= (0,[[]])
  ,  testCase "small example: 2 elements" $
      execState (enforceOneHot [1..2]) (initState 2)
        @?= (2,[[-1,-2],[1,2]])
  ,  testCase "small example: 3 elements" $
      execState (enforceOneHot [1..3]) (initState 3)
        @?= (3,[[-1,-2],[-1,-3],[-2,-3],[1,2,3]])
  ,  testCase "4 elements + indep of variables & cnf" $
      execState (enforceOneHot [1..4]) (initState 6)
        @?= (6,[[-1,-2],[-1,-3],[-1,-4],[-2,-3],[-2,-4],[-3,-4],[1,2,3,4]]) -- TODO: check against cryptosatmini
  ]

iffDerivationsTests = testGroup "low-level derivation constraints"
  [ testCase "3 wide, non-aligned entanglement test" $
      iffDerivations [[0, 2], [1, 3]] 4 derivedTestILBlock
        @?= [Entangle 5 [1,3],Entangle 5 [2,4],Entangle 11 [7,9],Entangle 11 [8,10],Entangle 17 [13,15],Entangle 17 [14,16],Entangle 23 [19,21],Entangle 23 [20,22]]
  ]

hlTreeTests = testGroup "verifying properties of the design trees"
  [ testCase "countLeaves 2 leaf tree" $
      countLeaves (Factor "color" [Level "red", Level "blue"])
        @?= 2
  , testCase "countLeaves 3 leaf tree" $
       countLeaves (Factor "color" [Level "red", Level "blue", Level "yellow"])
         @?= 3
   , testCase "countLeaves nested 4 leaf tree" $
        countLeaves (Factor "color"[Factor "ltcolor" [Level "red", Level "blue"], Factor "dkcolor" [Level "red", Level "blue"]])
          @?= 4
  ]


hltoilTests = testGroup "hl to il variable allocation tests"
  [ testCase "running example" $
      evalState (allocateVars testHLBlock) emptyState
        @?= testILBlock
  , testCase "not fullyCrossed example" $
      evalState (allocateVars notFullyCrossedTestHLBlock) emptyState
        @?= notFullyCrossedTestILBlock
  ]

iltollTests = testGroup "il to ll low level tests"
  [ testCase "constraint gen running example" $
      evalState (ilBlockToLLBlocks testILBlock) (initState 4)
        @?=  [OneHot [1,2],OneHot [3,4],AssertEq 1 [5,7],AssertEq 1 [6,8],Entangle 5 [1],Entangle 6 [2],Entangle 7 [3],Entangle 8 [4]]
  , testCase "getShapedLevel running example" $
      getShapedLevels testILBlock
        @?= [[[1, 2]], [[3, 4]]]
  , testCase "fullycross constraint gen running example" $
      evalState (llfullyCross testILBlock) (initState 4)
        @?= [AssertEq 1 [5,7],AssertEq 1 [6,8],Entangle 5 [1],Entangle 6 [2],Entangle 7 [3],Entangle 8 [4]]
  , testCase "getTrials helper test" $
      getTrialVars 1 [2, 2]
        @?= [[1, 2], [3, 4]]
  , testCase "getTrials helper test different start" $
      getTrialVars 5 [2, 2]
        @?= [[5, 6], [7, 8]]
  , testCase "getShapedLevels helper test running example" $
      getShapedLevels testILBlock
        @?= [[[1,2]],[[3,4]]]
  , testCase "entangleFC helper test 2 factors / 2 levels" $
      entangleFC [5, 6, 7, 8] [[1, 2], [3, 4]] -- first arg is "state" vars, second arg is "level" vars
        @?= [(5,[1,3]),(6,[1,4]),(7,[2,3]),(8,[2,4])]
  ]


---------------------------------------------------------------------------------------------------------------
-- DataStructures tests

helperTests = testGroup "var/cnf utility tests"
  [ testCase "getFresh" $
      runState getFresh emptyState
        @?= (1,(1,[]))
  , testCase "getNFresh" $
      runState (getNFresh 3) emptyState
        @?= ([1,2,3],(3,[]))
  , testCase "nested getNFresh" $
      runState (getNFresh 3) (execState (getNFresh 3) emptyState)
        @?= ([4,5,6],(6,[]))
  , testCase "appendCNF" $
      execState (appendCNF [[1, 2, -3]]) emptyState
        @?= (0,[[1,2,-3]])
  , testCase "nested appendCNF" $
      execState (appendCNF [[-4, 5], [4]]) (execState (appendCNF [[1, 2, -3]]) emptyState)
        @?= (0,[[-4,5],[4],[1,2,-3]])
  , testCase "zeroOut" $
        execState (zeroOut [1, 2, 3]) emptyState
          @?= (0,[[-1],[-2],[-3]])
  , testCase "setToOne" $
        execState (setToOne 1) emptyState
          @?= (0,[[1]])
  , testCase "setToOne + zeroOut nested" $
        execState (zeroOut [1, 2, 3]) (execState (setToOne 4) emptyState)
          @?= (0,[[-1],[-2],[-3],[4]])
  , testCase "setToZero" $
        execState (setToZero 1) emptyState
          @?= (0,[[-1]])
  ]



-------------- Test Blocks

-- Easy tests:
testHLBlock :: HLBlock
testHLBlock = block
  where color = Factor "color" [Level "red", Level "blue"]
        design = [color]
        crossingIdxs = [0]
        numTrials = fullyCrossSize design crossingIdxs
        block = makeBlock numTrials design crossingIdxs [FullyCross]

testILBlock :: ILBlock
testILBlock = ILBlock {ilnumTrials = 2, ilstartAddr = 1, ilendAddr = 4, ildesign = [Factor "color" [Level "red",Level "blue"]], ilcrossingIdxs = [0], ilconstraints = [Consistency,FullyCross]}


-- Trickier tests:
notFullyCrossedTestHLBlock :: HLBlock
notFullyCrossedTestHLBlock = block
  where color = Factor "color" [Level "red", Level "blue"]
        size  = Factor "size"  [Level "big", Level "medium", Level "small"]
        shape = Factor "shape" [Level "circle", Level "square"]
        design = [color, size, shape]
        crossingIdxs = [0, 2]
        numTrials = fullyCrossSize design crossingIdxs
        block = makeBlock numTrials design crossingIdxs [FullyCross]


notFullyCrossedTestILBlock :: ILBlock
notFullyCrossedTestILBlock = ILBlock {ilnumTrials = 4, ilstartAddr = 1, ilendAddr = 28, ildesign = [Factor "color" [Level "red",Level "blue"],Factor "size" [Level "big",Level "medium",Level "small"],Factor "shape" [Level "circle",Level "square"]], ilcrossingIdxs = [0,2], ilconstraints = [Consistency,FullyCross]}

derivedTestHLBlock :: HLBlock
derivedTestHLBlock = block
  where color = Factor "color" [Level "red", Level "blue"]
        text  = Factor "text"  [Level "red", Level "blue"]
        conLevel  = DerivedLevel  "con" (equal color text)
        incLevel  = DerivedLevel  "inc" (notEq color text)
        conFactor = Factor "congruent?"  [conLevel, incLevel]
        design       = [color, text, conFactor]
        crossingIdxs = [0, 1]
        numTrials = fullyCrossSize design crossingIdxs
        block = makeBlock numTrials design crossingIdxs [FullyCross]


derivedTestILBlock :: ILBlock
derivedTestILBlock = ILBlock {ilnumTrials = 4, ilstartAddr = 1, ilendAddr = 24, ildesign = [Factor "color" [Level "red",Level "blue"],Factor "text" [Level "red",Level "blue"],Factor "congruent?" [DerivedLevel "con" (Derivation (==) (Factor "color" [Level "red",Level "blue"]) (Factor "text" [Level "red",Level "blue"])),DerivedLevel "inc" (Derivation (==) (Factor "color" [Level "red",Level "blue"]) (Factor "text" [Level "red",Level "blue"]))]], ilcrossingIdxs = [0,1], ilconstraints = [HLDerivation [[0,2],[1,3]] 4,HLDerivation [[0,3],[1,2]] 5,Consistency,FullyCross]}
