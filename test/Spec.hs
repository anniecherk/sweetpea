{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.Trans.State
import Control.Monad (replicateM)



import FrontEnd
import DataStructures


main = defaultMain tests --putStrLn "not set up"




tests :: TestTree
tests = testGroup "Tests" [frontEndTests, dataStructureTests]

frontEndTests :: TestTree
frontEndTests = testGroup "FrontEnd Tests" [oneHotTests, hltoilTests, iltollTests]

dataStructureTests :: TestTree
dataStructureTests = testGroup "DataStructure Tests" [helperTests]

---------------------------------------------------------------------------------------------------------------
-- FRONT END TESTS

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


hltoilTests = testGroup "hl to il variable allocation tests"
  [ testCase "running example" $
      evalState (allocateVars testHLBlock) emptyState
        @?= testILBlock
  ]

iltollTests = testGroup "il to ll low level tests"
  [ testCase "constraint gen running example" $
      evalState (ilBlockToLLBlocks testILBlock) (initState 4)
        @?=  [OneHot [1,2],OneHot [3,4],OneHot [5,7],OneHot [6,8],Entangle 5 [1],Entangle 6 [2],Entangle 7 [3],Entangle 8 [4]]
  , testCase "getShapedLevel running example" $
      getShapedLevels testILBlock
        @?= [[[1, 2]], [[3, 4]]]
  , testCase "fullycross constraint gen running example" $
      evalState (llfullyCross testILBlock) (initState 4)
        @?= [OneHot [5,7],OneHot [6,8],Entangle 5 [1],Entangle 6 [2],Entangle 7 [3],Entangle 8 [4]]
  ]

--ilBlockToLLBlocks

testHLBlock :: HLBlock
testHLBlock = block
  where color = NTNode "color" [LeafNode "red", LeafNode "blue"]
        design = [color]
        block = makeBlock (fullyCrossSize design) [color] [FullyCross]


testILBlock :: ILBlock
testILBlock = ILBlock {ilnumTrials = 2, ilstartAddr = 1, ilendAddr = 4, ildesign = [NTNode "color" [LeafNode "red",LeafNode "blue"]], ilconstraints = [Consistency,FullyCross]}




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
