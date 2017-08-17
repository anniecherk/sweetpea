{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.Trans.State



import HLtoIL
import DataStructures


main = defaultMain tests --putStrLn "not set up"





tests :: TestTree
tests = testGroup "Tests" [hlToIlTests, dataStructureTests]

hlToIlTests :: TestTree
hlToIlTests = testGroup "HLtoIL Tests" [oneHotTests]

dataStructureTests :: TestTree
dataStructureTests = testGroup "DataStructure Tests" [helperTests]

---------------------------------------------------------------------------------------------------------------
-- HL to IL Tests

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


---------------------------------------------------------------------------------------------------------------
-- Parser Tests

-- factorPathTests = testGroup "factorPathTests"
--   [ testCase "empty paths" $
--       decodeFactorPaths "[[], []]"
--         @?= Just [[], []]
--   , testCase "simplest path" $
--       decodeFactorPaths "[[\"color\",\"red\"]]"
--         @?= Just [["color", "red"]]
--   , testCase "two paths" $
--       decodeFactorPaths "[[\"color\",\"red\"], [\"color\",\"blue\"]]"
--         @?= Just [["color", "red"], ["color", "blue"]]
--   , testCase "two paths of different lengths" $
--       decodeFactorPaths "[[\"color\",\"red\"], []]"
--         @?= Just [["color", "red"], []]
--   , testCase "many paths of different lengths" $
--       decodeFactorPaths "[[\"color\",\"red\"], [\"color\",\"blue\"], \
--       \ [\"shape\",\"small\",\"circle\"], [\"shape\",\"small\", \"square\"], \
--       \ [\"shape\",\"big\",\"circle\"], [\"shape\",\"big\",\"square\"]]"
--         @?= Just [["color", "red"], ["color", "blue"], ["shape", "small", "circle"]
--         , ["shape", "small", "square"], ["shape", "big", "circle"], ["shape", "big", "square"]]
--   ]
--
-- rawConstraintsTests = testGroup "rawConstraintsTests"
--   [ testCase "NoMoreThanKInARow" $ decodeRawConstraint
--       "{ \"constraint\": \"NoMoreThanKInARow\", \
--       \  \"applied_to\" : [[\"color\", \"lightColors\", \"pink\"]], \
--       \  \"k\": 3}" @?= Just (NoMoreThanKInARow [["color", "lightColors", "pink"]] 3)
--   , testCase "AtLeastKInARow" $ decodeRawConstraint
--       "{ \"constraint\": \"AtLeastKInARow\", \
--       \  \"applied_to\" : [[\"color\", \"lightColors\", \"pink\"]], \
--       \  \"k\": 3}" @?= Just (AtLeastKInARow [["color", "lightColors", "pink"]] 3)
--   , testCase "no more than k out of j" $ decodeRawConstraint
--       "{ \"constraint\": \"NoMoreThanKOutOfJ\", \
--       \  \"applied_to\" : [[\"color\", \"lightColors\", \"pink\"]], \
--       \  \"k\": 3, \
--       \  \"j\": 4 }" @?= Just (NoMoreThanKOutOfJ [["color", "lightColors", "pink"]] 3 4)
--   , testCase "AtLeastKOutOfJ" $ decodeRawConstraint
--       "{ \"constraint\": \"AtLeastKOutOfJ\", \
--       \  \"applied_to\" : [[\"color\", \"lightColors\", \"pink\"]], \
--       \  \"k\": 3, \
--       \  \"j\": 4 }" @?= Just (AtLeastKOutOfJ [["color", "lightColors", "pink"]] 3 4)
--   , testCase "BalanceTransitions" $ decodeRawConstraint
--       "{ \"constraint\": \"BalanceTransitions\", \
--       \  \"applied_to\" : [[\"color\", \"lightColors\"]]}"
--        @?= Just (BalanceTransitions [["color", "lightColors"]])
--    , testCase "Fail missing data" $ decodeRawConstraint
--        "{ \"constraint\": \"AtLeastKOutOfJ\", \
--        \  \"applied_to\" : [[\"color\", \"lightColors\", \"pink\"]],}"
--        @?= Nothing
--    , testCase "Fail wrong constraint name" $ decodeRawConstraint
--        "{ \"constraint\": \"NotAConstraint\", \
--        \  \"applied_to\" : [[\"color\", \"lightColors\", \"pink\"]],}"
--        @?= Nothing
--   ]
--
--
-- fullSpecTests = testGroup "fullSpecTests"
--   [ testCase "full example" $ decodeHL_IR fullTestString
--      @?= Just (HL_IR [["color","darkColors","black"],["color","darkColors","pink"],
--        ["color","lightColors","white"],["color","lightColors","pink"],["shape","circle"],
--        ["shape","square"],["shape","triangle"]] (FullyCross [["shape"],["color"]] 2)
--        [NoMoreThanKOutOfJ [["color","lightColors","pink"]] 3 4
--        , AtLeastKOutOfJ [["shape","circle"]] 2 7,
--        NoMoreThanKInARow [["color","lightColors","pink"]] 3])
--   ]
--
--
-- fullTestString = "{ \
-- \  \"factorPaths\": [    \
-- \    [\"color\", \"darkColors\", \"black\"],    \
-- \    [\"color\", \"darkColors\", \"pink\"],    \
-- \    [\"color\", \"lightColors\", \"white\"],    \
-- \    [\"color\", \"lightColors\", \"pink\"],    \
-- \    [\"shape\", \"circle\"],    \
-- \    [\"shape\", \"square\"],    \
-- \    [\"shape\", \"triangle\"]    \
-- \  ],    \
-- \\
-- \\
-- \  \"fullyCross\": {    \
-- \    \"applied_to\" :    \
-- \      [[\"shape\"], [\"color\"]],    \
-- \    \"repetitions\" : 2    \
-- \  },    \
-- \\
-- \\
-- \  \"constraints\": [    \
-- \    {    \
-- \      \"constraint\": \"NoMoreThanKOutOfJ\",    \
-- \      \"applied_to\" : [[\"color\", \"lightColors\", \"pink\"]],    \
-- \      \"k\": 3,    \
-- \      \"j\": 4    \
-- \    },    \
-- \    {    \
-- \      \"constraint\": \"AtLeastKOutOfJ\",    \
-- \      \"applied_to\" : [[\"shape\", \"circle\"]],    \
-- \      \"k\": 2,    \
-- \      \"j\": 7    \
-- \    },    \
-- \    {    \
-- \      \"constraint\": \"NoMoreThanKInARow\",    \
-- \      \"applied_to\" : [[\"color\", \"lightColors\", \"pink\"]],    \
-- \      \"k\": 3    \
-- \    }    \
-- \  ]    \
-- \ }"
