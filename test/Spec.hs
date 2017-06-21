{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit


import Parser

main = defaultMain tests

tests :: TestTree
tests = testGroup "Parser Tests" [factorPathTests, rawConstraintsTests, fullSpecTests]


factorPathTests = testGroup "factorPathTests"
  [ testCase "empty paths" $
      decodeFactorPaths "[[], []]"
        @?= Just [[], []]
  , testCase "simplest path" $
      decodeFactorPaths "[[\"color\",\"red\"]]"
        @?= Just [["color", "red"]]
  , testCase "two paths" $
      decodeFactorPaths "[[\"color\",\"red\"], [\"color\",\"blue\"]]"
        @?= Just [["color", "red"], ["color", "blue"]]
  , testCase "two paths of different lengths" $
      decodeFactorPaths "[[\"color\",\"red\"], []]"
        @?= Just [["color", "red"], []]
  , testCase "many paths of different lengths" $
      decodeFactorPaths "[[\"color\",\"red\"], [\"color\",\"blue\"], \
      \ [\"shape\",\"small\",\"circle\"], [\"shape\",\"small\", \"square\"], \
      \ [\"shape\",\"big\",\"circle\"], [\"shape\",\"big\",\"square\"]]"
        @?= Just [["color", "red"], ["color", "blue"], ["shape", "small", "circle"]
        , ["shape", "small", "square"], ["shape", "big", "circle"], ["shape", "big", "square"]]
  ]

rawConstraintsTests = testGroup "rawConstraintsTests"
  [ testCase "NoMoreThanKInARow" $ decodeRawConstraint
      "{ \"constraint\": \"NoMoreThanKInARow\", \
      \  \"applied_to\" : [[\"color\", \"lightColors\", \"pink\"]], \
      \  \"k\": 3}" @?= Just (NoMoreThanKInARow [["color", "lightColors", "pink"]] 3)
  , testCase "AtLeastKInARow" $ decodeRawConstraint
      "{ \"constraint\": \"AtLeastKInARow\", \
      \  \"applied_to\" : [[\"color\", \"lightColors\", \"pink\"]], \
      \  \"k\": 3}" @?= Just (AtLeastKInARow [["color", "lightColors", "pink"]] 3)
  , testCase "no more than k out of j" $ decodeRawConstraint
      "{ \"constraint\": \"NoMoreThanKOutOfJ\", \
      \  \"applied_to\" : [[\"color\", \"lightColors\", \"pink\"]], \
      \  \"k\": 3, \
      \  \"j\": 4 }" @?= Just (NoMoreThanKOutOfJ [["color", "lightColors", "pink"]] 3 4)
  , testCase "AtLeastKOutOfJ" $ decodeRawConstraint
      "{ \"constraint\": \"AtLeastKOutOfJ\", \
      \  \"applied_to\" : [[\"color\", \"lightColors\", \"pink\"]], \
      \  \"k\": 3, \
      \  \"j\": 4 }" @?= Just (AtLeastKOutOfJ [["color", "lightColors", "pink"]] 3 4)
  , testCase "BalanceTransitions" $ decodeRawConstraint
      "{ \"constraint\": \"BalanceTransitions\", \
      \  \"applied_to\" : [[\"color\", \"lightColors\"]]}"
       @?= Just (BalanceTransitions [["color", "lightColors"]])
   , testCase "Fail missing data" $ decodeRawConstraint
       "{ \"constraint\": \"AtLeastKOutOfJ\", \
       \  \"applied_to\" : [[\"color\", \"lightColors\", \"pink\"]],}"
       @?= Nothing
   , testCase "Fail wrong constraint name" $ decodeRawConstraint
       "{ \"constraint\": \"NotAConstraint\", \
       \  \"applied_to\" : [[\"color\", \"lightColors\", \"pink\"]],}"
       @?= Nothing
  ]


fullSpecTests = testGroup "fullSpecTests"
  [ testCase "full example" $ decodeHL_IR fullTestString
     @?= Just (HL_IR [["color","darkColors","black"],["color","darkColors","pink"],
       ["color","lightColors","white"],["color","lightColors","pink"],["shape","circle"],
       ["shape","square"],["shape","triangle"]] (FullyCross [["shape"],["color"]] 2)
       [NoMoreThanKOutOfJ [["color","lightColors","pink"]] 3 4
       , AtLeastKOutOfJ [["shape","circle"]] 2 7,
       NoMoreThanKInARow [["color","lightColors","pink"]] 3])
  ]


fullTestString = "{ \
\  \"factorPaths\": [    \
\    [\"color\", \"darkColors\", \"black\"],    \
\    [\"color\", \"darkColors\", \"pink\"],    \
\    [\"color\", \"lightColors\", \"white\"],    \
\    [\"color\", \"lightColors\", \"pink\"],    \
\    [\"shape\", \"circle\"],    \
\    [\"shape\", \"square\"],    \
\    [\"shape\", \"triangle\"]    \
\  ],    \
\\
\\
\  \"fullyCross\": {    \
\    \"applied_to\" :    \
\      [[\"shape\"], [\"color\"]],    \
\    \"repetitions\" : 2    \
\  },    \
\\
\\
\  \"constraints\": [    \
\    {    \
\      \"constraint\": \"NoMoreThanKOutOfJ\",    \
\      \"applied_to\" : [[\"color\", \"lightColors\", \"pink\"]],    \
\      \"k\": 3,    \
\      \"j\": 4    \
\    },    \
\    {    \
\      \"constraint\": \"AtLeastKOutOfJ\",    \
\      \"applied_to\" : [[\"shape\", \"circle\"]],    \
\      \"k\": 2,    \
\      \"j\": 7    \
\    },    \
\    {    \
\      \"constraint\": \"NoMoreThanKInARow\",    \
\      \"applied_to\" : [[\"color\", \"lightColors\", \"pink\"]],    \
\      \"k\": 3    \
\    }    \
\  ]    \
\ }"
