module FrontEndSugar
( fullyCrossedBlock -- , multiFullyCrossedBlock
, equal
, notEq
, transition
, remove )
where

import FrontEnd
import DataStructures
import Data.List ((\\))




transition :: String -> Derivation -> HLLabelTree
transition name = DerivedLevel name width stride
  where width = 2
        stride = 1

quality :: String -> Derivation -> HLLabelTree
quality name = DerivedLevel name width stride
  where width = 1
        stride = 1

equal :: (HLLabelTree, Int) -> (HLLabelTree, Int) -> Derivation
equal = Derivation (==)

notEq :: (HLLabelTree, Int) -> (HLLabelTree, Int) -> Derivation
notEq = Derivation (/=)

fullyCrossedBlock :: Design -> [Int] -> [HLConstraint] -> HLBlock
fullyCrossedBlock design crossingIdxs constraints = makeBlock numTrials design crossingIdxs allConstraints
  where numTrials      = fullyCrossSize design crossingIdxs
        allConstraints = FullyCross : constraints




-- -- TODO CANT HANDLE CROSSING /= DESIGN!!
-- multiFullyCrossedBlock :: Int -> Design -> [HLConstraint] -> HLBlock
-- multiFullyCrossedBlock reps design constraints =  makeBlock numTrials design allConstraints
--   where numTrials = multiFullyCrossSize design reps
--         allConstraints = FullyCross : constraints


remove :: Eq a => [a] -> [a] -> [a]
remove = (\\)
