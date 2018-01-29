module FrontEndSugar
( fullyCrossedBlock -- , multiFullyCrossedBlock
, remove )
where

import FrontEnd
import DataStructures
import Data.List ((\\))


-- TODO CANT HANDLE CROSSING /= DESIGN!!
fullyCrossedBlock :: Design -> [Int] -> [HLConstraint] -> HLBlock
fullyCrossedBlock design crossingIdxs constraints = makeBlock numTrials design crossingIdxs allConstraints
  where numTrials      = fullyCrossSize design crossingIdxs
        allConstraints = (FullyCross crossingIdxs) : constraints




-- -- TODO CANT HANDLE CROSSING /= DESIGN!!
-- multiFullyCrossedBlock :: Int -> Design -> [HLConstraint] -> HLBlock
-- multiFullyCrossedBlock reps design constraints =  makeBlock numTrials design allConstraints
--   where numTrials = multiFullyCrossSize design reps
--         allConstraints = FullyCross : constraints


remove :: Eq a => [a] -> [a] -> [a]
remove = (\\)
