module FrontEndSugar
( fullyCrossedBlock -- , multiFullyCrossedBlock
, remove )
where

import FrontEnd
import DataStructures
import Data.List ((\\))


-- TODO CANT HANDLE CROSSING /= DESIGN!!
fullyCrossedBlock :: Design -> Design -> [HLConstraint] -> HLBlock
fullyCrossedBlock design crossing constraints = makeBlock numTrials design allConstraints
  where numTrials      = fullyCrossSize crossing
        allConstraints = (FullyCross crossing) : constraints        

-- -- TODO CANT HANDLE CROSSING /= DESIGN!!
-- multiFullyCrossedBlock :: Int -> Design -> [HLConstraint] -> HLBlock
-- multiFullyCrossedBlock reps design constraints =  makeBlock numTrials design allConstraints
--   where numTrials = multiFullyCrossSize design reps
--         allConstraints = FullyCross : constraints


remove :: Eq a => [a] -> [a] -> [a]
remove = (\\)
