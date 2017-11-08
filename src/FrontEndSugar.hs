module FrontEndSugar
( fullyCrossedBlock, multiFullyCrossedBlock )
where

import FrontEnd
import DataStructures
import Data.List ((\\))


-- TODO CANT HANDLE CROSSING /= DESIGN!!
fullyCrossedBlock :: Design -> [HLConstraint] -> HLBlock
fullyCrossedBlock design constraints = makeBlock numTrials design allConstraints
  where numTrials = fullyCrossSize design -- function to gen fully crossed block: hide this inside
        allConstraints = FullyCross : constraints        -- function to write multifullycroseed block

-- TODO CANT HANDLE CROSSING /= DESIGN!!
multiFullyCrossedBlock :: Int -> Design -> [HLConstraint] -> HLBlock
multiFullyCrossedBlock reps design constraints =  makeBlock numTrials design allConstraints
  where numTrials = multiFullyCrossSize design reps
        allConstraints = FullyCross : constraints


remove :: Eq a => [a] -> [a] -> [a]
remove = (\\)
