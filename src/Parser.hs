{-# LANGUAGE OverloadedStrings #-}
module Parser -- ( decodeObjects, decodeConstraints )
where

import Control.Monad (mzero)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Text


type FactorPath = [String] -- an instance of a factorpath
type FactorPaths = [FactorPath] --can hold all factorpaths

data FullyCross = FullyCross FactorPaths Int deriving Show
instance FromJSON FullyCross where
  parseJSON (Object v) =
            FullyCross <$> v .: "applied_to"
                       <*> v .: "repetitions"
  parseJSON _ = mzero

-- resolving the ints...
data RawConstraint =
  NoMoreThanKInARow  FactorPaths Int |
  AtLeastKInARow     FactorPaths Int |
  NoMoreThanKOutOfJ  FactorPaths Int Int |
  AtLeastKOutOfJ     FactorPaths Int Int |
  BalanceTransitions FactorPaths deriving Show

instance FromJSON RawConstraint where
    parseJSON (Object x) =
        do (String oc) <- x .: "constraint"
           case oc of
               "NoMoreThanKInARow"  -> NoMoreThanKInARow  <$> x .: "applied_to" <*> x .: "k"
               "AtLeastKInARow"     -> AtLeastKInARow     <$> x .: "applied_to" <*> x .: "k"
               "NoMoreThanKOutOfJ"  -> NoMoreThanKOutOfJ  <$> x .: "applied_to" <*> x .: "k" <*> x .: "j"
               "AtLeastKOutOfJ"     -> AtLeastKOutOfJ     <$> x .: "applied_to" <*> x .: "k" <*> x .: "j"
               "BalanceTransitions" -> BalanceTransitions <$> x .: "applied_to"
               _                    -> mzero



data HL_IR = HL_IR FactorPaths FullyCross [RawConstraint] deriving Show
instance FromJSON HL_IR where
  parseJSON (Object v) =
            HL_IR <$> v .: "factorPaths"
                  <*> v .: "fullyCross"
                  <*> v .: "constraints"
  parseJSON _ = mzero


-- main :: IO ()
-- main = BL.getContents >>= print .(decode :: BL.ByteString -> Maybe HL_IR)
