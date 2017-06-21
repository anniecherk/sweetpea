{-# LANGUAGE OverloadedStrings #-}
module Parser
( decodeHL_IR, decodeFactorPaths, decodeRawConstraint
  , FactorPath, FactorPaths, FullyCross(..), RawConstraint(..), HL_IR(..) )
where

import Control.Monad (mzero)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Text


type FactorPath = [String] -- an instance of a factorpath
type FactorPaths = [FactorPath] --can hold all factorpaths

data FullyCross = FullyCross FactorPaths Int deriving (Show, Eq)
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
  BalanceTransitions FactorPaths deriving (Show, Eq)

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



data HL_IR = HL_IR FactorPaths FullyCross [RawConstraint] deriving (Show, Eq)
instance FromJSON HL_IR where
  parseJSON (Object v) =
            HL_IR <$> v .: "factorPaths"
                  <*> v .: "fullyCross"
                  <*> v .: "constraints"
  parseJSON _ = mzero


-- just some suga to make testing cleaner

decodeHL_IR :: BL.ByteString -> Maybe HL_IR
decodeHL_IR = decode

decodeFactorPaths :: BL.ByteString -> Maybe FactorPaths
decodeFactorPaths = decode

decodeRawConstraint :: BL.ByteString -> Maybe RawConstraint
decodeRawConstraint = decode


-- main :: IO ()
-- main = BL.getContents >>= print .(decode :: BL.ByteString -> Maybe HL_IR)
