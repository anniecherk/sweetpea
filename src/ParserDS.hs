{-# LANGUAGE OverloadedStrings #-}
module ParserDS
( decodeHLIR, decodeFactorPaths, decodeRawConstraint
  , FactorPath, FactorPaths, FullyCross(..), RawConstraint(..), HLIR(..) )
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



data HLIR = HLIR FactorPaths FullyCross [RawConstraint] deriving (Show, Eq)
instance FromJSON HLIR where
  parseJSON (Object v) =
            HLIR <$> v .: "factorPaths"
                  <*> v .: "fullyCross"
                  <*> v .: "constraints"
  parseJSON _ = mzero


-- just some suga to make testing cleaner

decodeHLIR :: BL.ByteString -> Maybe HLIR
decodeHLIR = decode

decodeFactorPaths :: BL.ByteString -> Maybe FactorPaths
decodeFactorPaths = decode

decodeRawConstraint :: BL.ByteString -> Maybe RawConstraint
decodeRawConstraint = decode
