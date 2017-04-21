{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Parser where


import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import GHC.Generics

-- | Type of each JSON entry in record syntax.
data Person =
  Person { firstName  :: !Text
         , lastName   :: !Text
         , age        :: Int
         , likesPizza :: Bool
           } deriving (Show,Generic)

-- Instances to convert our type to/from JSON.

instance FromJSON Person
instance ToJSON Person
