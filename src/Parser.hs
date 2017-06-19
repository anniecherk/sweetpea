{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Control.Monad (mzero)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Text

data BinValTree = Zero | One | Branch [BinValTree]
  deriving Show

instance FromJSON BinValTree where
  parseJSON (Number 0)  = return Zero
  parseJSON (Number 1)  = return One
  parseJSON a@(Array _) = Branch `fmap` parseJSON a
  parseJSON _           = mzero



-- ie
-- Color
--     Dark Colors
--        Black
--        Pink -- it's a really dark pink
--     Light Colors
--         Pink
--         White
-- -- this is represented as
-- (Feature Color
--       (NestedLabel  DarkColors (TerminalLabel Black) (TerminalLabel Pink))
--       (NestedLabel LightColors (TerminalLabel Pink)  (TerminalLabel White)))
--
-- Only leaf nodes are assigned variable #'s, and we store them. --> this happens outside the DS
-- These number will be important for propositionalizing HL constraints.

-- type LevelID = Int

data Feature = Feature String [Label] deriving Show
data Label = TerminalLabel String | NestedLabel String [Label] deriving Show

instance FromJSON Label where
  parseJSON (String s) = return $ TerminalLabel (unpack s)
  parseJSON (Object v) =
            NestedLabel <$> v .: pack "name"
                        <*> v .: pack "children"
  parseJSON _ = mzero

instance FromJSON Feature where
  parseJSON (Object v) =
            Feature <$> v .: pack "name"
                    <*> v .: pack "children"
  parseJSON _ = mzero


data HL_IR = HL_IR [Feature] [RawConstraint]
-- resolving the ints...
data RawConstraint =
  NoMoreThanKInARow Int Int |
  AtLeastKInARow Int Int |
  NoMoreThanKOutOfJ Int Int Int |
  AtLeastKOutOfJ Int Int Int |
  BalanceTransitions Int |
  FullyCross [Int] Int











data Example = Example String String deriving Show
instance FromJSON Example where
  parseJSON = withObject "tuple" $ \o -> do
    a <- o .: pack "a"
    b <- o .: pack "b"
    return (Example a b)






main :: IO ()
main = BL.getContents >>= print . decodeObjects


decodeObjects :: BL.ByteString -> Maybe Feature
decodeObjects = decode

-- decodeObjects :: BL.ByteString -> Maybe Feature
-- decodeObjects = decode


-- decodeExample :: BL.ByteString -> Maybe Example
-- decodeExample = decode



-- | Reads stdin, and parses as JSON encoded BinValTree
-- main :: IO ()
-- main = BL.getContents >>= print . decodeBinValTree
--
-- decodeBinValTree :: BL.ByteString -> Maybe BinValTree
-- decodeBinValTree = decode
