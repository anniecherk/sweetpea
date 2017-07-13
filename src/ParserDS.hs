module ParserDS
()
where






-- lightColors = Label 'light' ['pink', 'blue', 'yellow']   -- Maybe call it a set
-- darkColors  = Label 'dark'  ['blue', 'black']
-- color       = RLabel 'color' [lightColors, darkColors]

-- TODO: maybe think about using overloaded strings
data Label = RLabel String [Label] | Label String [String]

data Constraints = NoConstraints | FullyCross [[String]] | CountConstraints Int -- TODO

-- a block is how many trials & the constraints applied to them
-- TODO: sub-blocks
data Block = Block Int [Constraints] | RBlock Block [Constraints]

-- TODO: clearly. Do I want this here for syntax? idk
cross :: [[a]] -> [[a]]
cross = id

-- thanks SO: https://stackoverflow.com/questions/4119730/cartesian-product-of-2-lists-in-haskell
fullycross :: [[String]] -> Block
fullycross labels = Block (length $ sequence labels) [FullyCross labels]




-- experiment $ RBlock (fully-cross design) theConstraints)
--        where
--         color  = Stream "color" ["red", "blue"]
--         shape  = Stream "shape" ["circle", "square"]
--         noConstraints = Constraints NoConstraints
--         design = cross [color, shape]

--
-- theConstraints = Constraints (count (None, 3) color)     -- this count syntax doesn't fly
--
-- -- syntax/constructor for "no constraints"
-- noConstraints = []
--
--
--
-- type Constraints = Constraints [ParserConstraint]
--
-- data Count = IntCount Int | MaxCount
--
-- data ParserConstraint =
--   Count Int Count StreamReference  |  -- Count will desugar into <, > , ==
--   MaxInARow Int StreamReference |
--   Avoid StreamReference |
--   Include StreamReference |
--   NoConstraints deriving (Show, Eq)
--
--
-- -- can I use a state monad to hold the streams? for now assume no and refactor
--
--   maxInARow :: Int -> Stream -> ParserConstraint
--   maxInARow maxAmount whichStream = Count 0 (IntCount maxAmount) whichStream
--
--   minInARow
--
--   count :: (Count, Count) -> Stream -> ParserConstraint
--   avoid :: Stream -> ParserConstraint
--   include :: Stream -> ParserConstraint
--
--   noConstraints :: ParserConstraints
--
--
--   all
--
--
--
--
--
--
--
--
--
--
--
--
--
-- type FactorPath = [String] -- an instance of a factorpath
-- type FactorPaths = [FactorPath] --can hold all factorpaths
--
-- data FullyCross = FullyCross FactorPaths Int deriving (Show, Eq)
-- instance FromJSON FullyCross where
--   parseJSON (Object v) =
--             FullyCross <$> v .: "applied_to"
--                        <*> v .: "repetitions"
--   parseJSON _ = mzero
--
-- -- resolving the ints...
-- data RawConstraint =
--   NoMoreThanKInARow  FactorPaths Int |
--   AtLeastKInARow     FactorPaths Int |
--   NoMoreThanKOutOfJ  FactorPaths Int Int |
--   AtLeastKOutOfJ     FactorPaths Int Int |
--   BalanceTransitions FactorPaths deriving (Show, Eq)
--
-- instance FromJSON RawConstraint where
--     parseJSON (Object x) =
--         do (String oc) <- x .: "constraint"
--            case oc of
--                "NoMoreThanKInARow"  -> NoMoreThanKInARow  <$> x .: "applied_to" <*> x .: "k"
--                "AtLeastKInARow"     -> AtLeastKInARow     <$> x .: "applied_to" <*> x .: "k"
--                "NoMoreThanKOutOfJ"  -> NoMoreThanKOutOfJ  <$> x .: "applied_to" <*> x .: "k" <*> x .: "j"
--                "AtLeastKOutOfJ"     -> AtLeastKOutOfJ     <$> x .: "applied_to" <*> x .: "k" <*> x .: "j"
--                "BalanceTransitions" -> BalanceTransitions <$> x .: "applied_to"
--                _                    -> mzero
--
--
--
-- data HLIR = HLIR FactorPaths FullyCross [RawConstraint] deriving (Show, Eq)
-- instance FromJSON HLIR where
--   parseJSON (Object v) =
--             HLIR <$> v .: "factorPaths"
--                   <*> v .: "fullyCross"
--                   <*> v .: "constraints"
--   parseJSON _ = mzero
--
--
-- -- just some suga to make testing cleaner
--
-- decodeHLIR :: BL.ByteString -> Maybe HLIR
-- decodeHLIR = decode
--
-- decodeFactorPaths :: BL.ByteString -> Maybe FactorPaths
-- decodeFactorPaths = decode
--
-- decodeRawConstraint :: BL.ByteString -> Maybe RawConstraint
-- decodeRawConstraint = decode
