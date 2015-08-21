{-# LANGUAGE OverloadedStrings #-}

module Data.Time.Human where

import Data.Monoid
import Data.Word
import Text.Pretty

import qualified Data.List as L
import qualified Data.Text as T

data TimePeriod = TimePeriod TimeValue TimeUnit deriving (Eq, Show)

type TimeValue = Word64

data TimeUnit = MicroSecond
              | MilliSecond
              |      Second
              |      Minute
              |        Hour
              |         Day
              |        Week
              |       Month
              |        Year
    deriving (Eq, Ord, Show)

value :: TimePeriod -> TimeValue
value (TimePeriod v _) = v

unit :: TimePeriod -> TimeUnit
unit (TimePeriod _ u) = u

data TimeUnitEquivalence = TimeUnit := TimePeriod
infix 8 :=

equivalences :: [TimeUnitEquivalence]
equivalences =
    [        Year :=   12 ×       Month -- approximation
    ,       Month :=    4 ×        Week -- approximation
    ,        Week :=    7 ×         Day
    ,         Day :=   24 ×        Hour -- approximation
    ,        Hour :=   60 ×      Minute
    ,      Minute :=   60 ×      Second
    ,      Second := 1000 × MilliSecond
    , MilliSecond := 1000 × MicroSecond
    ]
    where (×) = TimePeriod

findEquivalenceForSmallUnit :: TimeUnit -> Maybe TimeUnitEquivalence
findEquivalenceForLargeUnit :: TimeUnit -> Maybe TimeUnitEquivalence

findEquivalenceForLargeUnit u = L.find (\(v := TimePeriod _ _) -> u == v) equivalences
findEquivalenceForSmallUnit u = L.find (\(_ := TimePeriod _ v) -> u == v) equivalences

largeUnit :: TimeUnitEquivalence -> TimeUnit
smallUnit :: TimeUnitEquivalence -> TimeUnit

largeUnit (u := TimePeriod _ _) = u
smallUnit (_ := TimePeriod _ u) = u

factor :: TimeUnitEquivalence -> TimeValue
factor (_ := TimePeriod f _) = f

split :: TimePeriod -> [TimePeriod]
split = minimize . L.reverse . minimize . divide where

    minimize :: [TimePeriod] -> [TimePeriod]
    minimize = L.dropWhile $ (== 0) . value

    divide :: TimePeriod -> [TimePeriod]
    divide = L.unfoldr divideOnce

    divideOnce :: TimePeriod -> Maybe (TimePeriod, TimePeriod)
    divideOnce (TimePeriod a u) = do
        v := TimePeriod b _ <- findEquivalenceForSmallUnit u
        return ( TimePeriod (a `mod` b) u
               , TimePeriod (a `div` b) v )

instance Pretty TimeUnit where
    pretty = T.toLower . T.take 1 . T.pack . show

instance Pretty TimePeriod where
    pretty (TimePeriod a u) =
        T.pack (show a)
     -- <> " "
        <> pretty u
     -- <> if a == 1 then "" else "s"

data HumanTimePeriod = Human Precision TimePeriod

type Precision = Int

instance Pretty HumanTimePeriod where
    pretty (Human precision period) =
        let periods = case split period of
                [] -> [TimePeriod 0 Second]
                x -> x in
        T.intercalate " " (fmap pretty (take precision periods))

