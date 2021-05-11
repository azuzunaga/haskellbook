module WritingFolds where

import           Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  , DbNumber 9003
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr getDbDates []
 where
  getDbDates (DbDate utc) utcs = utc : utcs
  getDbDates _            utcs = utcs

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr getDbNumbers []
 where
  getDbNumbers (DbNumber int) ints = int : ints
  getDbNumbers _              ints = ints

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb db = fromInteger (sumDb db) / fromIntegral (length $ filterDbNumber db)
