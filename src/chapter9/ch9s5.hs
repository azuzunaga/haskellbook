module Ranges where

eftBool :: Bool -> Bool -> [Bool]
eftBool True  _ = []
eftBool False _ = [False, True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd first last = go first last []

eftInt :: Int -> Int -> [Int]
eftInt first last = go first last []

eftChar :: Char -> Char -> [Char]
eftChar first last = go first last []

go :: (Ord a, Enum a) => a -> a -> [a] -> [a]
go a z list | a > z     = []
            | a == z    = a : list
            | otherwise = go a (pred z) (z : list)
