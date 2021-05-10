module Chapter9Exercises where

import           Data.Char

getUppercase :: String -> String
getUppercase = filter isUpper

capitalize :: String -> String
capitalize (x : xs) = toUpper x : xs

capitalize' :: String -> String
capitalize' ""       = ""
capitalize' (x : xs) = toUpper x : capitalize' xs

capFirstLetter :: String -> Char
capFirstLetter = toUpper . head

myOr :: [Bool] -> Bool
myOr []       = True
myOr (x : xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []       = False
myAny f (x : xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ []       = False
myElem y (x : xs) = x == y || myElem y xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' y = any (== y)

myReverse :: [a] -> [a]
myReverse []       = []
myReverse (x : xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish []       = []
squish (x : xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ []       = []
squishMap f (x : xs) = f x ++ squishMap f xs

squish' :: [[a]] -> [a]
squish' (x : xs) = squishMap (x ++) xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x : xs) = go f xs x
 where
  go f (y : ys) greatest | f greatest y == GT || f greatest y == EQ = greatest
                         | f greatest y == LT                       = go f ys y

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x : xs) = go f xs x
 where
  go f (y : ys) least | f least y == LT || f least y == EQ = least
                      | f least y == GT                    = go f ys y

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
