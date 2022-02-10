module Chapter10Exercises where

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [(Char, Char, Char)]
combos = [ (x, y, z) | x <- stops, x == 'p', y <- vowels, z <- stops ]

nouns :: [String]
nouns = ["time", "way", "year", "work", "government"]

verbs :: [String]
verbs = ["is", "has", "does", "says", "gets"]

wordCombos :: [(String, String, String)]
wordCombos = [ (x, y, z) | x <- nouns, y <- verbs, z <- nouns ]

seekritFunc :: Fractional a => String -> a
seekritFunc x =
  fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x acc -> f x || acc) False

myElem :: Eq a => a -> [a] -> Bool
myElem e = foldr (\x acc -> x == e || acc) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' e = myAny (== e)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x : acc) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x acc -> if f x then x : acc else acc) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x acc -> f x ++ acc) []

squish' :: [[a]] -> [a]
squish' []       = []
squish' (x : xs) = squishMap (x ++) xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x : xs) =
  foldr (\x acc -> if f x acc == GT then x else acc) x xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x : xs) =
  foldr (\x acc -> if f x acc == LT then x else acc) x xs
