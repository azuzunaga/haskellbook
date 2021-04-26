module ChapterFour where

awesome :: [[Char]]
awesome = ["Papuchon", "curry", ":)"]

also :: [[Char]]
also = ["Quake", "The Simons"]

allAwesome :: [[[Char]]]
allAwesome = [awesome, also]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse x == x

myAbs :: Integer -> Integer
myAbs x = if x > 0 then x else x * (-1)

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((b, d), (a, c))

x :: Int -> Int -> Int
x = (+)

f' :: Foldable t => t a -> Int
f' xs = w `x` 1
  where
    w = length xs

f'' :: p -> p
f'' x = x

f''' :: (a, b) -> a
f''' (a, b) = a
