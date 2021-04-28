module Guards where

pal :: Eq a => [a] -> Bool
pal xs | xs == reverse xs = True
       | otherwise        = False

numbers :: (Ord a, Num a) => a -> a
numbers x | x < 0  = -1
          | x == 0 = 0
          | x > 0  = 1
