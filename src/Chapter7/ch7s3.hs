module AnonymousFunctions where

addOneIfOdd :: Integral p => p -> p
addOneIfOdd n = case odd n of
  True  -> f n
  False -> n
  where f = \n -> n + 1

addFive :: (Num a, Ord a) => a -> a -> a
addFive = \x -> \y -> (if x > y then y else x) + 5

mflip :: (b -> a -> c) -> a -> b -> c
mflip f x y = f y x
