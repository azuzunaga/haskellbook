module Unfolds where

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f v = case f v of
  Nothing     -> []
  Just (a, b) -> a : myUnfoldr f b

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\y -> Just (y, f y))
