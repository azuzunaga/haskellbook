module Chapter5 where

i :: a -> a
i a = a

c :: a -> b -> a
c a b = a

c' :: a -> b -> b
c' a b = b

r :: [a] -> [a]
r = tail

co :: (b -> c) -> (a -> b) -> a -> c
co b2c a2b a = b2c $ a2b a

a :: (a -> c) -> a -> a
a _ x = x

a' :: (a -> b) -> a -> b
a' a2b = a2b
