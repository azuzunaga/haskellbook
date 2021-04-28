module PatternMatching where

k :: (a, b) -> a
k (x, y) = x

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))
