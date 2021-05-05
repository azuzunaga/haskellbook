module ListComprehensions where

mySqr :: [Integer]
mySqr = [ x ^ 2 | x <- [1 .. 5] ]

myCube :: [Integer]
myCube = [ y ^ 3 | y <- [1 .. 5] ]

a :: [Integer]
a = [ x | x <- mySqr, rem x 2 == 0 ]

a' :: [(Integer, Integer)]
a' = [ (x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50 ]

sct :: [(Integer, Integer)]
sct = [ (x, y) | x <- mySqr, x < 50, y <- myCube, y < 50 ]
