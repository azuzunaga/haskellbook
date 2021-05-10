module FilteringLists where

filterMultiples :: Integer -> [Integer] -> [Integer]
filterMultiples _ [] = []
filterMultiples n xs = [ x | x <- xs, rem x n == 0 ]

filterArticles :: String -> [String]
filterArticles = filter (\x -> x /= "the" && x /= "a" && x /= "an") . words
