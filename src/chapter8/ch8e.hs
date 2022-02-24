module Chapter8Exercises where

import           Data.List                      ( intersperse )

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappy :: String -> String
frappy = flippy "haha"

one :: String
one = appedCatty "woohoo" -- woops mrow woohoo

two :: String
two = frappy "1" -- 1 mrow haha

three :: String
three = frappy (appedCatty "2") -- woops mrow 2 mrow haha

four :: String
four = appedCatty (frappy "blue") -- woops mrow blue mrow haha

-- pink mrow haha mrow green mrow woops mrow blue
five :: String
five = cattyConny (frappy "pink") (cattyConny "green" (appedCatty "blue"))

-- are mrow pugs mrow awesome
six :: String
six = cattyConny (flippy "pugs" "are") "awesome"

recSum :: (Eq a, Num a) => a -> a
recSum 0 = 0
recSum 1 = 1
recSum n = n + recSum (n - 1)

recMult :: (Integral a) => a -> a -> a
recMult x y = go x y 0
 where
  go a b res | b == 0    = res
             | otherwise = go a (b - 1) (res + a)

data DividedResult = Result Integer | DividedByZero deriving Show

dividedBy :: Integer -> Integer -> DividedResult
dividedBy num den = go (abs num) (abs den) 0
 where
  go n d count | d == 0    = DividedByZero
               | n < d     = Result (getSign num * getSign den * count)
               | otherwise = go (n - d) d (count + 1)
  getSign a = if a < 0 then (-1) else 1

mc91 :: Integer -> Integer
mc91 n | n > 100   = n - 10
       | otherwise = mc91 $ mc91 $ n + 11

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"

digits :: Int -> [Int]
digits num = go num []
 where
  go n ns | n == 0    = ns
          | otherwise = go (n `div` 10) ((n `mod` 10) : ns)

wordNumber :: Int -> String
wordNumber n = concat $ intersperse ['-'] $ map digitToWord (digits n)
