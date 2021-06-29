module VigenereCipher where

import           Prelude

import           Data.Char                      ( chr
                                                , ord
                                                )

overlay :: String -> String -> String
overlay input encoder = go input encoder ""
 where
  go [] _  acc = acc
  go xs [] acc = go xs encoder acc
  go (x : xs) (y : ys) acc | [x] == " " = go xs (y : ys) (acc ++ " ")
                           | otherwise  = go xs ys (acc ++ [y])

distance :: Char -> Int
distance x = ord x - ord 'A'

shift :: Char -> Char -> Char
shift i e = chr $ 65 + mod (ord i + distance e - 65) 26

encode :: String -> String -> String
encode input encoder = go input (overlay input encoder) ""
 where
  go :: String -> String -> String -> String
  go [] _  acc = acc
  go _  [] acc = acc
  go (x : xs) (y : ys) acc | [x] == " " = go xs ys (acc ++ " ")
                           | otherwise  = go xs ys (acc ++ [shift x y])

test :: Bool
test = overlay "MEET AT DAWN" "ALLY" == "ALLY AL LYAL"

test' :: Bool
test' = encode "MEET AT DAWN" "ALLY" == "MPPR AE OYWY"
