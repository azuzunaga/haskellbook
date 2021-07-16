module AsPatterns where

import           Data.Char                      ( toUpper )

isSubseqOf :: String -> String -> Bool
isSubseqOf input word = go input word "" False
 where
  go _ [] acc _ = input == acc
  go [] ys acc res | input == acc = True
                   | otherwise    = go input ys acc res
  go sub@(x : xs) (y : ys) acc res | x == y    = go xs ys (x : acc) True
                                   | otherwise = go sub ys acc False

test :: Bool
test = isSubseqOf "blah" "blahwoot" == True

test' :: Bool
test' = isSubseqOf "blah" "wootblah" == True

test'' :: Bool
test'' = isSubseqOf "blah" "wboloath" == True

test''' :: Bool
test''' = isSubseqOf "blah" "wootbla" == False

test'''' :: Bool
test'''' = isSubseqOf "blah" "halbwoot" == False

test''''' :: Bool
test''''' = isSubseqOf "blah" "blawhoot" == True

capitalizeWords :: String -> [(String, String)]
capitalizeWords phrase = go (words phrase) []
 where
  go []                acc = acc
  go ([]         : _ ) acc = acc
  go (x@(w : ws) : xs) acc = go xs (acc ++ [(x, toUpper w : ws)])

testCapitalizeWords :: Bool
testCapitalizeWords =
  capitalizeWords "hello world" == [("hello", "Hello"), ("world", "World")]

capitalizeWord :: String -> String
capitalizeWord []       = ""
capitalizeWord (x : xs) = toUpper x : xs

testCapitalizeWord :: Bool
testCapitalizeWord = capitalizeWord "Chortle" == "Chortle"

testCapitalizeWord' :: Bool
testCapitalizeWord' = capitalizeWord "chortle" == "Chortle"
