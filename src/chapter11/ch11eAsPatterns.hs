module AsPatterns where

import           Data.Char                      ( toUpper )

isSubseqOf :: String -> String -> Bool
isSubseqOf input word = go input word "" False
 where
  go _ [] acc _ = input == acc
  go [] ys acc res | input == acc = True
                   | otherwise    = go input ys acc res
  go sub@(x : xs) (y : ys) acc res | x == y    = go xs ys (acc ++ [x]) True
                                   | otherwise = go sub ys acc False

test :: Bool
test = isSubseqOf "blah" "blahwoot"

test' :: Bool
test' = isSubseqOf "blah" "wootblah"

test'' :: Bool
test'' = isSubseqOf "blah" "wboloath"

test''' :: Bool
test''' = isSubseqOf "blah" "wootbla"

test'''' :: Bool
test'''' = isSubseqOf "blah" "halbwoot"

test''''' :: Bool
test''''' = isSubseqOf "blah" "blawhoot"

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
capitalizeWord [] = ""
capitalizeWord (x : xs) | x == ' '  = x : (toUpper (head xs) : tail xs)
                        | otherwise = toUpper x : xs

testCapitalizeWord :: Bool
testCapitalizeWord = capitalizeWord "Chortle" == "Chortle"

testCapitalizeWord' :: Bool
testCapitalizeWord' = capitalizeWord "chortle" == "Chortle"

capitalizeParagraph :: String -> String
capitalizeParagraph = join "." . map capitalizeWord . strSplit '.' []

strSplit :: Char -> [String] -> String -> [String]
strSplit sep list string
  | string == "" = reverse list
  | otherwise    = strSplit sep (beforeSep string : list) (afterSep string)
 where
  beforeSep = takeWhile (/= sep)
  afterSep  = dropWhile (== sep) . dropWhile (/= sep)

join :: String -> [String] -> String
join _   []       = ""
join _   [x     ] = x
join sep (x : xs) = x ++ sep ++ join sep xs
