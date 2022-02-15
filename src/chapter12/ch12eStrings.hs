module StringProcessing where

notThe :: String -> String
notThe "the" = "a"
notThe s     = s

--  Prelude> replaceThe "the cow loves us"
--  "a cow loves us"
replaceThe :: String -> String
replaceThe = unwords . map notThe . words

startsWithBowel :: String -> Bool
startsWithBowel []      = False
startsWithBowel (w : _) = w `elem` "aieou"

-- Prelude> countTheBeforeVowel "the cow"
-- 0
-- Prelude> countTheBeforeVowel "the evil cow"
-- 1
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel sentence = go (words sentence) "" 0
 where
  go [] _ count = count
  go (w : ws) prevWord count
    | w == "the"                             = go ws "the" count
    | prevWord == "the" && startsWithBowel w = go ws "" count + 1
    | otherwise                              = go ws prevWord count

-- Prelude> countVowels "the cow"
-- 2
-- Prelude> countVowels "Mikolajczak"
-- 4
countVowels :: String -> Int
countVowels sentence = go sentence 0
 where
  go [] count = count
  go (c : cs) count | c `elem` "aieou" = go cs count + 1
                    | otherwise        = go cs count

newtype Word' = Word' String deriving (Eq, Show)

vowels :: [Char]
vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord word = if vowels > length word - vowels
  then Nothing
  else Just (Word' word)
  where vowels = countVowels word
