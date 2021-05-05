module ExtractingFromLists where

myWords :: String -> [String]
myWords = strSplit ' ' []

firstSen :: String
firstSen = "Tyger Tyger, burning bright\n"

secondSen :: String
secondSen = "In the forests of the night\n"

thirdSen :: String
thirdSen = "What immortal hand or eye\n"

fourthSen :: String
fourthSen = "Could frame thy fearful\
            \ symmetry?"

sentences :: String
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines = strSplit '\n' []

strSplit :: Char -> [String] -> String -> [String]
strSplit sep list string
  | string == "" = reverse list
  | otherwise    = strSplit sep (beforeSep string : list) (afterSep string)
 where
  beforeSep = takeWhile (/= sep)
  afterSep  = dropWhile (== sep) . dropWhile (/= sep)

shouldEqual :: [String]
shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]
