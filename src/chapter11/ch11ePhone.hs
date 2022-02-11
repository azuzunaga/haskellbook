module Phone where

import           Data.Char                      ( isAlphaNum
                                                , isAsciiUpper
                                                , isDigit
                                                , toLower
                                                )
import           Data.List                      ( elemIndex
                                                , maximumBy
                                                , sortBy
                                                )
import           Data.Map                       ( (!)
                                                , Map
                                                )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromJust )

type Digit = Char

type Presses = Int

type KeyPress = [(Digit, Presses)]

phoneMap :: Map Digit [Char]
phoneMap = Map.fromList
  [ ('#', ['.', ',', '#'])
  , ('0', [' ', '0'])
  , ('1', ['1'])
  , ('2', ['a', 'b', 'c', '2'])
  , ('3', ['d', 'e', 'f', '3'])
  , ('4', ['g', 'h', 'i', '4'])
  , ('5', ['j', 'k', 'l', '5'])
  , ('6', ['m', 'n', 'o', '6'])
  , ('7', ['p', 'q', 'r', 's', '7'])
  , ('8', ['t', 'u', 'v', '7'])
  , ('9', ['w', 'x', 'y', 'z', '9'])
  ]

toDigit :: Maybe Char -> Maybe Digit
toDigit Nothing    = Nothing
toDigit (Just ' ') = Just '0'
toDigit (Just char) | isDigit char               = Just char
                    | char == '.' || char == ',' = Just '#'
                    | char >= 'a' && char <= 'c' = Just '2'
                    | char >= 'd' && char <= 'f' = Just '3'
                    | char >= 'g' && char <= 'i' = Just '4'
                    | char >= 'j' && char <= 'l' = Just '5'
                    | char >= 'm' && char <= 'o' = Just '6'
                    | char >= 'p' && char <= 's' = Just '7'
                    | char >= 't' && char <= 'v' = Just '8'
                    | otherwise                  = Just '9'

presses :: Maybe Char -> Maybe Presses
presses Nothing     = Nothing
presses (Just char) = case elemIndex char (phoneMap ! digit) of
  Nothing  -> Nothing
  Just int -> Just (int + 1)
  where digit = fromJust $ toDigit (Just char)

sanitize :: Char -> Maybe Char
sanitize char
  | isAlphaNum lowered || lowered == '.' || lowered == ',' || lowered == ' '
  = Just lowered
  | otherwise
  = Nothing
  where lowered = toLower char

charToCombo :: Char -> KeyPress
charToCombo char
  | isAsciiUpper char
  = [('*', 1), (fromJust $ toDigit sanitized, fromJust $ presses sanitized)]
  | otherwise
  = [(fromJust $ toDigit sanitized, fromJust $ presses sanitized)]
  where sanitized = sanitize char

convoToCombos :: [String] -> [KeyPress]
convoToCombos = map $ concatMap charToCombo

convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol OK. Have u ever tasted alcohol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "OK. Do u think I am pretty Lol"
  , "Lol ya"
  , "Just making sure rofl ur turn"
  ]

fingerTaps :: KeyPress -> Presses
fingerTaps = foldr ((+) . snd) 0

mostPopularLetter :: String -> Char
mostPopularLetter = getHighest . countStuff

countStuff :: Ord a => [a] -> Map a Int
countStuff list = go list Map.empty
 where
  go []       map = map
  go (x : xs) map = go xs (Map.insertWith (+) x 1 map)

getHighest :: Map a Int -> a
getHighest =
  fst . head . sortBy (\(_, v1) (_, v2) -> compare v2 v1) . Map.toList
