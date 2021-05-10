module Cipher where

import           Data.Char

-- handle upper and lowercase, ?guard with elem and range?

-- a = 97, z = 122, A = 65, Z = 90

uppercase :: [Char]
uppercase = ['A' .. 'Z']

lowercase :: [Char]
lowercase = ['a' .. 'z']

caesar :: Int -> String -> String
caesar o "" = ""
caesar o (x : xs)
  | x == ' '           = ' ' : caesar o xs
  | x `elem` lowercase = (!!) lowercase (newIndex x o (ord 'a')) : caesar o xs
  | otherwise          = (!!) uppercase (newIndex x o (ord 'A')) : caesar o xs
 where
  modAZ = flip mod 26
  newIndex letter offset startIdx =
    modAZ (ord letter + modAZ offset - startIdx)

unCaesar :: Int -> String -> String
unCaesar s = caesar (-s)
