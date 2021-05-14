module Scans where

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

fibs' :: [Integer]
fibs' = take 20 fibs

fibs'' :: [Integer]
fibs'' = filter (< 100) fibs'

factorial :: Integer -> [Integer]
factorial a = 1 : scanl factorial
