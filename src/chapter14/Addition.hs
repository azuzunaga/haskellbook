module Addition where

import           Test.Hspec
import           Test.QuickCheck

sayHello :: IO ()
sayHello = putStrLn "hello!"

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
 where
  go n d count | n < d     = (count, n)
               | otherwise = go (n - d) d (count + 1)

recMult :: (Integral a) => a -> a -> a
recMult x y = go x y 0
 where
  go a b res | b == 0    = res
             | otherwise = go a (b - 1) (res + a)

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0 :: Int)
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4, 2 :: Int)
    it "3 times 2 is 6" $ do
      recMult 3 2 `shouldBe` (6 :: Int)
    it "x + 1 is always greater that x" $ do
      property $ \x -> x + 1 > (x :: Int)
