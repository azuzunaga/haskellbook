module HeavyLifting where

import           Test.Hspec

-- 1. a = (+ 1) $ read "[1]" :: Int
a :: [Int]
a = (+ 1) <$> read "[1]" :: [Int]

-- 2. b = (++ "lol") (Just ["Hi,", "Hello"])
b :: Maybe [[Char]]
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- 3. c = (*2) (\x -> x - 2)
c :: Integer -> Integer
c = (* 2) <$> (\x -> x - 2)

-- 4. ((return '1' ++) . show) (\x -> [x, 1..3])
d :: Integer -> [Char]
d = (return '1' ++) . show <$> (\x -> [x, 1 .. 3])

-- 5.
e :: IO Integer
e =
  let ioi     = readIO "1" :: IO Integer
      changed = read . ("123" ++) . show <$> ioi
  in  (* 3) <$> changed

main :: IO ()
main = hspec $ do
  describe "Heavy Lifting" $ do
    it "1. a == [2]" $ do
      a `shouldBe` [2]
    it "2. b ==  Just [\"Hi,lol\",\"Hellolol\"]" $ do
      b `shouldBe` Just ["Hi,lol", "Hellolol"]
    it "3. c 1 == -2" $ do
      c 1 `shouldBe` (-2)
    it "4. d 0 == 1[0,1,2,3]" $ do
      d 0 `shouldBe` "1[0,1,2,3]"
    it "5. e == 3693" $ do
      e `shouldReturn` 3693
