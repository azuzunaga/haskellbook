module PropertyTests where

import           Data.List                      ( sort )
import           Test.Hspec
import           Test.QuickCheck

half :: Float -> Float
half x = x / 2

halfIdentity :: Float -> Float
halfIdentity = (* 2) . half

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
 where
  go _ status@(_      , False) = status
  go y (       Nothing, t    ) = (Just y, t)
  go y (       Just x , _    ) = (Just y, x >= y)

prop_quotRem :: Positive Integer -> Positive Integer -> Bool
prop_quotRem (Positive x) (Positive y) = quot x y * y + rem x y == x

prop_divMod :: Positive Integer -> Positive Integer -> Bool
prop_divMod (Positive x) (Positive y) = div x y * y + mod x y == x

prop_expAssoc :: Property
prop_expAssoc = forAll (elements [2 :: Int .. 4]) $ \x ->
  forAll (elements [5 :: Int .. 7]) $ \y ->
    forAll (elements [8 :: Int .. 9]) $ \z -> x ^ (y ^ z) /= (x ^ y) ^ z

prop_expCom :: Property
prop_expCom = forAll (elements [2 :: Int .. 5])
  $ \x -> forAll (elements [6 :: Int .. 9]) $ \y -> x ^ y /= y ^ x

main :: IO ()
main = hspec $ do
  it "halves" $ do
    property $ \x -> x == halfIdentity x
  it "sorts" $ do
    property $ \xs -> listOrdered $ sort (xs :: [Char]) -- Can I use a typeclass?
  it "addition associates" $ do
    property $ \x y z -> x + (y + z) == (x + y) + (z :: Int)
  it "addition commutes" $ do
    property $ \x y -> x + y == y + (x :: Int)
  it "multiplication associates" $ do
    property $ \x y z -> x * (y * z) == (x * y) * (z :: Int)
  it "multiplication commutes" $ do
    property $ \x y -> x * y == y * (x :: Int)
  it "quote rems" $ do
    property prop_quotRem
  it "div mods" $ do
    property prop_divMod
  -- it "exponentiation does not associate" $ do
  --   property prop_expAssoc
  it "exponentiation does not commute" $ do
    property prop_expCom
  it "double reverses" $ do
    property $ \xs -> (reverse . reverse $ xs) == id (xs :: [Char])
