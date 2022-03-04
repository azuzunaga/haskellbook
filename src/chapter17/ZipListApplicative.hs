module ZipListApplicative where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes


newtype ZipList' a = ZipList' [a]
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
   where
    xs' = let (ZipList' l) = xs in take 3000 l
    ys' = let (ZipList' l) = ys in take 3000 l

instance Semigroup (ZipList' a) where
  ZipList' xs <> ZipList' ys = ZipList' (xs <> ys)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' $ repeat x
  (<*>) (ZipList' []) (ZipList' []) = ZipList' []
  (<*>) (ZipList' []) (ZipList' xs) = ZipList' []
  (<*>) (ZipList' xs) (ZipList' []) = ZipList' []
  (<*>) (ZipList' (fx : fxs)) (ZipList' (x : xs)) =
    ZipList' [fx x] <> (ZipList' fxs <*> ZipList' xs)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = do
    x <- arbitrary
    return $ ZipList' [x]

main :: IO ()
main = do
  quickBatch (applicative (undefined :: ZipList' (String, String, String)))
