module ListApplicative where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where
  Nil         <> Nil         = Nil
  Nil         <> (Cons x xs) = Cons x xs
  (Cons x xs) <> Nil         = Cons x xs
  (Cons x xs) <> ys          = Cons x (xs <> ys)

instance Monoid (List a) where
  mempty = Nil

instance Functor List where
  fmap f Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _   = Nil
  (<*>) _   Nil = Nil
  (<*>) (Cons fx fxs) ys'@(Cons y ys) =
    Cons (fx y) (fx <$> ys) <> (fxs <*> ys')

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    return $ Cons x (Cons x (Cons x Nil))

instance Eq a => EqProp (List a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch (applicative (undefined :: List (String, String, String)))
