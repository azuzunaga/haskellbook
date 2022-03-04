module VariationsOnEither where

import           Test.QuickCheck                ( Arbitrary
                                                , arbitrary
                                                , frequency
                                                )
import           Test.QuickCheck.Checkers       ( EqProp(..)
                                                , eq
                                                , quickBatch
                                                )
import           Test.QuickCheck.Classes        ( applicative )

data Validation e a = Failure e | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap f (Failure x) = Failure x
  fmap f (Success y) = Success (f y)

instance Monoid e => Applicative (Validation e) where
  pure y = Success y
  (<*>) (Failure x ) (Failure x') = Failure (x <> x')
  (<*>) (Failure x ) (Success y ) = Failure x
  (<*>) (Success y ) (Failure x ) = Failure x
  (<*>) (Success fx) (Success x ) = Success (fx x)

instance (Arbitrary a, Arbitrary e)=> Arbitrary (Validation e a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(1, return (Failure x)), (1, return (Success y))]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch
    (applicative
      (undefined :: Validation (String, String, String) (String, String, String)
      )
    )
