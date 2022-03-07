module BadMonad where

import           Test.QuickCheck                ( Arbitrary(arbitrary) )
import           Test.QuickCheck.Checkers       ( EqProp(..)
                                                , eq
                                                , quickBatch
                                                )
import           Test.QuickCheck.Classes        ( applicative
                                                , functor
                                                , monad
                                                )

data CountMe a = CountMe Integer a
  deriving (Eq, Show)

instance Functor CountMe where
  fmap f (CountMe i a) = CountMe i (f a)

instance Applicative CountMe where
  pure = CountMe 0
  CountMe n f <*> CountMe n' a = CountMe (n + n') (f a)

instance Monad CountMe where
  return = pure
  -- The `let` below binds the result of evaluating `f a` so that those values
  -- can be used in the `in` clause. So `n'` is the first data constructor of
  -- `CountMe` and `b` is the second. Then, in the `in` clause we use `n'` to
  -- add it to the original `n`. We need to do this because we now `f a` will
  -- produce a `CountMe` with two values, and for this to be a valid monad those
  -- new two resulting values need to be used for the end result of the bind
  -- operator.
  (>>=) (CountMe n a) f = let CountMe n' b = f a in CountMe (n + n') b

instance Arbitrary a => Arbitrary (CountMe a) where
  arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where
  (=-=) = eq

main :: IO ()
main = do
  let trigger :: CountMe (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
