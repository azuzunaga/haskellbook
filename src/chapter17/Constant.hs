module Constant where

newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Show)

instance Functor (Constant a) where
  fmap f (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure y = Constant mempty
  (<*>) (Constant x) (Constant x') = Constant (x <> x')
