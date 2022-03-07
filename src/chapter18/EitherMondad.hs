module EitherMonad where

data Sum a b = First a | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First  x) = First x
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure y = Second y
  (<*>) (First x)   _          = First x
  (<*>) _           (First  x) = First x
  (<*>) (Second fb) (Second b) = Second (fb b)

instance Monad (Sum a) where
  return y = Second y
  (>>=) (First  x) _  = First x
  (>>=) (Second y) fy = fy y
