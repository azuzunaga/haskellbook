module Chapter20Exercises where

import           Control.Applicative            ( Applicative(liftA2) )
import           Data.Monoid

-- Part 1
-- 1.
sum :: (Foldable t, Num a) => t a -> a
sum = foldr (+) 0
-- sum xs = getSum $ foldMap Sum xs

-- 2.
product :: (Foldable t, Num a) => t a -> a
product = foldr (*) 1
-- product xs = getProduct $ foldMap Product xs

-- 3.
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem e = foldr (\x acc -> x == e || acc) False

-- 4.
minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum =
  foldr (\x acc -> if maybe False (x <) acc then Just x else acc) Nothing

-- 5.
maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum =
  foldr (\x acc -> if maybe False (x >) acc then Just x else acc) Nothing

-- 6.
null :: Foldable t => t a -> Bool
null = foldr (\_ _ -> False) True

-- 7.
length :: Foldable t => t a -> Int
length = foldr (\_ acc -> acc + 1) 0

-- 8.
toList :: Foldable t => t a -> [a]
toList = foldr (:) []

-- 9.
fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

-- 10.
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend . f) mempty

-- Part 2
-- 1.
newtype Constant a b = Constant b
  deriving (Eq, Show)

instance Foldable (Constant a) where
  foldMap f (Constant x) = f x

-- 2.
data Two a b = Two a b
  deriving (Eq, Show)

instance Foldable (Two a) where
  foldMap f (Two x y) = f y

-- 3.
data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three x y z) = f z

-- 4.
data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' x y z) = f y <> f z

-- 5.
data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' x y z v) = f y <> f z <> f v

-- 6.
filterF
  :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF boolF = foldr (\x acc -> if boolF x then pure x <> acc else acc) mempty
