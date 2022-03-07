module Chapter17Exercises where

import           Control.Monad                  ( join
                                                , liftM2
                                                )
import           Test.Hspec                     ( hspec
                                                , it
                                                , shouldBe
                                                )
import           Test.QuickCheck                ( Arbitrary(arbitrary)
                                                , frequency
                                                )
import           Test.QuickCheck.Checkers       ( EqProp(..)
                                                , eq
                                                , quickBatch
                                                )
import           Test.QuickCheck.Classes        ( applicative
                                                , functor
                                                , monad
                                                )

-- Part 1
-- 1.
data Nope a = NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return = pure
  (>>=) _ _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

-- 2.
data BahEither b a = PLeft a | PRight b
  deriving (Eq, Show)

instance Functor (BahEither b) where
  fmap f (PLeft  x) = PLeft (f x)
  fmap _ (PRight y) = PRight y

instance Applicative (BahEither b) where
  pure x = PLeft x
  (<*>) (PRight f) _          = PRight f
  (<*>) _          (PRight y) = PRight y
  (<*>) (PLeft f)  (PLeft  x) = PLeft (f x)

instance Monad (BahEither b) where
  return = pure
  (>>=) (PRight x) _ = PRight x
  (>>=) (PLeft  y) f = f y

instance (Arbitrary b, Arbitrary a) => Arbitrary (BahEither b a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(1, return (PLeft x)), (1, return (PRight y))]

instance (Eq b, Eq a) => EqProp (BahEither b a) where
  (=-=) = eq

-- 3.
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity (f x)

instance Monad Identity where
  return = pure
  (>>=) (Identity x) f = f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

-- 4.
data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where
  Nil         <> Nil         = Nil
  Nil         <> (Cons x xs) = Cons x xs
  (Cons x xs) <> Nil         = Cons x xs
  (Cons x xs) <> ys          = Cons x (xs <> ys)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil         _               = Nil
  (<*>) _           Nil             = Nil
  (<*>) (Cons f fs) ys'@(Cons y ys) = Cons (f y) (f <$> ys) <> (fs <*> ys')

instance Monad List where
  return = pure
  (>>=) Nil         _ = Nil
  (>>=) (Cons x xs) f = f x <> (xs >>= f)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    return $ Cons x (Cons x (Cons x Nil))

instance Eq a => EqProp (List a) where
  (=-=) = eq

-- Part 2
-- 1.
j :: Monad m => m (m a) -> m a
j = join

-- 2.
l1 :: Monad m => (a -> b) -> m a -> m b
l1 f m = f <$> m

-- 3.
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f mx my = f <$> mx <*> my
-- l2 = liftM2

-- 4.
a :: Monad m => m a -> m (a -> b) -> m b
a x fx = fx <*> x

-- 5.
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh xs f = foldr (\x -> liftM2 (++) (flip (:) [] <$> f x)) (return []) xs

meh' :: Monad m => [a] -> (a -> m b) -> m [b]
meh' xs f =
  foldr (\x -> (<*>) $ fmap (++) (flip (:) [] <$> f x)) (return []) xs

meh'' :: Monad m => [a] -> (a -> m b) -> m [b]
meh'' []       f = return []
meh'' (x : xs) f = liftM2 (++) (flip (:) [] <$> f x) (meh xs f)

-- 6.
flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs id

------------------------------------------------
type S = String

main :: IO ()
main = hspec $ do
  it "Works for Nope" $ do
    quickBatch $ functor (undefined :: Nope (S, S, S))
    quickBatch $ applicative (undefined :: Nope (S, S, S))
    quickBatch $ monad (undefined :: Nope (S, S, S))
  it "Works for BahEither" $ do
    quickBatch $ functor (undefined :: BahEither (S, S, S) (S, S, S))
    quickBatch $ applicative (undefined :: BahEither (S, S, S) (S, S, S))
    quickBatch $ monad (undefined :: BahEither (S, S, S) (S, S, S))
  it "Works for Identity" $ do
    quickBatch $ functor (undefined :: Identity (S, S, S))
    quickBatch $ applicative (undefined :: Identity (S, S, S))
    quickBatch $ monad (undefined :: Identity (S, S, S))
  it "Works for List" $ do
    quickBatch $ functor (undefined :: List (S, S, S))
    quickBatch $ applicative (undefined :: List (S, S, S))
    quickBatch $ monad (undefined :: List (S, S, S))
  it "j works as expected" $ do
    j [[1, 2], [], [3]] `shouldBe` [1, 2, 3]
    j (Just (Just 1)) `shouldBe` Just 1
    (j (Just Nothing) :: Maybe Int) `shouldBe` Nothing
    (j Nothing :: Maybe Int) `shouldBe` Nothing
  it "l1 works as expected" $ do
    l1 ("hello " ++) (Just "there") `shouldBe` Just "hello there"
  it "l2 works as expected" $ do
    l2 (++) (Just "Hai ") (Just "bai") `shouldBe` Just "Hai bai"
  it "meh works as expected" $ do
    meh ["hello"] (\x -> Just (x ++ "bye")) `shouldBe` Just ["hellobye"]
    meh' ["hello"] (\x -> Just (x ++ "bye")) `shouldBe` Just ["hellobye"]
    meh'' ["hello"] (\x -> Just (x ++ "bye")) `shouldBe` Just ["hellobye"]
  it "flipType works as expected" $ do
    flipType [Just 1, Just 2] `shouldBe` Just [1, 2]
    flipType [Just 1, Just 2, Nothing] `shouldBe` Nothing
