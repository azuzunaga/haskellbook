module Chapter21Exercises where

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

-- 1.
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity (f x)

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse fab (Identity a) = Identity <$> fab a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

-- 2.
newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure y = Constant mempty
  (Constant x) <*> (Constant x') = Constant (x <> x')

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  sequenceA (Constant fa) = pure (Constant fa)
  traverse f x = sequenceA $ fmap f x

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance (Eq a) => EqProp (Constant a b) where
  (=-=) = eq

-- 3.
data Optional a = Nada | Yep a
  deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada    = Nada
  fmap f (Yep x) = Yep (f x)

instance Applicative Optional where
  pure x = Yep x
  Nada     <*> _       = Nada
  _        <*> Nada    = Nada
  (Yep fx) <*> (Yep x) = Yep (fx x)

instance Foldable Optional where
  foldMap _ Nada    = mempty
  foldMap f (Yep x) = f x

instance Traversable Optional where
  traverse _ Nada    = pure Nada
  traverse f (Yep x) = Yep <$> f x

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    x <- arbitrary
    frequency [(1, return Nada), (2, return (Yep x))]

instance (Eq a) => EqProp (Optional a) where
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

instance Foldable List where
  foldMap _ Nil         = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs

instance Traversable List where
  traverse _ Nil         = pure Nil
  traverse f (Cons x xs) = (Cons <$> f x) <*> traverse f xs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    return $ Cons x (Cons x (Cons x Nil))

instance Eq a => EqProp (List a) where
  (=-=) = eq

-- 5.
data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure z = Three mempty mempty z
  (<*>) (Three x y fz) (Three x' y' z) = Three (x <> x') (y <> y') (fz z)

instance Foldable (Three a b) where
  foldMap f (Three x y z) = f z

instance Traversable (Three a b) where
  traverse f (Three x y z) = Three x y <$> f z

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    Three a b <$> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

-- 6.
data Pair a b = Pair a b
  deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair x y) = Pair x (f y)

instance Monoid a => Applicative (Pair a) where
  pure y = Pair mempty y
  (<*>) (Pair x fy) (Pair x' y) = Pair (x <> x') (fy y)

instance Foldable (Pair a) where
  foldMap f (Pair x y) = f y

instance Traversable (Pair a) where
  traverse f (Pair x y) = Pair x <$> f y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = do
    a <- arbitrary
    Pair a <$> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

-- 7.
data Big a b = Big a b b
  deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big x y y') = Big x (f y) (f y')

instance Monoid a => Applicative (Big a) where
  pure y = Big mempty y y
  (<*>) (Big x fy fy') (Big x' y y') = Big (x <> x') (fy y) (fy' y')

instance Foldable (Big a) where
  foldMap f (Big x y z) = f y <> f z

instance Traversable (Big a) where
  traverse f (Big x y y') = Big x <$> f y <*> f y'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Big a b b

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

-- 8.
data Bigger a b = Bigger a b b b
  deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger x y y' y'') = Bigger x (f y) (f y') (f y'')

instance Monoid a => Applicative (Bigger a) where
  pure y = Bigger mempty y y y
  (<*>) (Bigger x fy fy' fy'') (Bigger x' y y' y'') =
    Bigger (x <> x') (fy y) (fy' y') (fy'' y'')

instance Foldable (Bigger a) where
  foldMap f (Bigger x y y' y'') = f y <> f y' <> f y''

instance Traversable (Bigger a) where
  traverse f (Bigger x y y' y'') = Bigger x <$> f y <*> f y' <*> f y''

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Bigger a b b b

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

-- 9.
data S n a = S (n a) a
  deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S fx x) = S (f <$> fx) (f x)

instance Foldable n => Foldable (S n) where
  foldMap f (S fx x) = foldMap f fx <> f x

instance Traversable n => Traversable (S n) where
  traverse f (S fx x) = S <$> traverse f fx <*> f x

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance ( Applicative n
         , Testable (n Property)
         , Eq a
         , Eq (n a)
         , EqProp a)
        => EqProp (S n a) where
  (=-=) = eq

------------------------------------------------
-- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
-- sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)

main :: IO ()
main = hspec $ do
  it "Works for Identity" $ do
    quickBatch (functor (undefined :: Identity (Int, Int, [Int])))
    quickBatch (applicative (undefined :: Identity (Int, Int, [Int])))
    quickBatch
      (foldable (undefined :: Identity (String, String, String, Int, String)))
    quickBatch (traversable (undefined :: Identity (Int, Int, [Int])))
  it "Works for Constant" $ do
    quickBatch
      (functor (undefined :: Constant (Int, Int, [Int]) (Int, Int, [Int])))
    quickBatch
      (applicative
        (undefined :: Constant
            (String, String, [String])
            (String, String, [String])
        )
      )
    quickBatch
      (foldable
        (undefined :: Constant
            (String, String, String, Int, String)
            (String, String, String, Int, String)
        )
      )
    quickBatch
      (traversable (undefined :: Constant (Int, Int, [Int]) (Int, Int, [Int])))
  it "Works for Optional" $ do
    quickBatch (functor (undefined :: Optional (Int, Int, [Int])))
    quickBatch (applicative (undefined :: Optional (Int, Int, [Int])))
    quickBatch
      (foldable (undefined :: Optional (String, String, String, Int, String)))
    quickBatch (traversable (undefined :: Optional (Int, Int, [Int])))
  it "Works for List" $ do
    quickBatch (functor (undefined :: List (Int, Int, [Int])))
    quickBatch (applicative (undefined :: List (Int, Int, [Int])))
    quickBatch
      (foldable (undefined :: List (String, String, String, Int, String)))
    quickBatch (traversable (undefined :: List (Int, Int, [Int])))
  it "Works for Three" $ do
    quickBatch
      (functor
        (undefined :: Three
            (Int, Int, [Int])
            (Int, Int, [Int])
            (Int, Int, [Int])
        )
      )
    quickBatch
      (applicative
        (undefined :: Three
            (String, String, [String])
            (String, String, [String])
            (String, String, [String])
        )
      )
    quickBatch
      (foldable
        (undefined :: Three
            (String, String, String, Int, String)
            (String, String, String, Int, String)
            (String, String, String, Int, String)
        )
      )
    quickBatch
      (traversable
        (undefined :: Three
            (Int, Int, [Int])
            (Int, Int, [Int])
            (Int, Int, [Int])
        )
      )
  it "Works for Pair" $ do
    quickBatch (functor (undefined :: Pair (Int, Int, [Int]) (Int, Int, [Int])))
    quickBatch
      (applicative
        (undefined :: Pair (String, String, [String]) (String, String, [String])
        )
      )
    quickBatch
      (foldable
        (undefined :: Pair
            (String, String, String, Int, String)
            (String, String, String, Int, String)
        )
      )
    quickBatch
      (traversable (undefined :: Pair (Int, Int, [Int]) (Int, Int, [Int])))
  it "Works for Big" $ do
    quickBatch (functor (undefined :: Big (Int, Int, [Int]) (Int, Int, [Int])))
    quickBatch
      (applicative
        (undefined :: Big (String, String, [String]) (String, String, [String]))
      )
    quickBatch
      (foldable
        (undefined :: Big
            (String, String, String, Int, String)
            (String, String, String, Int, String)
        )
      )
    quickBatch
      (traversable (undefined :: Big (Int, Int, [Int]) (Int, Int, [Int])))
  it "Works for Bigger" $ do
    quickBatch
      (functor (undefined :: Bigger (Int, Int, [Int]) (Int, Int, [Int])))
    quickBatch
      (applicative
        (undefined :: Bigger
            (String, String, [String])
            (String, String, [String])
        )
      )
    quickBatch
      (foldable
        (undefined :: Bigger
            (String, String, String, Int, String)
            (String, String, String, Int, String)
        )
      )
    quickBatch
      (traversable (undefined :: Bigger (Int, Int, [Int]) (Int, Int, [Int])))
  it "Works for S" $ do
    quickBatch (functor (undefined :: S [] (Int, Int, [Int])))
    -- quickBatch (applicative (undefined :: S [] (String, String, [String])))
    quickBatch
      (foldable (undefined :: S [] (String, String, String, Int, String)))
    quickBatch (traversable (undefined :: S [] (Int, Int, [Int])))
