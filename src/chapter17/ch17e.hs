module Chapter17Exercises where

import           Control.Applicative            ( liftA3 )
import           Test.Hspec                     ( hspec
                                                , it
                                                , shouldBe
                                                )
import           Test.QuickCheck                ( Arbitrary
                                                , arbitrary
                                                , frequency
                                                )
import           Test.QuickCheck.Checkers       ( EqProp(..)
                                                , eq
                                                , quickBatch
                                                )
import           Test.QuickCheck.Classes        ( applicative
                                                , functor
                                                )

-- Part 1
-- 1.
type L = []

pure :: a -> L a
pure = undefined

(<*>>) :: L (a -> b) -> L a -> L b
(<*>>) = undefined

-- 2.
type Io = IO

pure' :: a -> Io a
pure' = undefined

(<<*>>) :: Io (a -> b) -> Io a -> Io b
(<<*>>) = undefined

-- 3.
type Tup a = (,) a

pure'' :: a -> Tup a a
pure'' = undefined

(<<<*>>>) :: Tup a (a -> b) -> Tup a a -> Tup a b
(<<<*>>>) = undefined

-- 4.
type F e = (->) e

pure''' :: a -> F a a
pure''' = undefined

(<<<<*>>>>) :: F a (a -> b) -> F a a -> F a b
(<<<<*>>>>) = undefined

-- Part 2
-- 1.
data Pair a = Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x x') = Pair (f x) (f x')

instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair fx fx') (Pair x x') = Pair (fx x) (fx' x')

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return $ Pair a a

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

-- 2.
data Two a b = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative (Two a) where
  pure y = Two mempty y
  (<*>) (Two x fy) (Two x' y) = Two (x <> x') (fy y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    Two a <$> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

-- 3.
data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure z = Three mempty mempty z
  (<*>) (Three x y fz) (Three x' y' z) = Three (x <> x') (y <> y') (fz z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    Three a b <$> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

-- 4.
data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y y') = Three' x (f y) (f y')

instance Monoid a => Applicative (Three' a) where
  pure y = Three' mempty y y
  (<*>) (Three' x fy fy') (Three' x' y y') = Three' (x <> x') (fy y) (fy' y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Three' a b b

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

-- 5.
data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four x y z j) = Four x y z (f j)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure j = Four mempty mempty mempty j
  (<*>) (Four x y z fj) (Four x' y' z' j) =
    Four (x <> x') (y <> y') (z <> z') (fj j)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    Four a b c <$> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

-- 6.
data Four' a b = Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' x x' x'' y) = Four' x x' x'' (f y)

instance Monoid a => Applicative (Four' a) where
  pure y = Four' mempty mempty mempty y
  (<*>) (Four' x i j fy) (Four' x' i' j' y) =
    Four' (x <> x') (i <> i') (j <> j') (fy y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    Four' a a a <$> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

-- Part 3: Combinations
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

------------------------------------------------
main :: IO ()
main = hspec $ do
  it "Pair is an applicative" $ do
    quickBatch (applicative (undefined :: Pair (String, String, String)))
  it "Three is an applicative" $ do
    quickBatch
      (applicative
        (undefined :: Two (String, String, String) (String, String, String))
      )
  it "Three is an applicative" $ do
    quickBatch
      (applicative
        (undefined :: Three
            (String, String, String)
            (String, String, String)
            (String, String, String)
        )
      )
  it "Three' is an applicative" $ do
    quickBatch
      (applicative
        (undefined :: Three' (String, String, String) (String, String, String))
      )
  it "Four is an applicative" $ do
    quickBatch
      (applicative
        (undefined :: Four
            (String, String, String)
            (String, String, String)
            (String, String, String)
            (String, String, String)
        )
      )
  it "Four' is an applicative" $ do
    quickBatch
      (applicative
        (undefined :: Four' (String, String, String) (String, String, String))
      )
  it "combos with liftA3 works as expec:ted" $ do
    combos stops vowels stops
      `shouldBe` [ (x, y, z) | x <- stops, y <- vowels, z <- stops ]
