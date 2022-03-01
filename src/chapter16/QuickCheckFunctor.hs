module QuickCheckFunctor where
import           Test.Hspec
import           Test.QuickCheck

-- 1. Identity
newtype Identity a = Identity a
  deriving (Show, Eq)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

-- 2. Pair
data Pair a = Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return $ Pair a a

-- 3. Two
data Two a b = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    Two a <$> arbitrary

-- 4. Three
data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    Three a b <$> arbitrary

-- 5. Three'
data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Three' a b b

-- 6. Four
data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    Four a b c <$> arbitrary

-- 7. Four'
data Four' a b = Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    Four' a a a <$> arbitrary

-- 8. Trivial
data Trivial = Trivial

-- Can't instantiate as functor because there are no values contained in the
-- Trivial data constructor

------------------------------------------------
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = fmap g (fmap f x) == fmap (g . f) x

functorComposedString :: (Eq (f String), Functor f) => f String -> Bool
functorComposedString = functorCompose ("yes" ++) ("no" ++)

main :: IO ()
main = hspec $ do
  it "example check" $ do
    quickCheck (functorIdentity :: [Int] -> Bool)
    quickCheck $ \x -> functorCompose (+ 1) (* 2) (x :: [Int])
  it "1. Identity" $ do
    quickCheck (functorIdentity :: Identity String -> Bool)
    quickCheck $ \x -> functorComposedString (x :: Identity String)
  it "2. Pair" $ do
    quickCheck (functorIdentity :: Pair Int -> Bool)
    quickCheck $ \x -> functorComposedString (x :: Pair String)
  it "3. Two" $ do
    quickCheck (functorIdentity :: Two Int String -> Bool)
    quickCheck $ \x -> functorComposedString (x :: Two Int String)
  it "4. Three" $ do
    quickCheck (functorIdentity :: Three Int Char String -> Bool)
    quickCheck $ \x -> functorComposedString (x :: Three Int Char String)
  it "5. Three'" $ do
    quickCheck (functorIdentity :: Three' Int String -> Bool)
    quickCheck $ \x -> functorComposedString (x :: Three' Int String)
  it "6. Four" $ do
    quickCheck (functorIdentity :: Four Int Char String String -> Bool)
    quickCheck $ \x -> functorComposedString (x :: Four Int Char String String)
  it "7. Four'" $ do
    quickCheck (functorIdentity :: Four' Int String -> Bool)
    quickCheck $ \x -> functorComposedString (x :: Four' Int String)
