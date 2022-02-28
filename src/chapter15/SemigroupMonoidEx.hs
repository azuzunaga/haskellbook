module SemigroupMonoidEx where

import           Data.Monoid                    ( Sum(Sum)
                                                , getSum
                                                )
import           Test.QuickCheck                ( Arbitrary(arbitrary)
                                                , frequency
                                                , quickCheck
                                                )

-- 1.-------------------------------------------
data Trivial = Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance Monoid Trivial where
  mempty  = Trivial
  mappend = (<>)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2. ------------------------------------------
newtype Identity a = Identity a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  Identity a <> Identity a' = Identity (a <> a')

instance Monoid m => Monoid (Identity m) where
  mempty  = Identity mempty
  mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type IdentityAssoc = Identity S -> Identity S -> Identity S -> Bool

-- 3. ------------------------------------------
data Two a b = Two a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two a b <> Two a' b' = Two (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty  = Two mempty mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoAssoc = Two S S -> Two S S -> Two S S -> Bool

-- 4. ------------------------------------------
data Three a b c = Three a b c
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  Three a b c <> Three a' b' c' = Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
  mempty  = Three mempty mempty mempty
  mappend = (<>)

type ThreeAssoc = Three S S S -> Three S S S -> Three S S S -> Bool

-- 5. ------------------------------------------
data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  Four a b c d <> Four a' b' c' d' =
    Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
  mempty  = Four mempty mempty mempty mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

type FourAssoc = Four S S S S -> Four S S S S -> Four S S S S -> Bool

-- 6. ------------------------------------------
newtype BoolConj = BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj True  <> BoolConj True  = BoolConj True
  BoolConj True  <> BoolConj False = BoolConj False
  BoolConj False <> BoolConj True  = BoolConj False
  BoolConj False <> BoolConj False = BoolConj False

instance Monoid BoolConj where
  mempty  = BoolConj True
  mappend = (<>)

instance Arbitrary BoolConj where
  arbitrary =
    frequency [(1, return (BoolConj False)), (1, return (BoolConj True))]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 7. ------------------------------------------
newtype BoolDisj = BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj True  <> BoolDisj True  = BoolDisj True
  BoolDisj True  <> BoolDisj False = BoolDisj True
  BoolDisj False <> BoolDisj True  = BoolDisj True
  BoolDisj False <> BoolDisj False = BoolDisj False

instance Monoid BoolDisj where
  mempty  = BoolDisj False
  mappend = (<>)

instance Arbitrary BoolDisj where
  arbitrary =
    frequency [(1, return (BoolDisj False)), (1, return (BoolDisj True))]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 8. ------------------------------------------
data Or a b = Fst a | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  _     <> Snd b = Snd b
  Snd b <> _     = Snd b
  _     <> Fst a = Fst a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return (Fst a)), (1, return (Snd b))]

type OrAssoc = Or String String -> Or String String -> Or String String -> Bool

-- 9. ------------------------------------------
newtype Combine a b = Combine { unCombine :: a -> b }

instance Semigroup b => Semigroup (Combine a b) where
  Combine a <> Combine a' = Combine $ a <> a'

instance Monoid b => Monoid (Combine a b) where
  mempty  = Combine mempty
  mappend = (<>)

f :: Combine Integer (Sum Integer)
f = Combine $ \n -> Sum (n + 1)

g :: Combine Integer (Sum Integer)
g = Combine $ \n -> Sum (n - 1)

-- 10. -----------------------------------------
newtype Comp a = Comp { unComp :: a -> a}

instance Semigroup a => Semigroup (Comp a) where
  Comp a <> Comp a' = Comp $ a <> a'

instance Monoid a => Monoid (Comp a) where
  mempty  = Comp mempty
  mappend = (<>)

f' :: Comp String
f' = Comp $ \n -> n <> "ing"

g' :: Comp String
g' = Comp $ \n -> n <> "ing"

-- 11. -----------------------------------------
data Validation a b = Failure a | Success b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  Success a <> _          = Success a
  _         <> Success a  = Success a
  Failure a <> Failure a' = Failure (a <> a')

failure :: String -> Validation String Int
failure = Failure

success :: Int -> Validation String Int
success = Success

-- 12. -----------------------------------------
newtype Mem s a = Mem { runMem :: s -> (a, s)}

instance Semigroup a => Semigroup (Mem s a) where
  (Mem f) <> (Mem f') =
    Mem $ \n -> (fst (f n) <> fst (f' n), snd $ f $ snd $ f' n)

instance Monoid a => Monoid (Mem s a) where
  mempty  = Mem $ \s -> (mempty, s)
  mappend = (<>)

f'' :: Mem Integer [Char]
f'' = Mem $ \s -> ("hi", s + 1)

------------------------------------------------
type S = String

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = a <> mempty == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = mempty <> a == a

main :: IO ()
main = do
  print "1. Trivial"
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  print "2. Identity"
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (monoidLeftIdentity :: Identity S -> Bool)
  quickCheck (monoidRightIdentity :: Identity S -> Bool)
  print "3. Two"
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: Two S S -> Bool)
  quickCheck (monoidRightIdentity :: Two S S -> Bool)
  print "4. Three"
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (monoidLeftIdentity :: Three S S S -> Bool)
  quickCheck (monoidRightIdentity :: Three S S S -> Bool)
  print "5. Four"
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (monoidLeftIdentity :: Four S S S S -> Bool)
  quickCheck (monoidRightIdentity :: Four S S S S -> Bool)
  print "6. BoolConj"
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  print "7. BoolDisj"
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  print "8. Or"
  quickCheck (semigroupAssoc :: OrAssoc)
  print "9. Combine"
  print $ unCombine (f <> g) 0 == (Sum { getSum = 0 })
  print $ unCombine (f <> g) 1 == (Sum { getSum = 2 })
  print $ unCombine (f <> f) 1 == (Sum { getSum = 4 })
  print $ unCombine (g <> f) 1 == (Sum { getSum = 2 })
  print $ unCombine (mappend f mempty) 1 == (Sum { getSum = 2 })
  print "10. Comp"
  print $ unComp (f' <> g') "push" == "pushingpushing"
  print $ unComp (f' <> g') "pull" == "pullingpulling"
  print $ unComp (f' <> f') "pull" == "pullingpulling"
  print $ unComp (g' <> f') "pull" == "pullingpulling"
  print $ unComp (mappend f' mempty) "push" == "pushing"
  print $ unComp (mappend mempty f') "push" == "pushing"
  print "11. Validation"
  print $ success 1 <> failure "blah" == Success 1
  print $ failure "woot" <> failure "blah" == Failure "wootblah"
  print $ success 1 <> success 2 == Success 1
  print $ failure "woot" <> success 2 == Success 2
  print "12. Weird Monoid"
  let rmzero  = runMem mempty 0
      rmleft  = runMem (f'' <> mempty) 0
      rmright = runMem (mempty <> f'') 0
  print $ rmleft == ("hi", 1)
  print $ rmright == ("hi", 1)
  print $ (rmzero :: (String, Int)) == ("", 0)
  print $ rmleft == runMem f'' 0
  print $ rmright == runMem f'' 0
