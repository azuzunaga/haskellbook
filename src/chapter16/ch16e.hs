{-# LANGUAGE FlexibleInstances #-}

module Chapter16Exercises where

import           GHC.Arr

--- Part 1
-- 1. No: empty structure, nothing to lift over, Bool is always kind *
data Bool = False | True

-- 2. Yes
data BoolAndSomethingElse a = False' a | True' a

instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' (f a)
  fmap f (True'  a) = True' (f a)

-- 3. Yes
data BoolAndMaybeSomethingElse a = Falsish | Trueish a

instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish     = Falsish
  fmap f (Trueish a) = Trueish (f a)

-- 4. No: Mu has kind (* -> *) -> *, and Mu f has kind *
newtype Mu f = InF { outF :: f (Mu f)}

-- 5. No, D is kind *
data D = D (Array Word Word) Int Int

--- Part 2
-- 1.
data Sum b a = First a | Second b

instance Functor (Sum e) where
  fmap f (First  a) = First (f a)
  fmap f (Second b) = Second b

-- 2.
data Company a c b = DeepBlue a c | Something b

instance Functor (Company e e') where
  fmap f (Something b ) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- 3
data More b a = L a b a | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

--- Part 3
-- 1.
data Quant a b = Finance | Desk a | Floor b
  deriving (Eq, Show)

instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap _ (Desk  a) = Desk a
  fmap f (Floor b) = Floor (f b)

-- 2.
data K' a b = K' a
  deriving (Eq, Show)

instance Functor (K' a) where
  fmap _ (K' a) = K' a

-- 3.
newtype Flip f a b = Flip (f b a)
  deriving (Eq, Show)

newtype K a b = K a
  deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip (K (f a))

-- 4.
data EvilGoateeConst a b = GoatyConst b
  deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

-- 5.
data LiftItOut f a = LiftItOut (f a)
  deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

-- fmap (+1) (LiftItOut [3])
-- LiftItOut [4]

-- 6.
data Parappa f g a = DaWrapa (f a) (g a)
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrapa fa ga) = DaWrapa (fmap f fa) (fmap f ga)

-- fmap (+1) (DaWrapa [1] [2])
-- DaWrapa [2] [3]

-- 7.
data IgnoreOne f g a b = IgnoreSomething (f a) (g b)
  deriving (Eq, Show)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoreSomething fa gb) = IgnoreSomething fa (fmap f gb)

-- fmap (+1) (IgnoreSomething [1] [2])
-- IgnoreSomething [1] [3]

-- 8.
data Notorious g o a t = Notorious (g o) (g a) (g t)
  deriving (Eq, Show)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

-- fmap (+1) (Notorious [1] [2] [3])
-- Notorious [1] [2] [4]

-- 9.
data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons a la) = Cons (f a) (fmap f la)

-- fmap (+1) (Cons 3 (Cons 5 Nil))
-- Cons 4 (Cons 6 Nil)

-- 10.
data GoatLord a =
  NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat            = NoGoat
  fmap f (OneGoat a      ) = OneGoat (f a)
  fmap f (MoreGoats l m r) = MoreGoats (fmap f l) (fmap f m) (fmap f r)

-- fmap (+1) (MoreGoats NoGoat (OneGoat 1) (MoreGoats NoGoat (OneGoat 3) NoGoat))
-- MoreGoats NoGoat (OneGoat 2) (MoreGoats NoGoat (OneGoat 4) NoGoat)

-- 11.
data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt        = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read fa  ) = Read (fmap f fa)
