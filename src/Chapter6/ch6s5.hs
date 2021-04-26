module DerivingInstances where

data DayOfWeek = Mon | Tue | Weds | Thu | Fri | Sat | Sun
  deriving Show

data Date = Date DayOfWeek Int
  deriving Show

instance Eq DayOfWeek where
  (==) Mon  Mon  = True
  (==) Tue  Tue  = True
  (==) Weds Weds = True
  (==) Thu  Thu  = True
  (==) Fri  Fri  = True
  (==) Sat  Sat  = True
  (==) Sun  Sun  = True
  (==) _    _    = False

instance Eq Date where
  (==) (Date weekday dayOfMonth) (Date weekday' dayOfMonth') =
    weekday == weekday' && dayOfMonth == dayOfMonth'

data Identity a = Identity a

instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn integer) (TisAn integer') = integer == integer'

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two int1 int2) (Two int1' int2') = int1 == int1' && int2 == int2'

data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt   int) (TisAnInt   int') = int == int'
  (==) (TisAString str) (TisAString str') = str == str'
  (==) _                _                 = False

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair x y) (Pair x' y') = x == x' && y == y'

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y) (Tuple x' y') = x == x' && y == y'

data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne x') = x == x'
  (==) (ThatOne x) (ThatOne x') = x == x'
  (==) _           _            = False

data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello   x) (Hello   x') = x == x'
  (==) (Goodbye x) (Goodbye x') = x == x'
  (==) _           _            = False
