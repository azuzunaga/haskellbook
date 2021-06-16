{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Newtype where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

instance TooMany (Int, String) where
  tooMany (n, _) = n > 42

newtype AllGoats = AllGoats (Int, Int) deriving (Eq, Show)

instance TooMany AllGoats where
  tooMany (AllGoats (x, y)) = x + y > 42

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = tooMany x || tooMany y

newtype Weird a = Weird (a, a) deriving (Eq, Show, TooMany)
