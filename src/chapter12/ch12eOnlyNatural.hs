module ItsOnlyNatural where

data Nat = Zero | Succ Nat deriving (Eq, Show)

{-
Prelude> natToInteger Zero
0
Prelude> natToInteger (Succ Zero)
1
Prelude> natToInteger (Succ (Succ Zero))
2
-}
natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ n) = 1 + natToInteger n

{-
Prelude> integerToNat 0
Just Zero
Prelude> integerToNat 1
Just (Succ Zero)
Prelude> integerToNat 2
Just (Succ (Succ Zero))
Prelude> integerToNat (-1)
Nothing
-}
integerToNat :: Integer -> Maybe Nat
integerToNat n | n < 0     = Nothing
               | otherwise = Just (go n)
 where
  go 0 = Zero
  go 1 = Succ Zero
  go n = Succ (go (n - 1))
