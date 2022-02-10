module Folds where

{-
foldl (flip (*)) 1 [1..3]
(3*(2*(1*1)))
-}

f :: String
f = foldr (++) "" ["woot", "WOOT", "woot"]

f' :: Char
f' = foldr max 'a' "fear is the little death"

f'' :: Bool
f'' = foldr (&&) False [False, True]

f''' :: Bool
f''' = foldr (flip (||)) True [False, True]

f'''' :: String
f'''' = foldl (flip ((++) . show)) "" [1 .. 5]

f''''' :: Char
f''''' = foldr (flip const) 'a' [1 .. 5]

f'''''' :: Int
f'''''' = foldr (flip const) 0 "tacos"

f''''''' :: Int
f''''''' = foldl const 0 "burritos"

f'''''''' :: Char
f'''''''' = foldl const 'z' [1 .. 5]
