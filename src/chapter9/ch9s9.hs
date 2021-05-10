module TransformingLists where

import           Data.Bool                      ( bool )

-- map (\x -> if x == 3 then (-x) else (x)) [1..10]
mapIf :: [Int] -> [Int]
mapIf = map (\x -> bool (-x) x (x == 3))
