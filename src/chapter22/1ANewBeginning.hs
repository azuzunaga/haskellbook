module ANewBeginning where

import Control.Applicative

boop :: Integer -> Integer
boop = (* 2)

doop :: Integer -> Integer
doop = (+ 10)

bip :: Integer -> Integer
bip = boop . doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer -> Integer
duwop = liftA2 (+) boop doop
