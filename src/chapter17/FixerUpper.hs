module FixerUpper where

-- 1.
f :: Maybe String
f = const <$> Just "Hello" <*> pure "World"

-- 2.
f' :: Maybe (Integer, Integer, String, [Integer])
f' = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]
