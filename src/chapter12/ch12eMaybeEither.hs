module MaybeEitherLibraries where


-- Prelude> isJust (Just 1)
-- True
-- Prelude> isJust Nothing
-- False
isJust :: Maybe a -> Bool
isJust Nothing  = False
isJust (Just _) = True

-- Prelude> isNothing (Just 1)
-- False
-- Prelude> isNothing Nothing
-- True
isNothing :: Maybe a -> Bool
isNothing Nothing  = True
isNothing (Just _) = False

-- Prelude> mayybee 0 (+1) Nothing
-- 0
-- Prelude> mayybee 0 (+1) (Just 1)
-- 2
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x f Nothing  = x
mayybee x f (Just a) = f a

-- Prelude> fromMaybe 0 Nothing
-- 0
-- Prelude> fromMaybe 0 (Just 1)
-- 1
fromMaybe :: a -> Maybe a -> a
fromMaybe v Nothing  = v
fromMaybe _ (Just n) = n

-- Prelude> listToMaybe [1, 2, 3]
-- Just 1
-- Prelude> listToMaybe []
-- Nothing
listToMaybe :: [a] -> Maybe a
listToMaybe []      = Nothing
listToMaybe (x : _) = Just x

-- Prelude> maybeToList (Just 1)
-- [1]
-- Prelude> maybeToList Nothing
-- []
maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just n) = [n]

-- Prelude> catMaybes [Just 1, Nothing, Just 2]
-- [1, 2]
-- Prelude> let xs = take 3 $ repeat Nothing
-- Prelude> catMaybes xs
-- []
catMaybes :: [Maybe a] -> [a]
catMaybes list = go list []
 where
  go []              acc = acc
  go (Nothing  : xs) acc = go xs acc
  go ((Just x) : xs) acc = x : go xs acc

--  Prelude> flipMaybe [Just 1, Just 2, Just 3]
--  Just [1, 2, 3]
--  Prelude> flipMaybe [Just 1, Nothing, Just 3]
--  Nothing
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe list = go list []
 where
  go :: [Maybe a] -> [a] -> Maybe [a]
  go []              acc = Just acc
  go (Nothing  : _ ) _   = Nothing
  go ((Just x) : xs) acc = go xs (acc ++ [x])

lefts' :: [Either a b] -> [a]
lefts' = foldr filt []
 where
  filt (Right _) acc = acc
  filt (Left  x) acc = x : acc

rights' :: [Either a b] -> [b]
rights' = foldr filt []
 where
  filt (Left  _) acc = acc
  filt (Right x) acc = x : acc

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr part ([], [])
 where
  part (Left  x) (lacc, racc) = (x : lacc, racc)
  part (Right x) (lacc, racc) = (lacc, x : racc)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left  _) = Nothing
eitherMaybe' f (Right x) = Just (f x)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fa _  (Left  a) = fa a
either' _  fb (Right b) = fb b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)
