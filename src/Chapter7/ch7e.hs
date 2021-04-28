module Chapter7Exercises where

f :: Char -> String
f = undefined

g :: String -> [String]
g = undefined

h :: Char -> [String]
h = g . f

tensDigit :: Integral a => a -> a
tensDigit x = d
 where
  xLast = x `div` 10
  d     = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = fst $ divMod x 10

hundredsDigit :: Integral a => a -> a
hundredsDigit x = fst $ divMod x 100

foldBool :: a -> a -> Bool -> a
foldBool x _ False = x
foldBool _ y True  = y

g' :: (a -> b) -> (a, c) -> (b, c)
g' f (a, c) = (f a, c)

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTrip' :: (Show a, Read a) => a -> a
roundTrip' = read . show

roundTrip'' :: (Show a, Read b) => a -> b
roundTrip'' = read . show

main :: IO ()
main = do
  print (roundTrip'' 4 :: Int)
  print (id 4)
