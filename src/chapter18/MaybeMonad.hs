module MaybeMonad where

f :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n

g :: Integer -> Maybe Integer
g i = if even i then Just (i + 1) else Nothing

h :: Integer -> Maybe String
h i = Just ("10181" ++ show i)

doSomething :: Integer -> Maybe (Integer, Integer, String)
doSomething n = do
  a <- f n
  b <- g a
  c <- h b
  return (a, b, c)

{-
instance Monad Maybe where
  return :: Monad m => a -> m a
  return x = Just x

  (>>=) :: Monad m => m a -> (a -> m b) -> m b
  (Just x) >>= k = k x
  Nothing >>= _ = Nothing

The way bind (>>=) short-circuts Nothing values is because bind passes the value
to the next operation without the monadic context, so in
  Just 2 >>= \x -> Just x == Just 2
what the function on the RHS of the bind operator is receiving is 2, but when
the value on the LHS is a Nothing, then the Nothing case gets evaluated and
that's that. If you have chained bind operators they'll all receive the Nothing
and again, that's that.
-}
