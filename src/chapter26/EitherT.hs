-- {-# LANGUAGE LambdaCase #-}

module EitherT where
import           Control.Applicative            ( )
import           Control.Monad.Trans.Class

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a)}

instance Functor m => Functor (EitherT e m) where
  -- Composing fmap lets us drill down past the first monadic structure and then
  -- lift f over only the Either value.
  fmap f (EitherT me) = EitherT $ (fmap . fmap) f me

instance Applicative m => Applicative (EitherT e m) where
  pure x = EitherT $ pure $ pure x
  {-
  This is the same as `liftA2 (<*>) f e`, but why? Looking at the types first:
  `f :: m (Either e (a -> b))`
  `e :: m (Either e a)`
  So we have an outer structure wrapping our Either. If we make that structure a
  list for simplicity, the types would look like this:
  `f :: [(Either e (a -> b))]`
  `e :: [(Either e a)]`
  If we do a simple <*> on `f` and `e` were trying to mesh two lists together,
  which makes no sense. For <*> to work we need to go one level deeper in the
  structure, i.e. lift over the outer structure. So we fmap (<$>) once to lift
  over the list (outer) structure, and now we have `Either e (a -> b)` and
  `Either e a`. We again need to go one level deeper, but since Either is an
  applicative we can simply do a <*>.
  -}
  (EitherT f) <*> (EitherT e) = EitherT $ (<*>) <$> f <*> e

instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT e) >>= f = EitherT $ do
    w <- e
    case w of
      Left  x -> return $ Left x
      Right y -> runEitherT $ f y

--- Alternatives -------------------------------
-- instance Monad m => Monad (EitherT e m) where
--   return = pure
--   (EitherT e) >>= f = EitherT $ e >>= \case
--     Left  x -> return $ Left x
--     Right y -> runEitherT $ f y

-- instance Monad m => Monad (EitherT e m) where
--   return = pure
--   (EitherT e) >>= f = EitherT $ do
--     w <- e
--     either (return . Left) (runEitherT . f) w

-- instance Monad m => Monad (EitherT e m) where
--   return = pure
--   (EitherT e) >>= f = EitherT $ e >>= either (return . Left) (runEitherT . f)
------------------------------------------------
{- This doesn't work:

instance Monad m => Monad (EitherT e m) where
  return = pure
  et >>= f = et >>= (\e -> e >>= either (EitherT $ return . Left) f)

mx >>= my is the same as
do x <- mx
   my x

In `et >>= f`, `et` has type `EitherT (m (Either e a))` or `EitherT me`. `>>=`
should bind `me` out of the `EitherT` context, but it doesn't know how to
because there is no `>>=` for `EitherT` because we are defining it in that same
line!
-}
------------------------------------------------

instance MonadTrans (EitherT e) where
  lift = EitherT . fmap Right

swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
swapEitherT (EitherT me) = EitherT $ swapEither <$> me

swapEither :: Either e a -> Either a e
swapEither (Left  x) = Right x
swapEither (Right y) = Left y

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT famc fbmc (EitherT mab) = mab >>= either famc fbmc
-- eitherT famc fbmc emab          = runEitherT emab >>= either famc fbmc
-- eitherT famc fbmc emab          = do
--   ab <- runEitherT emab
--   either famc fbmc ab
