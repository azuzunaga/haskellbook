module StateT where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class

newtype StateT s m a = StateT { runStateT :: s -> m (a, s)}

instance Functor m => Functor (StateT s m) where
  fmap f (StateT sma) = StateT $ \s -> do
    (\(x, y) -> (f x, y)) <$> sma s

instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  (StateT fmab) <*> (StateT sma) = StateT $ \s -> do
    (fxy, s1) <- fmab s
    (x  , s2) <- sma s1
    return (fxy x, s2)

instance (Monad m) => Monad (StateT s m) where
  return = pure
  (StateT sma) >>= f = StateT $ \s -> do
    (a, s1) <- sma s
    runStateT (f a) s1

instance MonadTrans (StateT e) where
  lift m = StateT $ \s -> do
    a <- m
    return (a, s)

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO = lift . liftIO
