module Bind where

import           Control.Monad                  ( join )

bind :: Monad m => (a -> m b) -> m a -> m b
bind fx x = join $ fmap fx x
