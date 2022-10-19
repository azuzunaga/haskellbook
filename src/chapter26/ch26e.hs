module Chapter21Exercises where

import           Control.Monad.Trans.Reader
import           Data.Functor.Identity

rDec :: Num a => Reader a a
rDec = flip (-) 1 <$> ask

rShow :: Show a => ReaderT a Identity String
rShow = do
  x <- ask
  _c
