module Control.Monad.Writer.Class where

import Prelude
import Control.Monad.Writer.Trans
import Data.Monoid

class MonadWriter w m where
  writer :: forall a. WriterData a w -> m a
  listen :: forall a. m a -> m (WriterData a w)
  pass :: forall a. m { value :: a, output :: w -> w } -> m a

tell :: forall w m a. (Monoid w, Monad m, MonadWriter w m) => w -> m {}
tell w = writer { value: {}, output: w }

listens :: forall w m a b. (Monoid w, Monad m, MonadWriter w m) => (w -> b) -> m a -> m (WriterData a b)
listens f m = do
  { value = a, output = w } <- listen m
  return { value: a, output: f w }

censor :: forall w m a. (Monoid w, Monad m, MonadWriter w m) => (w -> w) -> m a -> m a
censor f m = pass $ do
  a <- m
  return { value: a, output: f }

instance monadWriterWriterT :: (Monoid w, Monad m) => MonadWriter w (WriterT w m) where
  writer = WriterT <<< return
  listen m = WriterT $ do
    { value = a, output = w } <- runWriterT m
    return { value: { value: a, output: w }, output: w }
  pass m = WriterT $ do
    { value = { value = a, output = f }, output = w } <- runWriterT m
    return { value: a, output: f w }
