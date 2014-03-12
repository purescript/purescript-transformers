module Control.Monad.Writer.Trans where

import Prelude
import Control.Monad.Trans
import Data.Monoid

type WriterData a w = { value :: a, output :: w }

data WriterT w m a = WriterT (m (WriterData a w))

runWriterT :: forall w m a. WriterT w m a -> m (WriterData a w)
runWriterT (WriterT x) = x

mapWriterT :: forall w1 w2 m1 m2 a b. (m1 (WriterData a w1) -> m2 (WriterData b w2)) -> WriterT w1 m1 a -> WriterT w2 m2 b
mapWriterT f m = WriterT $ f (runWriterT m)

instance monadWriterT :: (Monoid w, Monad m) => Monad (WriterT w m) where
  return a = WriterT $ return { value: a, output: mempty }
  (>>=) m k  = WriterT $ do
    { value = a, output = w } <- runWriterT m
    { value = b, output = w' } <- runWriterT (k a)
    return { value: b, output: w <> w' }

instance appWriterT :: (Monoid w, Applicative m) => Applicative (WriterT w m) where
  pure a = WriterT $ pure { value: a, output: mempty }
  (<*>) f v = WriterT $
    let k x y = { value: x.value y.value, output: x.output <> y.output }
    in k <$> (runWriterT f) <*> (runWriterT v)

instance altWriterT :: (Monoid w, Alternative m) => Alternative (WriterT w m) where
  empty = WriterT empty
  (<|>) m n = WriterT $ runWriterT m <|> runWriterT n

instance functorWriterT :: (Functor m) => Functor (WriterT w m) where
  (<$>) f = mapWriterT $ (<$>) \x -> { value: f x.value, output: x.output }

instance monadTransWriterT :: (Monoid w) => MonadTrans (WriterT w) where
  lift m = WriterT $ do
    a <- m
    return { value: a, output: mempty }

liftCatchWriter :: forall w m e a. (m (WriterData a w) -> (e -> m (WriterData a w)) -> m (WriterData a w)) -> WriterT w m a -> (e -> WriterT w m a) -> WriterT w m a
liftCatchWriter catch m h = WriterT $ catch (runWriterT m) (\e -> runWriterT (h e))
