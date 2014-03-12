module Control.Monad.Writer.Trans where

import Prelude
import Control.Monad.Trans
import Data.Monoid
import Data.Tuple

data WriterT w m a = WriterT (m (Tuple a w))

runWriterT :: forall w m a. WriterT w m a -> m (Tuple a w)
runWriterT (WriterT x) = x

mapWriterT :: forall w1 w2 m1 m2 a b. (m1 (Tuple a w1) -> m2 (Tuple b w2)) -> WriterT w1 m1 a -> WriterT w2 m2 b
mapWriterT f m = WriterT $ f (runWriterT m)

instance monadWriterT :: (Monoid w, Monad m) => Monad (WriterT w m) where
  return a = WriterT $ return $ Tuple a mempty
  (>>=) m k  = WriterT $ do
    Tuple a w <- runWriterT m
    Tuple b w' <- runWriterT (k a)
    return $ Tuple b (w <> w')

instance appWriterT :: (Monoid w, Applicative m) => Applicative (WriterT w m) where
  pure a = WriterT $ pure $ Tuple a mempty
  (<*>) f v = WriterT $
    let k (Tuple a w) (Tuple b w') = Tuple (a b) (w <> w')
    in k <$> (runWriterT f) <*> (runWriterT v)

instance altWriterT :: (Monoid w, Alternative m) => Alternative (WriterT w m) where
  empty = WriterT empty
  (<|>) m n = WriterT $ runWriterT m <|> runWriterT n

instance functorWriterT :: (Functor m) => Functor (WriterT w m) where
  (<$>) f = mapWriterT $ (<$>) \(Tuple a w) -> Tuple (f a) w

instance monadTransWriterT :: (Monoid w) => MonadTrans (WriterT w) where
  lift m = WriterT $ do
    a <- m
    return $ Tuple a mempty

liftCatchWriter :: forall w m e a. (m (Tuple a w) -> (e -> m (Tuple a w)) -> m (Tuple a w)) -> WriterT w m a -> (e -> WriterT w m a) -> WriterT w m a
liftCatchWriter catch m h = WriterT $ catch (runWriterT m) (\e -> runWriterT (h e))
