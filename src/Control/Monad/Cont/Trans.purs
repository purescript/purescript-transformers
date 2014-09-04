module Control.Monad.Cont.Trans where

import Control.Monad.Trans

newtype ContT r m a = ContT ((a -> m r) -> m r)

runContT :: forall r m a. ContT r m a -> (a -> m r) -> m r
runContT (ContT f) k = f k

mapContT :: forall r m a. (m r -> m r) -> ContT r m a -> ContT r m a
mapContT f m = ContT (\k -> f $ runContT m k)

withContT :: forall r m a b. ((b -> m r) -> (a -> m r)) -> ContT r m a -> ContT r m b
withContT f m = ContT (\k -> (runContT m) (f k))

callCC :: forall r m a b. ((a -> ContT r m b) -> ContT r m a) -> ContT r m a
callCC f = ContT (\k -> runContT (f (\a -> ContT (\_ -> k a))) k)

instance functorContT :: (Monad m) => Functor (ContT r m) where
  (<$>) f m = ContT (\k -> runContT m (\a -> k $ f a))

instance applyContT :: (Functor m, Monad m) => Apply (ContT r m) where
  (<*>) f v = ContT (\k -> runContT f $ (\g -> runContT v (\a -> (k $ g a))))

instance applicativeContT :: (Functor m, Monad m) => Applicative (ContT r m) where
  pure a = ContT (\k -> k a)

instance bindContT :: (Monad m) => Bind (ContT r m) where
  (>>=) m k = ContT (\k' -> runContT m (\a -> runContT (k a) k'))

instance monadContT :: (Monad m) => Monad (ContT r m)

instance monadTransContT :: MonadTrans (ContT r) where
  lift m = ContT (\k -> m >>= k)
