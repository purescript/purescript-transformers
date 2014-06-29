module Control.Monad.RWS.Trans where

import Control.Monad.Trans
import Data.Monoid
import Data.Tuple

data Tuple3 a b c = Tuple3 a b c

data RWST r w s m a = RWST (r -> s -> m (Tuple3 a s w))

runRWST :: forall r w s m a. RWST r w s m a -> r -> s -> m (Tuple3 a s w)
runRWST (RWST x) = x

evalRWST :: forall r w s m a. (Monad m) => RWST r w s m a -> r -> s -> m (Tuple a w)
evalRWST m r s = runRWST m r s >>= \(Tuple3 a _ w) -> return (Tuple a w)

execRWST :: forall r w s m a. (Monad m) => RWST r w s m a -> r -> s -> m (Tuple s w)
execRWST m r s = runRWST m r s >>= \(Tuple3 _ s w) -> return (Tuple s w)

mapRWST :: forall r w1 w2 s m1 m2 a b. (m1 (Tuple3 a s w1) -> m2 (Tuple3 b s w2)) -> RWST r w1 s m1 a -> RWST r w2 s m2 b
mapRWST f m = RWST \r s -> f $ runRWST m r s

withRWST :: forall r1 r2 w s m a. (r2 -> s -> Tuple r1 s) -> RWST r1 w s m a -> RWST r2 w s m a
withRWST f m = RWST \r s -> uncurry (runRWST m) (f r s)

instance functorRWST :: (Functor m) => Functor (RWST r w s m) where
  (<$>) f m = RWST \r s -> (\(Tuple3 a s' w) -> Tuple3 (f a) s' w) <$> runRWST m r s

instance applyRWST :: (Apply m, Semigroup w) => Apply (RWST r w s m) where
  (<*>) f m = RWST \r s ->
    (\(Tuple3 f' _ w) (Tuple3 a s' w') -> Tuple3 (f' a) s' (w <> w')) <$> runRWST f r s <*> runRWST m r s

instance bindRWST :: (Bind m, Semigroup w) => Bind (RWST r w s m) where
  (>>=) m f = RWST \r s -> runRWST m r s >>= \(Tuple3 a s' w) ->
    (\(Tuple3 b s'' w') -> Tuple3 b s'' (w <> w')) <$> runRWST (f a) r s'

instance applicativeRWST :: (Applicative m, Monoid w) => Applicative (RWST r w s m) where
  pure a = RWST \_ s -> pure (Tuple3 a s mempty)

instance monadRWST :: (Monad m, Monoid w) => Monad (RWST r w s m)

instance monadTransRWST :: (Monoid w) => MonadTrans (RWST r w s) where
  lift m = RWST \_ s -> m >>= \a -> return $ Tuple3 a s mempty
