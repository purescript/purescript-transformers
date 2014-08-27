module Control.Monad.RWS.Trans where

import Control.Monad.Trans
import Data.Monoid
import Data.Tuple

type See s a w =
  { state  :: s
  , result :: a
  , log    :: w
  }

mkSee :: forall s a w. (Monoid w) => s -> a -> w -> See s a w
mkSee s a w = { state: s, result: a, log: w }

newtype RWST r w s m a = RWST (r -> s -> m (See s a w))

runRWST :: forall r w s m a. RWST r w s m a -> r -> s -> m (See s a w)
runRWST (RWST x) = x

evalRWST :: forall r w s m a. (Monad m) => RWST r w s m a -> r -> s -> m (Tuple a w)
evalRWST m r s = runRWST m r s >>= \see -> return (Tuple see.result see.log)

execRWST :: forall r w s m a. (Monad m) => RWST r w s m a -> r -> s -> m (Tuple s w)
execRWST m r s = runRWST m r s >>= \see -> return (Tuple see.state see.log)

mapRWST :: forall r w1 w2 s m1 m2 a1 a2. (m1 (See s a1 w1) -> m2 (See s a2 w2)) -> RWST r w1 s m1 a1 -> RWST r w2 s m2 a2
mapRWST f m = RWST \r s -> f $ runRWST m r s

withRWST :: forall r1 r2 w s m a. (r2 -> s -> Tuple r1 s) -> RWST r1 w s m a -> RWST r2 w s m a
withRWST f m = RWST \r s -> uncurry (runRWST m) (f r s)

instance functorRWST :: (Functor m) => Functor (RWST r w s m) where
  (<$>) f m = RWST \r s -> (\see -> see{result = f see.result}) <$> runRWST m r s

instance applyRWST :: (Apply m, Semigroup w) => Apply (RWST r w s m) where
  (<*>) f m = RWST \r s ->
    (\{result = f, log = l} see -> see{result = f see.result, log = l <> see.log}) <$> runRWST f r s <*> runRWST m r s

instance bindRWST :: (Bind m, Semigroup w) => Bind (RWST r w s m) where
  (>>=) m f = RWST \r s -> runRWST m r s >>= \{result = a, state = s', log = l} ->
    (\see' -> see'{log = l <> see'.log}) <$> runRWST (f a) r s'

instance applicativeRWST :: (Applicative m, Monoid w) => Applicative (RWST r w s m) where
  pure a = RWST \_ s -> pure $ mkSee s a mempty

instance monadRWST :: (Monad m, Monoid w) => Monad (RWST r w s m)

instance monadTransRWST :: (Monoid w) => MonadTrans (RWST r w s) where
  lift m = RWST \_ s -> m >>= \a -> return $ mkSee s a mempty
