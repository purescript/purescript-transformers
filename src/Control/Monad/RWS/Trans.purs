-- | This module defines the reader-writer-state monad transformer, `RWST`.

module Control.Monad.RWS.Trans 
  ( See(), mkSee
  , RWST(..), runRWST, evalRWST, execRWST, mapRWST, withRWST
  , module Control.Monad.Trans
  , module Control.Monad.RWS.Class
  ) where

import Prelude

import Data.Monoid
import Data.Tuple

import Control.Monad.Trans
import Control.Monad.Eff.Class
import Control.Monad.Reader.Class
import Control.Monad.Writer.Class
import Control.Monad.State.Class
import Control.Monad.RWS.Class

type See s a w =
  { state  :: s
  , result :: a
  , log    :: w
  }

mkSee :: forall s a w. (Monoid w) => s -> a -> w -> See s a w
mkSee s a w = { state: s, result: a, log: w }

-- | The reader-writer-state monad transformer, which combines the operations
-- | of `ReaderT`, `WriterT` and `StateT` into a single monad transformer.
newtype RWST r w s m a = RWST (r -> s -> m (See s a w))

-- | Run a computation in the `RWST` monad.
runRWST :: forall r w s m a. RWST r w s m a -> r -> s -> m (See s a w)
runRWST (RWST x) = x

-- | Run a computation in the `RWST` monad, discarding the final state.
evalRWST :: forall r w s m a. (Monad m) => RWST r w s m a -> r -> s -> m (Tuple a w)
evalRWST m r s = runRWST m r s >>= \see -> return (Tuple see.result see.log)

-- | Run a computation in the `RWST` monad, discarding the result.
execRWST :: forall r w s m a. (Monad m) => RWST r w s m a -> r -> s -> m (Tuple s w)
execRWST m r s = runRWST m r s >>= \see -> return (Tuple see.state see.log)

-- | Change the result and accumulator types in a `RWST` monad action.
mapRWST :: forall r w1 w2 s m1 m2 a1 a2. (m1 (See s a1 w1) -> m2 (See s a2 w2)) -> RWST r w1 s m1 a1 -> RWST r w2 s m2 a2
mapRWST f m = RWST \r s -> f $ runRWST m r s

-- | Change the context type in a `RWST` monad action.
withRWST :: forall r1 r2 w s m a. (r2 -> s -> Tuple r1 s) -> RWST r1 w s m a -> RWST r2 w s m a
withRWST f m = RWST \r s -> uncurry (runRWST m) (f r s)

instance functorRWST :: (Functor m) => Functor (RWST r w s m) where
  map f m = RWST \r s -> (\see -> see{result = f see.result}) <$> runRWST m r s

instance applyRWST :: (Bind m, Monoid w) => Apply (RWST r w s m) where
  apply f m = RWST \r s ->
    runRWST f r s  >>= \{state = s',  result = f',  log = w'}  ->
    runRWST m r s' <#> \{state = s'', result = a'', log = w''} ->
    mkSee s'' (f' a'') (w' ++ w'')

instance bindRWST :: (Bind m, Monoid w) => Bind (RWST r w s m) where
  bind m f = RWST \r s ->
    runRWST m     r s  >>= \{result = a, state = s', log = l} ->
    runRWST (f a) r s' <#> \see' ->
    see' { log = l ++ see'.log }

instance applicativeRWST :: (Monad m, Monoid w) => Applicative (RWST r w s m) where
  pure a = RWST \_ s -> pure $ mkSee s a mempty

instance monadRWST :: (Monad m, Monoid w) => Monad (RWST r w s m)

instance monadTransRWST :: (Monoid w) => MonadTrans (RWST r w s) where
  lift m = RWST \_ s -> m >>= \a -> return $ mkSee s a mempty

instance monadEffRWS :: (Monad m, Monoid w, MonadEff eff m) => MonadEff eff (RWST r w s m) where
  liftEff = lift <<< liftEff

instance monadReaderRWST :: (Monad m, Monoid w) => MonadReader r (RWST r w s m) where
  ask = RWST \r s -> pure $ mkSee s r mempty
  local f m = RWST \r s -> runRWST m (f r) s

instance monadStateRWST :: (Monad m, Monoid w) => MonadState s (RWST r w s m) where
  state f = RWST \_ s -> case f s of Tuple a s' -> pure $ mkSee s' a mempty

instance monadWriterRWST :: (Monad m, Monoid w) => MonadWriter w (RWST r w s m) where
  writer (Tuple a w) = RWST \_ s -> pure $ { state: s, result: a, log: w }
  listen m = RWST \r s -> runRWST m r s >>= \{ state: s', result: a, log: w} -> pure { state: s', result: Tuple a w, log: w }
  pass m = RWST \r s -> runRWST m r s >>= \{ result: Tuple a f, state: s', log: w} -> pure { state: s', result: a, log: f w }

instance monadRWSRWST :: (Monad m, Monoid w) => MonadRWS r w s (RWST r w s m)
