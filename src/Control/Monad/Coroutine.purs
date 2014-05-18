module Control.Monad.Coroutine where

import Data.Either

import Control.Monad.Thunk
import Control.Monad.Eff
import Control.Monad.Trans

import Control.Monad.Coroutine.Class

data Step m a
  = Done a
  | Step (m (CoroutineT m a))

data CoroutineT m a = CoroutineT (Step m a)

runCoroutineT :: forall m a. CoroutineT m a -> Step m a
runCoroutineT (CoroutineT j) = j

evalCoroutineTThunk :: forall a. CoroutineT Thunk a -> a
evalCoroutineTThunk (CoroutineT (Done a)) = a
evalCoroutineTThunk (CoroutineT (Step th)) = evalCoroutineTThunk (runThunk th)

evalCoroutineTEff :: forall eff a. CoroutineT (Eff eff) a -> Eff eff a
evalCoroutineTEff (CoroutineT (Done a)) = return a
evalCoroutineTEff (CoroutineT (Step action)) = do
  b <- action
  evalCoroutineTEff b

instance monadTransCoroutineT :: MonadTrans CoroutineT where
  lift m = CoroutineT (Step ((CoroutineT <<< Done) <$> m))

instance functorCoroutineT :: (Functor m) => Functor (CoroutineT m) where
  (<$>) f (CoroutineT (Done a)) = CoroutineT (Done (f a))
  (<$>) f (CoroutineT (Step m)) = CoroutineT $ Step $ ((<$>) f) <$> m

instance applyCoroutineT :: (Monad m) => Apply (CoroutineT m) where
  (<*>) = ap

instance applicativeCoroutineT :: (Monad m) => Applicative (CoroutineT m) where
  pure a = CoroutineT (Done a)

instance bindCoroutineT :: (Monad m) => Bind (CoroutineT m) where
  (>>=) (CoroutineT (Done a)) f = f a
  (>>=) (CoroutineT (Step m)) f = CoroutineT $ Step $ (\b -> b >>= f) <$> m

instance monadCoroutineT :: (Monad m) => Monad (CoroutineT m)

instance monadYieldCoroutineT :: (Monad m) => MonadYield (CoroutineT m) where
  yield = CoroutineT (Step (pure (return {})))
