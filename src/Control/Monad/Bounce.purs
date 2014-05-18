module Control.Monad.Bounce where

import Data.Either

import Control.Monad.Thunk
import Control.Monad.Eff
import Control.Monad.Trans

import Control.Monad.Bounce.Class

data Jump m a
  = Land a
  | Jump (m (BounceT m a))

data BounceT m a = BounceT (Jump m a)

runBounceT :: forall m a. BounceT m a -> Jump m a
runBounceT (BounceT j) = j

evalBounceTThunk :: forall a. BounceT Thunk a -> a
evalBounceTThunk (BounceT (Land a)) = a
evalBounceTThunk (BounceT (Jump th)) = evalBounceTThunk (runThunk th)

evalBounceTEff :: forall eff a. BounceT (Eff eff) a -> Eff eff a
evalBounceTEff (BounceT (Land a)) = return a
evalBounceTEff (BounceT (Jump action)) = do
  b <- action
  evalBounceTEff b

instance monadTransBounceT :: MonadTrans BounceT where
  lift m = BounceT (Jump ((BounceT <<< Land) <$> m))

instance functorBounceT :: (Functor m) => Functor (BounceT m) where
  (<$>) f (BounceT (Land a)) = BounceT (Land (f a))
  (<$>) f (BounceT (Jump m)) = BounceT $ Jump $ ((<$>) f) <$> m

instance applyBounceT :: (Monad m) => Apply (BounceT m) where
  (<*>) = ap

instance applicativeBounceT :: (Monad m) => Applicative (BounceT m) where
  pure a = BounceT (Land a)

instance bindBounceT :: (Monad m) => Bind (BounceT m) where
  (>>=) (BounceT (Land a)) f = f a
  (>>=) (BounceT (Jump m)) f = BounceT $ Jump $ (\b -> b >>= f) <$> m

instance monadBounceT :: (Monad m) => Monad (BounceT m)

instance monadBounceBounceT :: (Monad m) => MonadBounce (BounceT m) where
  bounce = BounceT (Jump (pure (return {})))
