-- | This module defines the state monad transformer, `StateT`.

module Control.Monad.State.Trans
  ( StateT(..), runStateT, evalStateT, execStateT, mapStateT, withStateT
  , module Control.Monad.Trans
  , module Control.Monad.State.Class
  ) where

import Prelude

import Data.Tuple (Tuple(..), fst, snd)
import Data.Either (Either(..))

import Control.Alt (Alt, (<|>))
import Control.Alternative (Alternative)
import Control.Lazy (Lazy)
import Control.Monad.Cont.Class (MonadCont, callCC)
import Control.Monad.Eff.Class (MonadEff, liftEff)
import Control.Monad.Error.Class (MonadError, throwError, catchError)
import Control.Monad.Reader.Class (MonadReader, ask, local)
import Control.Monad.Rec.Class (MonadRec, tailRecM)
import Control.Monad.State.Class
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Writer.Class (MonadWriter, writer, listen, pass)
import Control.MonadPlus (MonadPlus)
import Control.Plus (Plus, empty)

-- | The state monad transformer.
-- |
-- | This monad transformer extends the base monad with the operations `get`
-- | and `put` which can be used to model a single piece of mutable state.
-- |
-- | The `MonadState` type class describes the operations supported by this monad.
newtype StateT s m a = StateT (s -> m (Tuple a s))

-- | Run a computation in the `StateT` monad.
runStateT :: forall s m a. StateT s m a -> s -> m (Tuple a s)
runStateT (StateT s) = s

-- | Run a computation in the `StateT` monad, discarding the final state.
evalStateT :: forall s m a. (Apply m) => StateT s m a -> s -> m a
evalStateT m s = fst <$> runStateT m s

-- | Run a computation in the `StateT` monad discarding the result.
execStateT :: forall s m a. (Apply m) => StateT s m a -> s -> m s
execStateT m s = snd <$> runStateT m s

-- | Change the result type in a `StateT` monad action.
mapStateT :: forall s m1 m2 a b. (m1 (Tuple a s) -> m2 (Tuple b s)) -> StateT s m1 a -> StateT s m2 b
mapStateT f m = StateT $ f <<< runStateT m

-- | Modify the final state in a `StateT` monad action.
withStateT :: forall s m a. (s -> s) -> StateT s m a -> StateT s m a
withStateT f s = StateT $ runStateT s <<< f

instance functorStateT :: (Monad m) => Functor (StateT s m) where
  map = liftM1

instance applyStateT :: (Monad m) => Apply (StateT s m) where
  apply = ap

instance applicativeStateT :: (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> return $ Tuple a s

instance altStateT :: (Monad m, Alt m) => Alt (StateT s m) where
  alt x y = StateT $ \s -> runStateT x s <|> runStateT y s

instance plusStateT :: (Monad m, Plus m) => Plus (StateT s m) where
  empty = StateT $ \_ -> empty

instance alternativeStateT :: (Monad m, Alternative m) => Alternative (StateT s m)

instance bindStateT :: (Monad m) => Bind (StateT s m) where
  bind (StateT x) f = StateT \s -> do
    Tuple v s' <- x s
    runStateT (f v) s'

instance monadStateT :: (Monad m) => Monad (StateT s m)

instance monadRecStateT :: (MonadRec m) => MonadRec (StateT s m) where
  tailRecM f a = StateT \s -> tailRecM f' (Tuple a s)
    where
    f' (Tuple a s) = do
      Tuple m s1 <- runStateT (f a) s
      return case m of
        Left a -> Left (Tuple a s1)
        Right b -> Right (Tuple b s1)

instance monadPlusStateT :: (MonadPlus m) => MonadPlus (StateT s m)

instance monadTransStateT :: MonadTrans (StateT s) where
  lift m = StateT \s -> do
    x <- m
    return $ Tuple x s

instance lazyStateT :: Lazy (StateT s m a) where
  defer f = StateT $ \s -> runStateT (f unit) s

instance monadEffState :: (Monad m, MonadEff eff m) => MonadEff eff (StateT s m) where
  liftEff = lift <<< liftEff

instance monadContStateT :: (MonadCont m) => MonadCont (StateT s m) where
  callCC f = StateT $ \s -> callCC $ \c -> runStateT (f (\a -> StateT $ \s' -> c (Tuple a s'))) s

instance monadErrorStateT :: (MonadError e m) => MonadError e (StateT s m) where
  throwError e = lift (throwError e)
  catchError m h = StateT $ \s -> catchError (runStateT m s) (\e -> runStateT (h e) s)

instance monadReaderStateT :: (MonadReader r m) => MonadReader r (StateT s m) where
  ask = lift ask
  local f = mapStateT (local f)

instance monadStateStateT :: (Monad m) => MonadState s (StateT s m) where
  state f = StateT $ return <<< f

instance monadWriterStateT :: (Monad m, MonadWriter w m) => MonadWriter w (StateT s m) where
  writer wd = lift (writer wd)
  listen m = StateT $ \s -> do
    Tuple (Tuple a s') w <- listen (runStateT m s)
    return $ Tuple (Tuple a w) s'
  pass m = StateT $ \s -> pass $ do
    Tuple (Tuple a f) s' <- runStateT m s
    return $ Tuple (Tuple a s') f
