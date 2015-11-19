-- | This module defines the reader monad transformer, `ReaderT`.

module Control.Monad.Reader.Trans
  ( ReaderT(..), runReaderT, withReaderT, mapReaderT
  , module Control.Monad.Trans
  , module Control.Monad.Reader.Class
  ) where

import Prelude

import Control.Alt (Alt, (<|>))
import Control.Alternative (Alternative)
import Control.Monad.Cont.Class (MonadCont, callCC)
import Control.Monad.Eff.Class (MonadEff, liftEff)
import Control.Monad.Error.Class (MonadError, throwError, catchError)
import Control.Monad.Reader.Class
import Control.Monad.Rec.Class (MonadRec, tailRecM)
import Control.Monad.State.Class (MonadState, state)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Writer.Class (MonadWriter, writer, listen, pass)
import Control.MonadPlus (MonadPlus)
import Control.Plus (Plus, empty)

import Data.Distributive (Distributive, distribute, collect)
import Data.Either (Either(..), either)

-- | The reader monad transformer.
-- |
-- | This monad transformer extends the base monad transformer with a _global context_ of
-- | type `r`.
-- |
-- | The `MonadReader` type class describes the operations supported by this monad.
newtype ReaderT r m a = ReaderT (r -> m a)

-- | Run a computation in the `ReaderT` monad.
runReaderT :: forall r m a. ReaderT r m a -> (r -> m a)
runReaderT (ReaderT x) = x

-- | Change the type of the result in a `ReaderT` monad action.
mapReaderT :: forall r m1 m2 a b. (m1 a -> m2 b) -> ReaderT r m1 a -> ReaderT r m2 b
mapReaderT f m = ReaderT $ f <<< runReaderT m

-- | Change the type of the context in a `ReaderT` monad action.
withReaderT :: forall r1 r2 m a. (r2 -> r1) -> ReaderT r1 m a -> ReaderT r2 m a
withReaderT f m = ReaderT $ runReaderT m <<< f

instance functorReaderT :: (Functor m) => Functor (ReaderT r m) where
  map f = mapReaderT $ (<$>) f

instance applyReaderT :: (Applicative m) => Apply (ReaderT r m) where
  apply f v = ReaderT \r -> runReaderT f r <*> runReaderT v r

instance applicativeReaderT :: (Applicative m) => Applicative (ReaderT r m) where
  pure = ReaderT <<< const <<< pure

instance altReaderT :: (Alt m) => Alt (ReaderT r m) where
  alt m n = ReaderT \r -> runReaderT m r <|> runReaderT n r

instance plusReaderT :: (Plus m) => Plus (ReaderT r m) where
  empty = ReaderT (const empty)

instance alternativeReaderT :: (Alternative m) => Alternative (ReaderT r m)

instance bindReaderT :: (Monad m) => Bind (ReaderT r m) where
  bind m k = ReaderT \r -> do
    a <- runReaderT m r
    runReaderT (k a) r

instance monadReaderT :: (Monad m) => Monad (ReaderT r m)

instance monadPlusReaderT :: (MonadPlus m) => MonadPlus (ReaderT r m)

instance monadTransReaderT :: MonadTrans (ReaderT r) where
  lift = ReaderT <<< const

instance monadEffReader :: (MonadEff eff m) => MonadEff eff (ReaderT r m) where
  liftEff = lift <<< liftEff

instance monadContReaderT :: (MonadCont m) => MonadCont (ReaderT r m) where
  callCC f = ReaderT $ \r -> callCC $ \c -> runReaderT (f (\a -> ReaderT $ const $ c a)) r

instance monadErrorReaderT :: (MonadError e m) => MonadError e (ReaderT r m) where
  throwError e = lift (throwError e)
  catchError m h = ReaderT $ \r -> catchError (runReaderT m r) (\e -> runReaderT (h e) r)

instance monadReaderReaderT :: (Monad m) => MonadReader r (ReaderT r m) where
  ask = ReaderT return
  local = withReaderT

instance monadStateReaderT :: (MonadState s m) => MonadState s (ReaderT r m) where
  state f = lift (state f)

instance monadWriterReaderT :: (Monad m, MonadWriter w m) => MonadWriter w (ReaderT r m) where
  writer wd = lift (writer wd)
  listen = mapReaderT listen
  pass = mapReaderT pass

instance distributiveReaderT :: (Distributive g) => Distributive (ReaderT e g) where
  distribute a = ReaderT \e -> collect (flip runReaderT e) a
  collect f = distribute <<< map f

instance monadRecReaderT :: (MonadRec m) => MonadRec (ReaderT r m) where
  tailRecM k a = ReaderT \r -> tailRecM (k' r) a
    where
    k' r a = do
      result <- runReaderT (k a) r
      return $ either Left Right result
