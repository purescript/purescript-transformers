module Control.Monad.Cont.Class where

import Control.Monad.Error
import qualified Control.Monad.Cont.Trans as Cont
import Control.Monad.Error
import Control.Monad.Error.Trans
import Control.Monad.Maybe.Trans
import Control.Monad.Reader.Trans
import Control.Monad.State.Trans
import Control.Monad.Writer.Trans
import Data.Monoid

class MonadCont m where
  callCC :: forall a b. ((a -> m b) -> m a) -> m a

instance monadContContT :: (Monad m) => MonadCont (Cont.ContT r m) where
  callCC = Cont.callCC

instance monadContErrorT :: (Error e, MonadCont m) => MonadCont (ErrorT e m) where
  callCC = liftCallCCError callCC

instance monadContMaybeT :: (MonadCont m) => MonadCont (MaybeT m) where
  callCC = liftCallCCMaybe callCC

instance monadContReaderT :: (MonadCont m) => MonadCont (ReaderT r m) where
  callCC = liftCallCCReader callCC

instance monadContStateT :: (MonadCont m) => MonadCont (StateT s m) where
  callCC = liftCallCCState' callCC

instance monadWriterT :: (Monoid w, MonadCont m) => MonadCont (WriterT w m) where
  callCC = liftCallCCWriter callCC
