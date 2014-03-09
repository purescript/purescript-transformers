module Control.Monad.Reader where

import Prelude
import Control.Monad.Identity
import Control.Monad.Reader.Trans

type Reader r a = ReaderT r Identity a

runReader :: forall r a. Reader r a -> r -> a
runReader m = runIdentity <<< runReaderT m

withReader :: forall r1 r2 a b. (r2 -> r1) -> Reader r1 a -> Reader r2 a
withReader = withReaderT

mapReader :: forall r a b. (a -> b) -> Reader r a -> Reader r b
mapReader f = mapReaderT $ Identity <<< f <<< runIdentity
