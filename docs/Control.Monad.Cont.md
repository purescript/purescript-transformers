## Module Control.Monad.Cont.Class

This module defines the `MonadCont` type class and its instances.

#### `MonadCont`

``` purescript
class MonadCont m where
  callCC :: forall a b. ((a -> m b) -> m a) -> m a
```

The `MonadCont` type class represents those monads which support the
`callCC` operation.

An implementation is provided for `ContT`, and for other monad transformers
defined in this library.

##### Instances
``` purescript
instance monadContContT :: (Monad m) => MonadCont (ContT r m)
instance monadContErrorT :: (MonadCont m) => MonadCont (ErrorT e m)
instance monadContMaybeT :: (MonadCont m) => MonadCont (MaybeT m)
instance monadContReaderT :: (MonadCont m) => MonadCont (ReaderT r m)
instance monadContStateT :: (MonadCont m) => MonadCont (StateT s m)
instance monadWriterT :: (Monoid w, MonadCont m) => MonadCont (WriterT w m)
```


