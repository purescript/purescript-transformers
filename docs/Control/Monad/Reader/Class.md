## Module Control.Monad.Reader.Class

This module defines the `MonadReader` type class and its instances.

#### `MonadReader`

``` purescript
class MonadReader r m where
  ask :: m r
  local :: forall a. (r -> r) -> m a -> m a
```

The `MonadReader` type class represents those monads which support a global context via
`ask` and `local`.

- `ask` reads the current context.
- `local f x` changes the value of the local context during the execution of the action `x`.

An implementation is provided for `ReaderT`, and for other monad transformers
defined in this library.

Laws:

- `do { ask ; ask } = ask`
- `local f ask = f <$> ask`
- `local _ (pure a) = pure a`
- `local f (do { a <- x ; y }) = do { a <- local f x ; local f y }` 

##### Instances
``` purescript
instance monadReaderFun :: MonadReader r (Function r)
instance monadReaderReaderT :: (Monad m) => MonadReader r (ReaderT r m)
instance monadReaderErrorT :: (Monad m, MonadReader r m) => MonadReader r (ErrorT e m)
instance monadReaderMaybeT :: (Monad m, MonadReader r m) => MonadReader r (MaybeT m)
instance monadReaderWriterT :: (Monad m, Monoid w, MonadReader r m) => MonadReader r (WriterT w m)
instance monadReaderStateT :: (Monad m, MonadReader r m) => MonadReader r (StateT s m)
instance monadReaderRWST :: (Monad m, Monoid w) => MonadReader r (RWST r w s m)
```

#### `reader`

``` purescript
reader :: forall r m a. (Monad m, MonadReader r m) => (r -> a) -> m a
```

Read a value which depends on the global context in any `MonadReader`.


