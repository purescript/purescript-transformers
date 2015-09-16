## Module Control.Monad.Reader.Trans

This module defines the reader monad transformer, `ReaderT`.

#### `ReaderT`

``` purescript
newtype ReaderT r m a
  = ReaderT (r -> m a)
```

The reader monad transformer.

This monad transformer extends the base monad transformer with a _global context_ of
type `r`.

The `MonadReader` type class describes the operations supported by this monad.

##### Instances
``` purescript
instance functorReaderT :: (Functor m) => Functor (ReaderT r m)
instance applyReaderT :: (Applicative m) => Apply (ReaderT r m)
instance applicativeReaderT :: (Applicative m) => Applicative (ReaderT r m)
instance altReaderT :: (Alt m) => Alt (ReaderT r m)
instance plusReaderT :: (Plus m) => Plus (ReaderT r m)
instance alternativeReaderT :: (Alternative m) => Alternative (ReaderT r m)
instance bindReaderT :: (Monad m) => Bind (ReaderT r m)
instance monadReaderT :: (Monad m) => Monad (ReaderT r m)
instance monadPlusReaderT :: (MonadPlus m) => MonadPlus (ReaderT r m)
instance monadTransReaderT :: MonadTrans (ReaderT r)
instance monadEffReader :: (MonadEff eff m) => MonadEff eff (ReaderT r m)
instance monadContReaderT :: (MonadCont m) => MonadCont (ReaderT r m)
instance monadErrorReaderT :: (MonadError e m) => MonadError e (ReaderT r m)
instance monadReaderReaderT :: (Monad m) => MonadReader r (ReaderT r m)
instance monadStateReaderT :: (MonadState s m) => MonadState s (ReaderT r m)
instance monadWriterReaderT :: (Monad m, MonadWriter w m) => MonadWriter w (ReaderT r m)
instance distributiveReaderT :: (Distributive g) => Distributive (ReaderT e g)
instance monadRecReaderT :: (MonadRec m) => MonadRec (ReaderT r m)
```

#### `runReaderT`

``` purescript
runReaderT :: forall r m a. ReaderT r m a -> r -> m a
```

Run a computation in the `ReaderT` monad.

#### `mapReaderT`

``` purescript
mapReaderT :: forall r m1 m2 a b. (m1 a -> m2 b) -> ReaderT r m1 a -> ReaderT r m2 b
```

Change the type of the result in a `ReaderT` monad action.

#### `withReaderT`

``` purescript
withReaderT :: forall r1 r2 m a. (r2 -> r1) -> ReaderT r1 m a -> ReaderT r2 m a
```

Change the type of the context in a `ReaderT` monad action.


