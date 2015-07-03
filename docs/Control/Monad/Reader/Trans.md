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
instance monadEffReader :: (Monad m, MonadEff eff m) => MonadEff eff (ReaderT r m)
instance distributiveReaderT :: (Distributive g) => Distributive (ReaderT e g)
```

#### `runReaderT`

``` purescript
runReaderT :: forall r m a. ReaderT r m a -> r -> m a
```

Run a computation in the `ReaderT` monad.

#### `withReaderT`

``` purescript
withReaderT :: forall r1 r2 m a b. (r2 -> r1) -> ReaderT r1 m a -> ReaderT r2 m a
```

Change the type of the context in a `ReaderT` monad action.

#### `mapReaderT`

``` purescript
mapReaderT :: forall r m1 m2 a b. (m1 a -> m2 b) -> ReaderT r m1 a -> ReaderT r m2 b
```

Change the type of the result in a `ReaderT` monad action.

#### `liftReaderT`

``` purescript
liftReaderT :: forall r m a. m a -> ReaderT r m a
```

#### `liftCatchReader`

``` purescript
liftCatchReader :: forall r m e a. (m a -> (e -> m a) -> m a) -> ReaderT r m a -> (e -> ReaderT r m a) -> ReaderT r m a
```

#### `liftCallCCReader`

``` purescript
liftCallCCReader :: forall r m a b. (((a -> m b) -> m a) -> m a) -> ((a -> ReaderT r m b) -> ReaderT r m a) -> ReaderT r m a
```


