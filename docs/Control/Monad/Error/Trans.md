## Module Control.Monad.Error.Trans

This module defines the error monad transformer, `ErrorT`.

#### `ErrorT`

``` purescript
newtype ErrorT e m a
  = ErrorT (m (Either e a))
```

The error monad transformer

This monad transformer extends the base monad with the ability to throw and handle
errors.

The `MonadError` type class describes the operations supported by this monad.

##### Instances
``` purescript
instance functorErrorT :: (Functor m) => Functor (ErrorT e m)
instance applyErrorT :: (Apply m) => Apply (ErrorT e m)
instance applicativeErrorT :: (Applicative m) => Applicative (ErrorT e m)
instance altErrorT :: (Monad m) => Alt (ErrorT e m)
instance plusErrorT :: (Monad m, Error e) => Plus (ErrorT e m)
instance alternativeErrorT :: (Monad m, Error e) => Alternative (ErrorT e m)
instance bindErrorT :: (Monad m) => Bind (ErrorT e m)
instance monadErrorT :: (Monad m) => Monad (ErrorT e m)
instance monadRecErrorT :: (Error e, MonadRec m) => MonadRec (ErrorT e m)
instance monadPlusErrorT :: (Monad m, Error e) => MonadPlus (ErrorT e m)
instance monadTransErrorT :: MonadTrans (ErrorT e)
instance monadEffError :: (Monad m, MonadEff eff m) => MonadEff eff (ErrorT e m)
```

#### `runErrorT`

``` purescript
runErrorT :: forall e m a. ErrorT e m a -> m (Either e a)
```

Run a computation in the `ErrorT` monad.

#### `mapErrorT`

``` purescript
mapErrorT :: forall e1 e2 m1 m2 a b. (m1 (Either e1 a) -> m2 (Either e2 b)) -> ErrorT e1 m1 a -> ErrorT e2 m2 b
```

Change the error and result types in an `ErrorT` monad action.

#### `liftListenError`

``` purescript
liftListenError :: forall e m a w. (Monad m) => (m (Either e a) -> m (Tuple (Either e a) w)) -> ErrorT e m a -> ErrorT e m (Tuple a w)
```

#### `liftPassError`

``` purescript
liftPassError :: forall e m a w. (Monad m) => (m (Tuple (Either e a) (w -> w)) -> m (Either e a)) -> ErrorT e m (Tuple a (w -> w)) -> ErrorT e m a
```

#### `liftCallCCError`

``` purescript
liftCallCCError :: forall e m a b. (((Either e a -> m (Either e b)) -> m (Either e a)) -> m (Either e a)) -> ((a -> ErrorT e m b) -> ErrorT e m a) -> ErrorT e m a
```


