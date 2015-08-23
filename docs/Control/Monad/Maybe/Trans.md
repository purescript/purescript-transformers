## Module Control.Monad.Maybe.Trans

This module defines the `MaybeT` monad transformer.

#### `MaybeT`

``` purescript
newtype MaybeT m a
  = MaybeT (m (Maybe a))
```

The `MaybeT` monad transformer.

This monad transformer extends the base monad, supporting failure and alternation via
the `MonadPlus` type class.

##### Instances
``` purescript
instance functorMaybeT :: (Monad m) => Functor (MaybeT m)
instance applyMaybeT :: (Monad m) => Apply (MaybeT m)
instance applicativeMaybeT :: (Monad m) => Applicative (MaybeT m)
instance bindMaybeT :: (Monad m) => Bind (MaybeT m)
instance monadMaybeT :: (Monad m) => Monad (MaybeT m)
instance monadTransMaybeT :: MonadTrans MaybeT
instance altMaybeT :: (Monad m) => Alt (MaybeT m)
instance plusMaybeT :: (Monad m) => Plus (MaybeT m)
instance alternativeMaybeT :: (Monad m) => Alternative (MaybeT m)
instance monadPlusMaybeT :: (Monad m) => MonadPlus (MaybeT m)
instance monadRecMaybeT :: (MonadRec m) => MonadRec (MaybeT m)
instance monadEffMaybe :: (MonadEff eff m) => MonadEff eff (MaybeT m)
instance monadContMaybeT :: (MonadCont m) => MonadCont (MaybeT m)
instance monadErrorMaybeT :: (MonadError e m) => MonadError e (MaybeT m)
instance monadReaderMaybeT :: (MonadReader r m) => MonadReader r (MaybeT m)
instance monadStateMaybeT :: (MonadState s m) => MonadState s (MaybeT m)
instance monadWriterMaybeT :: (Monad m, MonadWriter w m) => MonadWriter w (MaybeT m)
instance monadRWSMaybeT :: (Monoid w, MonadRWS r w s m) => MonadRWS r w s (MaybeT m)
```

#### `runMaybeT`

``` purescript
runMaybeT :: forall m a. MaybeT m a -> m (Maybe a)
```

Run a computation in the `MaybeT` monad.

#### `mapMaybeT`

``` purescript
mapMaybeT :: forall m1 m2 a b. (m1 (Maybe a) -> m2 (Maybe b)) -> MaybeT m1 a -> MaybeT m2 b
```

Change the result type of a `MaybeT` monad action.


