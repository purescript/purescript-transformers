## Module Control.Monad.RWS.Trans

This module defines the reader-writer-state monad transformer, `RWST`.

#### `RWSResult`

``` purescript
data RWSResult state result writer
  = RWSResult state result writer
```

#### `RWST`

``` purescript
newtype RWST r w s m a
  = RWST (r -> s -> m (RWSResult s a w))
```

The reader-writer-state monad transformer, which combines the operations
of `ReaderT`, `WriterT` and `StateT` into a single monad transformer.

##### Instances
``` purescript
instance functorRWST :: (Functor m, Monoid w) => Functor (RWST r w s m)
instance applyRWST :: (Bind m, Monoid w) => Apply (RWST r w s m)
instance bindRWST :: (Bind m, Monoid w) => Bind (RWST r w s m)
instance applicativeRWST :: (Monad m, Monoid w) => Applicative (RWST r w s m)
instance monadRWST :: (Monad m, Monoid w) => Monad (RWST r w s m)
instance monadTransRWST :: (Monoid w) => MonadTrans (RWST r w s)
instance monadEffRWS :: (Monad m, Monoid w, MonadEff eff m) => MonadEff eff (RWST r w s m)
instance monadReaderRWST :: (Monad m, Monoid w) => MonadReader r (RWST r w s m)
instance monadStateRWST :: (Monad m, Monoid w) => MonadState s (RWST r w s m)
instance monadWriterRWST :: (Monad m, Monoid w) => MonadWriter w (RWST r w s m)
instance monadRWSRWST :: (Monad m, Monoid w) => MonadRWS r w s (RWST r w s m)
instance monadErrorRWST :: (MonadError e m, Monoid w) => MonadError e (RWST r w s m)
instance monadRecRWST :: (Monoid w, MonadRec m) => MonadRec (RWST r w s m)
```

#### `runRWST`

``` purescript
runRWST :: forall r w s m a. RWST r w s m a -> r -> s -> m (RWSResult s a w)
```

Run a computation in the `RWST` monad.

#### `evalRWST`

``` purescript
evalRWST :: forall r w s m a. (Monad m) => RWST r w s m a -> r -> s -> m (Tuple a w)
```

Run a computation in the `RWST` monad, discarding the final state.

#### `execRWST`

``` purescript
execRWST :: forall r w s m a. (Monad m) => RWST r w s m a -> r -> s -> m (Tuple s w)
```

Run a computation in the `RWST` monad, discarding the result.

#### `mapRWST`

``` purescript
mapRWST :: forall r w1 w2 s m1 m2 a1 a2. (m1 (RWSResult s a1 w1) -> m2 (RWSResult s a2 w2)) -> RWST r w1 s m1 a1 -> RWST r w2 s m2 a2
```

Change the result and accumulator types in a `RWST` monad action.

#### `withRWST`

``` purescript
withRWST :: forall r1 r2 w s m a. (r2 -> s -> Tuple r1 s) -> RWST r1 w s m a -> RWST r2 w s m a
```

Change the context type in a `RWST` monad action.


