## Module Control.Monad.State.Trans

This module defines the state monad transformer, `StateT`.

#### `StateT`

``` purescript
newtype StateT s m a
  = StateT (s -> m (Tuple a s))
```

The state monad transformer.

This monad transformer extends the base monad with the operations `get`
and `put` which can be used to model a single piece of mutable state.

The `MonadState` type class describes the operations supported by this monad.

##### Instances
``` purescript
instance functorStateT :: (Monad m) => Functor (StateT s m)
instance applyStateT :: (Monad m) => Apply (StateT s m)
instance applicativeStateT :: (Monad m) => Applicative (StateT s m)
instance altStateT :: (Monad m, Alt m) => Alt (StateT s m)
instance plusStateT :: (Monad m, Plus m) => Plus (StateT s m)
instance alternativeStateT :: (Monad m, Alternative m) => Alternative (StateT s m)
instance bindStateT :: (Monad m) => Bind (StateT s m)
instance monadStateT :: (Monad m) => Monad (StateT s m)
instance monadRecStateT :: (MonadRec m) => MonadRec (StateT s m)
instance monadPlusStateT :: (MonadPlus m) => MonadPlus (StateT s m)
instance monadTransStateT :: MonadTrans (StateT s)
instance lazyStateT :: Lazy (StateT s m a)
instance monadEffState :: (Monad m, MonadEff eff m) => MonadEff eff (StateT s m)
```

#### `runStateT`

``` purescript
runStateT :: forall s m a. StateT s m a -> s -> m (Tuple a s)
```

Run a computation in the `StateT` monad.

#### `evalStateT`

``` purescript
evalStateT :: forall s m a. (Apply m) => StateT s m a -> s -> m a
```

Run a computation in the `StateT` monad, discarding the final state.

#### `execStateT`

``` purescript
execStateT :: forall s m a. (Apply m) => StateT s m a -> s -> m s
```

Run a computation in the `StateT` monad discarding the result.

#### `mapStateT`

``` purescript
mapStateT :: forall s m1 m2 a b. (m1 (Tuple a s) -> m2 (Tuple b s)) -> StateT s m1 a -> StateT s m2 b
```

Change the result type in a `StateT` monad action.

#### `withStateT`

``` purescript
withStateT :: forall s m a. (s -> s) -> StateT s m a -> StateT s m a
```

Modify the final state in a `StateT` monad action.

#### `liftCatchState`

``` purescript
liftCatchState :: forall s m e a. (m (Tuple a s) -> (e -> m (Tuple a s)) -> m (Tuple a s)) -> StateT s m a -> (e -> StateT s m a) -> StateT s m a
```

#### `liftListenState`

``` purescript
liftListenState :: forall s m a w. (Monad m) => (m (Tuple a s) -> m (Tuple (Tuple a s) w)) -> StateT s m a -> StateT s m (Tuple a w)
```

#### `liftPassState`

``` purescript
liftPassState :: forall s m a b w. (Monad m) => (m (Tuple (Tuple a s) b) -> m (Tuple a s)) -> StateT s m (Tuple a b) -> StateT s m a
```

#### `liftCallCCState`

``` purescript
liftCallCCState :: forall s m a b. (((Tuple a s -> m (Tuple b s)) -> m (Tuple a s)) -> m (Tuple a s)) -> ((a -> StateT s m b) -> StateT s m a) -> StateT s m a
```

#### `liftCallCCState'`

``` purescript
liftCallCCState' :: forall s m a b. (((Tuple a s -> m (Tuple b s)) -> m (Tuple a s)) -> m (Tuple a s)) -> ((a -> StateT s m b) -> StateT s m a) -> StateT s m a
```


