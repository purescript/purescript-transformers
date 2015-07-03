## Module Control.Monad.State

This module defines the `State` monad.

#### `State`

``` purescript
type State s = StateT s Identity
```

The `State` monad is a synonym for the `StateT` monad transformer, applied
to the `Identity` monad.

#### `runState`

``` purescript
runState :: forall s a. State s a -> s -> Tuple a s
```

Run a computation in the `State` monad

#### `evalState`

``` purescript
evalState :: forall s a. State s a -> s -> a
```

Run a computation in the `State` monad, discarding the final state

#### `execState`

``` purescript
execState :: forall s a. State s a -> s -> s
```

Run a computation in the `State` monad, discarding the result

#### `mapState`

``` purescript
mapState :: forall s a b. (Tuple a s -> Tuple b s) -> State s a -> State s b
```

Change the type of the result in a `State` action

#### `withState`

``` purescript
withState :: forall s a. (s -> s) -> State s a -> State s a
```

Modify the state in a `State` action


