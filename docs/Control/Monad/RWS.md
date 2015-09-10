## Module Control.Monad.RWS

This module defines the `RWS` monad.

#### `RWS`

``` purescript
type RWS r w s = RWST r w s Identity
```

The `RWS` monad is a synonym for the `RWST` monad transformer, applied
to the `Identity` monad.

#### `rws`

``` purescript
rws :: forall r w s a. (r -> s -> RWSResult s a w) -> RWS r w s a
```

Create an action in the `RWS` monad from a function which uses the
global context and state explicitly.

#### `runRWS`

``` purescript
runRWS :: forall r w s a. RWS r w s a -> r -> s -> RWSResult s a w
```

Run a computation in the `RWS` monad.

#### `evalRWS`

``` purescript
evalRWS :: forall r w s a. RWS r w s a -> r -> s -> Tuple a w
```

Run a computation in the `RWS` monad, discarding the final state

#### `execRWS`

``` purescript
execRWS :: forall r w s a. RWS r w s a -> r -> s -> Tuple s w
```

Run a computation in the `RWS` monad, discarding the result

#### `mapRWS`

``` purescript
mapRWS :: forall r w1 w2 s a1 a2. (RWSResult s a1 w1 -> RWSResult s a2 w2) -> RWS r w1 s a1 -> RWS r w2 s a2
```

Change the types of the result and accumulator in a `RWS` action

#### `withRWS`

``` purescript
withRWS :: forall r1 r2 w s a. (r2 -> s -> Tuple r1 s) -> RWS r1 w s a -> RWS r2 w s a
```

Change the type of the context in a `RWS` action


