## Module Control.Monad.Writer

This module defines the `Writer` monad.

#### `Writer`

``` purescript
type Writer w = WriterT w Identity
```

The `Writer` monad is a synonym for the `WriterT` monad transformer, applied
to the `Identity` monad.

#### `runWriter`

``` purescript
runWriter :: forall w a. Writer w a -> Tuple a w
```

Run a computation in the `Writer` monad

#### `execWriter`

``` purescript
execWriter :: forall w a. Writer w a -> w
```

Run a computation in the `Writer` monad, discarding the result

#### `mapWriter`

``` purescript
mapWriter :: forall w1 w2 a b. (Tuple a w1 -> Tuple b w2) -> Writer w1 a -> Writer w2 b
```

Change the result and accumulator types in a `Writer` monad action


