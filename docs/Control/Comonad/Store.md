## Module Control.Comonad.Store

This module defines the `Store` comonad.

#### `Store`

``` purescript
type Store s a = StoreT s Identity a
```

The `Store` comonad is a synonym for the `StoreT` comonad transformer, applied
to the `Identity` monad.

#### `runStore`

``` purescript
runStore :: forall s a. Store s a -> Tuple (s -> a) s
```

Unwrap a value in the `Store` comonad.

#### `store`

``` purescript
store :: forall s a. (s -> a) -> s -> Store s a
```

Create a value in context in the `Store` comonad.


