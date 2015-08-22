## Module Control.Monad.State.Class

This module defines the `MonadState` type class and its instances.

#### `MonadState`

``` purescript
class (Monad m) <= MonadState s m where
  state :: forall a. (s -> Tuple a s) -> m a
```

The `MonadState s` type class represents those monads which support a single piece of mutable
state of type `s`.

- `state f` updates the state using the function `f`.

An implementation is provided for `StateT`, and for other monad transformers
defined in this library.

Laws:

- `do { get ; get } = get`
- `do { put x ; put y } = put y`
- `do { put x ; get } = put x $> x`
- `do { s <- get ; put s } = pure unit`


#### `get`

``` purescript
get :: forall m s. (MonadState s m) => m s
```

Get the current state.

#### `gets`

``` purescript
gets :: forall s m a. (MonadState s m) => (s -> a) -> m a
```

Get a value which depends on the current state.

#### `put`

``` purescript
put :: forall m s. (MonadState s m) => s -> m Unit
```

Set the state.

#### `modify`

``` purescript
modify :: forall s m. (MonadState s m) => (s -> s) -> m Unit
```

Modify the state by applying a function to the current state.


