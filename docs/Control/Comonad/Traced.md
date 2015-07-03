## Module Control.Comonad.Traced

This module defines the `Traced` comonad.

#### `Traced`

``` purescript
type Traced m = TracedT m Identity
```

The `Traced` comonad is a synonym for the `TracedT` comonad transformer, applied
to the `Identity` monad.

#### `runTraced`

``` purescript
runTraced :: forall m a. Traced m a -> m -> a
```

Unwrap a value in the `Traced` comonad.

#### `traced`

``` purescript
traced :: forall m a. (m -> a) -> Traced m a
```

Create a value in context in the `Traced` comonad.


