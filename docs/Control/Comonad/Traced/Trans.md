## Module Control.Comonad.Traced.Trans

This module defines the cowriter comonad transformer, `TracedT`.

#### `TracedT`

``` purescript
newtype TracedT t w a
  = TracedT (w (t -> a))
```

The cowriter comonad transformer.

This comonad transformer extends the context of a value in the base comonad so that the value
depends on a monoidal position of type `t`.

The `ComonadTraced` type class describes the operations supported by this comonad.

##### Instances
``` purescript
instance functorTracedT :: (Functor w) => Functor (TracedT t w)
instance extendTracedT :: (Extend w, Semigroup t) => Extend (TracedT t w)
instance comonadTracedT :: (Comonad w, Monoid t) => Comonad (TracedT t w)
instance comonadTransTracedT :: (Monoid t) => ComonadTrans (TracedT t)
```

#### `runTracedT`

``` purescript
runTracedT :: forall w a t. TracedT t w a -> w (t -> a)
```

Unwrap a value in the `TracedT` comonad.


