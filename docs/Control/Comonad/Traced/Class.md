## Module Control.Comonad.Traced.Class

This module defines the `ComonadTraced` type class and its instances.

#### `ComonadTraced`

``` purescript
class (Comonad w) <= ComonadTraced t w where
  track :: forall a. t -> w a -> a
```

The `ComonadTraced` type class represents those monads which support relative (monoidal)
position information via `track`.

- `track` extracts a value at the specified relative position.

An implementation is provided for `TracedT`.

Laws:

- `track mempty = extract`
- `track s <<= track t x = track (s <> t) x`

For example:

```purescript
blur :: forall w. (ComonadTraced (Additive Number) w) -> w Number -> w Number
blur = extend \r -> (track (Additive (-1)) r + track (Additive 1) r) / 2
```

##### Instances
``` purescript
instance comonadTracedTracedT :: (Comonad w, Monoid t) => ComonadTraced t (TracedT t w)
```

#### `tracks`

``` purescript
tracks :: forall w a t. (Comonad w, ComonadTraced t w) => (a -> t) -> w a -> a
```

Extracts a value at a relative position which depends on the current value.

#### `listen`

``` purescript
listen :: forall w a t. (Functor w) => TracedT t w a -> TracedT t w (Tuple a t)
```

Get the current position.

#### `listens`

``` purescript
listens :: forall w a t b. (Functor w) => (t -> b) -> TracedT t w a -> TracedT t w (Tuple a b)
```

Get a value which depends on the current position.

#### `censor`

``` purescript
censor :: forall w a t. (Functor w) => (t -> t) -> TracedT t w a -> TracedT t w a
```

Apply a function to the current position.


