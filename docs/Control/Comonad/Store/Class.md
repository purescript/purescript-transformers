## Module Control.Comonad.Store.Class

This module defines the `ComonadStore` type class and its instances.

#### `ComonadStore`

``` purescript
class (Comonad w) <= ComonadStore s w where
  pos :: forall a. w a -> s
  peek :: forall a. s -> w a -> a
```

The `ComonadStore` type class represents those monads which support local position information via
`pos` and `peek`.

- `pos` reads the current position.
- `peek` reads the value at the specified position in the specified context.

An implementation is provided for `StoreT`.

Laws:

- `pos (extend _ x) = pos x`
- `peek (pos x) x = extract x`

For example:

```purescript
blur :: forall w. (ComonadStore Number w) -> w Number -> w Number
blur = extend \r -> (peeks (\n -> n - 1) r + peeks (\n -> n + 1) r) / 2)
```

##### Instances
``` purescript
instance comonadStoreStoreT :: (Comonad w) => ComonadStore s (StoreT s w)
```

#### `experiment`

``` purescript
experiment :: forall f a w s. (ComonadStore s w, Functor f) => (s -> f s) -> w a -> f a
```

Extract a collection of values from positions which depend on the current position.

#### `peeks`

``` purescript
peeks :: forall s a w. (ComonadStore s w) => (s -> s) -> w a -> a
```

Extract a value from a position which depends on the current position.

#### `seek`

``` purescript
seek :: forall s a w. (ComonadStore s w, Extend w) => s -> w a -> w a
```

Reposition the focus at the specified position.

#### `seeks`

``` purescript
seeks :: forall s a w. (ComonadStore s w, Extend w) => (s -> s) -> w a -> w a
```

Reposition the focus at the specified position, which depends on the current position.


