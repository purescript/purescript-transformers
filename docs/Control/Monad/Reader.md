## Module Control.Monad.Reader

This module defines the `Reader` monad.

#### `Reader`

``` purescript
type Reader r = ReaderT r Identity
```

The `Reader` monad is a synonym for the `ReaderT` monad transformer, applied
to the `Identity` monad.

#### `runReader`

``` purescript
runReader :: forall r a. Reader r a -> r -> a
```

Run a computation in the `Reader` monad.

#### `withReader`

``` purescript
withReader :: forall r1 r2 a. (r2 -> r1) -> Reader r1 a -> Reader r2 a
```

Change the type of the context in a `Reader` monad action.

#### `mapReader`

``` purescript
mapReader :: forall r a b. (a -> b) -> Reader r a -> Reader r b
```

Change the type of the result in a `Reader` monad action.


