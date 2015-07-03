## Module Control.Comonad.Env

This module defines the `Env` comonad.

#### `Env`

``` purescript
type Env e = EnvT e Identity
```

The `Env` comonad is a synonym for the `EnvT` comonad transformer, applied
to the `Identity` monad.

#### `runEnv`

``` purescript
runEnv :: forall e a. Env e a -> Tuple e a
```

Unwrap a value in the `Env` comonad.

#### `withEnv`

``` purescript
withEnv :: forall e1 e2 a. (e1 -> e2) -> Env e1 a -> Env e2 a
```

Change the environment type in an `Env` computation.

#### `mapEnv`

``` purescript
mapEnv :: forall e a b. (a -> b) -> Env e a -> Env e b
```

Change the data type in an `Env` computation.

#### `env`

``` purescript
env :: forall e a. e -> a -> Env e a
```

Create a value in context in the `Env` comonad.


