## Module Control.Comonad.Env.Trans

This module defines the environment comonad transformer, `EnvT`.

#### `EnvT`

``` purescript
newtype EnvT e w a
  = EnvT (Tuple e (w a))
```

The environment comonad transformer.

This comonad transformer extends the context of a value in the base comonad with a _global environment_ of
type `e`.

The `ComonadEnv` type class describes the operations supported by this comonad.

##### Instances
``` purescript
instance functorEnvT :: (Functor w) => Functor (EnvT e w)
instance extendEnvT :: (Extend w) => Extend (EnvT e w)
instance comonadEnvT :: (Comonad w) => Comonad (EnvT e w)
instance comonadTransEnvT :: ComonadTrans (EnvT e)
```

#### `runEnvT`

``` purescript
runEnvT :: forall e w a. EnvT e w a -> Tuple e (w a)
```

Unwrap a value in the `EnvT` comonad.

#### `withEnvT`

``` purescript
withEnvT :: forall e1 e2 w a. (e1 -> e2) -> EnvT e1 w a -> EnvT e2 w a
```

Change the environment type in an `EnvT` context.

#### `mapEnvT`

``` purescript
mapEnvT :: forall e w1 w2 a b. (w1 a -> w2 b) -> EnvT e w1 a -> EnvT e w2 b
```

Change the underlying comonad and data type in an `EnvT` context.


