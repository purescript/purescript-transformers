## Module Control.Comonad.Env.Class

This module defines the `ComonadEnv` type class and its instances.

#### `ComonadEnv`

``` purescript
class (Comonad w) <= ComonadEnv e w where
  ask :: forall a. w a -> e
  local :: forall a. (e -> e) -> w a -> w a
```

The `ComonadEnv` type class represents those monads which support a global environment via
`ask` and `local`.

- `ask` reads the current environment from the context.
- `local` changes the value of the global environment.

An implementation is provided for `EnvT`.

Laws:

- `ask (local f x) = f (ask x)`
- `extract (local _ x) = extract a`
- `extend g (local f x) = extend (g <<< local f) x`

##### Instances
``` purescript
instance comonadEnvTuple :: ComonadEnv e (Tuple e)
instance comonadEnvEnvT :: (Comonad w) => ComonadEnv e (EnvT e w)
```

#### `asks`

``` purescript
asks :: forall e1 e2 w. (ComonadEnv e1 w) => (e1 -> e2) -> w e1 -> e2
```

Get a value which depends on the environment.


