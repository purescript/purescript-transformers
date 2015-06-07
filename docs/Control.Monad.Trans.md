## Module Control.Monad.Cont.Trans

This module defines the CPS monad transformer.

#### `ContT`

``` purescript
newtype ContT r m a
  = ContT ((a -> m r) -> m r)
```

The CPS monad transformer.

This monad transformer extends the base monad with the operation `callCC`.

##### Instances
``` purescript
instance functorContT :: (Monad m) => Functor (ContT r m)
instance applyContT :: (Functor m, Monad m) => Apply (ContT r m)
instance applicativeContT :: (Functor m, Monad m) => Applicative (ContT r m)
instance bindContT :: (Monad m) => Bind (ContT r m)
instance monadContT :: (Monad m) => Monad (ContT r m)
instance monadTransContT :: MonadTrans (ContT r)
```

#### `runContT`

``` purescript
runContT :: forall r m a. ContT r m a -> (a -> m r) -> m r
```

Run a computation in the `ContT` monad, by providing a continuation.

#### `mapContT`

``` purescript
mapContT :: forall r m a. (m r -> m r) -> ContT r m a -> ContT r m a
```

Modify the underlying action in a `ContT` monad action.

#### `withContT`

``` purescript
withContT :: forall r m a b. ((b -> m r) -> a -> m r) -> ContT r m a -> ContT r m b
```

Modify the continuation in a `ContT` monad action

#### `callCC`

``` purescript
callCC :: forall r m a b. ((a -> ContT r m b) -> ContT r m a) -> ContT r m a
```

`callCC`, or _call-with-current-continuation_.

This action makes the current continuation available to the caller.

For example:

```purescript
delay :: forall eff. Number -> ContT Unit (Eff (timeout :: Timeout | eff)) Unit
delay n = callCC \cont ->
  lift $ setTimeout n (runContT (cont unit) (\_ -> return unit))
```


## Module Control.Monad.Trans

This module defines the `MonadTrans` type class of _monad transformers_.

#### `MonadTrans`

``` purescript
class MonadTrans t where
  lift :: forall m a. (Monad m) => m a -> t m a
```

The `MonadTrans` type class represents _monad transformers_.

A monad transformer is a type constructor of kind `(* -> *) -> * -> *`, which
takes a `Monad` as its first argument, and returns another `Monad`.

This allows us to add additional effects to an existing monad. By iterating this
process, we create monad transformer _stacks_, which contain all of the effects
required for a particular computation.

The laws state that `lift` is a `Monad` morphism.

Laws:

- `lift (pure a) = pure a`
- `lift (do { x <- m ; y }) = do { x <- lift m ; lift y }`


