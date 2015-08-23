## Module Control.Monad.Error.Class

This module defines the `MonadError` type class and its instances.

#### `MonadError`

``` purescript
class (Monad m) <= MonadError e m where
  throwError :: forall a. e -> m a
  catchError :: forall a. m a -> (e -> m a) -> m a
```

The `MonadError` type class represents those monads which support errors via
`throwError` and `catchError`.

- `throwError e` throws the error `e`
- `catchError x f` calls the error handler `f` if an error is thrown during the
  evaluation of `x`.

An implementation is provided for `ErrorT`, and for other monad transformers
defined in this library.

Laws:

- Left zero: `throwError e >>= f = throwError e`
- Catch: `catchError (throwError e) f = f e`
- Pure: `catchError (pure a) f = pure a`


##### Instances
``` purescript
instance monadErrorEither :: MonadError e (Either e)
instance monadErrorMaybe :: MonadError Unit Maybe
```

#### `catchJust`

``` purescript
catchJust :: forall e m a b. (MonadError e m) => (e -> Maybe b) -> m a -> (b -> m a) -> m a
```

This function allows you to provide a predicate for selecting the
exceptions that you're interested in, and handle only those exceptons.
If the inner computation throws an exception, and the predicate returns
Nothing, then the whole computation will still fail with that exception.


