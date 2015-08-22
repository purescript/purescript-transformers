## Module Control.Monad.Except

#### `Except`

``` purescript
type Except e a = ExceptT e Identity a
```

A parametrizable exception monad; computations are either exceptions or
pure values. If an exception is thrown (see `throwError`), the computation
terminates with that value. Exceptions may also be caught with `catchError`,
allowing the computation to resume and exit successfully.

The type parameter `e` is the type of exceptions, and `a` is the type
of successful results.

A mechanism for trying many different computations until one succeeds is
provided via the `Alt` instance, specifically the `(<|>)` function.
The first computation to succeed is returned; if all fail, the exceptions
are combined using their `Semigroup` instance. The `Plus` instance goes
further and adds the possibility of a computation failing with an 'empty'
exception; naturally, this requires the stronger constraint of a `Monoid`
instance for the exception type.

#### `except`

``` purescript
except :: forall e a. Either e a -> Except e a
```

Construct a computation in the `Except` monad from an `Either` value.

#### `runExcept`

``` purescript
runExcept :: forall e a. Except e a -> Either e a
```

Run a computation in the `Except` monad. The inverse of `except`.

#### `mapExcept`

``` purescript
mapExcept :: forall e e' a b. (Either e a -> Either e' b) -> Except e a -> Except e' b
```

Transform the unwrapped computation using the given function.

#### `withExcept`

``` purescript
withExcept :: forall e e' a. (e -> e') -> Except e a -> Except e' a
```

Transform any exceptions thrown by an `Except` computation using the given function.


