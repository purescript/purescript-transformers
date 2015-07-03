## Module Control.Monad.Error

This module defines the `Error` type class, which is used with the error monad
transformer, `ErrorT`.

#### `Error`

``` purescript
class Error a where
  noMsg :: a
  strMsg :: String -> a
```

The `Error` type class represents _error_ types, which can be 
constructed from error message strings.

##### Instances
``` purescript
instance errorString :: Error String
```


