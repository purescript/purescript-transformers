## Module Control.Monad.RWS

This module defines the `RWS` monad.

#### `RWS`

``` purescript
type RWS r w s = RWST r w s Identity
```

The `RWS` monad is a synonym for the `RWST` monad transformer, applied
to the `Identity` monad.

#### `rws`

``` purescript
rws :: forall r w s a. (r -> s -> See s a w) -> RWS r w s a
```

Create an action in the `RWS` monad from a function which uses the 
global context and state explicitly.

#### `runRWS`

``` purescript
runRWS :: forall r w s a. RWS r w s a -> r -> s -> See s a w
```

Run a computation in the `RWS` monad.

#### `evalRWS`

``` purescript
evalRWS :: forall r w s a. RWS r w s a -> r -> s -> Tuple a w
```

Run a computation in the `RWS` monad, discarding the final state

#### `execRWS`

``` purescript
execRWS :: forall r w s a. RWS r w s a -> r -> s -> Tuple s w
```

Run a computation in the `RWS` monad, discarding the result

#### `mapRWS`

``` purescript
mapRWS :: forall r w1 w2 s a1 a2. (See s a1 w1 -> See s a2 w2) -> RWS r w1 s a1 -> RWS r w2 s a2
```

Change the types of the result and accumulator in a `RWS` action

#### `withRWS`

``` purescript
withRWS :: forall r1 r2 w s a. (r2 -> s -> Tuple r1 s) -> RWS r1 w s a -> RWS r2 w s a
```

Change the type of the context in a `RWS` action

#### `ask`

``` purescript
ask :: forall r w s m. (Applicative m, Monoid w) => RWST r w s m r
```

Get the context of a `RWS` action

#### `local`

``` purescript
local :: forall r w s m a. (r -> r) -> RWST r w s m a -> RWST r w s m a
```

Locally change the context of a `RWS` action.

#### `reader`

``` purescript
reader :: forall r w s m a. (Applicative m, Monoid w) => (r -> a) -> RWST r w s m a
```

Read a value which depends on the context in a `RWS` action.

#### `writer`

``` purescript
writer :: forall r w s m a. (Applicative m) => Tuple a w -> RWST r w s m a
```

Write to the accumulator in a `RWS` action

#### `listen`

``` purescript
listen :: forall r w s m a. (Monad m) => RWST r w s m a -> RWST r w s m (Tuple a w)
```

Execute a `RWS` action, and return the changes to the accumulator along with the return value

#### `pass`

``` purescript
pass :: forall r w s m a. (Monad m) => RWST r w s m (Tuple a (w -> w)) -> RWST r w s m a
```

Execute a `RWS` action and modify the accumulator

#### `tell`

``` purescript
tell :: forall r w s m. (Applicative m) => w -> RWST r w s m Unit
```

Append a value to the accumulator in a `RWS` action

#### `listens`

``` purescript
listens :: forall r w s m a b. (Monad m) => (w -> b) -> RWST r w s m a -> RWST r w s m (Tuple a b)
```

Execute a `RWS` action, and return a value which depends on the accumulator along with the return value

#### `censor`

``` purescript
censor :: forall r w s m a. (Monad m) => (w -> w) -> RWST r w s m a -> RWST r w s m a
```

Modify the accumulator in a `RWS` action

#### `state`

``` purescript
state :: forall r w s m a. (Applicative m, Monoid w) => (s -> Tuple a s) -> RWST r w s m a
```

Get or modify the state in a `RWS` action

#### `get`

``` purescript
get :: forall r w s m. (Applicative m, Monoid w) => RWST r w s m s
```

Get the state in a `RWS` action

#### `gets`

``` purescript
gets :: forall r w s m a. (Applicative m, Monoid w) => (s -> a) -> RWST r w s m a
```

Get a value which depends on the state in a `RWS` action

#### `put`

``` purescript
put :: forall r w s m. (Applicative m, Monoid w) => s -> RWST r w s m Unit
```

Set the state in a `RWS` action

#### `modify`

``` purescript
modify :: forall r w s m. (Applicative m, Monoid w) => (s -> s) -> RWST r w s m Unit
```

Modify the state in a `RWS` action


