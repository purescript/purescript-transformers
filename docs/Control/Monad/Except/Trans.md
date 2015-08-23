## Module Control.Monad.Except.Trans

This module defines the _exception monad transformer_ `ExceptT`.

#### `ExceptT`

``` purescript
newtype ExceptT e m a
  = ExceptT (m (Either e a))
```

A monad transformer which adds exceptions to other monads, in the same way
as `Except`. As before, `e` is the type of exceptions, and `a` is the type
of successful results. The new type parameter `m` is the inner monad that
computations run in.

##### Instances
``` purescript
instance functorExceptT :: (Functor m) => Functor (ExceptT e m)
instance applyExceptT :: (Apply m) => Apply (ExceptT e m)
instance applicativeExceptT :: (Applicative m) => Applicative (ExceptT e m)
instance bindExceptT :: (Monad m) => Bind (ExceptT e m)
instance monadExceptT :: (Monad m) => Monad (ExceptT e m)
instance monadRecExceptT :: (MonadRec m) => MonadRec (ExceptT e m)
instance altExceptT :: (Semigroup e, Monad m) => Alt (ExceptT e m)
instance plusExceptT :: (Monoid e, Monad m) => Plus (ExceptT e m)
instance alternativeExceptT :: (Monoid e, Monad m) => Alternative (ExceptT e m)
instance monadPlusExceptT :: (Monoid e, Monad m) => MonadPlus (ExceptT e m)
instance monadTransExceptT :: MonadTrans (ExceptT e)
instance monadEffExceptT :: (MonadEff eff m) => MonadEff eff (ExceptT e m)
instance monadContExceptT :: (MonadCont m) => MonadCont (ExceptT e m)
instance monadErrorExceptT :: (Monad m) => MonadError e (ExceptT e m)
instance monadReaderExceptT :: (MonadReader r m) => MonadReader r (ExceptT e m)
instance monadStateExceptT :: (MonadState s m) => MonadState s (ExceptT e m)
instance monadWriterExceptT :: (MonadWriter w m) => MonadWriter w (ExceptT e m)
instance monadRWSExceptT :: (Monoid w, MonadRWS r w s m) => MonadRWS r w s (ExceptT e m)
```

#### `runExceptT`

``` purescript
runExceptT :: forall e m a. ExceptT e m a -> m (Either e a)
```

The inverse of `ExceptT`. Run a computation in the `ExceptT` monad.

#### `withExceptT`

``` purescript
withExceptT :: forall e e' m a. (Functor m) => (e -> e') -> ExceptT e m a -> ExceptT e' m a
```

Transform any exceptions thrown by an `ExceptT` computation using the given function.

#### `mapExceptT`

``` purescript
mapExceptT :: forall e e' m n a b. (m (Either e a) -> n (Either e' b)) -> ExceptT e m a -> ExceptT e' n b
```

Transform the unwrapped computation using the given function.


