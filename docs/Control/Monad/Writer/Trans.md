## Module Control.Monad.Writer.Trans

This module defines the writer monad transformer, `WriterT`.

#### `WriterT`

``` purescript
newtype WriterT w m a
  = WriterT (m (Tuple a w))
```

The writer monad transformer.

This monad transformer extends the base monad with a monoidal accumulator of
type `w`.

The `MonadWriter` type class describes the operations supported by this monad.

##### Instances
``` purescript
instance functorWriterT :: (Functor m) => Functor (WriterT w m)
instance applyWriterT :: (Monoid w, Apply m) => Apply (WriterT w m)
instance applicativeWriterT :: (Monoid w, Applicative m) => Applicative (WriterT w m)
instance altWriterT :: (Monoid w, Alt m) => Alt (WriterT w m)
instance plusWriterT :: (Monoid w, Plus m) => Plus (WriterT w m)
instance alternativeWriterT :: (Monoid w, Alternative m) => Alternative (WriterT w m)
instance bindWriterT :: (Monoid w, Monad m) => Bind (WriterT w m)
instance monadWriterT :: (Monoid w, Monad m) => Monad (WriterT w m)
instance monadRecWriterT :: (Monoid w, MonadRec m) => MonadRec (WriterT w m)
instance monadPlusWriterT :: (Monoid w, MonadPlus m) => MonadPlus (WriterT w m)
instance monadTransWriterT :: (Monoid w) => MonadTrans (WriterT w)
instance monadEffWriter :: (Monad m, Monoid w, MonadEff eff m) => MonadEff eff (WriterT w m)
```

#### `runWriterT`

``` purescript
runWriterT :: forall w m a. WriterT w m a -> m (Tuple a w)
```

Run a computation in the `WriterT` monad.

#### `execWriterT`

``` purescript
execWriterT :: forall w m a. (Apply m) => WriterT w m a -> m w
```

Run a computation in the `WriterT` monad, discarding the result.

#### `mapWriterT`

``` purescript
mapWriterT :: forall w1 w2 m1 m2 a b. (m1 (Tuple a w1) -> m2 (Tuple b w2)) -> WriterT w1 m1 a -> WriterT w2 m2 b
```

Change the accumulator and base monad types in a `WriterT` monad action.

#### `liftCatchWriter`

``` purescript
liftCatchWriter :: forall w m e a. (m (Tuple a w) -> (e -> m (Tuple a w)) -> m (Tuple a w)) -> WriterT w m a -> (e -> WriterT w m a) -> WriterT w m a
```

#### `liftCallCCWriter`

``` purescript
liftCallCCWriter :: forall w m a b. (Monoid w) => (((Tuple a w -> m (Tuple b w)) -> m (Tuple a w)) -> m (Tuple a w)) -> ((a -> WriterT w m b) -> WriterT w m a) -> WriterT w m a
```


