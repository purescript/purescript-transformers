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
instance applyWriterT :: (Semigroup w, Apply m) => Apply (WriterT w m)
instance applicativeWriterT :: (Monoid w, Applicative m) => Applicative (WriterT w m)
instance altWriterT :: (Alt m) => Alt (WriterT w m)
instance plusWriterT :: (Plus m) => Plus (WriterT w m)
instance alternativeWriterT :: (Monoid w, Alternative m) => Alternative (WriterT w m)
instance bindWriterT :: (Semigroup w, Monad m) => Bind (WriterT w m)
instance monadWriterT :: (Monoid w, Monad m) => Monad (WriterT w m)
instance monadRecWriterT :: (Monoid w, MonadRec m) => MonadRec (WriterT w m)
instance monadPlusWriterT :: (Monoid w, MonadPlus m) => MonadPlus (WriterT w m)
instance monadTransWriterT :: (Monoid w) => MonadTrans (WriterT w)
instance monadEffWriter :: (Monad m, Monoid w, MonadEff eff m) => MonadEff eff (WriterT w m)
instance monadContWriterT :: (Monoid w, MonadCont m) => MonadCont (WriterT w m)
instance monadErrorWriterT :: (Monoid w, MonadError e m) => MonadError e (WriterT w m)
instance monadReaderWriterT :: (Monoid w, MonadReader r m) => MonadReader r (WriterT w m)
instance monadStateWriterT :: (Monoid w, MonadState s m) => MonadState s (WriterT w m)
instance monadWriterWriterT :: (Monoid w, Monad m) => MonadWriter w (WriterT w m)
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


