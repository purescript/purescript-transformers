-- | This module defines the `MonadWriter` type class and its instances.

module Control.Monad.Writer.Class where

import Prelude

import Data.Monoid (Monoid)
import Data.Tuple (Tuple(..))

-- | The `MonadWriter w` type class represents those monads which support a monoidal accumulator
-- | of type `w`.
-- |
-- | - `writer` appends a value to the accumulator.
-- | - `listen` modifies the result to include the changes to the accumulator.
-- | - `pass` applies the returned function to the accumulator.
-- |
-- | An implementation is provided for `WriterT`, and for other monad transformers
-- | defined in this library.
-- |
-- | Laws:
-- |
-- | - `writer a mempty = pure a`
-- | - `do { tell x ; tell y } = tell (x <> y)`
-- | - `listen (pure a) = pure (Tuple a mempty)`
-- | - `listen (writer a x) = tell x $> Tuple a x`
-- |
class (Monad m) <= MonadWriter w m where
  writer :: forall a. Tuple a w -> m a
  listen :: forall a. m a -> m (Tuple a w)
  pass :: forall a. m (Tuple a (w -> w)) -> m a

-- | Append a value to the accumulator.
tell :: forall w m. (Monoid w, Monad m, MonadWriter w m) => w -> m Unit
tell w = writer $ Tuple unit w

-- | Read a value which depends on the modifications made to the accumulator during an action.
listens :: forall w m a b. (Monoid w, Monad m, MonadWriter w m) => (w -> b) -> m a -> m (Tuple a b)
listens f m = do
  Tuple a w <- listen m
  return $ Tuple a (f w)

-- | Modify the final accumulator value by applying a function.
censor :: forall w m a. (Monoid w, Monad m, MonadWriter w m) => (w -> w) -> m a -> m a
censor f m = pass $ do
  a <- m
  return $ Tuple a f
