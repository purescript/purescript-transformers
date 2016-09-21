-- | This module defines the `MonadError` type class and its instances.

module Control.Monad.Error.Class where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

-- | The `MonadError` type class represents those monads which support errors via
-- | `throwError` and `catchError`.
-- |
-- | - `throwError e` throws the error `e`
-- | - `catchError x f` calls the error handler `f` if an error is thrown during the
-- |   evaluation of `x`.
-- |
-- | An implementation is provided for `ErrorT`, and for other monad transformers
-- | defined in this library.
-- |
-- | Laws:
-- |
-- | - Left zero: `throwError e >>= f = throwError e`
-- | - Catch: `catchError (throwError e) f = f e`
-- | - Pure: `catchError (pure a) f = pure a`
-- |
class Monad m <= MonadError e m | m -> e where
  throwError :: forall a. e -> m a
  catchError :: forall a. m a -> (e -> m a) -> m a

-- | This function allows you to provide a predicate for selecting the
-- | exceptions that you're interested in, and handle only those exceptons.
-- | If the inner computation throws an exception, and the predicate returns
-- | Nothing, then the whole computation will still fail with that exception.
catchJust
  :: forall e m a b
   . MonadError e m
  => (e -> Maybe b) -- ^ Predicate to select exceptions
  -> m a            -- ^ Computation to run
  -> (b -> m a)     -- ^ Handler
  -> m a
catchJust p act handler = catchError act handle
  where
  handle e =
    case p e of
      Nothing -> throwError e
      Just b -> handler b

instance monadErrorEither :: MonadError e (Either e) where
  throwError = Left
  catchError (Left e) h = h e
  catchError (Right x) _ = Right x

instance monadErrorMaybe :: MonadError Unit Maybe where
  throwError = const Nothing
  catchError Nothing f  = f unit
  catchError (Just a) _ = Just a
