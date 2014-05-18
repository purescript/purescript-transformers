module Control.Monad.Thunk where

data Thunk a = Thunk ({} -> a)

runThunk :: forall a. Thunk a -> a
runThunk (Thunk f) = f {}

instance functorThunk :: Functor Thunk where
  (<$>) f th = Thunk $ \_ -> f (runThunk th)

instance applyThunk :: Apply Thunk where
  (<*>) f x = Thunk $ \_ -> (runThunk f) (runThunk x)

instance applicativeThunk :: Applicative Thunk where
  pure a = Thunk $ \_ -> a

instance bindThunk :: Bind Thunk where
  (>>=) th f = Thunk $ \_ -> runThunk (f (runThunk th))

instance monadThunk :: Monad Thunk


