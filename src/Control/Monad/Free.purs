module Control.Monad.Free
  ( Free(..)
  , MonadFree, wrap
  , liftF
  , pureF
  , iterM
  , goM
  , go
  , goEff
  ) where

import Control.Monad.Trans
import Control.Monad.Eff
import Data.Either

data Free f a = Pure a
              | Free (f (Free f a))
              | Gosub (forall s. (forall r. (Unit -> Free f r) -> (r -> Free f a) -> s) -> s)

class MonadFree f m where
  wrap :: forall a. f (m a) -> m a

instance functorFree :: (Functor f) => Functor (Free f) where
  (<$>) f (Pure a) = Pure (f a)
  (<$>) f g = liftA1 f g

instance applyFree :: (Functor f) => Apply (Free f) where
  (<*>) = ap

instance applicativeFree :: (Functor f) => Applicative (Free f) where
  pure = Pure

instance bindFree :: (Functor f) => Bind (Free f) where
  (>>=) (Gosub g) f = Gosub (\h -> g (\a i -> h a (\x -> Gosub (\j -> j (const (i x)) f))))
  (>>=) a         f = Gosub (\h -> h (const a) f)

instance monadFree :: (Functor f) => Monad (Free f)

instance monadTransFree :: MonadTrans Free where
  lift f = Free $ do
    a <- f
    return (Pure a)

instance monadFreeFree :: (Functor f) => MonadFree f (Free f) where
  wrap = Free

liftF :: forall f m a. (Functor f, Monad m, MonadFree f m) => f a -> m a
liftF fa = wrap $ return <$> fa

pureF :: forall f a. (Applicative f) => a -> Free f a
pureF a = Free (pure (Pure  a))

-- Note: can blow the stack!
iterM :: forall f m a. (Functor f, Monad m) => (forall a. f (m a) -> m a) -> Free f a -> m a
iterM _ (Pure a) = return a
iterM k (Free f) = k $ iterM k <$> f
iterM k (Gosub f) = f (\req recv -> iterM k (req unit) >>= (iterM k <<< recv))

resume :: forall f a. (Functor f) => Free f a -> Either (f (Free f a)) a
resume (Gosub f) = case go (Gosub f) of
  Left fs -> Left fs
  Right f -> resume f
  where
  go :: forall f a. (Functor f) => Free f a -> Either (f (Free f a)) (Free f a)
  go (Gosub f) = f (\a g ->
    case a unit of
      Pure a -> Right (g a)
      Free t -> Left ((\h -> h >>= g) <$> t)
      Gosub h -> Right (h (\b i -> b unit >>= (\x -> i x >>= g))))
resume (Pure a) = Right a
resume (Free fs) = Left fs

-- Note: can blow the stack!
goM :: forall f m a. (Functor f, Monad m) => (f (Free f a) -> m (Free f a)) -> Free f a -> m a
goM k f = case resume f of
            Left s -> k s >>= goM k
            Right a -> return a

go :: forall f a. (Functor f) => (f (Free f a) -> Free f a) -> Free f a -> a
go fn f = 
  case resume f of
    Left fs -> go fn (fn fs)
    Right a -> a 

goEff :: forall e f a. (Functor f) => (f (Free f a) -> Eff e (Free f a)) -> Free f a -> Eff e a
goEff fn f = case resume f of
  Left fs -> do f' <- fn fs 
                goEff fn f'
  Right a -> return a  
