module Control.Comonad.Store.Class where

import Control.Comonad
import Control.Comonad.Store.Trans
import Control.Extend

import Data.Tuple

class (Comonad w) <= ComonadStore s w where
  pos :: forall a. w a -> s
  peek :: forall a. s -> w a -> a

instance comonadStoreStoreT :: (Comonad w) => ComonadStore s (StoreT s w) where
  pos (StoreT (Tuple f s)) = s
  peek s (StoreT (Tuple f _)) = extract f s

experiment :: forall f a w s. (ComonadStore s w, Functor f) => (s -> f s) ->  w a -> f a
experiment f x = flip peek x <$> f (pos x)

peeks :: forall s a w. (ComonadStore s w) => (s -> s) -> w a -> a
peeks f x = peek (f $ pos x) x

seek :: forall s a w. (ComonadStore s w, Extend w) => s -> w a -> w a
seek s x = peek s $ duplicate x

seeks :: forall s a w. (ComonadStore s w, Extend w) => (s -> s) -> w a -> w a
seeks f x = peeks f $ duplicate x
