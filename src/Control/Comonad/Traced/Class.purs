module Control.Comonad.Traced.Class where

import Control.Comonad
import Control.Comonad.Traced.Trans

import Data.Monoid
import Data.Tuple

class (Comonad w) <= ComonadTraced t w where
  track :: forall a. t -> w a -> a

instance comonadTracedTracedT :: (Comonad w, Monoid t) => ComonadTraced t (TracedT t w) where
  track t tr = extract (runTracedT tr) t

tracks :: forall w a t. (Comonad w, ComonadTraced t w) => (a -> t) -> w a -> a
tracks f w = track (f $ extract w) w

listen :: forall w a t. (Functor w) => TracedT t w a -> TracedT t w (Tuple a t)
listen tr = TracedT ((\f t -> Tuple (f t) t) <$> runTracedT tr)

listens :: forall w a t b. (Functor w) => (t -> b) -> TracedT t w a -> TracedT t w (Tuple a b)
listens f tr = TracedT ((\g t -> Tuple (g t) (f t)) <$> runTracedT tr)

censor :: forall w a t b. (Functor w) => (t -> t) -> TracedT t w a -> TracedT t w a
censor f tr = TracedT ((>>>) f <$> runTracedT tr)
