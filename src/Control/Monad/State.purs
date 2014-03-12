module Control.Monad.State where

import Prelude
import Control.Monad.Identity
import Control.Monad.State.Trans
import Data.Tuple

type State s a = StateT s Identity a

runState :: forall s a. State s a -> s -> Tuple a s
runState s = runIdentity <<< runStateT s

withState :: forall s a. (s -> s) -> State s a -> State s a
withState = withStateT
