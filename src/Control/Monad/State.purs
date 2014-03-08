module Control.Monad.State where

import Prelude
import Control.Monad.Identity
import Control.Monad.State.Trans

type State s a = StateT s Identity a

runState :: forall s a. State s a -> s -> StateData s a
runState s = runIdentity <<< runStateT s

withState :: forall s a. (s -> s) -> State s a -> State s a
withState = withStateT
