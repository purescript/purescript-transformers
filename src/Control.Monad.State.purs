module Control.Monad.State where

import Prelude
import Control.Monad.Eff
import Control.Monad.Identity

type StateData s a = { state :: s, value :: a }

data StateT s m a = StateT (s -> m (StateData s a))

type State s a = StateT s Identity a

instance (Prelude.Monad m) => Prelude.Monad (StateT s m) where
  return a = StateT \s -> return { state: s, value: a }
  (>>=) (StateT x) f = StateT \s -> do
    { state = s', value = v } <- x s
    runStateT (f v) s'
    
runStateT :: forall s m a. StateT s m a -> s -> m (StateData s a)
runStateT (StateT s) = s

runState :: forall s a. State s a -> s -> StateData s a
runState s = runIdentity <<< runStateT s

withStateT :: forall s m a. (s -> s) -> StateT s m a -> StateT s m a
withStateT f s = StateT $ runStateT s <<< f

withState :: forall s a. (s -> s) -> State s a -> State s a
withState = withStateT
    
state :: forall s m a. (Prelude.Monad m) => (s -> StateData s a) -> StateT s m a
state f = StateT $ return <<< f

get :: forall s m. (Prelude.Monad m) => StateT s m s
get = state \s -> { state: s, value: s }

put :: forall s m. (Prelude.Monad m) => s -> StateT s m {}
put s = state \_ -> { state: s, value: {} }

modify :: forall s m. (Prelude.Monad m) => (s -> s) -> StateT s m {}
modify f = state \s -> { state: (f s), value: {} }

gets :: forall s m a. (Prelude.Monad m) => (s -> a) -> StateT s m a
gets f = state \s -> { state: s, value: f s }
