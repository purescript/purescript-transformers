module Control.Monad.RWS where

import Control.Monad.RWS.Trans
import Data.Identity
import Data.Monoid
import Data.Tuple

type RWS r w s = RWST r w s Identity

rws :: forall r w s a. (r -> s -> See s a w) -> RWS r w s a
rws f = RWST \r s -> return $ f r s

runRWS :: forall r w s a. RWS r w s a -> r -> s -> See s a w
runRWS m r s = runIdentity $ runRWST m r s

evalRWS :: forall r w s a. RWS r w s a -> r -> s -> Tuple a w
evalRWS m r s = runIdentity $ evalRWST m r s

execRWS :: forall r w s a. RWS r w s a -> r -> s -> Tuple s w
execRWS m r s = runIdentity $ execRWST m r s

mapRWS :: forall r w1 w2 s a1 a2. (See s a1 w1 -> See s a2 w2) -> RWS r w1 s a1 -> RWS r w2 s a2
mapRWS f = mapRWST (runIdentity >>> f >>> Identity)

withRWS :: forall r1 r2 w s a. (r2 -> s -> Tuple r1 s) -> RWS r1 w s a -> RWS r2 w s a
withRWS = withRWST

-- | Reader operations

ask :: forall r w s m. (Applicative m, Monoid w) => RWST r w s m r
ask = RWST \r s -> pure $ mkSee s r mempty

local :: forall  r w s m a. (r -> r) -> RWST r w s m a -> RWST r w s m a
local f m = RWST \r s -> runRWST m (f r) s

reader :: forall r w s m a. (Applicative m, Monoid w) => (r -> a) -> RWST r w s m a
reader f = RWST \r s -> pure $ mkSee s (f r) mempty

-- | Writer operations

writer :: forall r w s m a. (Applicative m) => Tuple a w -> RWST r w s m a
writer (Tuple a w) = RWST \_ s -> pure $ {state: s, result: a, log: w}

listen :: forall r w s m a. (Monad m) => RWST r w s m a -> RWST r w s m (Tuple a w)
listen m = RWST \r s -> runRWST m r s >>= \{state = s', result = a, log = w} -> pure $ {state: s', result: Tuple a w, log: w}

pass :: forall r w s m a. (Monad m) => RWST r w s m (Tuple a (w -> w)) -> RWST r w s m a
pass m = RWST \r s -> runRWST m r s >>= \{result = Tuple a f, state = s', log = w} -> pure $ {state: s', result: a, log: f w}

tell :: forall r w s m. (Applicative m) => w -> RWST r w s m Unit
tell w = writer (Tuple unit w)

listens :: forall r w s m a b. (Monad m) => (w -> b) -> RWST r w s m a -> RWST r w s m (Tuple a b)
listens f m = RWST \r s -> runRWST m r s >>= \{state = s', result = a, log = w} -> pure $ {state: s', result: Tuple a (f w), log: w}

censor :: forall r w s m a. (Monad m) => (w -> w) -> RWST r w s m a -> RWST r w s m a
censor f m = RWST \r s -> runRWST m r s >>= \see -> pure $ see{log = f see.log}

-- | State operations

state :: forall r w s m a. (Applicative m, Monoid w) => (s -> Tuple a s) -> RWST r w s m a
state f = RWST \_ s -> case f s of Tuple a s' -> pure $ mkSee s' a mempty

get :: forall r w s m. (Applicative m, Monoid w) => RWST r w s m s
get = state \s -> Tuple s s

gets :: forall r w s m a. (Applicative m, Monoid w) => (s -> a) -> RWST r w s m a
gets f = state \s -> Tuple (f s) s

put :: forall r w s m. (Applicative m, Monoid w) => s -> RWST r w s m Unit
put s = state \_ -> Tuple unit s

modify :: forall r w s m. (Applicative m, Monoid w) => (s -> s) -> RWST r w s m Unit
modify f = state \s -> Tuple unit (f s)
