# Module Documentation

## Module Control.Comonad.Env

#### `Env`

``` purescript
type Env e = EnvT e Identity
```


#### `runEnv`

``` purescript
runEnv :: forall e a. Env e a -> Tuple e a
```


#### `withEnv`

``` purescript
withEnv :: forall e1 e2 a. (e1 -> e2) -> Env e1 a -> Env e2 a
```


#### `mapEnv`

``` purescript
mapEnv :: forall e a b. (a -> b) -> Env e a -> Env e b
```


#### `env`

``` purescript
env :: forall e a. e -> a -> Env e a
```



## Module Control.Comonad.Env.Class

#### `ComonadEnv`

``` purescript
class (Comonad w) <= ComonadEnv e w where
  ask :: forall a. w a -> e
  local :: forall a. (e -> e) -> w a -> w a
```


#### `comonadEnvTuple`

``` purescript
instance comonadEnvTuple :: ComonadEnv e (Tuple e)
```


#### `comonadEnvEnvT`

``` purescript
instance comonadEnvEnvT :: (Comonad w) => ComonadEnv e (EnvT e w)
```


#### `asks`

``` purescript
asks :: forall e1 e2 w a. (ComonadEnv e1 w) => (e1 -> e2) -> w e1 -> e2
```



## Module Control.Comonad.Env.Trans

#### `EnvT`

``` purescript
newtype EnvT e w a
  = EnvT (Tuple e (w a))
```


#### `runEnvT`

``` purescript
runEnvT :: forall e w a. EnvT e w a -> Tuple e (w a)
```


#### `withEnvT`

``` purescript
withEnvT :: forall e1 e2 w a. (e1 -> e2) -> EnvT e1 w a -> EnvT e2 w a
```


#### `mapEnvT`

``` purescript
mapEnvT :: forall e w1 w2 a b. (w1 a -> w2 b) -> EnvT e w1 a -> EnvT e w2 b
```


#### `functorEnvT`

``` purescript
instance functorEnvT :: (Functor w) => Functor (EnvT e w)
```


#### `extendEnvT`

``` purescript
instance extendEnvT :: (Extend w) => Extend (EnvT e w)
```


#### `comonadEnvT`

``` purescript
instance comonadEnvT :: (Comonad w) => Comonad (EnvT e w)
```


#### `comonadTransEnvT`

``` purescript
instance comonadTransEnvT :: ComonadTrans (EnvT e)
```



## Module Control.Comonad.Store

#### `Store`

``` purescript
type Store s a = StoreT s Identity a
```


#### `runStore`

``` purescript
runStore :: forall s a. Store s a -> Tuple (s -> a) s
```


#### `store`

``` purescript
store :: forall s a. (s -> a) -> s -> Store s a
```



## Module Control.Comonad.Store.Class

#### `ComonadStore`

``` purescript
class (Comonad w) <= ComonadStore s w where
  pos :: forall a. w a -> s
  peek :: forall a. s -> w a -> a
```


#### `comonadStoreStoreT`

``` purescript
instance comonadStoreStoreT :: (Comonad w) => ComonadStore s (StoreT s w)
```


#### `experiment`

``` purescript
experiment :: forall f a w s. (ComonadStore s w, Functor f) => (s -> f s) -> w a -> f a
```


#### `peeks`

``` purescript
peeks :: forall s a w. (ComonadStore s w) => (s -> s) -> w a -> a
```


#### `seek`

``` purescript
seek :: forall s a w. (ComonadStore s w, Extend w) => s -> w a -> w a
```


#### `seeks`

``` purescript
seeks :: forall s a w. (ComonadStore s w, Extend w) => (s -> s) -> w a -> w a
```



## Module Control.Comonad.Store.Trans

#### `StoreT`

``` purescript
newtype StoreT s w a
  = StoreT (Tuple (w (s -> a)) s)
```


#### `runStoreT`

``` purescript
runStoreT :: forall s w a. StoreT s w a -> Tuple (w (s -> a)) s
```


#### `functorStoreT`

``` purescript
instance functorStoreT :: (Functor w) => Functor (StoreT s w)
```


#### `extendStoreT`

``` purescript
instance extendStoreT :: (Extend w) => Extend (StoreT s w)
```


#### `comonadStoreT`

``` purescript
instance comonadStoreT :: (Comonad w) => Comonad (StoreT s w)
```


#### `comonadTransStoreT`

``` purescript
instance comonadTransStoreT :: ComonadTrans (StoreT s)
```



## Module Control.Comonad.Traced

#### `Traced`

``` purescript
type Traced m = TracedT m Identity
```


#### `runTraced`

``` purescript
runTraced :: forall m a. Traced m a -> m -> a
```


#### `traced`

``` purescript
traced :: forall m a. (m -> a) -> Traced m a
```



## Module Control.Comonad.Traced.Class

#### `ComonadTraced`

``` purescript
class (Comonad w) <= ComonadTraced t w where
  track :: forall a. t -> w a -> a
```


#### `comonadTracedTracedT`

``` purescript
instance comonadTracedTracedT :: (Comonad w, Monoid t) => ComonadTraced t (TracedT t w)
```


#### `tracks`

``` purescript
tracks :: forall w a t. (Comonad w, ComonadTraced t w) => (a -> t) -> w a -> a
```


#### `listen`

``` purescript
listen :: forall w a t. (Functor w) => TracedT t w a -> TracedT t w (Tuple a t)
```


#### `listens`

``` purescript
listens :: forall w a t b. (Functor w) => (t -> b) -> TracedT t w a -> TracedT t w (Tuple a b)
```


#### `censor`

``` purescript
censor :: forall w a t b. (Functor w) => (t -> t) -> TracedT t w a -> TracedT t w a
```



## Module Control.Comonad.Traced.Trans

#### `TracedT`

``` purescript
newtype TracedT t w a
  = TracedT (w (t -> a))
```


#### `runTracedT`

``` purescript
runTracedT :: forall w a t. TracedT t w a -> w (t -> a)
```


#### `functorTracedT`

``` purescript
instance functorTracedT :: (Functor w) => Functor (TracedT t w)
```


#### `extendTracedT`

``` purescript
instance extendTracedT :: (Extend w, Semigroup t) => Extend (TracedT t w)
```


#### `comonadTracedT`

``` purescript
instance comonadTracedT :: (Comonad w, Monoid t) => Comonad (TracedT t w)
```


#### `comonadTransTracedT`

``` purescript
instance comonadTransTracedT :: (Monoid t) => ComonadTrans (TracedT t)
```



## Module Control.Comonad.Trans

#### `ComonadTrans`

``` purescript
class ComonadTrans f where
  lower :: forall w a. (Comonad w) => f w a -> w a
```



## Module Control.Monad.Cont.Class

#### `MonadCont`

``` purescript
class MonadCont m where
  callCC :: forall a b. ((a -> m b) -> m a) -> m a
```


#### `monadContContT`

``` purescript
instance monadContContT :: (Monad m) => MonadCont (Cont.ContT r m)
```


#### `monadContErrorT`

``` purescript
instance monadContErrorT :: (MonadCont m) => MonadCont (ErrorT e m)
```


#### `monadContMaybeT`

``` purescript
instance monadContMaybeT :: (MonadCont m) => MonadCont (MaybeT m)
```


#### `monadContReaderT`

``` purescript
instance monadContReaderT :: (MonadCont m) => MonadCont (ReaderT r m)
```


#### `monadContStateT`

``` purescript
instance monadContStateT :: (MonadCont m) => MonadCont (StateT s m)
```


#### `monadWriterT`

``` purescript
instance monadWriterT :: (Monoid w, MonadCont m) => MonadCont (WriterT w m)
```



## Module Control.Monad.Cont.Trans

#### `ContT`

``` purescript
newtype ContT r m a
  = ContT ((a -> m r) -> m r)
```


#### `runContT`

``` purescript
runContT :: forall r m a. ContT r m a -> (a -> m r) -> m r
```


#### `mapContT`

``` purescript
mapContT :: forall r m a. (m r -> m r) -> ContT r m a -> ContT r m a
```


#### `withContT`

``` purescript
withContT :: forall r m a b. ((b -> m r) -> a -> m r) -> ContT r m a -> ContT r m b
```


#### `callCC`

``` purescript
callCC :: forall r m a b. ((a -> ContT r m b) -> ContT r m a) -> ContT r m a
```


#### `functorContT`

``` purescript
instance functorContT :: (Monad m) => Functor (ContT r m)
```


#### `applyContT`

``` purescript
instance applyContT :: (Functor m, Monad m) => Apply (ContT r m)
```


#### `applicativeContT`

``` purescript
instance applicativeContT :: (Functor m, Monad m) => Applicative (ContT r m)
```


#### `bindContT`

``` purescript
instance bindContT :: (Monad m) => Bind (ContT r m)
```


#### `monadContT`

``` purescript
instance monadContT :: (Monad m) => Monad (ContT r m)
```


#### `monadTransContT`

``` purescript
instance monadTransContT :: MonadTrans (ContT r)
```



## Module Control.Monad.Error

#### `Error`

``` purescript
class Error a where
  noMsg :: a
  strMsg :: String -> a
```


#### `errorString`

``` purescript
instance errorString :: Error String
```



## Module Control.Monad.Error.Class

#### `MonadError`

``` purescript
class MonadError e m where
  throwError :: forall a. e -> m a
  catchError :: forall a. m a -> (e -> m a) -> m a
```


#### `monadErrorError`

``` purescript
instance monadErrorError :: MonadError e (Either e)
```


#### `monadErrorErrorT`

``` purescript
instance monadErrorErrorT :: (Monad m) => MonadError e (ErrorT e m)
```


#### `monadErrorMaybeT`

``` purescript
instance monadErrorMaybeT :: (Monad m, MonadError e m) => MonadError e (MaybeT m)
```


#### `monadErrorReaderT`

``` purescript
instance monadErrorReaderT :: (Monad m, MonadError e m) => MonadError e (ReaderT r m)
```


#### `monadErrorWriterT`

``` purescript
instance monadErrorWriterT :: (Monad m, Monoid w, MonadError e m) => MonadError e (WriterT w m)
```


#### `monadErrorStateT`

``` purescript
instance monadErrorStateT :: (Monad m, MonadError e m) => MonadError e (StateT s m)
```



## Module Control.Monad.Error.Trans

#### `ErrorT`

``` purescript
newtype ErrorT e m a
  = ErrorT (m (Either e a))
```


#### `runErrorT`

``` purescript
runErrorT :: forall e m a. ErrorT e m a -> m (Either e a)
```


#### `mapErrorT`

``` purescript
mapErrorT :: forall e1 e2 m1 m2 a b. (m1 (Either e1 a) -> m2 (Either e2 b)) -> ErrorT e1 m1 a -> ErrorT e2 m2 b
```


#### `functorErrorT`

``` purescript
instance functorErrorT :: (Functor m) => Functor (ErrorT e m)
```


#### `applyErrorT`

``` purescript
instance applyErrorT :: (Apply m) => Apply (ErrorT e m)
```


#### `applicativeErrorT`

``` purescript
instance applicativeErrorT :: (Applicative m) => Applicative (ErrorT e m)
```


#### `altErrorT`

``` purescript
instance altErrorT :: (Monad m) => Alt (ErrorT e m)
```


#### `plusErrorT`

``` purescript
instance plusErrorT :: (Monad m, Error e) => Plus (ErrorT e m)
```


#### `alternativeErrorT`

``` purescript
instance alternativeErrorT :: (Monad m, Error e) => Alternative (ErrorT e m)
```


#### `bindErrorT`

``` purescript
instance bindErrorT :: (Monad m) => Bind (ErrorT e m)
```


#### `monadErrorT`

``` purescript
instance monadErrorT :: (Monad m) => Monad (ErrorT e m)
```


#### `monadPlusErrorT`

``` purescript
instance monadPlusErrorT :: (Monad m, Error e) => MonadPlus (ErrorT e m)
```


#### `monadTransErrorT`

``` purescript
instance monadTransErrorT :: MonadTrans (ErrorT e)
```


#### `liftListenError`

``` purescript
liftListenError :: forall e m a w. (Monad m) => (m (Either e a) -> m (Tuple (Either e a) w)) -> ErrorT e m a -> ErrorT e m (Tuple a w)
```


#### `liftPassError`

``` purescript
liftPassError :: forall e m a w. (Monad m) => (m (Tuple (Either e a) (w -> w)) -> m (Either e a)) -> ErrorT e m (Tuple a (w -> w)) -> ErrorT e m a
```


#### `liftCallCCError`

``` purescript
liftCallCCError :: forall e m a b. (((Either e a -> m (Either e b)) -> m (Either e a)) -> m (Either e a)) -> ((a -> ErrorT e m b) -> ErrorT e m a) -> ErrorT e m a
```



## Module Control.Monad.Maybe.Trans

#### `MaybeT`

``` purescript
newtype MaybeT m a
  = MaybeT (m (Maybe a))
```


#### `functorMaybeT`

``` purescript
instance functorMaybeT :: (Monad m) => Functor (MaybeT m)
```


#### `applyMaybeT`

``` purescript
instance applyMaybeT :: (Monad m) => Apply (MaybeT m)
```


#### `applicativeMaybeT`

``` purescript
instance applicativeMaybeT :: (Monad m) => Applicative (MaybeT m)
```


#### `bindMaybeT`

``` purescript
instance bindMaybeT :: (Monad m) => Bind (MaybeT m)
```


#### `monadMaybeT`

``` purescript
instance monadMaybeT :: (Monad m) => Monad (MaybeT m)
```


#### `monadTransMaybeT`

``` purescript
instance monadTransMaybeT :: MonadTrans MaybeT
```


#### `runMaybeT`

``` purescript
runMaybeT :: forall m a. MaybeT m a -> m (Maybe a)
```


#### `mapMaybeT`

``` purescript
mapMaybeT :: forall m1 m2 a b. (m1 (Maybe a) -> m2 (Maybe b)) -> MaybeT m1 a -> MaybeT m2 b
```


#### `liftCatchMaybe`

``` purescript
liftCatchMaybe :: forall m e a. (m (Maybe a) -> (e -> m (Maybe a)) -> m (Maybe a)) -> MaybeT m a -> (e -> MaybeT m a) -> MaybeT m a
```


#### `liftListenMaybe`

``` purescript
liftListenMaybe :: forall m a w. (Monad m) => (m (Maybe a) -> m (Tuple (Maybe a) w)) -> MaybeT m a -> MaybeT m (Tuple a w)
```


#### `liftPassMaybe`

``` purescript
liftPassMaybe :: forall m a w. (Monad m) => (m (Tuple (Maybe a) (w -> w)) -> m (Maybe a)) -> MaybeT m (Tuple a (w -> w)) -> MaybeT m a
```


#### `liftCallCCMaybe`

``` purescript
liftCallCCMaybe :: forall m a b. (((Maybe a -> m (Maybe b)) -> m (Maybe a)) -> m (Maybe a)) -> ((a -> MaybeT m b) -> MaybeT m a) -> MaybeT m a
```



## Module Control.Monad.RWS

#### `RWS`

``` purescript
type RWS r w s = RWST r w s Identity
```


#### `rws`

``` purescript
rws :: forall r w s a. (r -> s -> See s a w) -> RWS r w s a
```


#### `runRWS`

``` purescript
runRWS :: forall r w s a. RWS r w s a -> r -> s -> See s a w
```


#### `evalRWS`

``` purescript
evalRWS :: forall r w s a. RWS r w s a -> r -> s -> Tuple a w
```


#### `execRWS`

``` purescript
execRWS :: forall r w s a. RWS r w s a -> r -> s -> Tuple s w
```


#### `mapRWS`

``` purescript
mapRWS :: forall r w1 w2 s a1 a2. (See s a1 w1 -> See s a2 w2) -> RWS r w1 s a1 -> RWS r w2 s a2
```


#### `withRWS`

``` purescript
withRWS :: forall r1 r2 w s a. (r2 -> s -> Tuple r1 s) -> RWS r1 w s a -> RWS r2 w s a
```


#### `ask`

``` purescript
ask :: forall r w s m. (Applicative m, Monoid w) => RWST r w s m r
```

Reader operations

#### `local`

``` purescript
local :: forall r w s m a. (r -> r) -> RWST r w s m a -> RWST r w s m a
```


#### `reader`

``` purescript
reader :: forall r w s m a. (Applicative m, Monoid w) => (r -> a) -> RWST r w s m a
```


#### `writer`

``` purescript
writer :: forall r w s m a. (Applicative m) => Tuple a w -> RWST r w s m a
```

Writer operations

#### `listen`

``` purescript
listen :: forall r w s m a. (Monad m) => RWST r w s m a -> RWST r w s m (Tuple a w)
```


#### `pass`

``` purescript
pass :: forall r w s m a. (Monad m) => RWST r w s m (Tuple a (w -> w)) -> RWST r w s m a
```


#### `tell`

``` purescript
tell :: forall r w s m. (Applicative m) => w -> RWST r w s m Unit
```


#### `listens`

``` purescript
listens :: forall r w s m a b. (Monad m) => (w -> b) -> RWST r w s m a -> RWST r w s m (Tuple a b)
```


#### `censor`

``` purescript
censor :: forall r w s m a. (Monad m) => (w -> w) -> RWST r w s m a -> RWST r w s m a
```


#### `state`

``` purescript
state :: forall r w s m a. (Applicative m, Monoid w) => (s -> Tuple a s) -> RWST r w s m a
```

State operations

#### `get`

``` purescript
get :: forall r w s m. (Applicative m, Monoid w) => RWST r w s m s
```


#### `gets`

``` purescript
gets :: forall r w s m a. (Applicative m, Monoid w) => (s -> a) -> RWST r w s m a
```


#### `put`

``` purescript
put :: forall r w s m. (Applicative m, Monoid w) => s -> RWST r w s m Unit
```


#### `modify`

``` purescript
modify :: forall r w s m. (Applicative m, Monoid w) => (s -> s) -> RWST r w s m Unit
```



## Module Control.Monad.RWS.Class

#### `MonadRWS`

``` purescript
class (Monad m, Monoid w, MonadReader r m, MonadWriter w m, MonadState s m) <= MonadRWS r w s m where
```


#### `monadRWSRWST`

``` purescript
instance monadRWSRWST :: (Monad m, Monoid w) => MonadRWS r w s (RWST r w s m)
```


#### `monadRWSErrorT`

``` purescript
instance monadRWSErrorT :: (Monad m, Monoid w, MonadRWS r w s m, MonadReader r m, MonadWriter w m, MonadState s m) => MonadRWS r w s (ErrorT e m)
```


#### `monadRWSMaybeT`

``` purescript
instance monadRWSMaybeT :: (Monad m, Monoid w, MonadRWS r w s m, MonadReader r m, MonadWriter w m, MonadState s m) => MonadRWS r w s (MaybeT m)
```



## Module Control.Monad.RWS.Trans

#### `See`

``` purescript
type See s a w = { log :: w, result :: a, state :: s }
```


#### `mkSee`

``` purescript
mkSee :: forall s a w. (Monoid w) => s -> a -> w -> See s a w
```


#### `RWST`

``` purescript
newtype RWST r w s m a
  = RWST (r -> s -> m (See s a w))
```


#### `runRWST`

``` purescript
runRWST :: forall r w s m a. RWST r w s m a -> r -> s -> m (See s a w)
```


#### `evalRWST`

``` purescript
evalRWST :: forall r w s m a. (Monad m) => RWST r w s m a -> r -> s -> m (Tuple a w)
```


#### `execRWST`

``` purescript
execRWST :: forall r w s m a. (Monad m) => RWST r w s m a -> r -> s -> m (Tuple s w)
```


#### `mapRWST`

``` purescript
mapRWST :: forall r w1 w2 s m1 m2 a1 a2. (m1 (See s a1 w1) -> m2 (See s a2 w2)) -> RWST r w1 s m1 a1 -> RWST r w2 s m2 a2
```


#### `withRWST`

``` purescript
withRWST :: forall r1 r2 w s m a. (r2 -> s -> Tuple r1 s) -> RWST r1 w s m a -> RWST r2 w s m a
```


#### `functorRWST`

``` purescript
instance functorRWST :: (Functor m) => Functor (RWST r w s m)
```


#### `applyRWST`

``` purescript
instance applyRWST :: (Bind m, Monoid w) => Apply (RWST r w s m)
```


#### `bindRWST`

``` purescript
instance bindRWST :: (Bind m, Monoid w) => Bind (RWST r w s m)
```


#### `applicativeRWST`

``` purescript
instance applicativeRWST :: (Monad m, Monoid w) => Applicative (RWST r w s m)
```


#### `monadRWST`

``` purescript
instance monadRWST :: (Monad m, Monoid w) => Monad (RWST r w s m)
```


#### `monadTransRWST`

``` purescript
instance monadTransRWST :: (Monoid w) => MonadTrans (RWST r w s)
```



## Module Control.Monad.Reader

#### `Reader`

``` purescript
type Reader r = ReaderT r Identity
```


#### `runReader`

``` purescript
runReader :: forall r a. Reader r a -> r -> a
```


#### `withReader`

``` purescript
withReader :: forall r1 r2 a b. (r2 -> r1) -> Reader r1 a -> Reader r2 a
```


#### `mapReader`

``` purescript
mapReader :: forall r a b. (a -> b) -> Reader r a -> Reader r b
```



## Module Control.Monad.Reader.Class

#### `MonadReader`

``` purescript
class MonadReader r m where
  ask :: m r
  local :: forall a. (r -> r) -> m a -> m a
```


#### `reader`

``` purescript
reader :: forall r m a. (Monad m, MonadReader r m) => (r -> a) -> m a
```


#### `monadReaderFun`

``` purescript
instance monadReaderFun :: MonadReader r (Prim.Function r)
```


#### `monadReaderReaderT`

``` purescript
instance monadReaderReaderT :: (Monad m) => MonadReader r (ReaderT r m)
```


#### `monadReaderErrorT`

``` purescript
instance monadReaderErrorT :: (Monad m, MonadReader r m) => MonadReader r (ErrorT e m)
```


#### `monadReaderMaybeT`

``` purescript
instance monadReaderMaybeT :: (Monad m, MonadReader r m) => MonadReader r (MaybeT m)
```


#### `monadReaderWriterT`

``` purescript
instance monadReaderWriterT :: (Monad m, Monoid w, MonadReader r m) => MonadReader r (WriterT w m)
```


#### `monadReaderStateT`

``` purescript
instance monadReaderStateT :: (Monad m, MonadReader r m) => MonadReader r (StateT s m)
```


#### `monadReaderRWST`

``` purescript
instance monadReaderRWST :: (Monad m, Monoid w) => MonadReader r (RWST r w s m)
```



## Module Control.Monad.Reader.Trans

#### `ReaderT`

``` purescript
newtype ReaderT r m a
  = ReaderT (r -> m a)
```


#### `runReaderT`

``` purescript
runReaderT :: forall r m a. ReaderT r m a -> r -> m a
```


#### `withReaderT`

``` purescript
withReaderT :: forall r1 r2 m a b. (r2 -> r1) -> ReaderT r1 m a -> ReaderT r2 m a
```


#### `mapReaderT`

``` purescript
mapReaderT :: forall r m1 m2 a b. (m1 a -> m2 b) -> ReaderT r m1 a -> ReaderT r m2 b
```


#### `liftReaderT`

``` purescript
liftReaderT :: forall r m a. m a -> ReaderT r m a
```


#### `functorReaderT`

``` purescript
instance functorReaderT :: (Functor m) => Functor (ReaderT r m)
```


#### `applyReaderT`

``` purescript
instance applyReaderT :: (Applicative m) => Apply (ReaderT r m)
```


#### `applicativeReaderT`

``` purescript
instance applicativeReaderT :: (Applicative m) => Applicative (ReaderT r m)
```


#### `altReaderT`

``` purescript
instance altReaderT :: (Alt m) => Alt (ReaderT r m)
```


#### `plusReaderT`

``` purescript
instance plusReaderT :: (Plus m) => Plus (ReaderT r m)
```


#### `alternativeReaderT`

``` purescript
instance alternativeReaderT :: (Alternative m) => Alternative (ReaderT r m)
```


#### `bindReaderT`

``` purescript
instance bindReaderT :: (Monad m) => Bind (ReaderT r m)
```


#### `monadReaderT`

``` purescript
instance monadReaderT :: (Monad m) => Monad (ReaderT r m)
```


#### `monadPlusReaderT`

``` purescript
instance monadPlusReaderT :: (MonadPlus m) => MonadPlus (ReaderT r m)
```


#### `monadTransReaderT`

``` purescript
instance monadTransReaderT :: MonadTrans (ReaderT r)
```


#### `liftCatchReader`

``` purescript
liftCatchReader :: forall r m e a. (m a -> (e -> m a) -> m a) -> ReaderT r m a -> (e -> ReaderT r m a) -> ReaderT r m a
```


#### `liftCallCCReader`

``` purescript
liftCallCCReader :: forall r m a b. (((a -> m b) -> m a) -> m a) -> ((a -> ReaderT r m b) -> ReaderT r m a) -> ReaderT r m a
```



## Module Control.Monad.State

#### `State`

``` purescript
type State s = StateT s Identity
```


#### `runState`

``` purescript
runState :: forall s a. State s a -> s -> Tuple a s
```


#### `evalState`

``` purescript
evalState :: forall s a. State s a -> s -> a
```


#### `execState`

``` purescript
execState :: forall s a. State s a -> s -> s
```


#### `mapState`

``` purescript
mapState :: forall s a b. (Tuple a s -> Tuple b s) -> State s a -> State s b
```


#### `withState`

``` purescript
withState :: forall s a. (s -> s) -> State s a -> State s a
```



## Module Control.Monad.State.Class

#### `MonadState`

``` purescript
class MonadState s m where
  state :: forall a. (s -> Tuple a s) -> m a
```


#### `get`

``` purescript
get :: forall m s. (Monad m, MonadState s m) => m s
```


#### `gets`

``` purescript
gets :: forall s m a. (Monad m, MonadState s m) => (s -> a) -> m a
```


#### `put`

``` purescript
put :: forall m s. (Monad m, MonadState s m) => s -> m Unit
```


#### `modify`

``` purescript
modify :: forall s m. (Monad m, MonadState s m) => (s -> s) -> m Unit
```


#### `monadStateStateT`

``` purescript
instance monadStateStateT :: (Monad m) => MonadState s (StateT s m)
```


#### `monadStateStateT1`

``` purescript
instance monadStateStateT1 :: (Monad m, MonadState s m) => MonadState s (StateT s1 m)
```


#### `monadStateErrorT`

``` purescript
instance monadStateErrorT :: (Monad m, MonadState s m) => MonadState s (ErrorT e m)
```


#### `monadStateMaybeT`

``` purescript
instance monadStateMaybeT :: (Monad m, MonadState s m) => MonadState s (MaybeT m)
```


#### `monadStateReaderT`

``` purescript
instance monadStateReaderT :: (Monad m, MonadState s m) => MonadState s (ReaderT r m)
```


#### `monadStateWriterT`

``` purescript
instance monadStateWriterT :: (Monad m, Monoid w, MonadState s m) => MonadState s (WriterT w m)
```


#### `monadStateRWST`

``` purescript
instance monadStateRWST :: (Monad m, Monoid w) => MonadState s (RWST r w s m)
```



## Module Control.Monad.State.Trans

#### `StateT`

``` purescript
newtype StateT s m a
  = StateT (s -> m (Tuple a s))
```


#### `runStateT`

``` purescript
runStateT :: forall s m a. StateT s m a -> s -> m (Tuple a s)
```


#### `evalStateT`

``` purescript
evalStateT :: forall s m a. (Apply m) => StateT s m a -> s -> m a
```


#### `execStateT`

``` purescript
execStateT :: forall s m a. (Apply m) => StateT s m a -> s -> m s
```


#### `mapStateT`

``` purescript
mapStateT :: forall s m1 m2 a b. (m1 (Tuple a s) -> m2 (Tuple b s)) -> StateT s m1 a -> StateT s m2 b
```


#### `withStateT`

``` purescript
withStateT :: forall s m a. (s -> s) -> StateT s m a -> StateT s m a
```


#### `functorStateT`

``` purescript
instance functorStateT :: (Monad m) => Functor (StateT s m)
```


#### `applyStateT`

``` purescript
instance applyStateT :: (Monad m) => Apply (StateT s m)
```


#### `applicativeStateT`

``` purescript
instance applicativeStateT :: (Monad m) => Applicative (StateT s m)
```


#### `altStateT`

``` purescript
instance altStateT :: (Monad m, Alt m) => Alt (StateT s m)
```


#### `plusStateT`

``` purescript
instance plusStateT :: (Monad m, Plus m) => Plus (StateT s m)
```


#### `alternativeStateT`

``` purescript
instance alternativeStateT :: (Monad m, Alternative m) => Alternative (StateT s m)
```


#### `bindStateT`

``` purescript
instance bindStateT :: (Monad m) => Bind (StateT s m)
```


#### `monadStateT`

``` purescript
instance monadStateT :: (Monad m) => Monad (StateT s m)
```


#### `monadPlusStateT`

``` purescript
instance monadPlusStateT :: (MonadPlus m) => MonadPlus (StateT s m)
```


#### `monadTransStateT`

``` purescript
instance monadTransStateT :: MonadTrans (StateT s)
```


#### `lazy1StateT`

``` purescript
instance lazy1StateT :: Lazy1 (StateT s m)
```


#### `liftCatchState`

``` purescript
liftCatchState :: forall s m e a. (m (Tuple a s) -> (e -> m (Tuple a s)) -> m (Tuple a s)) -> StateT s m a -> (e -> StateT s m a) -> StateT s m a
```


#### `liftListenState`

``` purescript
liftListenState :: forall s m a w. (Monad m) => (m (Tuple a s) -> m (Tuple (Tuple a s) w)) -> StateT s m a -> StateT s m (Tuple a w)
```


#### `liftPassState`

``` purescript
liftPassState :: forall s m a b w. (Monad m) => (m (Tuple (Tuple a s) b) -> m (Tuple a s)) -> StateT s m (Tuple a b) -> StateT s m a
```


#### `liftCallCCState`

``` purescript
liftCallCCState :: forall s m a b. (((Tuple a s -> m (Tuple b s)) -> m (Tuple a s)) -> m (Tuple a s)) -> ((a -> StateT s m b) -> StateT s m a) -> StateT s m a
```


#### `liftCallCCState'`

``` purescript
liftCallCCState' :: forall s m a b. (((Tuple a s -> m (Tuple b s)) -> m (Tuple a s)) -> m (Tuple a s)) -> ((a -> StateT s m b) -> StateT s m a) -> StateT s m a
```



## Module Control.Monad.Trans

#### `MonadTrans`

``` purescript
class MonadTrans t where
  lift :: forall m a. (Monad m) => m a -> t m a
```



## Module Control.Monad.Writer

#### `Writer`

``` purescript
type Writer w = WriterT w Identity
```


#### `runWriter`

``` purescript
runWriter :: forall w a. Writer w a -> Tuple a w
```


#### `execWriter`

``` purescript
execWriter :: forall w a. Writer w a -> w
```


#### `mapWriter`

``` purescript
mapWriter :: forall w1 w2 a b. (Tuple a w1 -> Tuple b w2) -> Writer w1 a -> Writer w2 b
```



## Module Control.Monad.Writer.Class

#### `MonadWriter`

``` purescript
class MonadWriter w m where
  writer :: forall a. Tuple a w -> m a
  listen :: forall a. m a -> m (Tuple a w)
  pass :: forall a. m (Tuple a (w -> w)) -> m a
```


#### `tell`

``` purescript
tell :: forall w m a. (Monoid w, Monad m, MonadWriter w m) => w -> m Unit
```


#### `listens`

``` purescript
listens :: forall w m a b. (Monoid w, Monad m, MonadWriter w m) => (w -> b) -> m a -> m (Tuple a b)
```


#### `censor`

``` purescript
censor :: forall w m a. (Monoid w, Monad m, MonadWriter w m) => (w -> w) -> m a -> m a
```


#### `monadWriterWriterT`

``` purescript
instance monadWriterWriterT :: (Monoid w, Monad m) => MonadWriter w (WriterT w m)
```


#### `monadWriterErrorT`

``` purescript
instance monadWriterErrorT :: (Monad m, MonadWriter w m) => MonadWriter w (ErrorT e m)
```


#### `monadWriterMaybeT`

``` purescript
instance monadWriterMaybeT :: (Monad m, MonadWriter w m) => MonadWriter w (MaybeT m)
```


#### `monadWriterStateT`

``` purescript
instance monadWriterStateT :: (Monad m, MonadWriter w m) => MonadWriter w (StateT s m)
```


#### `monadWriterReaderT`

``` purescript
instance monadWriterReaderT :: (Monad m, MonadWriter w m) => MonadWriter w (ReaderT r m)
```


#### `monadWriterRWST`

``` purescript
instance monadWriterRWST :: (Monad m, Monoid w) => MonadWriter w (RWST r w s m)
```



## Module Control.Monad.Writer.Trans

#### `WriterT`

``` purescript
newtype WriterT w m a
  = WriterT (m (Tuple a w))
```


#### `runWriterT`

``` purescript
runWriterT :: forall w m a. WriterT w m a -> m (Tuple a w)
```


#### `execWriterT`

``` purescript
execWriterT :: forall w m a. (Apply m) => WriterT w m a -> m w
```


#### `mapWriterT`

``` purescript
mapWriterT :: forall w1 w2 m1 m2 a b. (m1 (Tuple a w1) -> m2 (Tuple b w2)) -> WriterT w1 m1 a -> WriterT w2 m2 b
```


#### `functorWriterT`

``` purescript
instance functorWriterT :: (Functor m) => Functor (WriterT w m)
```


#### `applyWriterT`

``` purescript
instance applyWriterT :: (Monoid w, Apply m) => Apply (WriterT w m)
```


#### `applicativeWriterT`

``` purescript
instance applicativeWriterT :: (Monoid w, Applicative m) => Applicative (WriterT w m)
```


#### `altWriterT`

``` purescript
instance altWriterT :: (Monoid w, Alt m) => Alt (WriterT w m)
```


#### `plusWriterT`

``` purescript
instance plusWriterT :: (Monoid w, Plus m) => Plus (WriterT w m)
```


#### `alternativeWriterT`

``` purescript
instance alternativeWriterT :: (Monoid w, Alternative m) => Alternative (WriterT w m)
```


#### `bindWriterT`

``` purescript
instance bindWriterT :: (Monoid w, Monad m) => Bind (WriterT w m)
```


#### `monadWriterT`

``` purescript
instance monadWriterT :: (Monoid w, Monad m) => Monad (WriterT w m)
```


#### `monadPlusWriterT`

``` purescript
instance monadPlusWriterT :: (Monoid w, MonadPlus m) => MonadPlus (WriterT w m)
```


#### `monadTransWriterT`

``` purescript
instance monadTransWriterT :: (Monoid w) => MonadTrans (WriterT w)
```


#### `liftCatchWriter`

``` purescript
liftCatchWriter :: forall w m e a. (m (Tuple a w) -> (e -> m (Tuple a w)) -> m (Tuple a w)) -> WriterT w m a -> (e -> WriterT w m a) -> WriterT w m a
```


#### `liftCallCCWriter`

``` purescript
liftCallCCWriter :: forall w m a b. (Monoid w) => (((Tuple a w -> m (Tuple b w)) -> m (Tuple a w)) -> m (Tuple a w)) -> ((a -> WriterT w m b) -> WriterT w m a) -> WriterT w m a
```