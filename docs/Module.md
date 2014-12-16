# Module Documentation

## Module Control.Comonad.Env

### Types

    type Env e = EnvT e Identity


### Values

    env :: forall e a. e -> a -> Env e a

    mapEnv :: forall e a b. (a -> b) -> Env e a -> Env e b

    runEnv :: forall e a. Env e a -> Tuple e a

    withEnv :: forall e1 e2 a. (e1 -> e2) -> Env e1 a -> Env e2 a


## Module Control.Comonad.Env.Class

### Type Classes

    class (Comonad w) <= ComonadEnv e w where
      ask :: forall a. w a -> e
      local :: forall a. (e -> e) -> w a -> w a


### Type Class Instances

    instance comonadEnvEnvT :: (Comonad w) => ComonadEnv e (EnvT e w)

    instance comonadEnvTuple :: ComonadEnv e (Tuple e)


### Values

    asks :: forall e1 e2 w a. (ComonadEnv e1 w) => (e1 -> e2) -> w e1 -> e2


## Module Control.Comonad.Env.Trans

### Types

    newtype EnvT e w a where
      EnvT :: Tuple e (w a) -> EnvT e w a


### Type Class Instances

    instance comonadEnvT :: (Comonad w) => Comonad (EnvT e w)

    instance comonadTransEnvT :: ComonadTrans (EnvT e)

    instance extendEnvT :: (Extend w) => Extend (EnvT e w)

    instance functorEnvT :: (Functor w) => Functor (EnvT e w)


### Values

    mapEnvT :: forall e w1 w2 a b. (w1 a -> w2 b) -> EnvT e w1 a -> EnvT e w2 b

    runEnvT :: forall e w a. EnvT e w a -> Tuple e (w a)

    withEnvT :: forall e1 e2 w a. (e1 -> e2) -> EnvT e1 w a -> EnvT e2 w a


## Module Control.Comonad.Store

### Types

    type Store s a = StoreT s Identity a


### Values

    runStore :: forall s a. Store s a -> Tuple (s -> a) s

    store :: forall s a. (s -> a) -> s -> Store s a


## Module Control.Comonad.Store.Class

### Type Classes

    class (Comonad w) <= ComonadStore s w where
      pos :: forall a. w a -> s
      peek :: forall a. s -> w a -> a


### Type Class Instances

    instance comonadStoreStoreT :: (Comonad w) => ComonadStore s (StoreT s w)


### Values

    experiment :: forall f a w s. (ComonadStore s w, Functor f) => (s -> f s) -> w a -> f a

    peeks :: forall s a w. (ComonadStore s w) => (s -> s) -> w a -> a

    seek :: forall s a w. (ComonadStore s w, Extend w) => s -> w a -> w a

    seeks :: forall s a w. (ComonadStore s w, Extend w) => (s -> s) -> w a -> w a


## Module Control.Comonad.Store.Trans

### Types

    newtype StoreT s w a where
      StoreT :: Tuple (w (s -> a)) s -> StoreT s w a


### Type Class Instances

    instance comonadStoreT :: (Comonad w) => Comonad (StoreT s w)

    instance comonadTransStoreT :: ComonadTrans (StoreT s)

    instance extendStoreT :: (Extend w) => Extend (StoreT s w)

    instance functorStoreT :: (Functor w) => Functor (StoreT s w)


### Values

    runStoreT :: forall s w a. StoreT s w a -> Tuple (w (s -> a)) s


## Module Control.Comonad.Traced

### Types

    type Traced m = TracedT m Identity


### Values

    runTraced :: forall m a. Traced m a -> m -> a

    traced :: forall m a. (m -> a) -> Traced m a


## Module Control.Comonad.Traced.Class

### Type Classes

    class (Comonad w) <= ComonadTraced t w where
      track :: forall a. t -> w a -> a


### Type Class Instances

    instance comonadTracedTracedT :: (Comonad w, Monoid t) => ComonadTraced t (TracedT t w)


### Values

    censor :: forall w a t b. (Functor w) => (t -> t) -> TracedT t w a -> TracedT t w a

    listen :: forall w a t. (Functor w) => TracedT t w a -> TracedT t w (Tuple a t)

    listens :: forall w a t b. (Functor w) => (t -> b) -> TracedT t w a -> TracedT t w (Tuple a b)

    tracks :: forall w a t. (Comonad w, ComonadTraced t w) => (a -> t) -> w a -> a


## Module Control.Comonad.Traced.Trans

### Types

    newtype TracedT t w a where
      TracedT :: w (t -> a) -> TracedT t w a


### Type Class Instances

    instance comonadTracedT :: (Comonad w, Monoid t) => Comonad (TracedT t w)

    instance comonadTransTracedT :: (Monoid t) => ComonadTrans (TracedT t)

    instance extendTracedT :: (Extend w, Semigroup t) => Extend (TracedT t w)

    instance functorTracedT :: (Functor w) => Functor (TracedT t w)


### Values

    runTracedT :: forall w a t. TracedT t w a -> w (t -> a)


## Module Control.Comonad.Trans

### Type Classes

    class ComonadTrans f where
      lower :: forall w a. (Comonad w) => f w a -> w a


## Module Control.Monad.Cont.Class

### Type Classes

    class MonadCont m where
      callCC :: forall a b. ((a -> m b) -> m a) -> m a


### Type Class Instances

    instance monadContContT :: (Monad m) => MonadCont (Cont.ContT r m)

    instance monadContErrorT :: (Error e, MonadCont m) => MonadCont (ErrorT e m)

    instance monadContMaybeT :: (MonadCont m) => MonadCont (MaybeT m)

    instance monadContReaderT :: (MonadCont m) => MonadCont (ReaderT r m)

    instance monadContStateT :: (MonadCont m) => MonadCont (StateT s m)

    instance monadWriterT :: (Monoid w, MonadCont m) => MonadCont (WriterT w m)


## Module Control.Monad.Cont.Trans

### Types

    newtype ContT r m a where
      ContT :: ((a -> m r) -> m r) -> ContT r m a


### Type Class Instances

    instance applicativeContT :: (Functor m, Monad m) => Applicative (ContT r m)

    instance applyContT :: (Functor m, Monad m) => Apply (ContT r m)

    instance bindContT :: (Monad m) => Bind (ContT r m)

    instance functorContT :: (Monad m) => Functor (ContT r m)

    instance monadContT :: (Monad m) => Monad (ContT r m)

    instance monadTransContT :: MonadTrans (ContT r)


### Values

    callCC :: forall r m a b. ((a -> ContT r m b) -> ContT r m a) -> ContT r m a

    mapContT :: forall r m a. (m r -> m r) -> ContT r m a -> ContT r m a

    runContT :: forall r m a. ContT r m a -> (a -> m r) -> m r

    withContT :: forall r m a b. ((b -> m r) -> a -> m r) -> ContT r m a -> ContT r m b


## Module Control.Monad.Error

### Type Classes

    class Error a where
      noMsg :: a
      strMsg :: String -> a


### Type Class Instances

    instance errorString :: Error String


## Module Control.Monad.Error.Class

### Type Classes

    class MonadError e m where
      throwError :: forall a. e -> m a
      catchError :: forall a. m a -> (e -> m a) -> m a


### Type Class Instances

    instance monadErrorError :: (Error e) => MonadError e (Either e)

    instance monadErrorErrorT :: (Monad m, Error e) => MonadError e (ErrorT e m)

    instance monadErrorMaybeT :: (Monad m, MonadError e m) => MonadError e (MaybeT m)

    instance monadErrorReaderT :: (Monad m, MonadError e m) => MonadError e (ReaderT r m)

    instance monadErrorStateT :: (Monad m, MonadError e m) => MonadError e (StateT s m)

    instance monadErrorWriterT :: (Monad m, Monoid w, MonadError e m) => MonadError e (WriterT w m)


## Module Control.Monad.Error.Trans

### Types

    newtype ErrorT e m a where
      ErrorT :: m (Either e a) -> ErrorT e m a


### Type Class Instances

    instance altErrorT :: (Monad m, Error e) => Alt (ErrorT e m)

    instance alternativeErrorT :: (Monad m, Error e) => Alternative (ErrorT e m)

    instance applicativeErrorT :: (Applicative m) => Applicative (ErrorT e m)

    instance applyErrorT :: (Apply m) => Apply (ErrorT e m)

    instance bindErrorT :: (Monad m, Error e) => Bind (ErrorT e m)

    instance functorErrorT :: (Functor m) => Functor (ErrorT e m)

    instance monadErrorT :: (Monad m, Error e) => Monad (ErrorT e m)

    instance monadPlusErrorT :: (Monad m, Error e) => MonadPlus (ErrorT e m)

    instance monadTransErrorT :: (Error e) => MonadTrans (ErrorT e)

    instance plusErrorT :: (Monad m, Error e) => Plus (ErrorT e m)


### Values

    liftCallCCError :: forall e m a b. (((Either e a -> m (Either e b)) -> m (Either e a)) -> m (Either e a)) -> ((a -> ErrorT e m b) -> ErrorT e m a) -> ErrorT e m a

    liftListenError :: forall e m a w. (Monad m) => (m (Either e a) -> m (Tuple (Either e a) w)) -> ErrorT e m a -> ErrorT e m (Tuple a w)

    liftPassError :: forall e m a w. (Monad m) => (m (Tuple (Either e a) (w -> w)) -> m (Either e a)) -> ErrorT e m (Tuple a (w -> w)) -> ErrorT e m a

    mapErrorT :: forall e1 e2 m1 m2 a b. (m1 (Either e1 a) -> m2 (Either e2 b)) -> ErrorT e1 m1 a -> ErrorT e2 m2 b

    runErrorT :: forall e m a. ErrorT e m a -> m (Either e a)


## Module Control.Monad.Maybe.Trans

### Types

    newtype MaybeT m a where
      MaybeT :: m (Maybe a) -> MaybeT m a


### Type Class Instances

    instance applicativeMaybeT :: (Monad m) => Applicative (MaybeT m)

    instance applyMaybeT :: (Monad m) => Apply (MaybeT m)

    instance bindMaybeT :: (Monad m) => Bind (MaybeT m)

    instance functorMaybeT :: (Monad m) => Functor (MaybeT m)

    instance monadMaybeT :: (Monad m) => Monad (MaybeT m)

    instance monadTransMaybeT :: MonadTrans MaybeT


### Values

    liftCallCCMaybe :: forall m a b. (((Maybe a -> m (Maybe b)) -> m (Maybe a)) -> m (Maybe a)) -> ((a -> MaybeT m b) -> MaybeT m a) -> MaybeT m a

    liftCatchMaybe :: forall m e a. (m (Maybe a) -> (e -> m (Maybe a)) -> m (Maybe a)) -> MaybeT m a -> (e -> MaybeT m a) -> MaybeT m a

    liftListenMaybe :: forall m a w. (Monad m) => (m (Maybe a) -> m (Tuple (Maybe a) w)) -> MaybeT m a -> MaybeT m (Tuple a w)

    liftPassMaybe :: forall m a w. (Monad m) => (m (Tuple (Maybe a) (w -> w)) -> m (Maybe a)) -> MaybeT m (Tuple a (w -> w)) -> MaybeT m a

    mapMaybeT :: forall m1 m2 a b. (m1 (Maybe a) -> m2 (Maybe b)) -> MaybeT m1 a -> MaybeT m2 b

    runMaybeT :: forall m a. MaybeT m a -> m (Maybe a)


## Module Control.Monad.RWS

### Types

    type RWS r w s = RWST r w s Identity


### Values

    ask :: forall r w s m. (Applicative m, Monoid w) => RWST r w s m r

    censor :: forall r w s m a. (Monad m) => (w -> w) -> RWST r w s m a -> RWST r w s m a

    evalRWS :: forall r w s a. RWS r w s a -> r -> s -> Tuple a w

    execRWS :: forall r w s a. RWS r w s a -> r -> s -> Tuple s w

    get :: forall r w s m. (Applicative m, Monoid w) => RWST r w s m s

    gets :: forall r w s m a. (Applicative m, Monoid w) => (s -> a) -> RWST r w s m a

    listen :: forall r w s m a. (Monad m) => RWST r w s m a -> RWST r w s m (Tuple a w)

    listens :: forall r w s m a b. (Monad m) => (w -> b) -> RWST r w s m a -> RWST r w s m (Tuple a b)

    local :: forall r w s m a. (r -> r) -> RWST r w s m a -> RWST r w s m a

    mapRWS :: forall r w1 w2 s a1 a2. (See s a1 w1 -> See s a2 w2) -> RWS r w1 s a1 -> RWS r w2 s a2

    modify :: forall r w s m. (Applicative m, Monoid w) => (s -> s) -> RWST r w s m Unit

    pass :: forall r w s m a. (Monad m) => RWST r w s m (Tuple a (w -> w)) -> RWST r w s m a

    put :: forall r w s m. (Applicative m, Monoid w) => s -> RWST r w s m Unit

    reader :: forall r w s m a. (Applicative m, Monoid w) => (r -> a) -> RWST r w s m a

    runRWS :: forall r w s a. RWS r w s a -> r -> s -> See s a w

    rws :: forall r w s a. (r -> s -> See s a w) -> RWS r w s a

    state :: forall r w s m a. (Applicative m, Monoid w) => (s -> Tuple a s) -> RWST r w s m a

    tell :: forall r w s m. (Applicative m) => w -> RWST r w s m Unit

    withRWS :: forall r1 r2 w s a. (r2 -> s -> Tuple r1 s) -> RWS r1 w s a -> RWS r2 w s a

    writer :: forall r w s m a. (Applicative m) => Tuple a w -> RWST r w s m a


## Module Control.Monad.RWS.Class

### Type Classes

    class (Monad m, Monoid w, MonadReader r m, MonadWriter w m, MonadState s m) <= MonadRWS r w s m where


### Type Class Instances

    instance monadRWSErrorT :: (Monad m, Monoid w, MonadRWS r w s m, MonadReader r m, MonadWriter w m, MonadState s m, Error e) => MonadRWS r w s (ErrorT e m)

    instance monadRWSMaybeT :: (Monad m, Monoid w, MonadRWS r w s m, MonadReader r m, MonadWriter w m, MonadState s m) => MonadRWS r w s (MaybeT m)

    instance monadRWSRWST :: (Monad m, Monoid w) => MonadRWS r w s (RWST r w s m)


## Module Control.Monad.RWS.Trans

### Types

    newtype RWST r w s m a where
      RWST :: (r -> s -> m (See s a w)) -> RWST r w s m a

    type See s a w = { log :: w, result :: a, state :: s }


### Type Class Instances

    instance applicativeRWST :: (Applicative m, Monoid w) => Applicative (RWST r w s m)

    instance applyRWST :: (Apply m, Semigroup w) => Apply (RWST r w s m)

    instance bindRWST :: (Bind m, Semigroup w) => Bind (RWST r w s m)

    instance functorRWST :: (Functor m) => Functor (RWST r w s m)

    instance monadRWST :: (Monad m, Monoid w) => Monad (RWST r w s m)

    instance monadTransRWST :: (Monoid w) => MonadTrans (RWST r w s)


### Values

    evalRWST :: forall r w s m a. (Monad m) => RWST r w s m a -> r -> s -> m (Tuple a w)

    execRWST :: forall r w s m a. (Monad m) => RWST r w s m a -> r -> s -> m (Tuple s w)

    mapRWST :: forall r w1 w2 s m1 m2 a1 a2. (m1 (See s a1 w1) -> m2 (See s a2 w2)) -> RWST r w1 s m1 a1 -> RWST r w2 s m2 a2

    mkSee :: forall s a w. (Monoid w) => s -> a -> w -> See s a w

    runRWST :: forall r w s m a. RWST r w s m a -> r -> s -> m (See s a w)

    withRWST :: forall r1 r2 w s m a. (r2 -> s -> Tuple r1 s) -> RWST r1 w s m a -> RWST r2 w s m a


## Module Control.Monad.Reader

### Types

    type Reader r = ReaderT r Identity


### Values

    mapReader :: forall r a b. (a -> b) -> Reader r a -> Reader r b

    runReader :: forall r a. Reader r a -> r -> a

    withReader :: forall r1 r2 a b. (r2 -> r1) -> Reader r1 a -> Reader r2 a


## Module Control.Monad.Reader.Class

### Type Classes

    class MonadReader r m where
      ask :: m r
      local :: forall a. (r -> r) -> m a -> m a


### Type Class Instances

    instance monadReaderErrorT :: (Monad m, Error e, MonadReader r m) => MonadReader r (ErrorT e m)

    instance monadReaderFun :: MonadReader r (Prim.Function r)

    instance monadReaderMaybeT :: (Monad m, MonadReader r m) => MonadReader r (MaybeT m)

    instance monadReaderRWST :: (Monad m, Monoid w) => MonadReader r (RWST r w s m)

    instance monadReaderReaderT :: (Monad m) => MonadReader r (ReaderT r m)

    instance monadReaderStateT :: (Monad m, MonadReader r m) => MonadReader r (StateT s m)

    instance monadReaderWriterT :: (Monad m, Monoid w, MonadReader r m) => MonadReader r (WriterT w m)


### Values

    reader :: forall r m a. (Monad m, MonadReader r m) => (r -> a) -> m a


## Module Control.Monad.Reader.Trans

### Types

    newtype ReaderT r m a where
      ReaderT :: (r -> m a) -> ReaderT r m a


### Type Class Instances

    instance altReaderT :: (Alt m) => Alt (ReaderT r m)

    instance alternativeReaderT :: (Alternative m) => Alternative (ReaderT r m)

    instance applicativeReaderT :: (Applicative m) => Applicative (ReaderT r m)

    instance applyReaderT :: (Applicative m) => Apply (ReaderT r m)

    instance bindReaderT :: (Monad m) => Bind (ReaderT r m)

    instance functorReaderT :: (Functor m) => Functor (ReaderT r m)

    instance monadPlusReaderT :: (MonadPlus m) => MonadPlus (ReaderT r m)

    instance monadReaderT :: (Monad m) => Monad (ReaderT r m)

    instance monadTransReaderT :: MonadTrans (ReaderT r)

    instance plusReaderT :: (Plus m) => Plus (ReaderT r m)


### Values

    liftCallCCReader :: forall r m a b. (((a -> m b) -> m a) -> m a) -> ((a -> ReaderT r m b) -> ReaderT r m a) -> ReaderT r m a

    liftCatchReader :: forall r m e a. (m a -> (e -> m a) -> m a) -> ReaderT r m a -> (e -> ReaderT r m a) -> ReaderT r m a

    liftReaderT :: forall r m a. m a -> ReaderT r m a

    mapReaderT :: forall r m1 m2 a b. (m1 a -> m2 b) -> ReaderT r m1 a -> ReaderT r m2 b

    runReaderT :: forall r m a. ReaderT r m a -> r -> m a

    withReaderT :: forall r1 r2 m a b. (r2 -> r1) -> ReaderT r1 m a -> ReaderT r2 m a


## Module Control.Monad.State

### Types

    type State s = StateT s Identity


### Values

    evalState :: forall s a. State s a -> s -> a

    execState :: forall s a. State s a -> s -> s

    mapState :: forall s a b. (Tuple a s -> Tuple b s) -> State s a -> State s b

    runState :: forall s a. State s a -> s -> Tuple a s

    withState :: forall s a. (s -> s) -> State s a -> State s a


## Module Control.Monad.State.Class

### Type Classes

    class MonadState s m where
      state :: forall a. (s -> Tuple a s) -> m a


### Type Class Instances

    instance monadStateErrorT :: (Monad m, Error e, MonadState s m) => MonadState s (ErrorT e m)

    instance monadStateMaybeT :: (Monad m, MonadState s m) => MonadState s (MaybeT m)

    instance monadStateRWST :: (Monad m, Monoid w) => MonadState s (RWST r w s m)

    instance monadStateReaderT :: (Monad m, MonadState s m) => MonadState s (ReaderT r m)

    instance monadStateStateT :: (Monad m) => MonadState s (StateT s m)

    instance monadStateStateT1 :: (Monad m, MonadState s m) => MonadState s (StateT s1 m)

    instance monadStateWriterT :: (Monad m, Monoid w, MonadState s m) => MonadState s (WriterT w m)


### Values

    get :: forall m s. (Monad m, MonadState s m) => m s

    gets :: forall s m a. (Monad m, MonadState s m) => (s -> a) -> m a

    modify :: forall s m. (Monad m, MonadState s m) => (s -> s) -> m Unit

    put :: forall m s. (Monad m, MonadState s m) => s -> m Unit


## Module Control.Monad.State.Trans

### Types

    newtype StateT s m a where
      StateT :: (s -> m (Tuple a s)) -> StateT s m a


### Type Class Instances

    instance altStateT :: (Monad m, Alt m) => Alt (StateT s m)

    instance alternativeStateT :: (Monad m, Alternative m) => Alternative (StateT s m)

    instance applicativeStateT :: (Monad m) => Applicative (StateT s m)

    instance applyStateT :: (Monad m) => Apply (StateT s m)

    instance bindStateT :: (Monad m) => Bind (StateT s m)

    instance functorStateT :: (Monad m) => Functor (StateT s m)

    instance lazy1StateT :: Lazy1 (StateT s m)

    instance monadPlusStateT :: (MonadPlus m) => MonadPlus (StateT s m)

    instance monadStateT :: (Monad m) => Monad (StateT s m)

    instance monadTransStateT :: MonadTrans (StateT s)

    instance plusStateT :: (Monad m, Plus m) => Plus (StateT s m)


### Values

    evalStateT :: forall s m a. (Apply m) => StateT s m a -> s -> m a

    execStateT :: forall s m a. (Apply m) => StateT s m a -> s -> m s

    liftCallCCState :: forall s m a b. (((Tuple a s -> m (Tuple b s)) -> m (Tuple a s)) -> m (Tuple a s)) -> ((a -> StateT s m b) -> StateT s m a) -> StateT s m a

    liftCallCCState' :: forall s m a b. (((Tuple a s -> m (Tuple b s)) -> m (Tuple a s)) -> m (Tuple a s)) -> ((a -> StateT s m b) -> StateT s m a) -> StateT s m a

    liftCatchState :: forall s m e a. (m (Tuple a s) -> (e -> m (Tuple a s)) -> m (Tuple a s)) -> StateT s m a -> (e -> StateT s m a) -> StateT s m a

    liftListenState :: forall s m a w. (Monad m) => (m (Tuple a s) -> m (Tuple (Tuple a s) w)) -> StateT s m a -> StateT s m (Tuple a w)

    liftPassState :: forall s m a b w. (Monad m) => (m (Tuple (Tuple a s) b) -> m (Tuple a s)) -> StateT s m (Tuple a b) -> StateT s m a

    mapStateT :: forall s m1 m2 a b. (m1 (Tuple a s) -> m2 (Tuple b s)) -> StateT s m1 a -> StateT s m2 b

    runStateT :: forall s m a. StateT s m a -> s -> m (Tuple a s)

    withStateT :: forall s m a. (s -> s) -> StateT s m a -> StateT s m a


## Module Control.Monad.Trans

### Type Classes

    class MonadTrans t where
      lift :: forall m a. (Monad m) => m a -> t m a


## Module Control.Monad.Writer

### Types

    type Writer w = WriterT w Identity


### Values

    execWriter :: forall w a. Writer w a -> w

    mapWriter :: forall w1 w2 a b. (Tuple a w1 -> Tuple b w2) -> Writer w1 a -> Writer w2 b

    runWriter :: forall w a. Writer w a -> Tuple a w


## Module Control.Monad.Writer.Class

### Type Classes

    class MonadWriter w m where
      writer :: forall a. Tuple a w -> m a
      listen :: forall a. m a -> m (Tuple a w)
      pass :: forall a. m (Tuple a (w -> w)) -> m a


### Type Class Instances

    instance monadWriterErrorT :: (Monad m, Error e, MonadWriter w m) => MonadWriter w (ErrorT e m)

    instance monadWriterMaybeT :: (Monad m, MonadWriter w m) => MonadWriter w (MaybeT m)

    instance monadWriterRWST :: (Monad m, Monoid w) => MonadWriter w (RWST r w s m)

    instance monadWriterReaderT :: (Monad m, MonadWriter w m) => MonadWriter w (ReaderT r m)

    instance monadWriterStateT :: (Monad m, MonadWriter w m) => MonadWriter w (StateT s m)

    instance monadWriterWriterT :: (Monoid w, Monad m) => MonadWriter w (WriterT w m)


### Values

    censor :: forall w m a. (Monoid w, Monad m, MonadWriter w m) => (w -> w) -> m a -> m a

    listens :: forall w m a b. (Monoid w, Monad m, MonadWriter w m) => (w -> b) -> m a -> m (Tuple a b)

    tell :: forall w m a. (Monoid w, Monad m, MonadWriter w m) => w -> m Unit


## Module Control.Monad.Writer.Trans

### Types

    newtype WriterT w m a where
      WriterT :: m (Tuple a w) -> WriterT w m a


### Type Class Instances

    instance altWriterT :: (Monoid w, Alt m) => Alt (WriterT w m)

    instance alternativeWriterT :: (Monoid w, Alternative m) => Alternative (WriterT w m)

    instance applicativeWriterT :: (Monoid w, Applicative m) => Applicative (WriterT w m)

    instance applyWriterT :: (Monoid w, Apply m) => Apply (WriterT w m)

    instance bindWriterT :: (Monoid w, Monad m) => Bind (WriterT w m)

    instance functorWriterT :: (Functor m) => Functor (WriterT w m)

    instance monadPlusWriterT :: (Monoid w, MonadPlus m) => MonadPlus (WriterT w m)

    instance monadTransWriterT :: (Monoid w) => MonadTrans (WriterT w)

    instance monadWriterT :: (Monoid w, Monad m) => Monad (WriterT w m)

    instance plusWriterT :: (Monoid w, Plus m) => Plus (WriterT w m)


### Values

    execWriterT :: forall w m a. (Apply m) => WriterT w m a -> m w

    liftCallCCWriter :: forall w m a b. (Monoid w) => (((Tuple a w -> m (Tuple b w)) -> m (Tuple a w)) -> m (Tuple a w)) -> ((a -> WriterT w m b) -> WriterT w m a) -> WriterT w m a

    liftCatchWriter :: forall w m e a. (m (Tuple a w) -> (e -> m (Tuple a w)) -> m (Tuple a w)) -> WriterT w m a -> (e -> WriterT w m a) -> WriterT w m a

    mapWriterT :: forall w1 w2 m1 m2 a b. (m1 (Tuple a w1) -> m2 (Tuple b w2)) -> WriterT w1 m1 a -> WriterT w2 m2 b

    runWriterT :: forall w m a. WriterT w m a -> m (Tuple a w)