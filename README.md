# Module Documentation

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

    data ContT r m a where
      ContT :: (a -> m r) -> m r -> ContT r m a


### Type Class Instances

    instance applicativeContT :: (Functor m, Monad m) => Applicative (ContT r m)

    instance appluContT :: (Functor m, Monad m) => Apply (ContT r m)

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
      strMsg :: Prim.String -> a


### Type Class Instances

    instance errorEitherAlternative :: (Error e) => Alternative (Either e)

    instance errorString :: Error Prim.String


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

    data ErrorT e m a where
      ErrorT :: m (Either e a) -> ErrorT e m a


### Type Class Instances

    instance alternativeErrorT :: (Monad m, Error e) => Alternative (ErrorT e m)

    instance applicativeErrorT :: (Functor m, Monad m) => Applicative (ErrorT e m)

    instance applyErrorT :: (Functor m, Monad m) => Apply (ErrorT e m)

    instance bindErrorT :: (Monad m, Error e) => Bind (ErrorT e m)

    instance functorErrorT :: (Functor m) => Functor (ErrorT e m)

    instance monadErrorT :: (Monad m, Error e) => Monad (ErrorT e m)

    instance monadTransErrorT :: (Error e) => MonadTrans (ErrorT e)


### Values

    liftCallCCError :: forall e m a b. (((Either e a -> m (Either e b)) -> m (Either e a)) -> m (Either e a)) -> ((a -> ErrorT e m b) -> ErrorT e m a) -> ErrorT e m a

    liftListenError :: forall e m a w. (Monad m) => (m (Either e a) -> m (Tuple (Either e a) w)) -> ErrorT e m a -> ErrorT e m (Tuple a w)

    liftPassError :: forall e m a w. (Monad m) => (m (Tuple (Either e a) (w -> w)) -> m (Either e a)) -> ErrorT e m (Tuple a (w -> w)) -> ErrorT e m a

    mapErrorT :: forall e1 e2 m1 m2 a b. (m1 (Either e1 a) -> m2 (Either e2 b)) -> ErrorT e1 m1 a -> ErrorT e2 m2 b

    runErrorT :: forall e m a. ErrorT e m a -> m (Either e a)


## Module Control.Monad.Free

### Types

    data Free f a where
      Pure :: a -> Free f a
      Free :: f (Free f a) -> Free f a
      Gosub :: forall s. (forall r. ({  } -> Free f r) -> (r -> Free f a) -> s) -> s -> Free f a


### Type Classes

    class MonadFree f m where
      wrap :: forall a. f (m a) -> m a


### Type Class Instances

    instance applicativeFree :: (Functor f) => Applicative (Free f)

    instance applyFree :: (Functor f) => Apply (Free f)

    instance bindFree :: (Functor f) => Bind (Free f)

    instance functorFree :: (Functor f) => Functor (Free f)

    instance monadFree :: (Functor f) => Monad (Free f)

    instance monadFreeFree :: (Functor f) => MonadFree f (Free f)

    instance monadTransFree :: MonadTrans Free


### Values

    go :: forall f a. (Functor f) => (f (Free f a) -> Free f a) -> Free f a -> a

    goEff :: forall e f a. (Functor f) => (f (Free f a) -> Eff e (Free f a)) -> Free f a -> Eff e a

    goM :: forall f m a. (Functor f, Monad m) => (f (Free f a) -> m (Free f a)) -> Free f a -> m a

    iterM :: forall f m a. (Functor f, Monad m) => (forall a. f (m a) -> m a) -> Free f a -> m a

    liftF :: forall f m a. (Functor f, Monad m, MonadFree f m) => f a -> m a

    pureF :: forall f a. (Applicative f) => a -> Free f a

    resume :: forall f a. (Functor f) => Free f a -> Either (f (Free f a)) a

    resumeGosub :: forall f a. (Functor f) => (forall s. (forall r. ({  } -> Free f r) -> (r -> Free f a) -> s) -> s) -> Either (f (Free f a)) (Free f a)


## Module Control.Monad.Identity

### Types

    data Identity a where
      Identity :: a -> Identity a


### Type Class Instances

    instance applicativeIdentity :: Applicative Identity

    instance applyIdentity :: Apply Identity

    instance bindIdentity :: Bind Identity

    instance functorIdentity :: Functor Identity

    instance monadIdentity :: Monad Identity


### Values

    runIdentity :: forall a. Identity a -> a


## Module Control.Monad.Maybe.Trans

### Types

    data MaybeT m a where
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


## Module Control.Monad.Reader

### Types

    type Reader r a = ReaderT r Identity a


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

    instance monadReaderReaderT :: (Monad m) => MonadReader r (ReaderT r m)

    instance monadReaderStateT :: (Monad m, MonadReader r m) => MonadReader r (StateT s m)

    instance monadReaderWriterT :: (Monad m, Monoid w, MonadReader r m) => MonadReader r (WriterT w m)


### Values

    reader :: forall r m a. (Monad m, MonadReader r m) => (r -> a) -> m a


## Module Control.Monad.Reader.Trans

### Types

    data ReaderT r m a where
      ReaderT :: r -> m a -> ReaderT r m a


### Type Class Instances

    instance alternativeReaderT :: (Alternative m) => Alternative (ReaderT r m)

    instance applicativeReaderT :: (Applicative m) => Applicative (ReaderT r m)

    instance applyReaderT :: (Applicative m) => Apply (ReaderT r m)

    instance bindReaderT :: (Monad m) => Bind (ReaderT r m)

    instance functorReaderT :: (Functor m) => Functor (ReaderT r m)

    instance monadReaderT :: (Monad m) => Monad (ReaderT r m)

    instance monadTransReaderT :: MonadTrans (ReaderT r)


### Values

    liftCallCCReader :: forall r m a b. (((a -> m b) -> m a) -> m a) -> ((a -> ReaderT r m b) -> ReaderT r m a) -> ReaderT r m a

    liftCatchReader :: forall r m e a. (m a -> (e -> m a) -> m a) -> ReaderT r m a -> (e -> ReaderT r m a) -> ReaderT r m a

    liftReaderT :: forall r m a. m a -> ReaderT r m a

    mapReaderT :: forall r m1 m2 a b. (m1 a -> m2 b) -> ReaderT r m1 a -> ReaderT r m2 b

    runReaderT :: forall r m a. ReaderT r m a -> r -> m a

    withReaderT :: forall r1 r2 m a b. (r2 -> r1) -> ReaderT r1 m a -> ReaderT r2 m a


## Module Control.Monad.State

### Types

    type State s a = StateT s Identity a


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

    instance monadStateReaderT :: (Monad m, MonadState s m) => MonadState s (ReaderT r m)

    instance monadStateStateT :: (Monad m) => MonadState s (StateT s m)

    instance monadStateStateT1 :: (Monad m, MonadState s m) => MonadState s (StateT s1 m)

    instance monadStateWriterT :: (Monad m, Monoid w, MonadState s m) => MonadState s (WriterT w m)


### Values

    get :: forall m s. (Monad m, MonadState s m) => m s

    gets :: forall s m a. (Monad m, MonadState s m) => (s -> a) -> m a

    modify :: forall s m. (Monad m, MonadState s m) => (s -> s) -> m {  }

    put :: forall m s. (Monad m, MonadState s m) => s -> m {  }


## Module Control.Monad.State.Trans

### Types

    data StateT s m a where
      StateT :: s -> m (Tuple a s) -> StateT s m a


### Type Class Instances

    instance alternativeStateT :: (Alternative m) => Alternative (StateT s m)

    instance applicativeStateT :: (Monad m) => Applicative (StateT s m)

    instance applyStateT :: (Monad m) => Apply (StateT s m)

    instance bindStateT :: (Monad m) => Bind (StateT s m)

    instance functorStateT :: (Monad m) => Functor (StateT s m)

    instance monadStateT :: (Monad m) => Monad (StateT s m)

    instance monadTransStateT :: MonadTrans (StateT s)


### Values

    evalStateT :: forall s m a. (Monad m) => StateT s m a -> s -> m a

    execStateT :: forall s m a. (Monad m) => StateT s m a -> s -> m s

    liftCallCCState :: forall s m a b. (((Tuple a s -> m (Tuple b s)) -> m (Tuple a s)) -> m (Tuple a s)) -> ((a -> StateT s m b) -> StateT s m a) -> StateT s m a

    liftCallCCState' :: forall s m a b. (((Tuple a s -> m (Tuple b s)) -> m (Tuple a s)) -> m (Tuple a s)) -> ((a -> StateT s m b) -> StateT s m a) -> StateT s m a

    liftCatchState :: forall s m e a. (m (Tuple a s) -> (e -> m (Tuple a s)) -> m (Tuple a s)) -> StateT s m a -> (e -> StateT s m a) -> StateT s m a

    liftListenState :: forall s m a w. (Monad m) => (m (Tuple a s) -> m (Tuple (Tuple a s) w)) -> StateT s m a -> StateT s m (Tuple a w)

    liftPassState :: forall s m a b w. (Monad m) => (m (Tuple (Tuple a s) b) -> m (Tuple a s)) -> StateT s m (Tuple a b) -> StateT s m a

    mapStateT :: forall s m1 m2 a b. (m1 (Tuple a s) -> m2 (Tuple b s)) -> StateT s m1 a -> StateT s m2 b

    runStateT :: forall s m a. StateT s m a -> s -> m (Tuple a s)

    withStateT :: forall s m a. (s -> s) -> StateT s m a -> StateT s m a


## Module Control.Monad.Trampoline

### Types

    data Delay a where
      Delay :: {  } -> a -> Delay a

    type Trampoline a = Free Delay a


### Type Class Instances

    instance delayApplicative :: Applicative Delay

    instance delayApply :: Apply Delay

    instance delayFunctor :: Functor Delay


### Values

    delay :: forall a. ({  } -> a) -> Trampoline a

    done :: forall a. a -> Trampoline a

    runTrampoline :: forall a. Trampoline a -> a

    suspend :: forall a. Trampoline a -> Trampoline a


## Module Control.Monad.Trans

### Type Classes

    class MonadTrans t where
      lift :: forall m a. (Monad m) => m a -> t m a


## Module Control.Monad.Writer

### Types

    type Writer w a = WriterT w Identity a


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

    instance monadWriterReaderT :: (Monad m, MonadWriter w m) => MonadWriter w (ReaderT r m)

    instance monadWriterStateT :: (Monad m, MonadWriter w m) => MonadWriter w (StateT s m)

    instance monadWriterWriterT :: (Monoid w, Monad m) => MonadWriter w (WriterT w m)


### Values

    censor :: forall w m a. (Monoid w, Monad m, MonadWriter w m) => (w -> w) -> m a -> m a

    listens :: forall w m a b. (Monoid w, Monad m, MonadWriter w m) => (w -> b) -> m a -> m (Tuple a b)

    tell :: forall w m a. (Monoid w, Monad m, MonadWriter w m) => w -> m {  }


## Module Control.Monad.Writer.Trans

### Types

    data WriterT w m a where
      WriterT :: m (Tuple a w) -> WriterT w m a


### Type Class Instances

    instance alternativeWriterT :: (Monoid w, Alternative m) => Alternative (WriterT w m)

    instance applicativeWriterT :: (Monoid w, Functor m, Applicative m) => Applicative (WriterT w m)

    instance applyWriterT :: (Monoid w, Functor m, Applicative m) => Apply (WriterT w m)

    instance bindWriterT :: (Monoid w, Monad m) => Bind (WriterT w m)

    instance functorWriterT :: (Functor m) => Functor (WriterT w m)

    instance monadTransWriterT :: (Monoid w) => MonadTrans (WriterT w)

    instance monadWriterT :: (Monoid w, Monad m) => Monad (WriterT w m)


### Values

    liftCallCCWriter :: forall w m a b. (Monoid w) => (((Tuple a w -> m (Tuple b w)) -> m (Tuple a w)) -> m (Tuple a w)) -> ((a -> WriterT w m b) -> WriterT w m a) -> WriterT w m a

    liftCatchWriter :: forall w m e a. (m (Tuple a w) -> (e -> m (Tuple a w)) -> m (Tuple a w)) -> WriterT w m a -> (e -> WriterT w m a) -> WriterT w m a

    mapWriterT :: forall w1 w2 m1 m2 a b. (m1 (Tuple a w1) -> m2 (Tuple b w2)) -> WriterT w1 m1 a -> WriterT w2 m2 b

    runWriterT :: forall w m a. WriterT w m a -> m (Tuple a w)