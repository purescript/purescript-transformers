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