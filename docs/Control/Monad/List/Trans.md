## Module Control.Monad.List.Trans

This module defines the list monad transformer, `ListT`.

#### `ListT`

``` purescript
data ListT f a
```

The list monad transformer.

This monad transformer extends the base monad with _non-determinism_.
That is, the transformed monad supports the same effects as the base monad
but with multiple return values.

##### Instances
``` purescript
instance semigroupListT :: (Applicative f) => Semigroup (ListT f a)
instance monoidListT :: (Applicative f) => Monoid (ListT f a)
instance functorListT :: (Functor f) => Functor (ListT f)
instance unfoldableListT :: (Monad f) => Unfoldable (ListT f)
instance applyListT :: (Monad f) => Apply (ListT f)
instance applicativeListT :: (Monad f) => Applicative (ListT f)
instance bindListT :: (Monad f) => Bind (ListT f)
instance monadListT :: (Monad f) => Monad (ListT f)
instance monadTransListT :: MonadTrans ListT
instance altListT :: (Applicative f) => Alt (ListT f)
instance plusListT :: (Monad f) => Plus (ListT f)
instance alternativeListT :: (Monad f) => Alternative (ListT f)
instance monadPlusListT :: (Monad f) => MonadPlus (ListT f)
instance monadEffListT :: (MonadEff eff m) => MonadEff eff (ListT m)
```

#### `nil`

``` purescript
nil :: forall f a. (Applicative f) => ListT f a
```

The empty list.

#### `cons`

``` purescript
cons :: forall f a. (Applicative f) => Lazy a -> Lazy (ListT f a) -> ListT f a
```

Attach an element to the front of a list.

#### `prepend'`

``` purescript
prepend' :: forall f a. (Applicative f) => a -> Lazy (ListT f a) -> ListT f a
```

Prepend an element to a lazily-evaluated list.

#### `prepend`

``` purescript
prepend :: forall f a. (Applicative f) => a -> ListT f a -> ListT f a
```

Prepend an element to a list.

#### `singleton`

``` purescript
singleton :: forall f a. (Applicative f) => a -> ListT f a
```

Create a list with one element.

#### `fromEffect`

``` purescript
fromEffect :: forall f a. (Applicative f) => f a -> ListT f a
```

Lift a computation from the base functor.

#### `wrapEffect`

``` purescript
wrapEffect :: forall f a. (Functor f) => f (ListT f a) -> ListT f a
```

Lift a computation from the base monad.

#### `wrapLazy`

``` purescript
wrapLazy :: forall f a. (Applicative f) => Lazy (ListT f a) -> ListT f a
```

Defer evaluation of a list.

#### `unfold`

``` purescript
unfold :: forall f a z. (Monad f) => (z -> f (Maybe (Tuple z a))) -> z -> ListT f a
```

Unfold a list using an effectful generator function.

#### `iterate`

``` purescript
iterate :: forall f a. (Monad f) => (a -> a) -> a -> ListT f a
```

Generate an infinite list by iterating a function.

#### `repeat`

``` purescript
repeat :: forall f a. (Monad f) => a -> ListT f a
```

Generate an infinite list by repeating a value.

#### `take`

``` purescript
take :: forall f a. (Applicative f) => Int -> ListT f a -> ListT f a
```

Take a number of elements from the front of a list.

#### `takeWhile`

``` purescript
takeWhile :: forall f a. (Applicative f) => (a -> Boolean) -> ListT f a -> ListT f a
```

Take elements from the front of a list while a predicate holds.

#### `drop`

``` purescript
drop :: forall f a. (Applicative f) => Int -> ListT f a -> ListT f a
```

Drop a number of elements from the front of a list.

#### `dropWhile`

``` purescript
dropWhile :: forall f a. (Applicative f) => (a -> Boolean) -> ListT f a -> ListT f a
```

Drop elements from the front of a list while a predicate holds.

#### `filter`

``` purescript
filter :: forall f a. (Functor f) => (a -> Boolean) -> ListT f a -> ListT f a
```

Remove elements from a list for which a predicate fails to hold.

#### `mapMaybe`

``` purescript
mapMaybe :: forall f a b. (Functor f) => (a -> Maybe b) -> ListT f a -> ListT f b
```

Apply a function to the elements of a list, keeping only those return values which contain a result.

#### `catMaybes`

``` purescript
catMaybes :: forall f a. (Functor f) => ListT f (Maybe a) -> ListT f a
```

Remove elements from a list which do not contain a value.

#### `uncons`

``` purescript
uncons :: forall f a. (Monad f) => ListT f a -> f (Maybe (Tuple a (ListT f a)))
```

Perform the first step of a computation in the `ListT` monad.

#### `head`

``` purescript
head :: forall f a. (Monad f) => ListT f a -> f (Maybe a)
```

Extract the first element of a list.

#### `tail`

``` purescript
tail :: forall f a. (Monad f) => ListT f a -> f (Maybe (ListT f a))
```

Extract all but the first element of a list.

#### `foldl'`

``` purescript
foldl' :: forall f a b. (Monad f) => (b -> a -> f b) -> b -> ListT f a -> f b
```

Fold a list from the left, accumulating the result (effectfully) using the specified function.

#### `foldl`

``` purescript
foldl :: forall f a b. (Monad f) => (b -> a -> b) -> b -> ListT f a -> f b
```

Fold a list from the left, accumulating the result using the specified function.

#### `scanl`

``` purescript
scanl :: forall f a b. (Monad f) => (b -> a -> b) -> b -> ListT f a -> ListT f b
```

Fold a list from the left, accumulating the list of results using the specified function.

#### `zipWith'`

``` purescript
zipWith' :: forall f a b c. (Monad f) => (a -> b -> f c) -> ListT f a -> ListT f b -> ListT f c
```

Zip the elements of two lists, combining elements at the same position from each list.

#### `zipWith`

``` purescript
zipWith :: forall f a b c. (Monad f) => (a -> b -> c) -> ListT f a -> ListT f b -> ListT f c
```

Zip the elements of two lists, combining elements at the same position from each list.


