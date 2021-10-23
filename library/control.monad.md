# Control.Monad

```haskell
import Control.Monad
```



```haskell
-- モナドアクションをリストなどの要素に適用
mapM  :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
mapM_ :: (Foldable t,    Monad m) => (a -> m b) -> t a -> m ()
-- flip version
forM  :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
forM_ :: (Foldable t,    Monad m) => t a -> (a -> m b) -> m ()

-- モナドアクションのリストなどを全て実行
sequence  :: (Traversable t, Monad m) => t (m a) -> m (t a)
sequence_ :: (Foldable t,    Monad m) => t (m a) -> m ()
```

```haskell
-- モナドアクション２つを連結
-- (>=>) = \f g x -> f x >>= g かな？
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
```

```haskell
-- モナドアクションを何度か繰り返す
replicateM  :: Applicative m => Int -> m a -> m [a]
replicateM_ :: Applicative m => Int -> m a -> m ()
```

モナドの実行を途中で止める方法

```haskell
guard :: Alternative f => Bool -> f ()

ex)
(guard False >> Just 3 ) = Nothing
(guard True  >> Just 3 ) = Just 3
(guard False >> [1,2,3]) = []
(guard True  >> [1,2,3]) = [1,2,3]
```

実行の途中にアクションを挟んだり挟まなかったたりする方法

```haskell
when :: Applicative f => Bool -> f () -> f ()
```
