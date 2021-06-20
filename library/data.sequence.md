---
description: 前からも後ろからも効率的に操作できるリスト
---

# Data.Sequence

Haskell Platform標準ライブラリに入っている。

```haskell
import Data.Sequence
```

ただしいろいろな名前が衝突する。

```haskell
data Seq a where
  Empty :: Seq a                -- 空の列
  (:<|) :: a -> Seq a -> Seq a  -- 先頭と残り(cons)
  (:|>) :: Seq a -> a -> Seq a  -- 末尾とそれより前(snoc)
```

これらはパターンマッチで使える。

### 列を作る

```haskell
-- 空列 O(1)
empty :: Seq a
-- 一要素の列 O(1)
singleton :: a -> Seq a
-- cons O(1)
(<|) :: a -> Seq a -> Seq a
-- snoc O(1)
(|>) :: Seq a -> a -> Seq a
-- 連結 O(log(min(n1,n2)))
(><) :: Seq a -> Seq a -> Seq a
-- リストから変換 O(n)
fromList :: [a] -> Seq a
-- 関数で作る O(n)
fromFunction :: Int -> (Int -> a) -> Seq a
-- 配列から O(n)
fromArray :: Ix i => Array i a -> Seq a
-- 繰り返し O(log n)
replicate :: Int -> a -> Seq a
-- 連続適用 O(n)
iterateN :: Int -> (a -> a) -> a -> Seq a
-- 展開
unfoldr :: (b -> Maybe (a, b)) -> b -> Seq a
unfoldl :: (b -> Maybe (b, a)) -> b -> Seq a
```

### 性質

```haskell
-- 空列か O(1)
null :: Seq a -> Bool
-- 長さ O(1)
length :: Seq a -> Int
```

### スキャン

```haskell
scanl  :: (a -> b -> a) -> a -> Seq b -> Seq a
scanl1 :: (a -> a -> a) ->      Seq a -> Seq a
scanr  :: (a -> b -> b) -> b -> Seq a -> Seq b
scanr1 :: (a -> a -> a) ->      Seq a -> Seq a
```

### 部分列

```haskell
tails :: Seq a -> Seq (Seq a) -- O(n)
inits :: Seq a -> Seq (Seq a) -- O(n)
-- chunksOf c xs xsをc個ずつに分ける O(n/c log c)
chunksOf :: Int -> Seq a -> Seq (Seq a)
```

### 線形探索

```haskell
takeWhileL :: (a -> Bool) -> Seq a -> Seq a
takeWhileR :: (a -> Bool) -> Seq a -> Seq a
dropWhileL :: (a -> Bool) -> Seq a -> Seq a
dropWhileR :: (a -> Bool) -> Seq a -> Seq a
spanl :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
spanr :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
breakl :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
breakr :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
partition :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
filter :: (a -> Bool) -> Seq a -> Seq a
```

### 整列

```haskell
sort :: Ord a => Seq a -> Seq a
sortBy :: (a -> a -> Ordering) -> Seq a -> Seq a
sortOn :: Ord b => (a -> b) -> Seq a -> Seq a
```

全て$$O(n \log n)$$。少し軽い`unstable～`という版がある。

### 添字

```haskell
lookup :: Int -> Seq a -> Maybe a
(!?)   :: Seq a -> Int -> Maybe a
index  :: Seq a -> Int -> a
adjust ::           (a -> a) -> Int -> Seq a -> Seq a
adjust':: forall a. (a -> a) -> Int -> Seq a -> Seq a
update :: Int -> a -> Seq a -> Seq a
take :: Int -> Seq a -> Seq a
drop :: Int -> Seq a -> Seq a
splitAt :: Int -> Seq a -> (Seq a, Seq a)
insertAt :: Int -> a -> Seq a -> Seq a
deleteAt :: Int -> Seq a -> Seq a
```

\(in progress...\)

