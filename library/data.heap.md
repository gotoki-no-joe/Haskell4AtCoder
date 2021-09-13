---
description: 優先度付きキュー
---

# Data.Heap

Data.Heapにはheapsパッケージのものとheapパッケージのものと二つある。  
AtCoderで利用できるものは前者。  
[https://hackage.haskell.org/package/heaps-0.4/docs/Data-Heap.html](https://hackage.haskell.org/package/heaps-0.4/docs/Data-Heap.html)  
Haskell Platformにはバンドルされていない。

```haskell
import qualified Data.Heap as H
```

```haskell
-- ヒープの型
data Heap a

instance Foldable (Heap a) -- not Traversable

-- 要素のOrdでない優先順位を使うための要素型
data Entry p a = Entry { priority :: p, payload :: a }
```

#### 作る

```haskell
-- 空 O(1)
empty :: Heap a
-- 単一要素 O(1)
singleton :: Ord a => a -> Heap a
-- リストから O(n)
fromList :: Ord a => [a] -> Heap a
```

#### 属性

```haskell
-- 空 O(1)
null :: Heap a -> Bool
-- 要素数
size :: Heap a -> Int
```

#### 操作

```haskell
-- 挿入 O(1)
insert :: Ord a => a -> Heap a -> Heap a

-- 最小値 O(1)
minimum :: Heap a -> a
-- 最小値を除く O(log n)
deleteMin :: Heap a -> Heap a
-- 最小値の取り出し
uncons :: Heap a -> Maybe (a, Heap a)
viewMin = uncons

-- 最小値を書き換える O(log n)
adjustMin :: (a -> a) -> Heap a -> Heap a
-- 合わせる O(1)
union :: Heap a -> Heap a -> Heap a

-- マップ O(n)
map :: Ord b => (a -> b) -> Heap a -> Heap b
-- 効率のよい版、ただし関数は単調増加であること、構造の破壊は検出されない
mapMonotonic :: Ord b => (a -> b) -> Heap a -> Heap b

-- 順不同で全て吐き出す O(n)
toUnsortedlist :: Heap a -> [a]
```

```haskell
-- ヒープソート
sort :: Ord a => [a] -> [a]
```

in progress...

