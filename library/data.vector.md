---
description: ちゃんとO(1)な、整数添字のみの配列
---

# Data.Vector

同じインタフェースでUnboxedな（正格でより速い）ベクタを提供する`Data.Vector.Unboxed`もある。モナド版の関数については省略。`(//)`は$$O(m(n+m))$$なので使うとがっかりする。あくまでimmutableな対象。

[https://hackage.haskell.org/package/vector-0.12.2.0/docs/Data-Vector.html](https://hackage.haskell.org/package/vector-0.12.2.0/docs/Data-Vector.html)

```haskell
import qualified Data.Vector as V

import qualified Data.Vector.Unboxed as V
```

```haskell
data Vector a    -- a 要素の型 → ベクタ型
```

#### 大きさ

```haskell
-- 長さ
V.length :: Vector a -> Int

-- 空か
V.null :: Vector a -> Bool
```

#### アクセス

```haskell
-- 要素取り出し
(V.!) :: Vector a -> Int -> a

-- 範囲外検査つき
(V.!?) :: Vector a -> Int -> Maybe a
```

#### リストから類推できる操作

```haskell
V.head :: Vector a -> a -- 先頭要素
V.last :: Vector a -> a -- 末尾要素
V.init :: Vector a -> Vector a -- 末尾以外
V.tail :: Vector a -> Vector a -- 先頭以外
V.take :: Int -> Vector a -> Vector a -- 先頭n個
V.drop :: Int -> Vector a -> Vector a  -- 先頭n個以外
V.splitAt :: Int -> Vector a -> (Vector a, Vector a) -- n個で分割
V.uncons :: Vector a -> Maybe (a, Vector a) -- headとtail
V.unsnoc :: Vector a -> Maybe (Vector a, a) -- initとlast
V.cons :: a -> Vector a -> Vector a -- O(n)
V.snoc :: Vector a -> a -> Vector a -- O(n)
(V.++) :: Vector a -> Vector a -> Vector a -- O(m+n)
V.concat :: [Vector a] -> Vector a -- O(n)
```

一般化：`V.slice i n v` はベクタvの添字iからn要素のみを切り出したベクタ

```haskell
V.slice :: Int -> Int -> Vector a -> Vector a
```

#### ベクタの作成

```haskell
-- 空
V.empty :: Vector a

-- 単一要素
V.singleton :: a -> Vector a

-- 輸入
V.fromList :: [a] -> Vector a
V.fromArray :: Array a -> Vector a -- 添字は？
```

#### リストから類推できる生成

```haskell
-- n要素の連続したa
V.replicate :: Int -> a -> Vector a

-- n要素を関数で生成
V.generate :: Int -> (Int -> a) -> Vector a

-- n要素を繰り返し関数で生成
V.iterateN :: Int -> (a -> a) -> a -> Vector a
V.iterateN n f x = fromList $ take n $ iterate f x

-- 展開
V.unfoldr :: (b -> Maybe (a, b)) -> b -> Vector a
```

#### Data.Array.accumArray的な

```haskell
V.create :: (forall s. ST s (MVector s a)) -> Vector a
```

Mutable Vectorを作るアクションを実行して、最終結果のImmutable Vectorをpureに得る関数。例えば下のようにして、エラトステネスの篩を実行できる。

```haskell
primev = V.create (do
  v <- MV.replicate (n+1) True
  forM_ [2..n] (\i -> do
    f <- MV.read v i
    when f (forM_ [i*2,i*3..n] (\j -> MV.write v j False)))
  return v
  )
```

#### 他の値へ変換

```haskell
V.toList :: Vector a -> [a]
V.toArray :: Vector a -> Array a
```

#### 更新

```haskell
(V.//) :: Vector a -> [(Int,a)] -> Vector a
V.update :: Vector a -> Vector (Int,a) -> Vector a

V.accum :: (a -> b -> a) -- 継ぎ足し関数
        -> Vector a
        -> [(Int, b)]
        -> Vector a
V.accumlate :: (a -> b -> a)
        -> Vector a
        -> Vector (Int, b)
        -> Vector a
```

#### リストから類推できる高度な操作

```haskell
V.reverse :: Vector a -> Vector a
V.map :: (a -> b) -> Vector a -> Vector b
V.filter :: (a -> Bool) -> Vector a -> Vector a
V.zip     :: Vector a -> Vector b -> Vector (a, b) -- also 3～6
V.zipWith :: (a -> b -> c) -> Vector a -> Vector b -> Vector c -- also 3～6
V.uniq :: Eq a => Vector a -> Vector a -- 隣接した繰り返しを除く
V.takeWhile :: (a -> Bool) -> Vector a -> Vector a
V.dropWhile :: (a -> Bool) -> Vector a -> Vector a
V.span  :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
V.break :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
V.partition :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
V.elem    :: Eq a => a -> Vector a -> Bool
V.notElem :: Eq a => a -> Vector a -> Bool

V.find :: (a -> Bool) -> Vector a -> Maybe a
V.findIndex :: (a -> Bool) -> Vector a -> Maybe Int
V.findIndices :: (a -> Bool) -> Vector a -> Vector Int
V.elemIndex :: Eq a => a -> Vector a -> Maybe Int
V.elemIndices :: Eq a => a -> Vector a -> Vector Int

V.foldl  :: (a -> b -> a) -> a -> Vector b -> a
V.foldl' :: (a -> b -> a) -> a -> Vector b -> a
V.foldl1 :: (a -> a -> a) -> Vector a -> a

V.foldr  :: (a -> b -> b) -> b -> Vector a -> b
V.foldr' :: (a -> b -> b) -> b -> Vector a -> b
V.foldr1 :: (a -> a -> a) -> Vector a -> a

V.all :: (a -> Bool) -> Vector a -> Bool
V.any :: (a -> Bool) -> Vector a -> Bool
V.and :: Vector Bool -> Bool
V.or  :: Vector Bool -> Bool

V.sum     :: Num a => Vector a -> a
V.product :: Num a => Vector a -> a

V.maximum :: Ord a => Vector a -> a
V.minimum :: Ord a => Vector a -> a
V.maximumBy :: (a -> a -> Ordering) -> Vector a -> a
V.minimumBy :: (a -> a -> Ordering) -> Vector a -> a
V.minIndex :: Ord a => Vector a -> Int
V.maxIndex :: Ord a => Vector a -> Int
V.minIndexBy :: (a -> a -> Ordering) -> Vector a -> Int
V.maxIndexBy :: (a -> a -> Ordering) -> Vector a -> Int

V.scanl  :: (a -> b -> a) -> a -> Vector b -> Vector a
V.scanl' :: (a -> b -> a) -> a -> Vector b -> Vector a
V.scanl1 :: (a -> a -> a) -> Vector a -> Vector a -- also 1'
```

backpermuteがちょっと変わってる。

```haskell
V.backpermute :: Vector a -> Vector Int -> Vector a
V.backpermute av iv = V.map (av V.!) iv
```

#### 比較

```haskell
V.eqBy :: (a -> b -> Bool) -> Vector a -> Vector b -> Bool

V.cmpBy :: (a -> b -> Ordering) -> Vector a -> Vector b -> Ordering
```

cons, snocがO\(n\)なのでちまちま作るのには向かない。  
リストで要素を作っておいて一気にfromListで変換した後は、リストで親しんだ操作は一通り備わっている感じ。使うかわからんが。

