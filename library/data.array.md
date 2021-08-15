---
description: 'Immutable, Non-Strictな配列っぽい何か。'
---

# Data.Array

## API

```haskell
import Data.Array
```

```haskell
data Array i e  -- i 添字型, e 要素型 → 配列の型
```

### 配列を作る

```haskell
array :: Ix i   -- 添字型の制約
  => (i,i)      -- 下限、上限
  -> [(i,e)]    -- 添字と要素の対のリスト
  -> Array i e
```

```haskell
listArray :: Ix i  -- 添字型の制約
  => (i,i)         -- 下限、上限
  -> [e]           -- 要素のリストは添字の順に
  -> Array i e
```

```haskell
accumArray :: Ix i =>  -- 添字型の制約
  => (e -> a -> e)     -- 継ぎ足し関数
  -> e                 -- 初期値
  -> (i,i)             -- 下限、上限
  -> [(i,a)]           -- 添字と継ぎ足す値の対のリスト
  -> Array i e	
```

例えば ``accumArray (flip (:)) [] (0, 2) [(i, i `mod` 3) | i <- [1..9]]`` などとできる。

### アクセス

```haskell
-- 添字で1要素を取り出す
(!) :: Ix i => Array i e -> i -> e
-- 配列の範囲
bounds :: Array i e -> (i, i)
-- 添字のリスト
indices :: Ix i => Array i e -> [i]
-- 要素のリスト
elems :: Array i e -> [e]
-- 添字と要素の対応リスト
assocs :: Ix i => Array i e -> [(i, e)]
```

### 更新

```haskell
-- まとめて上書き
(//) :: Ix i => Array i e -> [(i,e)] -> Array i e
-- まとめて継ぎ足し
accum :: Ix i => (e -> a -> e) -> Array i e -> [(i,a)] -> Array i e
```

## 説明

要素へのアクセスは$$O(\log n)$$、要素の更新はかなりかかるが、インデックスの自由度が高いのは利点。古くから標準で入っているので安心して使える、Immutable, Non-Strictな配列っぽい何か。

ある種の動的プログラミングを実現するのにも使える。配列の要素を関数で生成し、その関数に今作っている配列の要素を適宜参照させる。フィボナッチ数列の例を示す。

```haskell
fibs = listArray (1,ub) $ map fibfunc [1..ub]
  where
    ub = 10000
    fibfunc 1 = 1
    fibfunc 2 = 1
    fibfunc n = fibs ! (n-2) + fibs ! (n-1)
```

