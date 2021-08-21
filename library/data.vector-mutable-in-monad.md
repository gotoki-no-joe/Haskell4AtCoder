---
description: 命令型言語の配列そのもの。整数添字でmutable
---

# Data.Vector.Mutable

同じインタフェースでUnboxedな（正格でより速い）ベクタを提供する`Data.Vector.Unboxed.Mutable`もある。

[https://hackage.haskell.org/package/vector-0.12.3.0/docs/Data-Vector-Mutable.html](https://hackage.haskell.org/package/vector-0.12.3.0/docs/Data-Vector-Mutable.html)

```haskell
import Data.Vector.Mutable as MV

import Data.Vector.Unboxed.Mutable as MV
```

```haskell
data MVector s a

type IOVector = MVector RealWorld
```

#### 大きさ

```haskell
-- 長さ
MV.length :: MVector s a -> Int

-- 空か
null :: MVector s a -> Bool
```

#### リストから類推できる操作

```haskell
MV.init :: MVector s a -> MVector s a
MV.tail :: MVector s a -> MVector s a
MV.take :: Int -> MVector s a -> MVector s a
MV.drop :: Int -> MVector s a -> MVector s a
MV.splitAt :: Int -> MVector s a -> (MVector s a, MVector s a)
```

一般化：`MV.slice i n v` はベクタvの添字iからn要素のみを切り出したベクタ

```haskell
MV.slice :: Int	-> Int -> MVector s a -> MVector s a
```

#### アクセス（モナドアクション）

```haskell
-- 1要素読む
MV.read :: PrimMonad m => MVector (PrimState m) a -> Int -> m a

-- 1要素書き込む
MV.write :: PrimMonad m => MVector (PrimState m) a -> Int -> a -> m ()

-- 1要素を関数またはモナドアクションで変更
MV.modify  :: PrimMonad m => MVector (PrimState m) a -> (a ->   a) -> Int -> m ()
MV.modifyM :: PrimMonad m => MVector (PrimState m) a -> (a -> m a) -> Int -> m () 

-- ２つの要素を入れ替え
MV.swap :: PrimMonad m => MVector (PrimState m) a -> Int -> Int -> m ()

-- 1要素を書き込みつつ、古い値を返す
MV.exchange :: PrimMonad m => MVector (PrimState m) a -> Int -> a -> m a
```

#### ベクタの作成（アクション）

```haskell
-- 指定長、初期値不定？
MV.new :: PrimMonad m => Int -> m (MVector (PrimState m) a)

-- 全要素に同じ値を書き込み
MV.set :: PrimMonad m => MVector (PrimState m) a -> a -> m ()
```

#### リストから類推できる生成（アクション）

```haskell
-- n要素の連続するaまたはn回のアクションの結果
MV.replicate  :: PrimMonad m => Int ->   a -> m (MVector (PrimState m) a)
MV.replicateM :: PrimMonad m => Int -> m a -> m (MVector (PrimState m) a)

-- n要素を関数またはアクションで生成
MV.generate  :: PrimMonad m => Int -> (Int ->   a) -> m (MVector (PrimState m) a)
MV.generateM :: PrimMonad m => Int -> (Int -> m a) -> m (MVector (PrimState m) a)
```

#### リストから類推できる高度な操作

```haskell
MV.mapM_  :: (PrimMonad m, Unbox a) => (       a -> m b) -> MVector (PrimState m) a -> m ()
MV.imapM_ :: (PrimMonad m, Unbox a) => (Int -> a -> m b) -> MVector (PrimState m) a -> m ()
MV.forM_  :: (PrimMonad m, Unbox a) => MVector (PrimState m) a -> (       a -> m b) -> m ()
MV.iforM_ :: (PrimMonad m, Unbox a) => MVector (PrimState m) a -> (Int -> a -> m b) -> m (
-- fold系色々
```

#### その他

```haskell
-- コピーを作る
MV.clone :: PrimMonad m => MVector (PrimState m) a -> m (MVector (PrimState m) a)
-- 要素を追加する
MV.grow :: PrimMonad m => MVector (PrimState m) a -> Int -> m (MVector (PrimState m) a
-- サンクを捨てて内容を不定にする
MV.clear :: PrimMonad m => MVector (PrimState m) a -> m ()
-- 順列の次を上書きで作る
nextPermutation :: (PrimMonad m, Ord e) => MVector (PrimState m) e -> m Bool
```

