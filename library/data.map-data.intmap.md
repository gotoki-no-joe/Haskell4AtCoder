---
description: 数学の写像、いわゆる連想配列（ただしimmutable）
---

# Data.Map, Data.IntMap \(part\)

```haskell
import qualified Data.Map as M
import qualified Data.IntMap as IM
```

```haskell
data Map k a                -- k キーの型, a 値の型 → 写像の型
data IntMap a
```

#### 写像を作る

```haskell
-- 空の写像
M.empty :: Map k a
IM.empty :: IntMap a

-- 単一要素
M.singleton :: k -> a -> Map k a
M.singleton :: Int -> a -> IntMap a

-- リストから
M.fromList :: Ord k => [(k,a)] -> Map k a
IM.fromList :: [(Int,a)] -> IntMap a

-- リストから積み上げて
M.fromListWith :: Ord k => (a -> a -> a) -> [(k,a)] -> Map k a
IM.fromListWith :: (a -> a -> a) -> [(Int,a)] -> IntMap a

-- リストから積み上げて、添字も使って
M.fromListWithKey :: Ord k => (k -> a -> a -> a) -> [(k,a)] -> Map k a
IM.fromListWithKey :: (Int -> a -> a -> a) -> [(Int,a)] -> IntMap a
```

#### 写像の変更

```haskell
-- 上書き、なければ挿入
M.insert :: Ord k => k -> a -> Map k a -> Map k a
IM.insert :: Int -> a -> IntMap a -> IntMap a

-- 継ぎ足し、なければ挿入
M.insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
IM.insertWith :: (a -> a -> a) -> Key -> a -> IntMap a -> IntMap a

-- 添字付き
M.insertWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
IM.insertWithKey :: (Int -> a -> a -> a) -> Int -> a -> IntMap a -> IntMap a

-- 削除
M.delete :: Ord k => k -> Map k a -> Map k a
IM.delete :: Int -> IntMap a -> IntMap a
```

#### アクセス

```haskell
-- 対応を含む、含まない
M.member,   M.notMember :: Ord k => k -> Map k a -> Bool
IM.member, IM.notMember :: Int -> IntMap a -> Bool

-- 直接アクセス
(M.!) :: Ord k => Map k a -> k -> a
(IM.!) :: IntMap a -> Int -> a

-- Maybeつき
(M.!?) :: Ord k => Map k a -> k -> Maybe a
(IM.!?) :: IntMap a -> Int -> Maybe a

-- デフォルト値つき
M.findWithDefault :: Ord k => a -> k -> Map k a -> a
IM.findWithDefault :: a -> Int -> IntMap a -> a
```

#### 検査

```haskell
-- 空か
M.null :: Map k a -> Bool
IM.null :: IntMap a -> Bool

-- 要素数
M.size :: Map k a -> Int
IM.size :: IntMap a -> Int
```

これ以上は使った実績があったら追加することにしよう。toList くらいは必須か。

