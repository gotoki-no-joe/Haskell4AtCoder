---
description: 数学的集合
---

# Data.Set, Data.IntSet \(part\)

```haskell
import qualified Data.Set as S
import qualified Data.IntSet as IS
```

```haskell
data Set a    -- a 要素の型
data IntSet   -- 要素はInt
```

#### 集合を作る

```haskell
-- 空集合
S.empty :: Set a
IS.empty :: IntSet

-- 一点集合
S.singleton :: a -> Set a
IS.singleton :: Int -> IntSet

-- リストから
S.fromList :: [a] -> Set a
IS.fromList :: [Int] -> IntSet

-- 冪集合
S.powerSet :: Set a -> Set (Set a)
```

#### 変更

```haskell
-- 挿入
S.insert :: Ord a => a -> Set a -> Set a
IS.insert :: Int -> IntSet -> IntSet

-- 削除
S.delete :: Ord a => a -> Set a -> Set a
IS.delete :: Int -> IntSet -> IntSet
```

#### アクセス

```haskell
-- 要素を含む、含まない
S.member,   S.notMember :: Ord a => a -> Set a -> Bool
IS.member, IS.notMember :: Int -> IntSet -> Bool
```

性質

```haskell
-- 空集合
S.null :: Set a -> Bool
IS.null :: IntSet -> Bool

-- 要素数
S.size :: Set a -> Int
IS.size :: IntSet -> Int

-- 包含
S.isSubsetOf :: Ord a => Set a -> Set a -> Bool
IS.isSubsetOf :: IntSet -> IntSet -> Bool

-- 真に包含
S.isProperSubsetOf :: Ord a => Set a -> Set a -> Bool
IS.isProperSubsetOf :: IntSet -> IntSet -> Bool

-- 疎
S.disjoint :: Ord a => Set a -> Set a -> Bool
IS.disjoint :: IntSet -> IntSet -> Bool
```

その他いろいろ

