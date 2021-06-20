# 優先度付きキュー

出典：あのアルゴリズムはどこ？39.優先度付きキュー

優先度を付けて投入した要素を、優先度の順に取り出す入れ物。

優先度は小さいものを優先する。  
等しい優先度をもつもの同士は、投入順の逆順で取り出される。16行めの`(++)`を`flip (++)`に変えれば投入順になるが、snocになるので場合によっては性能劣化の恐れがある。

## 整数特化版

優先度が整数の場合。順序を反転させたいならば符号反転で。

```haskell
import qualified Data.IntMap as IM

-- @gotoki_no_joe
type IPQueue a = IM.IntMap [a]

-- 空のキュー
emptyIQ :: IPQueue a
emptyIQ = IM.empty

-- キューが空か
nullIQ :: IPQueue a -> Bool
nullIQ = IM.null

-- キューqに優先度pな要素xを挿入 O(1)
insertIQ :: Int -> a -> IPQueue a -> IPQueue a
insertIQ p x q = IM.insertWith (++) p [x] q

-- 空でないキューの最小要素を得る O(1)
getIQ :: IPQueue a -> (Int, a)
getIQ q = let (k,(a:_)) = IM.findMin q in (k, a)

-- 空でないキューの最小要素を取り除く O(1)
deleteIQ :: IPQueue a -> IPQueue a
deleteIQ q = IM.updateMin f q
  where
    f [_] = Nothing
    f xs  = Just $ tail xs
```

## 汎用版

優先度に`Ord`型クラスの任意の型が使える。

```haskell
import qualified Data.Map as M

-- @gotoki_no_joe
type PQueue p a = M.Map p [a]

-- 空のキュー
emptyQ :: PQueue p a
emptyQ = M.empty

-- キューが空か
nullQ :: PQueue p a -> Bool
nullQ = M.null

-- キューqに優先度pな要素xを挿入 O(log n)
insertQ :: Ord p => p -> a -> PQueue p a -> PQueue p a
insertQ p x q = M.insertWith (++) p [x] q

-- 空でないキューの最小要素を得る O(log n)
getQ :: PQueue p a -> (p,a)
getQ q = let (k,(a:_)) = M.findMin q in (k,a)

-- 空でないキューの最小要素を取り除く O(log n)
deleteQ :: PQueue p a -> PQueue p a
deleteQ q = M.updateMin f q
  where
    f [_] = Nothing
    f xs  = Just $ tail xs
```

### 追記

ここでは原理主義的に、優先度の小さい順にのみ取り出せるAPIを定義したが、Map, IntMapには最大要素側を同様に扱う関数があるので、キューの反対側からも取り出すようにできる。

