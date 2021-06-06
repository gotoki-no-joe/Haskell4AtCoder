# 優先度付きキュー \(-&gt;story\)

標準ライブラリの `Data.Map`, `Data.IntMap` は、「キーの最小値を見つけ、キーと割り当てられた値を取り出し、またその対を削除した写像を作る」までをする関数

```haskell
deleteFindMin :: Map k a -> ((k, a), Map k a) -- O(log n)
deleteFindMin :: IntMap a -> ((Key, a), IntMap a) -- O(min n W) ≒ O(1)
```

がある。Mapの方は要素数$$n$$ に対して$$O(\log n)$$である。IntMapの方は要素数$$n$$とIntのビット幅$$W (= 64)$$に対して$$O(\min(n,W))$$で、これは実質$$O(1)$$といえる。

優先度付きキューとは、要素に優先度を付けて投入すると、優先度の順に取り出せるコンテナである。アルゴリズムとデータ構造の教科書的にはヒープを用いて実装するものであるが、`deleteFindMin`を悪用した手抜きの実現について述べる。

つまり、Mapのキーを優先度とし、内容を値とする。異なる値が同じ優先度を持つ場合がありうるので、内容は値のリストとする。

### 汎用版

優先度の型に自由度のある汎用版は、Mapを利用する。

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

-- キューqに優先度pな要素xを挿入
insertQ :: Ord p => p -> a -> PQueue p a -> PQueue p a -- O(log n)
insertQ p x q = M.insertWith (++) p [x] q

-- 空でないキューの最小要素を得る
getQ :: PQueue p a -> (p,a) -- O(log n)
getQ q = let (k,(a:_)) = M.findMin q in (k,a)

-- 空でないキューの最小要素を取り除く
deleteQ :: PQueue p a -> PQueue p a -- O(log n)
deleteQ q = M.updateMin f q
  where
    f [_] = Nothing
    f xs  = Just $ tail xs
```

### Int特化版

優先度がIntである場合はIntMapを用いて高速にできる。

```haskell
import qualified Data.IntMap as IM

-- @gotoki_no_joe
type IPQueue a = IM.IntMap [a]

emptyIQ :: IPQueue a
emptyIQ = IM.empty

nullIQ :: IPQueue a -> Bool
nullIQ = IM.null

insertIQ :: Int -> a -> IPQueue a -> IPQueue a -- O(1)
insertIQ p x q = IM.insertWith (++) p [x] q

getIQ :: IPQueue a -> (Int, a) -- O(1)
getIQ q = let (k,(a:_)) = IM.findMin q in (k, a)

deleteIQ :: IPQueue a -> IPQueue a -- O(1)
deleteIQ q = IM.updateMin f q
  where
    f [_] = Nothing
    f xs  = Just $ tail xs
```

### 注意

等しい優先度をもつ値を、スタックをなすリストで格納したため、挿入した順序と逆順に取り出される。これを嫌う場合はinsertWithでsnocを使うなどしてFIFOキューにする必要がある。

また、優先度に重複がないことが保証されるなら、優先度と値を直接Mapで対応付けてしまえばよい。

