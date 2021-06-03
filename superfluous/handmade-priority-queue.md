# 最小値を取り出す疑似ヒープ

お気楽に部分順序付き木としてのヒープで優先度付きキューを実現した例。  
Data.Mapを流用する方が手短なので取り下げて、読み物として記念に残しておく。

&lt;hr/&gt;

配列を用いたいわゆるヒープは、完全二分木であることを維持する。  
ここで示すものは、最小値を根に持つことだけが保証される。  
配列での実装は親へたどる操作が容易にできるが、immutableな木ではそれが困難なことが原因である。

```haskell
data HeapTree a = HeapLeaf | HeapNode a (HeapTree a) (HeapTree a)

-- 挿入
insertHeap :: Ord a => a -> HeapTree a -> HeapTree a
insertHeap x HeapLeaf = HeapNode x HeapLeaf HeapLeaf
insertHeap x (HeapNode v l r) = HeapNode a r (insertHeap b l)
  where
    a = min x v
    b = max x v

-- 最小値の取り出し
getHeap (HeapNode x _ _) = x

-- 根ノードの取り除き
deleteMin (HeapNode _ l r) = heapMerge l r

-- 補助関数 ２つの木の統合
heapMerge t1 HeapLeaf = t1
heapMerge HeapLeaf t2 = t2
heapMerge ta@(HeapNode a la ra) tb@(HeapNode b lb rb)
  | a < b = HeapNode a (heapMerge la ra) tb
  | True  = HeapNode b ta (heapMerge lb rb)
```

挿入は、左の木に入れてそれを右と交代させることで、なんとなくバランスをとる。

heapMergeはこれで充分。配列版では、末尾の値を抜き去り、根に上書きし、ヒープ条件を満たすように大きい方の値を下ろしていく。場合によっては途中で止まる。こちらは、正孔に電子が吸い込まれるように小さい値が登って木が再構成されていく。必ず葉まで、というか枝分かれがなくなるところまで進む。

これは素数生成のために書いたものだったので、そうするとこうなる。

```haskell
import Data.List

primes = 2 : 3 : 5 : sieve hp [7,9..]
  where
    hp = insertHeap (9,3) $ insertHeap (10,5) HeapLeaf

sieve hp xxs@(x:xs) =
  case compare q x of
    LT -> sieve hp1 xxs
    EQ -> sieve hp1 xs
    GT -> x : sieve (insertHeap (x+x,x) hp) xs
  where
    (q,p) = getHeap hp
    hp1 = insert (q+p,p) $ deleteMin hp
```

これを見ると、最小値を取り除きつつ即座に次の値を入れる操作があるので、一つにした方がいいかもだ。定数倍でよくなる。

```haskell
-- 最小値を指定の値で置き換えて、ヒープを整える
swapMin :: Ord a => a -> HeapTree a -> HeapTree a
swapMin x (HeapNode _ l HeapLeaf) = insertHeap x l
swapMin x (HeapNode _ HeapLeaf r) = insertHeap x r
swapMin x (HeapNode _ ta tb)
  | x < a && x < b = HeapNode x ta tb
  | a < b = HeapNode a (swapMin x ta) tb
  | True  = HeapNode b ta (swapMin x tb)
  where
    a = getHeap ta
    b = getHeap tb
```

そういうことでやり直すと

```haskell
import Data.List

primes = 2 : 3 : 5 : sieve hp [7,9..]
  where
    hp = insertHeap (9,3) $ insertHeap (10,5) HeapLeaf

sieve hp xxs@(x:xs) =
  case compare q x of
    LT ->     sieve (swapMin (q+p,p) hp) xxs
    EQ ->     sieve (swapMin (q+p,p) hp) xs
    GT -> x : sieve (insertHeap (x+x,x) hp) xs
  where
    (q,p) = getHeap hp
```

