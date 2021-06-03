# エラトステネスの篩に至る道

自己満足なお話を残しておく。

&lt;hr/&gt;

TL;DR 最終版は一番下の`primes5`である。

エラトステネスの篩はmutableな配列を前提としたアルゴリズムなので、Haskellとの相性はきわめてよくない。その本質は、新たに素数pが見つかったとき、全てのpの倍数npを候補から除くことにある。

現在の値と、その値以降がまだ候補かどうかの真理値リストを管理し、素数pが見つかったとき、リストに対して長さpのリスト \[True, ..., True, False\] を cycle したものを zipWith \(&&\) することで篩の更新を模倣できる。これはシンプルであるが効率はすぐに悪化する。

```haskell
primes1 :: [Int]
primes1 = 2 : loop 3 (cycle [True, False])
  where
    loop k (False:bs) =     loop (succ k) bs
    loop p (True :bs) = p : loop (succ p) bs1
      where
        bs1 = zipWith (&&) bs $ tail $ cycle $ False : replicate (pred p) True
```

Haskellの教科書によくあるやり方は、新たに素数pが見つかったとき、pで割り切れない数だけ残すフィルタ ``filter (\k -> k `mod` p /= 0)``を追加するものである。エラトステネスの篩の肝はpでの除算をしないでpの倍数だけを考えることにあるのでこれでは台無しである。

```haskell
primes2 :: [Int]
primes2 = 2 : sieve [3,5..]
  where
    sieve (p:ns) = p : sieve (filter ((0 /=).(flip mod p)) ns)
```

次に消すべき値npと、その元になったpを保持して、npに遭遇した場合はそれを消し、次に消すべき値を\(n+1\)pに更新する、状態を持ったフィルタで、modなしに実現できる。

```haskell
primes3 :: [Int]
primes3 = 2 : sieve [3,5..]
  where
    sieve (p:ps) = p : sieve (sv (p+p) p ps)
    sv np p xxs@(x:xs) =
      case compare np x of
        LT ->     sv (np+p) p xxs
        EQ ->     sv (np+p) p  xs
        GT -> x : sv  np    p  xs
```

npとpを引数に持つフィルタsvのサンクを重ねる代わりに、\(np,p\)という対の優先度付きキューを管理すると、サンクの肥大化を避けることができる。（svは終わらないが、insertは目的を達成したら終わる。）

```haskell
import Data.List (insert)

primes4 :: [Int]
primes4 = 2 : 3 : sieve [(9,3)] [5,7..]
  where
    sieve qpqps@((q,p):qps) xxs@(x:xs) =
      case compare q x of
        LT ->     sieve qps1 xxs
        EQ ->     sieve qps1  xs
        GT -> x : sieve qps2  xs
      where
        qps1 = insert (q+p,p)   qps
        qps2 = insert (x+x,x) qpqps
```

優先度付きキューをData.List.insertで実現するのは手抜きなので、部分順序付き木（バランスを無視したヒープ）を用いて実現する。

```haskell
primes5 :: [Int]
primes5 = 2 : 3 : sieve hp0 [5,7..]
  where
    hp0 = insertPOT (6,3) POTLeaf
    sieve hp xxs@(x:xs) =
      case compare q x of
        LT ->     sieve hp1 xxs
        EQ ->     sieve hp1  xs
        GT -> x : sieve hp2  xs
      where
        (q,p) = getPOT hp
        hp1 = swapMin (q+p,p) hp
        hp2 = insertPOT (x+x,x) hp

-- 部分順序付き木
data POT a = POTLeaf | POTNode a (POT a) (POT a)

insertPOT :: Ord a => a -> POT a -> POT a
insertPOT x POTLeaf = POTNode x POTLeaf POTLeaf
insertPOT x (POTNode v l r) = POTNode a r (insertPOT b l)
  where
    a = min x v
    b = max x v

getPOT (POTNode x _ _) = x

swapMin :: Ord a => a -> POT a -> POT a
swapMin x (POTNode _ l POTLeaf) = insertPOT x l
swapMin x (POTNode _ POTLeaf r) = insertPOT x r
swapMin x (POTNode _ ta tb)
  | x < a && x < b = POTNode x ta tb
  | a < b          = POTNode a (swapMin x ta) tb
  | otherwise      = POTNode b ta (swapMin x tb)
  where
    a = getPOT ta
    b = getPOT tb
```

