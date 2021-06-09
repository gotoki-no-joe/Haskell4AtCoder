# 素数（エラトステネスの篩）

出典：あのアルゴリズムはどこ？の5

無限リストとして昇順に素数が取り出せる。  
内部で[優先度付きキューのInt版](../../routines/priority-queue.md#int-te-hua-ban)を利用している。

```haskell
-- @gotoki_no_joe
primes :: [Int]
primes = 2 : 3 : sieve q0 [5,7..]
  where
    q0 = insertIQ 6 3 IM.empty
    sieve queue xxs@(x:xs) =
      case compare np x of
        LT ->     sieve queue1 xxs
        EQ ->     sieve queue1  xs
        GT -> x : sieve queue2  xs
      where
        (np,p) = getIQ queue
        queue1 = insertIQ (np+p) p $ deleteIQ queue
        queue2 = insertIQ (x+x)  x queue
```

