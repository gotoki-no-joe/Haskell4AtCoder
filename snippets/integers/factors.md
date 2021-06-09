# 約数

出典：あのアルゴリズムはどこ？の3

正整数を割り切ることのできる数を昇順に。  
与えられた数が平方数$$n^2$$のときに$$n$$は一度しか出力しない。

```haskell
-- @gotoki_no_joe
factors :: Int -> [Int]
factors 1 = [1]
factors n = 1 : loop 2 [n]
  where
    loop k us
      | k2 >  n =     us
      | k2 == n = k : us
      | q  == 0 = k : next (p:us)
      | True    =     next    us
      where
        (p,q) = divMod n k
        next = loop (succ k)
        k2 = k * k
```



