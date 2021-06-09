# 3.約数列挙 \(4\) \(Story\)

1から順に割ってみて、割り切れたなら除数は小さい方の約数、商は大きい方の約数。  
この作業は$$n$$に対して$$\sqrt n$$まで試せば終わるので、$$O(\sqrt n)$$で十分に速い。  
$$n$$が平方数のときに2度列挙しないように少し工夫してある。

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

### 関連問題

ABC180C [ACコード](https://atcoder.jp/contests/abc180/submissions/22727220)  
MojaCoder Polygon of Polygons  
ABC112D  
ABC190D  
divera2019D  
ARC108A

