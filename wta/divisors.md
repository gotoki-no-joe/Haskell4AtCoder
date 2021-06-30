# 3.約数列挙\(1\) \(Story\)

1から順に割ってみて、割り切れたなら除数は小さい方の約数、商は大きい方の約数。  
この作業は$$n$$に対して$$\sqrt n$$まで試せば終わるので、$$O(\sqrt n)$$でそれなりに速い。これ以上の性能を求めるなら、フェルマーの小定理を使う方法というものがあるらしい。  
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
ABC112D [ACコード](https://atcoder.jp/contests/abc112/submissions/23709880)  
ABC190D [ACコード](https://atcoder.jp/contests/abc190/submissions/23710955)  
divera2019D [ACコード](https://atcoder.jp/contests/diverta2019/submissions/23711579) 普通に上を使ったら失敗したのはなぜだろう。  
ARC108A [ACコード](https://atcoder.jp/contests/arc108/submissions/23711077)

