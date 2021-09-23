# 2^n通りの探索

[https://algo-logic.info/rec-bit-search/](https://algo-logic.info/rec-bit-search/)

2^Nとおりの組み合わせに対する全探索のこと。

長さNの\[False,True\]の組み合わせ全てを列挙する方法

```haskell
-- bssss !! n が長さnの[Bool]
bsss = [[]] : map (\bss -> [b:bs | bs <- bss, b <- [False,True]]) bsss

-- Listモナドによる方法
bss n = sequence $ replicate n [False,True]
```

後者は、`sequence` に与えるリストの要素が do 記法の一つ一つの行に相当するので、例えば `bss 3` は

```haskell
do
  b1 <- [False,True]
  b2 <- [False,True]
  b3 <- [False,True]
  return [b1,b2,b3]
```

と同じことになる。

* ABC128C \([ACコード](https://atcoder.jp/contests/abc128/submissions/26050877)\)
* ABC147C \([ACコード](https://atcoder.jp/contests/abc147/submissions/8879426)\)
* ABC119C \([ACコード](https://atcoder.jp/contests/abc119/submissions/26061046)\) 2^n通りでなく4^n通りという派生形

