# nが大きい場合

前の方法では表を構築したり保存したりするために、$$n \leq 10^7$$ぐらいまでしか対応できない。それよりもnが大きい（ただし $$n \leq 10^9$$ぐらいまで）とき、そして$$k \leq 10^7$$辺りまでの場合、メモリに収まる表の立て方があるという話。

### 異なるnについて求める必要がある場合

$${}_nC_k = n! / k! / (n-k)! = ((n-k+1) \times \dots \times (n-1) \times n) / k!$$  
より  
$${}_nC_k = \{ (n-k+1) \times \dots \times n \} \times (k!)^{-1} \mod P$$  
となるので、前の方法と同様に逆元の表を作っておくと後半は$$O(1)$$で、前半は$$O(k)$$で求めることができる。

```haskell
-- nCk mod P のP(素数)を固定、kの上限(<10^7)を設定
-- f n k = nCk を計算する関数を得る
combf p k = c
  where
    mul x y = mod (x * y) p
    c n k = foldl' mul 1 [n-k+1..n] `mul` (factinv ! k)
    inv = invArray p k
    factinv = listArray (0,k) $ scanl mul 1 (elems inv)
```

### nが固定値の場合

$${}_nC_k = {}_nC_{k-1} \times (n-k+1)/k$$の関係を用いて、  
$$f(k) = {}_nC_k \mod P, \; f(0) = 1, \; f(k) = f(k-1)  \times (n-k+1) \times k^{-1}$$  
の表を作っておくことかできる。

```haskell
-- nCk mod PのP(素数)とnを固定、kの上限(<10^7)を設定
-- f k = nCk を計算する関数を得る
combf p n k = (arr !)
  where
    inv = invArray p k
    fs = 1 : zipWith3 m3 fs [n,n-1..] (elems inv)
    m3 x y z = x `mul` y `mul` z
    mul x y = mod (x*y) p
    arr = listArray (0,k) fs
```

```haskell
-- こうしないと局所環境にリストfsが残るか？
combf p n k = (arr !)
  where
    inv = invArray p k
    mul x y = mod (x*y) p
    arr = listArray (0,k) $ map f [0..k]
    f 0 = 1
    f k = (arr ! pred k) `mul` (n-k+1) `mul` (inv ! k)
```

