# Pが素数でないとき

パスカルの三角形を使って事前に計算するべき、この場合メモリ的に$$n \leq 5000$$が限度とのこと。

$${}_nC_k = {}_{n-1}C_{k-1} + {}_{n-1}C_k$$  
$${}_nC_0 = {}_nC_n = 1$$  
より、一つ前の行の連続する要素を足すと次の要素が得られる。

```haskell
-- nの上限を与えると、nCk = c ! n ! k な配列が得られる
comb nmax = c
  where
    c = listArray (0,nmax) $ map cn [0..nmax]
    cn n = listArray (0,n) $ map (cnk n) [0..n]
    cnk n k
      | k == 0 = 1
      | n == k = 1
      | True = mod (c ! pred n ! pred k + c ! pred n ! k) p
```

