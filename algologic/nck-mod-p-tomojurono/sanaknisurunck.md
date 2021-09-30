# 小さなkに対するnCk

「あのどこ」に書いた方法に通じる。

$${}_nC_k = \{(n-k+1) \times \dots \times n\} / k!$$  
$${}_nC_k = \{(n-k+1) \times \dots \times n \} \times 1^{-1} \times \dots \times k^{-1} \mod P$$  
を、2k回の乗算とk回の逆元計算で素朴に求める方法。

逆元計算の関数とこれだけで動くのでコードはコンパクト。

```haskell
comb p n k = a `mul` b
  where
    mul x y = mod (x * y) p
    a = foldl' mul 1 [n-k+1..n]
    b = foldl' mul 1 $ map (inv p) [1..k]
```

