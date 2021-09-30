# 逆元の表を作る

いろいろな数に対して逆元を求める必要があるとき、表を用いて高速化できるという話。

これも引き写し…

Pをaで割った商をq,余りをrとする。  
$$a \times q + r = P \; (0 \leq r < a)$$  
mod Pで考えると  
$$qa + r = 0 \mod P, \; r = -qa \mod P$$  
両辺に$$a^{-1} \cdot r^{-1}$$を掛けると  
$$a^{-1} = -q \cdot r ^{-1} \mod P$$

結局、aの逆元は、Pをaで割った余りrの逆元に商を掛けたものから求められる。  
これをDPな表にしておくと速く求められる。  
といっても要素数をPにするのはおそらく現実的ではない。

```haskell
-- 素数p、1からnまでの逆元のArrayを作る
invArray p n = inv
  where
    inv = listArray (1,n) $ 1 : map f [2..n]
    f a = let (q,r) = divMod p a in mod (negate q * inv ! r) p
```

余りは除数よりも小さい$$(r < a)$$ので、表は前から計算すれば未確定な場所を踏むことはない。

```haskell
-- 素数p、1からnまでの逆元のVectorを作る(0は空き)
invVector p n = V.create (do
  let n1 = succ n
  v <- MV.new n1
  MV.write v 1 1
  forM_ [2..n] (\a -> do
    let (q,r) = divMod p a
    rinv <- MV.read v r
    let ainv = mod (negate q * rinv) p
    MV.write v a ainv
    )
  return v
  )
```

続きで二項係数を求める際には、任意の数の逆元ではなく階乗の逆元のみを使うので、この表は出番がないようだ。

