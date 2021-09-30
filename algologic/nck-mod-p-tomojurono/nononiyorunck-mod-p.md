# 階乗の逆元の表によるnCk mod P

あのどこ、に書いた方法よりも洗練されている。

$${}_nC_k = n! / k ! / (n-k) !$$  
$${}_nC_k = n! \cdot (k!)^{-1} \cdot ((n-k)!)^{-1} \mod P$$

$$m=[1,n]$$について、

* 第1項のために$$f(m) = m! \mod P$$
* 逆元の計算のために $$g(m) = m^{-1} \mod P$$（これがnまででよい理由？）
* 第2,3項のために $$h(m) = (m!)^{-1} \mod P$$

を表にしておいて、これを引くことで高速に計算できると言っている。

* $$f(0) = 1, f(1) = 1, f(m) = f(m-1) \cdot m \mod P$$
* $$g(m)$$は「逆元の表を作る」の方法で作る
* $$h(m) = (m!)^{-1} = ((m-1)!)^{-1} \cdot m^{-1} = h(m-1) \cdot g(m) \mod P$$

最後の式がなんか謎。  
$$(m-1)! \times ((m-1)!)^{-1} = 1 \mod P$$  
$$m! \times (m!)^{-1} =1 \mod P$$  
つなげると  
$$m! \times (m!)^{-1} = (m-1)! \times ((m-1)!)^{-1} \mod P$$  
両辺に$$1^{-1}, 2^{-1}, \dots ,m^{-1}$$を掛けると、それぞれの逆元を階乗から消して  
$$(m!)^{-1} = m^{-1} \times ((m-1)!)^{-1} \mod P$$

```haskell
-- nCk mod P のP(素数)を固定、n(<10^7)の上限を指定
-- f n k = nCk な関数を得る
combf p n = c
  where
    c n k = (fact ! n) `mul` (factinv ! k) `mul` (factinv ! (n-k))
    mul x y = mod (x * y) p
    fact = listArray (0,n) $ scanl mul 1 [1..n]
    inv = invArray p n
    factinv = listArray (0,n) $ scanl mul 1 (elems inv)
```

$$g(m)$$は、$$h(m)$$での使い方を見ると、1からnまでしか使われないので、n! まで計算する必要がないとわかる。（すごい！）

