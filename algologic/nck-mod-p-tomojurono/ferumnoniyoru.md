# フェルマーの小定理による逆元

こちらも引き写しになってしまう…

フェルマーの小定理：  
$$p$$を素数、$$a$$を$$p$$の倍数でない整数とすると、  
$$a^{p-1} = 1 \mod p$$  
左辺のべき乗から一つ$$a$$を取り出して  
$$a \cdot a^{p-2} = 1 \mod P$$  
$$a$$に掛けることで1になる$$a^{P-2}$$は$$a$$の逆元に他ならない。  
（両辺に$$a^{-1}$$を掛ける、でも説明できる）  
$$a^{p-2} = a^{-1} \mod P$$  
つまり、繰り返しによるべき乗を用いて$$a^{p-2} \mod P$$を求めれば逆元が得られる。

```haskell
inv p a = powerish mul 1 a (p-2)
  where
    mul x y = mod (x*y) p
```


