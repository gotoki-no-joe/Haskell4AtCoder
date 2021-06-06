# 二分探索

出典：あのアルゴリズムはどこ？の22.二分探索

整数列に対する単調な性質があり、満たす値と満たさない値があるとき、その境界を見つける。

第1引数は性質`p`を与える。  
第2,第3引数はそれぞれ`p(i) == False` となるような`i`と`p(j) == True`となるような`j`を与える。`p(i)`や`p(j)`は評価しないので、もう1だけ「外側」でもよい。iとjの大小関係は問わない。  
結果`(a,b)`は、`a`と`b`の差が1で、`p(a) == False`かつ`p(b) == True`となる境界である。

```haskell
-- @gotoki_no_joe
binary_search :: (Int -> Bool) -> Int -> Int -> (Int, Int)
binary_search prop unsat sat = loop unsat sat
  where
    loop a b
      | ende   = a
      | prop m = loop a m
      | True   = loop m b
      where
        ende = a == m || b == m 
        m = div (a + b) 2
```

