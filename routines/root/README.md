# 整数平方根

正確な値かどうかも添える真面目な版  
Nに対してk^2=NのときRight k, k^2 &lt; N && N &lt; \(k+1\)^2 のとき Left k

```haskell
intSqrt n
  | n < 0 = error "Negative Sqrt"
intSqrt 0 = Right 0
intSqrt 1 = Right 1
intSqrt n = loop 1 (div n 2)
  where
    loop a b
      | a+1 >= b = Left a
      | True = case compare (m*m) n of
          LT -> loop m b
          EQ -> Right m
          GT -> loop a m
      where
        m = div (a+b) 2
```

要は二分探索している。ニュートン法とかではない。

base乗に一般化した版

```haskell
intSqrt 0 = 0
intSqrt 1 = 1
intSqrt n = loop 1 (div n base)
  where
    loop a b
      | a+1 >= b = Left a
      | True = case compare (m^base) n of
          LT -> loop m b
          EQ -> Right m
          GT -> loop a m
      where
        m = div (a+b) 2
```

### 蛇足

数学的に div 2 でするよりも、ビット演算で候補を探す方がよりスマートかもしれない。

