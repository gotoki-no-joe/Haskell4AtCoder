# 整数平方根

オリジナルなネタ。

$$N$$に対して$$k^2=N$$のとき`Right k`、 $$k^2 < N \land N < (k+1)^2$$のとき`Left k`を返す。

```haskell
-- @gotoki_no_joe
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

