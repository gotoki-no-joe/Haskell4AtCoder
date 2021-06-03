# 行列演算

リストのリストで行列を表すとする。

```haskell
import Data.List (transpose)

-- 内積
ip xs ys = sum $ zipWith (*) xs ys

-- 行列とベクトルの積 Av
matvprod ass xs = map (ip xs) ass

-- 行列と行列の積 AB
matprod ass bss = map (matvprod (transpose bss)) ass
```

