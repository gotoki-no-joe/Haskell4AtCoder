# 行列演算

オリジナルネタ。

リストでベクトル、リストのリストで行列を表すとする。内側が行。

```haskell
import Data.List (transpose)

-- 内積
ip xs ys = sum $ zipWith (*) xs ys

-- 行列とベクトルの積 Av
matvprod ass xs = map (ip xs) ass

-- 行列と行列の積 AB
matprod ass bss = map (matvprod (transpose bss)) ass

-- 3次元ベクトルの外積
vp (a1,a2,a3) (b1,b2,b3) =
  ( a2 * b3 - a3 * b2
  , a3 * b1 - a1 * b3
  , a1 * b2 - a2 * b1)
```



