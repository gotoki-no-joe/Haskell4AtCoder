# ビット操作で整数平方根

はじめは平方根に限定した話にしておく。  
与えられた数$$n$$に対して、これが2進数で$$k$$桁の数であるとき、つまり  
$$n < 2^k$$  
のとき、両辺の平方根をとって  
$$\sqrt n < 2^{k/2} \leq 2^{\lceil k/2 \rceil}$$  
が成り立つ。そこで、$$b_0 = 2^{\lceil k/2 \rceil}$$を初期値として、2進数の桁ごとの探索で整数平方根を求める方法を考える。これは下限の初期値を0、上限の初期値を$$2b_0$$として二分探索を行うことと同義である。

```haskell
import Data.Bits

-- @gotoki_no_joe
intSqrt :: Int -> Either Int Int
intSqrt n =
  case compare n 0 of
    LT -> error "Negative Sqrt"
    EQ -> Right 0
    GT -> loop b0 0
  where
    b0 = shiftL 1 $ flip div 2 $ succ $
         length $ takeWhile (0 <) $ iterate (flip shiftR 1) n
    loop 0 acc = Left acc
    loop b acc =
      case compare (cand^2) n of
        LT -> loop b1 cand
        EQ -> Right cand
        GT -> loop b1 acc
      where
        cand = acc + b
        b1 = shiftR b 1
```

一般化も容易である。が、負数の奇数乗根に関する挙動は安直に実装してある。

```haskell
import Data.Bits

-- @gotoki_no_joe
intRoot :: Int -> Int -> Either Int Int
intRoot base n =
  case compare n 0 of
    LT | even base -> error "Negative Sqrt"
       | True      -> doNegate $ loop b0 0
    EQ -> Right 0
    GT -> loop b0 0
  where
    pn = abs n
    b0 = shiftL 1 $ flip div base $ (base - 1 +) $
         length $ takeWhile (0 <) $ iterate (flip shiftR 1) $ pn
    loop 0 acc = Left acc
    loop b acc =
      case compare (cand^base) pn of
        LT -> loop b1 cand
        EQ -> Right cand
        GT -> loop b1 acc
      where
        cand = acc + b
        b1 = shiftR b 1
    doNegate (Left  x) = Left  (negate x)
    doNegate (Right x) = Right (negate x)
```

