---
description: 素数に関する機能
---

# Data.Numbers.Primes

Project Eulerをするには欠かせない。

[http://hackage.haskell.org/package/primes-0.2.1.0/docs/Data-Numbers-Primes.html](http://hackage.haskell.org/package/primes-0.2.1.0/docs/Data-Numbers-Primes.html)

#### 素数

```haskell
primes :: Integral int => [int]
```

#### 素数判定

```haskell
isPrime :: Integral int => int -> Bool
```

#### 素因数分解

```haskell
primeFactors :: Integral int => int -> [int]
```

