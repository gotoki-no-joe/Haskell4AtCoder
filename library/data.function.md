---
description: 小技な関数を提供する。
---

# Data.Function

```haskell
import Data.Function
```

`on`  : 二項演算の両方の引数に同じ前処理を加えた版を作る

```haskell
on :: (b -> b -> c) -> (a -> b) -> (a -> a -> c)

((+) `on` f) x y = f x + f y
```

`(&)` : `($)` の逆向き、優先度も1高い

```haskell
(&) :: a -> (a -> b) -> b

x & f = f x
```

