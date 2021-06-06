---
description: bitwise operation
---

# Data.Bits

ビット操作の関数を提供する。

```haskell
import Data.Bits
```

`Int`, `Integer`, `Int8`, `Word16` など整数型は`Bit`型クラス。以下、これら対象の型を`a`とする。

```haskell
(.&.) :: a -> a -> a -- 論理積
(.|.) :: a -> a -> a -- 論理和
xor   :: a -> a -> a -- 排他的論理和
complement :: a -> a -- ビット反転

shift  :: a -> Int -> a -- シフト（正で左）
shiftL :: a -> Int -> a -- 左シフト
shiftR :: a -> Int -> a -- 右シフト

rotate  :: a -> Int -> a -- ローテート（正で左）
rotateL :: a -> Int -> a -- 左ローテート
rotateR :: a -> Int -> a -- 右ローテート

zeroBits :: a -- 全ビットが0

bit :: Int -> a -- 指定したビットのみ1な値 2^i

setBit        :: a -> Int -> a -- 指定したビットを1にする
clearBit      :: a -> Int -> a -- 指定したビットを0にする
complementBit :: a -> Int -> a -- 指定したビットを反転する

testBit :: a -> Int -> Bool -- 指定したビットが1ならTrue

popCount :: a -> Int -- 1のビットの個数
```

