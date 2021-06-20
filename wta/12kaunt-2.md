# 12.カウンター \(2\)

## ある特定の要素だけ数えたい

```haskell
count :: Eq a => a -> [a] -> Int
count x xs = length $ filter (x ==) xs
```

## 全ての種類について数える

### Data.Map：高い汎用性

汎用性の高いのは`Data.Map`を用いる方法。  
対象が飛び飛びの整数なら、特化した`Data.IntMap`が使える。

```haskell
import qualified Data.Map as M
import qualified Data.IntMap as IM

counts :: Ord a => [a] -> M.Map a Int
counts xs = M.fromListWith (+) $ zip xs $ repeat 1

countsi :: [Int] -> IM.IntMap Int
countsi xs = IM.fromListWith (+) $ zip xs $ repeat 1
```

### Data.Array : 連続する範囲の集団

狭い既知の範囲で連続する要素が対象なら、`Data.Array`でもできる。  
この方法は、配列の添字になる型なら、整数に限定されない。

```haskell
import Data.Array

counts :: Ix a => a -> a -> [a] -> Array a Int
counts lb ub xs = accumArray (+) 0 (lb,ub) $ zip xs $ repeat 1
```

### Data.Vector : 0始まりの自然数の範囲

0始まりの自然数範囲ならば、`Data.Vector`でもできる。

```haskell
import qualified Data.Vector as V

counts :: Int -> [Int] -> V.Vector Int
counts ub xs = V.accum (+) v0 $ zip xs $ repeat 1
  where
    v0 = V.replicate (succ ub) 0
```

## リストに出現する要素の種類数を数えたい

`Data.Map`の対域がない変種 `Data.Set`で重複を無視できる。

```haskell
import qualified Data.Set as S
import qualified Data.IntSet as IS

count :: Ord a => [a] -> Int
count = S.size . S.fromList
```

## 関連問題

yukicoder No.1468 `Data.Set`を使えばよいのでは…  
ABC118B [ACコード](https://atcoder.jp/contests/abc118/submissions/22775025) 単一カウントとVectorカウント  
ABC082C [ACコード](https://atcoder.jp/contests/abc082/submissions/22780844) `Data.IntMap`  
ABC163C [ACコード](https://atcoder.jp/contests/abc163/submissions/12154023) `Data.Array` 1始まりの整数が対象  
ABC171D [ACコード](https://atcoder.jp/contests/abc171/submissions/22781940) `Data.IntMap` 加えて、同じような計算を繰り返さない精神も重要  
AGC031 A Colorful Subsequence  
ABC181D [ACコード](https://atcoder.jp/contests/abc181/submissions/17799249) `Data.Array`  
ABC111C [ACコード](https://atcoder.jp/contests/abc111/submissions/22789263) `Data.IntMap`  
ABC052 C Factors of Factorial

