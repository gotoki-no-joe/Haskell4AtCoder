# 5.素数列挙（エラトステネスの篩）\(2\)

必要なものは`Data.Numbers.Primes`に全て揃っているので、Project Eulerをするなら[そちら](../library/data.numbers.primes.md)を使ったほうがよい。AtCoderにはこのライブラリがないので自作する必要がある。

```haskell
import qualified Data.IntMap as IM

-- @gotoki_no_joe
primes :: [Int]
primes = 2 : 3 : sieve q0 [5,7..]
  where
    q0 = insertIQ 6 3 IM.empty
    sieve queue xxs@(x:xs) =
      case compare np x of
        LT ->     sieve queue1 xxs
        EQ ->     sieve queue1  xs
        GT -> x : sieve queue2  xs
      where
        (np,p) = getIQ queue
        queue1 = insertIQ (np+p) p $ deleteIQ queue
        queue2 = insertIQ (x+x)  x queue
```

なお、[優先度付きキューのInt版](../routines/priority-queue.md#int-te-hua-ban)を利用している。

### 関連問題

天下一プログラマーコンテスト2012 予選C A [ACコード](https://atcoder.jp/contests/tenka1-2012-qualC/submissions/22739024)  
ABC149C [ACコード](https://atcoder.jp/contests/abc149/submissions/22738962)  
\(expert!\) ABC170 D 

### ストーリー

この実装に至るまでの蛇足は[ゴミ箱](../superfluous/eratos.md)に。

