# 素数（エラトステネスの篩）

### コード

```haskell
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

### 説明

無限リストとして昇順に素数が取り出せる。  
内部で[優先度付きキューのInt版](../../routines/priority-queue.md#int-te-hua-ban)を利用しており、そちらも入れる必要がある。

## お話

あのアルゴリズムはどこ？の5 より。

必要なものは`Data.Numbers.Primes`に全て揃っているので、Project Eulerをするなら[そちら](../../library/data.numbers.primes.md)を使ったほうがよい。AtCoderにはこのライブラリがないので[自作する](eratosutenesuno.md)必要がある。

[最終版に至るまでのお話](../../story/eratosutenesuno.md)を別にまとめた。

### 関連問題

天下一プログラマーコンテスト2012 予選C A [ACコード](https://atcoder.jp/contests/tenka1-2012-qualC/submissions/22739024)  
ABC149C [ACコード](https://atcoder.jp/contests/abc149/submissions/22738962)  
ABC170D もう一方のページで解けた。が。

