# 4.素因数分解（試し割り法）\(1\)

素朴には、対象nと素因数候補f=2から始めて、割りきれる限り、nを商に置き換え、fを素因数として記録することを繰り返す。割り切れなくなったらfを1増やして、n &gt; f となるまで続ける。

p=2の場合だけ特別扱いすると、3以上についてfを1増やす代わりに2増やすことができる。3以上の素数は奇数だから。

```haskell
-- @gotoki_no_joe
primeFactors :: Int -> [Int]
primeFactors n = loop1 n
  where
    loop1 n
      | even n = 2 : loop1 (div n 2)
      | otherwise = loop2 n 3
    loop2 n f
      | n < f*f = [n | n /= 1]
      | otherwise =
          case divMod n f of
            (p, 0) -> f : loop2 p f
            _      ->     loop2 n (f+2)
```

### 関連問題

MojaCoder 楔数  
ABC169D [ACコード](https://atcoder.jp/contests/abc169/submissions/22775113)  
ABC177E [ACコード](https://atcoder.jp/contests/abc177/submissions/22737449)  
ABC152E \(Intの範囲でやろうとするとTLEになってしまう。IntegerとPrelude.lcmなら余裕なのだが\)

### 蛇足

* 整数平方根も使えば、毎回 n &lt; f\*f を計算しないで済む。
* 素数リストがあれば、奇数を全て試さなくても済む。

