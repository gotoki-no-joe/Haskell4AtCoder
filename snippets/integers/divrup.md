# 切り上げ除算

### コード

```haskell
-- @gotoki_no_joe
divrup a b = div (pred (a+b)) b
```

### 説明

負数を考えない自然数の除算において、`div`は余りを切り捨てる。  
これは、余りが出るときに切り上げる除算。

## お話

あのアルゴリズムはどこ？の2 より。

元ネタによると、整数除算の負の数における丸め方向を利用することで次のようにできるという。整数除算には丸めの方法によっていくつか種類がある、特定のものの性質を「悪用」したやり方の印象。

```haskell
divrup a b = negate (div (negate a) b)
```

最も愚直には、余りを見ればよい。

```haskell
divrup a b = case divMod a b of
  (p, 0) ->      p
  (p, _) -> succ p
```

被除数に除数-1を加えておくと、切り捨ての除算が切り上げに変わる。

```text
divrup a b = div (a+b-1) b
```

### 関連問題

ABC176A [ACコード](https://atcoder.jp/contests/abc176/submissions/22555330)  
ABC195B [ACコード](https://atcoder.jp/contests/abc195/submissions/22556731)  
ABC046C [ACコード](https://atcoder.jp/contests/abc046/submissions/23709081)

