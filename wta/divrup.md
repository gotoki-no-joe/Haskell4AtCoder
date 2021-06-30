# 2.切り上げ除算 \(Story\)

完成品は[こちら](../snippets/integers/divrup.md)に。

## お話

自然数除算を行い、割り切れずに余りが出るときは商を1増やして切り上げた結果を求める。

整数除算の負の数における丸め方向を利用することで次のようにできるという。整数除算には丸めの方法によっていくつか種類がある、特定のものの性質を「悪用」したやり方の印象。

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

