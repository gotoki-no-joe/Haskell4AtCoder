# 2.切り上げ除算 \(1\) -&gt; story

自然数除算を行い、割り切れずに余りが出るときは商を1増やして切り上げた結果を求める。

整数除算の負の数における丸め方向を利用することで次のようにできる。

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
ABC046C \(まだ解けていない\)

