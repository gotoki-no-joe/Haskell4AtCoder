# 素数判定

[https://algo-logic.info/is-prime/](https://algo-logic.info/is-prime/)

2から$$\lfloor \sqrt{n} \rfloor$$までどれでも割り切れなければ素数、という普通のアルゴリズムと、上限Nまでのエラトステネスの篩を作っておいて判定、という二つの方法が紹介されている。

前者については[素数判定](../snippets/integers/eratosutenesuno.md)で実装済み。ただし優先度付きキューが古いので直すべき。

後者について、`Data.Vector` を用いる方法が最近浮上したのでここに書いておく。というか [Data.Vector のページ](../library/data.vector.md)に例として書いてあった。見かけ上pureな計算で上限nのエラトステネスの篩を構築できる。

```haskell
primev = V.create (do
  v <- MV.replicate (n+1) True
  forM_ [2..n] (\i -> do
    f <- MV.read v i
    when f (forM_ [i*2,i*3..n] (\j -> MV.write v j False)))
  return v
  )
```

* ABC170D \([ACコード](https://atcoder.jp/contests/abc170/submissions/26063071)\) 「エラトステネスの篩」の応用問題であって、「素数判定」の問題ではないので分類が微妙では。

