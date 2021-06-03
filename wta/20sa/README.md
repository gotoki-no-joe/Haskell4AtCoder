# 20.深さ優先探索

状態sから答えxが0個以上得られ、さらに探索するべき状態が0個以上作られるとき、初期状態iから深さ優先で探索する。再帰呼び出しでの `b++xs` を `xs++b` にすれば幅優先探索になるが、後ろに++するのは…

```haskell
-- @gotoki_no_joe
dfs :: (s -> ([x],[s])) -> s -> [x]
dfs f i = loop [i]
  where
    loop [] = []
    loop (x:xs) = let (a,b) = f x in a ++ loop (b ++ xs)
```

`(++)`を使いたくないなら

```haskell
-- @gotoki_no_joe
dfs :: (s -> ([x]->[x],[s]->[s])) -> s -> [x]
dfs f i = loop [i]
  where
    loop [] = []
    loop (x:xs) = let (a,b) = f x in a $ loop (b xs)
```

### 関連問題

ABC114C [ACコード](https://atcoder.jp/contests/abc114/submissions/22947417)  
ABC161D [ACコード](https://atcoder.jp/contests/abc161/submissions/11538255) 元記事ではDFSした結果をsortしてk番目を取り出しているが、リンクのようにBFS\(?\)すれば最初から昇順になるので例として適切さが見劣りする  
ABC165C [ACコード](https://atcoder.jp/contests/abc165/submissions/12627062) 探索空間が初めから既知かつ刈る枝もないので、単なるgenerate&testになっている  
パナソニックプログラミングコンテストD [ACコード](https://atcoder.jp/contests/panasonic2020/submissions/22948106)  
ABC119 C Synthetic Kadomatsu - 【ACコード】  
ABC198 E Unique Color - 【ACコード】  
ABC196 D Hanjo - 【ACコード】

