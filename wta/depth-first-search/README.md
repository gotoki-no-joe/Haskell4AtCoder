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

ほかに  
ABC203C [ACコード](https://atcoder.jp/contests/abc203/submissions/23390004)

### 続き

探索する必要のある状態をリストで管理しているところで `(++)` を使うのがやはり気になる。普通に再帰呼び出しで探索するとこうなる。

```haskell
dfs f i = loop i
  where
    loop x = rs ++ concatMap loop ys
      where
        (rs,ys) = f x
```

`concatMap`が現れて事態が悪化した。いつもの変形で `(++)` を除去すると

```haskell
dfs f i = loop i []
  where
    loop x rest = rs ++ foldr loop rest ys
      where
        (rs,ys) = f x
```

`concatMap` だけは除去できたが、もともと内部ではこうなってないか？

一つの状態からは一つの成功または次状態リストのみが生成される、と限定できれば、

```haskell
dfs :: (s -> Either x [s]) -> s -> [x]
dfs f i = loop i []
  where
    loop i rest =
      case f i of
        Left  r  -> r : rest
        Right ys -> foldr loop rest ys
```

として `(++)` を完全に除去できるが、幅優先探索の実装へはつながりにくくなる。

