# 順列の全探索

[https://algo-logic.info/permutation/](https://algo-logic.info/permutation/)

Data.Listの

```haskell
permutations :: [a] -> [[a]]
```

で、リスト要素の並べ替えを全て生成できる。ただし、引数のリストを昇順で与えても、結果は辞書順にはならない。

Data.Vector\(.Unboxed\).Mutableの

```haskell
nextPermutation :: (PrimMonad m, Ord e) => MVector (PrimState m) e -> m Bool
```

は、C++の `next_permutation` そのものである。

辞書順に順列を生成するには次のようにすればよい。

```haskell
-- @gotoki_no_joe
perms [] = [[]]
perms xs = [y:zs | (y,ys) <- one [] xs, zs <- perms ys]
  where
    one bs (a:as) = (a, rev bs as) : one (a:bs) as
    one _  []     = []
    rev (b:bs) as = rev bs (b:as)
    rev []     as = as
```

* ABC054C \([ACコード](https://atcoder.jp/contests/abc054/submissions/26051432)\) この問題をこの文脈に置くのはテクニカルな印象がある。グラフを深さ優先探索で普通にたどる版の[ACコード](https://atcoder.jp/contests/abc054/submissions/26051537)も示しておく。
* ABC145C \([ACコード](https://atcoder.jp/contests/abc145/submissions/26052138)\) 対象が大した数ではないので素朴に計算するこの方法でもよいが、数学的直観を働かせると、Nが膨大になっても[高速に計算するやり方](https://atcoder.jp/contests/abc145/submissions/19201117)がある。
* ABC150C \([ACコード](https://atcoder.jp/contests/abc150/submissions/26052183)\) 辞書順に生成して、何番目に出てきたかを数える。しかしもっと[直接的にそれらが何番目かを求める方法](https://atcoder.jp/contests/abc150/submissions/14216099)なら、対象が多くても高速に求められる。
