# 幅優先探索

深さ優先探索では、新たに増えた探索するべき状態を、探索するべき状態列の先頭に付けたが、これを末尾に付けるキューにすれば幅優先探索となる。しかし、おそらく大量に既にある探索する状態リストの末尾に、増えた状態を連結するのはコストが高い。

Okasakiの純粋関数型キューを使う方法もあるが、遅延評価を使うとリストでも解決できる。  
loopの結果は出力情報のストリームxsと状態のストリームssのペアで、xsはbfsの結果として送り出されるが、ssは最初のloopの呼び出しにループバックしており、loopの実行により追加された状態は古いものから順にfによって消費されていく。

```haskell
-- 探索し尽くすとハングする
bfs :: (s -> ([x],[s])) -> s -> [x]
bfs f i = xs
  where
    (xs,ss) = loop (i:ss)
    loop (s:ss) = (xs1++xs2,ss1++ss2)
      where
        (xs1,ss1) = f s
        (xs2,ss2) = loop ss
```

ただしこのままでは、枝刈りなどによって状態が出尽くした場合に問題が起きる。ループバックしたssの次の要素をloop関数がパターンマッチによって求めてしまい、それは次のloopの呼び出しを引き起こし、止まらなくなる。

キューに残っている状態の数を管理することで、この問題に対処できる。

```haskell
-- @gotoki_no_joe
bfs :: (s -> ([x],[s])) -> s -> [x]
bfs f i = xs
  where
    (xs,ss) = loop 1 (i:ss)
    loop 0 _ = ([],[])
    loop n (s:ss) = (xs1++xs2,ss1++ss2)
      where
        (xs1,ss1) = f s
        (xs2,ss2) = loop (pred n + length ss1) ss
```

### 関連問題

ABC161D [ACコード](https://atcoder.jp/contests/abc161/submissions/22951386) 無理に上のbfsを使ったが、状態は無限に増えるのでキューの長さ処理は確認できていない。  
ABC114C [ACコード](https://atcoder.jp/contests/abc114/submissions/22955437) 単純にdfsをbfsに差し替えた。枝刈りも機能していると確認できた。

### 続き

ここまでして結局 `(++)` の呪縛から逃れられないなら、素直にやった方がいい気がする。

```haskell
import qualified Data.Sequence as Q

bfs2 :: (s -> ([x],[s])) -> s -> [x]
bfs2 f i = loop $ Q.singleton i
  where
    loop (Q.Empty) = []
    loop (s Q.:<| q) = xs ++ loop (foldl (Q.|>) q ss)
      where
        (xs,ss) = f s
```

