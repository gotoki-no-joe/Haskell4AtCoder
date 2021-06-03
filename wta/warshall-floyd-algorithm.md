# 28.ワーシャルフロイド法

グラフの全ての頂点間の距離を一度に求める。ただし$$O(V^3)$$である。

頂点は整数で1からnまで、グラフは、頂点a,b間に辺がないときRight \(\)、重みwの辺があるときLeft wを返す関数で与える。結果はArray \(\(1,1\),\(n,n\)\) で返す。

```haskell
import Data.Array
import Data.List

-- @gotoki_no_joe
warshallFloyd :: (Ord a, Num a)
              => Int                         -- 頂点数
              -> (Int -> Int -> Either a ()) -- グラフ
              -> Array (Int,Int) a           -- 距離
warshallFloyd n graph = result
  where
    range = [1..n]
    d0 = array ((1,1),(n,n)) [((i,j),graph i j) | i <- range, j <- range]
    dn = foldl' step d0 range
    result = array ((1,1),(n,n))
      [((i,j),w) | i <- range, j <- range, let Left w = dn ! (i,j)]
    step d k = d //
      [((i,j), dikj)
      | i <- range, j <- range
      , let dikj = plus (d ! (i,k)) (d ! (k,j))
      , d ! (i,j) > dikj
      ]
    plus (Left a) (Left b) = Left (a+b)
    plus _ _ = Right ()
```

pediaに

> $$i$$から$$j$$への最短経路を$$p_{i,j}$$とし、$$u$$と$$v$$を$$p_{i,j}$$上の頂点とすると、$$u$$から$$v$$への最短経路（の一つ）は$$p_{i,j}$$を$$u$$,$$v$$間に制限したものと一致する。 したがって$$p_{i,j}$$さえ知っていれば$$p_{u,v}$$を計算する必要もないし記憶する必要もない。 このことを利用すると、ワーシャル–フロイド法における計算量と記憶量を大幅に減らすことができる。

とあるその方法が具体的に書かれていない。$$O(V^3)$$から「大幅に」減らせるのなら非常に魅力的だが、どうやるのか見当もつかない。

### 関連問題

ARC079 D Wall - 【ACコード】  
ARC073 D joisino's travel - 【ACコード】  
\(expert!\) yukicoder No.1344 Typical Shortest Path Sum - 【ACコード】  
\(expert!\) ABC074 D Restoring Road Network - 【ACコード】  
他に  
AtCoder Beginner Contest 012 D - バスと避けられない運命  
まずは27にあるやつをこれでやってみたい。

ABC160Dは上のコードで見事にTLEした。mutable unboxed vectorを使えと？それでも無理そうだ。

### 蛇足

pediaのアルゴリズムでは、kを固定してi,jを全域で回してdijを更新している。このとき、計算に用いる dikとdkjは、iとjを回す途中で更新されうる。おそらく、どうせ次のループではその値になるようなもので、上のHaskell版ではimmutableに「現在のdij」を固定して次のdijを作るスタイルとは、結果は変わらず途中経過だけが違うのだろうとは思うが、もにょる。

Unboxed Mutable Vectorを用いて最も早そうな実装もしたが、それでも300頂点で実用的な速度が得られる気配がない。

