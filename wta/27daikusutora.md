# 27.ダイクストラ法

グラフにおいて、辺の重みが非負であるとき、指定した開始点から他の全ての頂点間の経路で重みの和が最小のものを探すアルゴリズム。重みを辺の長さとすれば「最短距離」と呼べる。

[Wikipedia](https://ja.wikipedia.org/wiki/%E3%83%80%E3%82%A4%E3%82%AF%E3%82%B9%E3%83%88%E3%83%A9%E6%B3%95)にある、優先度付きキューを用いたアルゴリズムを読み解く。

入力：グラフの頂点集合V、開始点s、頂点u,v間の辺の重みlength\(u,v\)

```text
// 初期化
for (v ← V)
  d(v) ← (v = s ならば 0、ソレ以外は∞)
  prev(v) ← 「無し」
  Q(v) ← d(v)

// 本計算
while (Qが空集合でない)
  QからQ(u)が最小である頂点uを取り出す
  foreach (uからの辺があるv ∈ V)
    alt ← d(u) + length(u,v)
    if (d(v) > alt)
      d(v) ← alt
      prev(v) ← u
      Q(v) ← alt
```

出力：開始点から各頂点vまでの最短距離d\(v\)、最短距離の経路の、vの前の頂点prev\(v\)

Qは、まだ距離が確定していない、処理するべき頂点の集合。

d\(v\)は、頂点に対して、探索済み頂点からもう一歩までで到達できる経路における既知の最短距離を割り当てる、配列的なもの。更新操作ができる。最終的な結果の中核。  
より小さい値へと行12～13で更新されていく初期値として、行3で∞を与えられている。これをmaxBoundで処理するとダサいので、dはMapとし、写像に含まれないとき最短距離が不明であることを表すとしよう。

prev\(v\)は、頂点に対して、既知の最短距離になるsからの経路で、vの一つ前の頂点。過去の値を参照することはなく行14で上書き更新されるだけなので、これも普通にMapで扱える。

Q\(v\)は距離を優先度として頂点を格納する優先度付きキューである。行9で、Qに含まれるvの中でd\(v\)が最小なものを、d\(v\)をスキャンせずに取り出すために用いられる。未完了の頂点vについて、d\(v\)とQ\(v\)は常に同じ値を持つ。（行13と15で同じaltで更新されるから）  
その中で最小のuが行9で1つずつ選択されては処理完了とされ、以降の更新から除外される。  
さて、行15の Q\(v\) ← alt とは何だろうか。優先度付きキューに値を登録するには優先度と値が必要で、それはキューへの追加となる。Q\(v\) ← alt とは、「キューに既に異なる（より大きい優先度で）登録されているかもしれないvがあればそれは忘れて、優先度altでvを登録する」という、優先度付きキューが持たない操作をしれっと書いている。とんでもない。

この問題への対処として、「異なる優先度でもvが登録されているかもしれないけれど構わずに、altでもvを重複を無視して登録してしまう」という方法がとられているようだ。  
特定のvに注目してどうなるか考えると、行15で何度かvがQに登録され、あるタイミングで、最小の優先度を持つとして行9でその輪廻を抜ける。すると、Qから外れた頂点uについて、Qにまだ異なる優先度で登録が残っている可能性もあるが、それは決定されたd\(u\)の値よりも大きな距離で登録された無駄な項目であるとして無視できる。  
この判断をするには、キューに登録した際のそれぞれの優先度が必要である。上のアルゴリズムではそれは使っていないが、要素とともに優先度も取り出せるキューの実装を用いると、逆に、未探索の頂点集合Qを管理しなくてもよくなる。

という考察に基づいて、Haskell実装を行う。

重みは0, \(+\), \(&gt;\) が必要なのでNum（とOrd）である必要がある。  
頂点はd\(v\)とpred\(v\)でMapのキーとして用いるので、EqかつOrdである必要がある。  
競技プログラミング限定だと、どちらもIntでよい場合も多そうで、その場合IntMapで高速化できる。ということで2バージョン書いておく。

グラフそのものは、データ構造で与える代わりに、頂点を渡されたときに隣接する頂点とその辺の重みの対のリストを返す関数として与えることとする。この関数が様々なグラフの表現に対応するためのアダプタとして機能する。  
キューは[優先度付きキュー](../routines/priority-queue.md)の項にあるもの。

```haskell
-- @gotoki_no_joe
dijkstra :: (Eq v, Ord v, Ord w, Num w) -- v 頂点 w 辺の重み
         => (v -> [(v,w)])              -- 隣接頂点とその辺の重み、グラフの情報
         -> v                           -- 開始点
         -> (M.Map v w, M.Map v v)      -- 最短経路の重み、最短経路の前の頂点 (routeに渡すもの)
dijkstra graph start = loop queue0 dee0 prev0
  where
    -- 開始点から点vまでの既知の最短距離 d(v) 初期値は d(start) = 0 その他は∞
    dee0 = M.singleton start 0
    -- 既知の最短経路において、vからstartに一つ戻る頂点 prev(v) 初期値は何もなし
    prev0 = M.empty
    -- 探索済み頂点から到達できる頂点のキュー、既知の最小コストが優先度
    queue0 = insertQ 0 start emptyQ
    -- メインループ
    loop queue dee prev
    -- キューが空になったら完了
      | nullQ queue = (dee, prev)
    -- キューの情報が上書き済みならスルーして次へ
      | du < cost = loop q' dee prev
    -- 本題
      | otherwise = loop queue1 dee1 prev1
      where
        ((cost,u),q') = (getQ queue, deleteQ queue)
        du = dee M.! u
        -- uから到達できる全てのvについて
        -- u経由でvに到達する経路のコスト du+l=d1 が、既知のコスト dee M.! v を下回っているものについて
        -- d(v)をd1に、prev(v)をuに更新し、Qのd1にvを登録する
        vds = [(v,d1) | (v,l) <- graph u, let d1 = du + l, M.notMember v dee || d1 < dee M.! v]
        dee1  = M.union (M.fromList vds) dee
        prev1 = M.union (M.fromList [(v,u) | (v,_) <- vds]) prev
        queue1 = foldl (\q (v,d) -> insertQ d v q) q' vds
```

prevから最短経路を構築できる。不要ならそもそもprevを作らないでもいいが、遅延評価なら触らなければprevはthunkのままで一切計算されないだろう。

```haskell
-- prevとstartとvをとり、startからvの系列をリストにする
route prev start v = loop [] v
  where
    loop acc u
      | u == start = u:acc
      | otherwise = loop (u:acc) (prev M.! u)
```

Int限定版。その中間の、片方だけIntな版がもう2つ考えられるが、ゴチャるので勘弁。

```haskell
-- @gotoki_no_joe
dijkstraI :: (Int -> [(Int,Int)])           -- 隣接頂点とその辺の重み、グラフの情報
          -> Int                            -- 開始点
          -> (IM.IntMap Int, IM.IntMap Int) -- 最短経路の重み、最短経路の前の頂点 (routeIに渡すもの)
dijkstraI graph start = loop queue0 dee0 prev0
  where
    dee0 = IM.singleton start 0
    prev0 = IM.empty
    queue0 = insertIQ 0 start emptyIQ
    loop queue dee prev
      | nullIQ queue = (dee, prev)
      | du < cost = loop q' dee prev
      | otherwise = loop queue1 dee1 prev1
      where
        ((cost,u),q') = (getIQ queue, deleteIQ queue)
        du = dee IM.! u
        vds = [(v,d1) | (v,l) <- graph u, let d1 = du + l, IM.notMember v dee || d1 < dee IM.! v]
        dee1  = IM.union (IM.fromList vds) dee
        prev1 = IM.union (IM.fromList [(v,u) | (v,_) <- vds]) prev
        queue1 = foldl (\q (v,d) -> insertIQ d v q) q' vds

routeI prev start v = loop [] v
  where
    loop acc u
      | u == start = u:acc
      | otherwise = loop (u:acc) (prev IM.! u)
```

### 関連問題

ARC109 A Hands - 【ACコード】  
ABC160D [ACコード](https://atcoder.jp/contests/abc160/submissions/23109951) 全ての頂点についてダイクストラ法をやりなおすアプローチでACしたが、これはWarshall-Floyd法の出番では？\(todo\)  
ABC191 E Come Back Quickly - 【ACコード】  
\(expert!\) MojaCoder As Soon As Possible - 【ACコード】  
\(expert!\) ABC192 E Train - 【ACコード】  
\(expert!※\) yukicoder No.807 umg tours - 【ACコード】  
\(expert!※\)ZONeエナジー プログラミングコンテスト E 潜入 - 【ACコード経路復元あり】【ACコード経路復元なし】

