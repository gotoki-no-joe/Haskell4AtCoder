# 動的計画法

[https://algo-logic.info/dynamic-programming/](https://algo-logic.info/dynamic-programming/)

純粋関数をメモ化するラッパーみたいなのを見たことがある気がするが、見失ったのでスルー。

フィボナッチ数列をDPするのに、メモ化でする方法は、次のような遅延評価を用いるやり方が対応するだろう。

```haskell
-- 上限を与えると、fibを計算する関数を返す
fibf ub = (arr !)
  where
    arr = listArray (1,ub) $
          1 : 1 : [arr ! (n-2) + arr ! (n-1) | n <- [3..ub]]
```

と思ったが、次に示されている、漸化式で表を埋める方法も同じ形になってしまう。

### 0/1ナップザック問題

荷物はそれぞれ重さと価値を持つ。それぞれ一つしかないので、選ぶか選ばないか。総重量の限度内で、価値を最大にする。

#### ナイーブな方法

2^n通りの探索をする。

```haskell
compute :: Int -> Int -> [[Int]] -> Int
compute n wlim vws = maximum vals
  where
    value bs = map sum $ transpose $
               map (! 1) $ filter fst $ zip bs vws
    vals = [val | bs <- bss n, let [val,w] = value bs, w <= wlim]

-- 2^n通りの探索
bss :: Int -> [[Bool]]
bss n = sequence $ replicate n [False,True]
```

こんなに行儀よく書いてしまうとここから分解することができない。vwsを先頭から消費する再帰関数として書く。

```haskell
compute n w vws = recur w vws
  where
    -- 残り容量、残りの荷物 をとり、最大価値を返す
    recur wrest ([v,w]:vws)
      | wrest >= w = max use nouse
      | otherwise  = nouse
      where
        use = v + recur (wrest - w) vws
        nouse =   recur wrest       vws
    recur wrest [] = 0
```

さらに、ここまでの価値の和を持って降りる反復型にしないといけないのかしら。

```haskell
compute n w vws = maximum $ recur 0 w vws []
  where
    -- 価値の和、残り容量、残りの荷物をとり、価値のリストを返す
    recur vsum wrest ([v,w]:vws) rest
      | wrest >= w = use nouse
      | otherwise  = nouse
      where
        use = recur (vsum + v) (wrest - w) vws
        nouse = recur vsum      wrest      vws rest
    recur vsum _ [] rest = vsum : rest
```

しゃらくさいことをやめて見通しよくしてみる。

```haskell
compute n w vws = maximum $ recur 0 w vws
  where
    -- 価値の和、残り容量、残りの荷物をとり、価値のリストを返す
    recur vsum wrest ([v,w]:vws)
      | wrest >= w = use ++ nouse
      | otherwise  = nouse
      where
        use = recur (vsum + v) (wrest - w) vws
        nouse = recur vsum      wrest      vws
    recur vsum _ [] rest = [vsum]
```

再帰呼び出しの回数は$$2^n$$になるため、大変な計算量である。

#### メモ化

引数vwsの同じものについて何度もrecurが呼び出されるが、wrest引数はさまざまに異なる。残り荷物数は0からnまですべてを取るが、残り容量は上限がかなり大きく、また全ての値をとるわけでもなくかなり疎になり、また重複するとも限らないので、これをexactな値でメモ化して、意味のあるものになるとは思えない。そもそも0は結果としてとりうる値なので、未探索のフラグとして用いるのも適当でない。

#### 工夫

メモ化と同様に、vwsの消費量と残り容量wrestを添え字とする2次元配列を考えるが、「その容量以上残して達成できる最大の価値合計」を記録することにする。\
するとこの表は、どちらの軸についても単調増加する。\
次に考える荷物の容量w、価値vのとき、表の一つ前の行vals\[]と今回作成する行vals1\[]との関係は、それぞれの容量残量wrestについて、この荷物を使わないで前の値のままと、この荷物を使うことで容量残量をw減らして価値をv増やした場合との、大きい方をとればよいので、\
`vals1[wrest] = vals[wrest] `\```max` (vals[wrest+w] + v) ``

とする。valsの初期値は全0である。また、wrest+wが上限を超える場合は選択できない。

```haskell
compute n wlim vws = valsn ! 0
  where
    vals0 = listArray (0,wlim) $ replicate (succ wlim) 0
    valsn = foldl' step vals0 vws
    step vals [w,v] = listArray (0,wlim) $ map f [0..wlim]
      where
        f wrest
          | wrest + w > wlim = vals ! wrest
          | otherwise = max (vals ! wrest) (v + vals ! (wrest + w))
```

wlimまでの配列を全て舐めるので$$O(NW)$$はかなり大きい印象があるが、$$O(2^n)$$と比較すれば圧倒的に少ない。\
しかしこの方法はwlimまでの配列が張れるという仮定がつらい。

#### 階段状の情報を保持する写像

Data.IntMapで、残容量から最大価値をひく写像を維持する。lookupGEで検索すれば、任意の残容量に対する最大価値が調べられる。この写像が表す右下がり階段状のグラフを、次の荷物(w,v)はwだけ左に、vだけ上にずらしたグラフを作る。そして両者の大きい方をなぞるようなグラフになる写像を作ることで情報が更新できる。

```haskell
compute :: Int -> Int -> [[Int]] -> Int
compute n wlim vws = snd $ fromJust $ IM.lookupGE 0 valsn
  where
    vals0 = IM.singleton wlim 0
    valsn = foldl' step vals0 vws
    step vals [v,w] = foldl' ins vals news
      where
        news =
          [ (wa1, va1)
          | (wa,va) <- IM.assocs vals
          , wa >= w, let wa1 = wa - w
          , let va1 = va + v
          , (snd $ fromJust $ IM.lookupGE wa1 vals) < va1]
        ins vals (wa1, va1) = IM.insert wa1 va1 vals
```

階段を更新するとき、元の階段に埋もれる節を追加しないという向きは処理したが、新たな段に埋もれる古い段も削除しないとおかしくなるので、stepの処理がこれでは正しくない。

直した。ABC032D [ACコード](https://atcoder.jp/contests/abc032/submissions/26245035)

```haskell
compute :: Int -> Int -> [[Int]] -> Int
compute n wlim vws = access valsn 0
  where
    vals0 = IM.singleton wlim 0
    valsn = foldl' step vals0 vws
    step vals [v,w] = IM.union vals1 vals2
      where
        vals1 = IM.fromList $ (wlim,0) :
          [ (wa1, va1)
          | (wa,va) <- IM.assocs vals
          , wa >= w, let wa1 = wa - w
          , let va1 = va + v
          , access vals wa1 < va1]
        vals2 = IM.fromList
          [ (wa,va)
          | (wa,va) <- IM.assocs vals
          , access vals1 wa < va]

access m k = snd $ fromJust $ IM.lookupGE k m
```

この方式なら、整数でない重量にも対応できる。
