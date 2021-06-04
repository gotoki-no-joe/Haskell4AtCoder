# フェニック木

固定長の数列があり、その先頭からの部分和と要素の更新を繰り返し行うときに、両方を$$O(\log n)$$で行えるデータ構造とアルゴリズム。手続き的な実装では二分木をmutableな配列に敷き詰めるが、ここでは原理に忠実に木で実装する。

## 解説

ひとつのノードは、ある$$2^m$$個の連続する要素の和と、それ未満の情報を持つ木と、続く$$2^m-1$$個の連続する要素の情報を持つ木へのリンクを持つ。

```haskell
data FWT a = FWTNull
           | FWT Int -- 管理する要素数 2^m
                 a   -- その要素の総和
                 (FWT a) (FWT a)
```

問い合わせは、$$k \; (0 \leq k < 2^{m+1})$$個の要素の和を知りたいということ。

```haskell
query :: Num a => Int -> (FWT a) -> a
```

0個の和は0に決まっている。

```haskell
query 0 _ = 0
```

$$k < 2^m$$ならば、より短い木に問い合わせを丸投げする。  
$$2^m \le k$$ならば、$$2^m$$個についてはすぐわかり、残りは続きに問い合わせる。

```haskell
query k (FWT size val lt rt)
  | k < size = query k lt
  | otherwise = val + query (k - size) rt
```

k = 0 での再帰呼び出しを避けるには、ちょうど等しいときは止めてもよい。  
ただしハナからk=0で呼び出されることもあるので、無視はできない。0の場合でひっかけるのがいやなので、Nullでひっかける。

```haskell
query k (FWT size val lt rt) =
  case compare k size of
    LT -> query k lt
    EQ -> val
    GT -> val + query (k - size) rt
query k FWTNull = if k == 0 then 0 else error "FWT nonzero Null"
```

queryの計算量は$$O(\log n)$$である。

このような木を初期値のリストから構築するには、要素数を覆える$$2^m$$を根に据えて始め、要素数$$2^n$$個に対する木を作るには、

* 前から$$2^{n-1}$$要素に対する木を作る。$$\ell = \texttt{FWT}(2^{n-1},s_1,\ell_1,\texttt{FWTNull})$$
* その続き$$2^{n-1}$$要素に対する木を作る。$$r = \texttt{FWT}(2^{n-1},s_2,\ell_2,\texttt{FWTNull})$$
* 最終的な結果はノード$$\texttt{FWT}(2^n,s_1+s_2, \texttt{FWT}(2^{n-1},s_1,\ell_1,\ell2), \texttt{FWTNull})$$
* なお、1要素$$x$$に対する木は$$\texttt{FWT}(1,x,\texttt{FWTNull},\texttt{FWTNull})$$

と再帰的に構築できる。この計算から返される木の根の右部分木は常にNullであることに注意。

```haskell
makeFWT :: Num a => [a] -> FWT a
makeFWT xs = loop n (xs ++ repeat 0)
  where
    len = length xs
    n = head $ dropWhile (len >) $ iterate (2 *) 1
    loop 1 (x:_) = FWT 1 x FWTNull FWTNull
    loop n xs = FWT n (s1+s2) (FWT n2 s1 lt1 lt2) FWTNull
      where
        n2 = div n 2
        (FWT _n2 s1 lt1 _) = loop n2 xs -- take n2 xs は不要
        (FWT _n2 s2 lt2 _) = loop n2 (drop n2 xs)
```

k番めの要素をxに変更したいとき、まず現在の値を求めるためにqueryを二度呼ぶ。そして差分を、関連するノード全てに足し込む。計算量は$$O(\log n)$$である。

```haskell
-- 値の更新は、0始まりの位置と、要素への変化量で指定する。

modifyFWT :: Num a => Int -> a -> FWT a -> FWT a
modifyFWT _ _ FWTNull = FWTNull
modifyFWT i d (FWT size val lt rt)
  | i < size  = FWT size (val+d) (modifyFWT i d lt) rt
  | otherwise = FWT size val lt (modifyFWT (i-size) d rt)

writeFWT :: Num a => Int -> a -> FWT a -> FWT a
writeFWT i v fwt = modifyFWT i (v-x) fwt
  where
    x = query (succ i) fwt - query i fwt
```

書き換え可能な配列が使えるとき、これらのノードの位置を配列の添字に落とし込むことができる。一般的にはフェニック木とはそちらのことをいう。Haskellでも可能ならmutable vectorを用いてそのような実装をした方が速いのだろう。

## 実装

完全二分木にしているのでノードから葉までの距離でそのノードのsizeは定まるため、全てのノードにsize情報を持たせる必要はなくメモリの無駄である。この点を改良して次のようにできた。

```haskell
-- @gotoki_no_joe
data FenwickTree a = FenwickTree Int     -- 幅
                                 (FWT a) -- 木

data FWT a = FWT a -- 配下の要素の和
                 (FWT a) (FWT a)

-- 先頭からk要素の総和を返す
queryFWT :: Num a => Int -> (FenwickTree a) -> a
queryFWT 0 _ = 0
queryFWT k (FenwickTree w t) = loop k w t
  where
    loop k w (FWT val lt rt) =
      case compare k w of
        LT -> loop k w2 lt
        EQ -> val
        GT -> val + loop (k - w) w2 rt
      where
        w2 = div w 2

-- 数リストに対するフェニック木を作る
makeFWT :: Num a => [a] -> FenwickTree a
makeFWT xs = FenwickTree w $ loop w (xs ++ repeat 0)
  where
    len = length xs
    w = head $ dropWhile (len >) $ iterate (2 *) 1
    loop 1 (x:_) = FWT x undefined undefined
    loop w xs = FWT (s+t) (FWT t lt1 lt2) undefined
      where
        w2 = div w 2
        (FWT t lt1 _) = loop w2 xs
        (FWT s lt2 _) = loop w2 (drop w2 xs)

-- 木の第i要素にdを足し込む
modifyFWT :: Num a => Int -> a -> FenwickTree a -> FenwickTree a
modifyFWT i d (FenwickTree w t) = FenwickTree w (loop w i d t)
  where
    loop 0 _ _ t = t -- undefined
    loop w i d (FWT val lt rt)
      | i < w = FWT (val+d) (loop w2 i d lt) rt
      | True  = FWT val lt (loop w2 (i-w) d rt)
      where
        w2 = div w 2

-- 木の第i要素をvに変更する
writeFWT :: Num a => Int -> a -> FenwickTree a -> FenwickTree a
writeFWT i v fwt = modifyFWT i (v-x) fwt
  where
    x = queryFWT (succ i) fwt - queryFWT i fwt
```

子がないことを`FWTNull`構成子で表していたところを、`undefined`で火遊びしている。

