# セグメント木

一次元配列があり、その様々な区間に対して固定の演算を行った結果を何度も求めたり、配列の要素を更新したりする操作が$$O(\log n)$$で行えるデータ構造。  
ACL for Pythonの説明  
[https://github.com/shakayami/ACL-for-python/wiki/segtree](https://github.com/shakayami/ACL-for-python/wiki/segtree)  
原理の説明  
[https://algo-logic.info/segment-tree/](https://algo-logic.info/segment-tree/#:~:text=%E3%82%BB%E3%82%B0%E3%83%A1%E3%83%B3%E3%83%88%E6%9C%A8%E3%81%A8%E3%81%AF%E3%80%81%E5%AE%8C%E5%85%A8,%E9%A0%BB%E5%87%BA%E3%81%A8%E3%81%AA%E3%81%A3%E3%81%A6%E3%81%84%E3%81%BE%E3%81%99%E3%80%82)  
原理の方では、要素一つの更新でなく区間に対する更新を効率的に行う「遅延評価セグメント木」も説明されているが、ACL for Pythonにはなさそうなのと、理解が追いつかないので今後の課題とする。

## 解説

正体は完全二分木である。配列の元データは葉に記録する。節はその子の値に対して固定の演算を行った結果を持つ。つまり演算はセグメント木に対して一つである。それはモノイドの性質を持つ演算である必要があるらしい。データが$$2^n$$個に満たないとき、余った要素を埋めるためにか、モノイドの単位元も必要とされる。

フェニック木もどきのときのように、ノードが関与している要素数がノードの高さで定まる。  
データ構造には、木の本体の他に、木の幅と、モノイド演算も記録しておくこととする。

```haskell
data SegmentTree a = SegmentTree Int       -- 木の幅
                                 (a->a->a) -- 演算
                                 a         -- 単位元
                                 (STree a) -- 二分木

data STree a = Leaf a | Node a (STree a) (STree a) -- private
```

木を作るやり方はフェニック木もどきに似ている。

```haskell
makeSegTree :: [a] -> (a->a->a) -> a -> SegmentTree a
makeSegTree xs op i = SegmentTree w op i t
  where
    len = length xs
    w = head $ dropWhile (len >) $ iterate (2 *) 1
    t = loop w (xs ++ repeat i)
    loop 1 (x:_) = Leaf x
    loop w xs = mkNode op lt rt
      where
        w2 = div w 2
        lt = loop w2 xs
        rt = loop w2 $ drop w2 xs

-- utilities
getVal (Leaf x) = x
getVal (Node x _ _) = x

mkNode op lt rt = Node (getVal lt `op` getVal rt) lt rt
```

要素を更新した際、その親ノードの値も影響を受けるので更新する。  
自分の管理する要素数の半分を超える位置の要素なら右、さもなくば左に更新を伝達し、結果を受けて自分も更新する。

```haskell
updateSegTree :: SegmentTree a -> Int -> a -> SegmentTree a
updateSegTree (SegmentTree w op i t) j x = SegmentTree w op i t1
  where
    t1 = loop w j x t
    loop 1 _ x _ = Leaf x -- arg2 = 0, arg4 = Leaf oldval
    loop w j x (Node _ lt rt)
      | j < w2 = mkNode op lt1 rt
      | True   = mkNode op lt rt1
      where
        w2 = div w 2
        lt1 = loop w2 j x lt
        rt1 = loop w2 (j-w2) x rt
```

区間に対する計算結果は、その区間を覆うノードの値を集めることで求められる。区間は、下限と要素数で指定する。  
あるノードに対する問い合わせに対して、次のように振る舞うことになる。

* 下限が0、要素数が自分の幅と等しければ、自身の値が結果である。
* 上限が半分を超えなければ、左に全てを委譲する。
* 下限が半分を超えるならば、右に全てを以上する、このとき位置をずらす。
* さもなくば両方にまたがっているので、範囲を適宜調整して両方に問い合わせて、結果を結合する。

```haskell
querySegTree :: SegmentTree a -> Int -> Int -> a
querySegTree (SegmentTree w op i t) p q
  | q == 0 = i
  | True   = loop w t p q
  where
    loop w t 0 q | w == q = getVal t
    loop w (Node _ lt rt) p q
      | p+q <= w2 = loop w2 lt p q
      | w2  <= p  = loop w2 rt (p-w2) q
      | True = loop w2 lt p q1 `op` loop w2 rt 0 (q-q1)
      where
        w2 = div w 2
        q1 = w2-p
```

区間に対する更新も、この関数の振る舞いを使えばできそうだ。遅延評価はHaskellが勝手にやってくれる。

この木に対する二分探索をどう抽象化してよいかよくわからない。のでこれも保留。

## 実装

完成した実装まとめ

```haskell
-- @gotoki_no_joe
data SegmentTree a = SegmentTree Int       -- 木の幅
                                 (a->a->a) -- 演算
                                 a         -- 単位元
                                 (STree a) -- 二分木

data STree a = Leaf a | Node a (STree a) (STree a)

-- データ列、モノイド演算、単位元を元にセグメント木を作る
makeSegTree :: [a] -> (a->a->a) -> a -> SegmentTree a
makeSegTree xs op i = SegmentTree w op i t
  where
    len = length xs
    w = head $ dropWhile (len >) $ iterate (2 *) 1
    t = loop w (xs ++ repeat i)
    loop 1 (x:_) = Leaf x
    loop w xs = mkNode op lt rt
      where
        w2 = div w 2
        lt = loop w2 xs
        rt = loop w2 $ drop w2 xs

-- utilities
getVal (Leaf x) = x
getVal (Node x _ _) = x

mkNode op lt rt = Node (getVal lt `op` getVal rt) lt rt

-- 木の第j要素(0始まり)をxに変更する
updateSegTree :: SegmentTree a -> Int -> a -> SegmentTree a
updateSegTree (SegmentTree w op i t) j x = SegmentTree w op i t1
  where
    t1 = loop w j x t
    loop 1 _ x _ = Leaf x -- arg2 = 0, arg4 = Leaf oldval
    loop w j x (Node _ lt rt)
      | j < w2 = mkNode op lt1 rt
      | True   = mkNode op lt rt1
      where
        w2 = div w 2
        lt1 = loop w2 j x lt
        rt1 = loop w2 (j-w2) x rt

-- 木の第i要素から連続するj要素に対する演算結果を求める
querySegTree :: SegmentTree a -> Int -> Int -> a
querySegTree (SegmentTree w op i t) p q
  | q == 0 = i
  | True   = loop w t p q
  where
    loop w t 0 q | w == q = getVal t
    loop w (Node _ lt rt) p q
      | p+q <= w2 = loop w2 lt p q
      | w2  <= p  = loop w2 rt (p-w2) q
      | True = loop w2 lt p q1 `op` loop w2 rt 0 (q-q1)
      where
        w2 = div w 2
        q1 = w2-p
```

使えるモノイド演算の例をまとめておく。丸パクリだが。

|  | 演算 | 単位元 |
| :--- | :--- | :--- |
| 和 | \(+\) | 0 |
| 積 | \(\*\) | 1 |
| 最小値 | min | maxBound |
| 最大値 | max | minBound |
| 最大公約数 | gcd | 0 \(\*\) |
| 最小公倍数 | lcm | 1 |
| 排他的論理和 \(\*\) | xor | zeroBits |
| ビットごとの論理積 | \(.&.\) | complement zeroBits |
| ビットごとの論理和 | \(.\|.\) | zeroBits |

0は任意の数で「割り切れる」ため、0と任意の数xの最大公約数はxとなり、0はgcdの単位元といえる、という理屈のようだ。ただgcdを0まで拡張すると、0と0のgcdが「全ての素数の積」になってしまうような。

ビット演算は Data.Bits モジュールにある。

他に、多項式の積、単位元は定数式1、`\(a,b) (c,d) -> (a*c, a*d+b)` 単位元は\(1,0\)、行列積で単位元は単位行列、というものもあるらしい。

