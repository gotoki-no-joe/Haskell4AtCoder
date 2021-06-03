# 24.累積和など \(7\)

Pythonにitertoolsというものがあるらしい。  
[https://qiita.com/anmint/items/37ca0ded5e1d360b51f3](https://qiita.com/anmint/items/37ca0ded5e1d360b51f3#%E7%B4%AF%E7%A9%8D%E5%92%8C) すこいぞitertoolsくん  
[https://docs.python.org/ja/3/library/itertools.html](https://docs.python.org/ja/3/library/itertools.html) 効率的なループ実行のためのイテレータ生成関数

> このモジュールは イテレータ を構築する部品を実装しています。プログラム言語 APL, Haskell, SML からアイデアを得ていますが、 Python に適した形に修正されています。

逆輸入する必要性はないと思うが、itertoolが競技プログラミングでどう利用できるか、という点は役に立つと思われる。

### 累積和

数列$$a_n$$に対して、前から足し合わせた値の列$$S_n = \sum_{k=1}^n a_k$$を作る。  
区間和が$$\sum_{k=i}^j a_k = S_j - S_{i-1}$$ということで$$O(1)$$で得られるようになる。

```haskell
ss = scanl1' (+) as
```

### ある値が何個あるか

並びに対して、等しいものが連続しているところをまとめる。  
それ`Data.List.group`と`Data.List.groupBy`そのもの。

### 組み合わせ

#### 順列

順列組み合わせは`Data.List.permutations`でできる。ただし出現順が微妙に謎。

与えられるリストを昇順として、辞書順に生成するには自作することになる。  
全てを使い切るのではなく、指定個とする場合も対応できる。

```haskell
-- @gotoki_no_joe
-- 順列を辞書順で
perm [] = [[]]
perm xs = [a : ys | (a,as) <- one [] xs, ys <- perm as]
-- 要素を一つ選び、残りの要素を付けて返す
one bs [] = []
one bs (a:as) = (a,rev bs as) : one (a:bs) as
-- reverse bs ++ as
rev bs as = foldl' (flip (:)) as bs

-- 個数制限
permofN 0 _ = [[]]
permofN n xs = [a : ys | (a,as) <- one [] xs, ys <- permofN (pred n) as]
```

#### 組み合わせ

```haskell
-- @gotoki_no_joe
combofN 0 _ = [[]]
combofN _ [] = []
combofN n (a:as) = [a:bs | bs <- combofN (pred n) as] ++ combofN n as
```

#### 直積

```haskell
-- @gotoki_no_joe
prodofN 0 _ = [[]]
prodofN n as = [a:bs | a <- as, bs <- prodofN (pred n) as]
```

リストのモナドだのApplicativeだのを駆使するとワンライナーで書けるみたいな話もあるけれど、自分は書けないのでこんな感じで。

「いもす法」というものの話の方がメインなのかもしれないが、またこの手のあだ名かよ、という気持ち。

### 関連問題

AGC023 A Zero-Sum Ranges - 【ACコード】  
ABC014C [ACコード](https://atcoder.jp/contests/abc014/submissions/23032199) group, いもす法?  
ABC183 D Water Heater - 【ACコード】  
ABC035 D オセロ - 【ACコード】  
ABC188 D Snuke Prime - 【ACコード】  
東京海上日動 プログラミングコンテスト2020 C Lamps - 【ACコード】  
ABC105 D Candy Distribution - 【ACコード】  
ARC100 D Equal Cut - 【ACコード】  
他に  
ABC084D \(累積和\)  
ABC122C ditto

