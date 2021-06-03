---
description: number of inversions
---

# 30.転倒数、あるいは反転数

## お話

[https://coonevo.hatenablog.com/entry/2020/03/19/174849](https://coonevo.hatenablog.com/entry/2020/03/19/174849)  
何が説明されているのかちょっとも読み取れない。  
[https://tjkendev.github.io/procon-library/python/sequence/number\_of\_inversions.html](https://tjkendev.github.io/procon-library/python/sequence/number_of_inversions.html)  
Pythonライブラリの説明、動作の解説はない。

前者では、数列の中の昇順でない対の数、$$a_1,\dots,a_N$$に対して$$i<j$$で$$a_i > a_j$$であるような$$(i,j)$$の個数、としている。  
一方後者では「隣接要素を交換しながらソートする際に必要な交換回数」としている。  
例で確認すると両者は同じ数になるようだが、そもそもこの両者が同じ数になることがピンとこないようではこの先の戦いにはついてこれないということかもしれないと愕然とする。

[https://ikatakos.com/pot/programming\_algorithm/dynamic\_programming/inversion](https://ikatakos.com/pot/programming_algorithm/dynamic_programming/inversion)  
ここになんとか読み解ける説明があった。  
入力は任意の順序列ではなく、上限\($$ub$$未満\)有りの自然数列と限定する。  
前者の定義に従って、数えたいのは、位置$$j$$に注目したとき、それより手前にあって\($$i < j$$\)自分よりも大きなもの\($$a_i > a_j$$\)の個数である。  
これを高速に数えるには、部分数列における値$$a$$の個数を数えるフェニック木を用いる。すなわち初期値は0で、サイズは$$ub$$とする。  
数列を前から順に見ていき、$$a_j$$に注目したとき、木の$$a_j$$要素に1を加える。同時に、$$a_j$$から上限$$ub$$までの和を求める。フェニック木は先頭から指定要素までの和を返すことができるので、$$ub$$までの和から$$a_j$$までの和を引くことで得られるが、$$ub$$までの和とはそもそも現在までに注目した要素数なのでフェニック木に聞くまでもなく$$j$$である。

以上をまとめたアルゴリズムは次のようになる。

入力：0以上$$ub$$未満の自然数列（重複を許す？許さない？）$$a_0,\dots,a_N$$  
出力：転倒数  
道具：  
0から$$ub-1$$までの$$ub$$要素を持つフェニック木$$T$$、初期値は全て0  
要素0から$$k$$までの和が$$\textrm{query}(k)$$で得られ、要素kを1増やす操作を$$\textrm{incr}(k)$$とする。  
手順：  
$$j$$を$$0$$から$$N$$まで変えて、以下を繰り返し、個数の総和を得る。  
　$$a_j$$より手前にあってこれより大きい要素の個数は$$j - \textrm{query}(a_j)$$である。  
　以降の調査のために$$\textrm{incr}(a_j)$$する。

ここで  
$$\sum_{j=0}^N (j - \textrm{query}(a_j)) = \\ \sum_{j=0}^N j - \sum_{j=0}^N \textrm{query}(a_j) =$$  
$$N(N+1)/2 - \sum_{j=0}^N \textrm{query}(a_j)$$  
と変形すれば毎回引き算しなくてよいが、$$N$$が必要となるのでやめておく。  
数えなくても持っているなら使えばよい。

```haskell
-- @gotoki_no_joe
numInversions :: Int -> [Int] -> Int
numInversions ub as = sum $ loop 0 t0 as
  where
    t0 = makeFWT (replicate ub 0)
    loop _ _ [] = []
    loop j t (aj:as) = j - queryFWT aj t :
                       loop (succ j) (modifyFWT aj 1 t) as
```

こういう応用を考えると、サイズ指定初期値0のフェニック木を作る関数もあってもいいかもしれない。

さて、一般に入力が自然数列でない場合も、「その要素が小さい方から何番目か」という背の順を付けてやれば、上のバージョンで扱える問題に落とし込める。上限は要素数である。

```haskell
-- @gotoki_no_joe
inject2nat :: Ord a => [a] -> [Int]
inject2nat xs = map snd ijs
  where
    xis = sort $ zip xs [0..]
    ijs = sort $ zip (map snd xis) [0..]
```

後半はData.Arrayを使うほうが速いかも。

```haskell
-- @gotoki_no_joe
inject2nat :: Ord a => [a] -> [Int]
inject2nat xs = elems ja
  where
    n = length xs
    xis = sort $ zip xs [1..]
    ja = array (1,n) $ zip (map snd xis) [0..]
```

### 関連問題

yukicoder No.742 にゃんにゃんにゃん　猫の挨拶 - 【ACコード】  
yukicoder No.1115 二つの数列 / Two Sequences - 【ACコード】  
ABC190F [ACコード](https://atcoder.jp/contests/abc190/submissions/23145183) 典型的な例題  
ARC120 C Swaps 2 - 【ACコード】

