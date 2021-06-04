# 32.ダブリング \(2\)

有限状態の状態遷移が定義され、その状態遷移をかなり多くの回数行った結果について問う問題の解法。

状態$$q_1 \sim q_N$$があり、一度の状態遷移でそれぞれ$$q_k \rightarrow q_{f(k)}$$と遷移するとする。$$k$$から$$f(k)$$の対応関係を配列A\[0\] に保存する。

```haskell
arr0 = listArray (1,n) [f k | k <- [1..n]]
```

2度続けて状態遷移を行うと$$q_k \rightarrow q_{f(k)} \rightarrow q_{f^2(k)}$$へと遷移する。$$k$$から$$f^2(k)$$への対応関係は、配列A\[0\]\[k\]を2度参照することで求められる。

```haskell
arr1 = listArray (1,n) [arr0 ! (arr0 ! k) | k <- [1..n]]
```

以降は同様に、$$2^m$$回の遷移を行ったときどうなるかは$$2^{m-1}$$回の遷移を2度辿ることで、すなわち配列A\[m-1\]を2度参照して求めることができる。その結果は配列A\[m\]に保存しておく。

```haskell
arrs = iterate arr2arr arr0

arr2arr arrm = listArray (1,n) [arrm ! (arrm ! k) | k <- [1..n]]
arr2arr arrm = listArray (1,n) $ map ((arrm !).(arrm !)) [1..n]
```

初期状態$$x$$から状態遷移を$$2^n$$回行ったときの遷移先は A\[n\]\[x\] である。  
遷移回数$$k$$が2のべき乗でない場合は、2のべき乗回の遷移を組み合わせて求める。すなわち、2進数表記で1のある桁の遷移を行う。例えば5回の遷移は$$2^2$$回の遷移と$$2^0$$回の遷移の合成で求められる。

```haskell
transition arrs k x = loop arrs k x
  where
    loop _ 0 q = q
    loop (arrm:arrs) k q
      | odd k = next (arrm ! q)
      | True  = next q
      where
        next = loop arrs (div k 2)
```

### 関連問題

ABC179 E Sequence Sum - 【ACコード】

ABC013 D 阿弥陀 - 【ACコード】

ほかに

ABC167D [ACコード](https://atcoder.jp/contests/abc167/submissions/23150308) UArrayにより遷移を毎回正格評価させ、遷移表の生成と消費を同時に行っている

ABC136 D - Gathering Children



