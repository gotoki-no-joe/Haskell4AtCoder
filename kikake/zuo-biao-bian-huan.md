# 座標変換

（どこかに書きかけたような気もするのだが）

VectorはCの配列と同様に、0からn-1のn要素という一次元の添え字しか持たない。  
Ixクラスなら何でも添え字にできるArrayの便利さは素晴らしい。

AtCoderでよくある、$$1 \leq r \leq H, 1 \leq c \leq W$$な二次元配列を$$[0,HW-1]$$に投影する変換とその逆変換を考える。

```haskell
rc2i :: Int -> Int -> Int -> Int
rc2i w r c = pred r * w + pred c

i2rc :: Int -> Int -> (Int, Int)
i2rc w i = let (a,b) = divMod i w in (succ a, succ b)
```

Vectorとは関係ないが、$$0 \leq X, 0 \leq Y$$な全ての$$(X,Y)$$に順序を付けたいときがある。上の方法だとWが無限大になるので無理だが、斜めに数えることで可能になる。（二次元の格子点と整数の個数が同じという議論のときに出てくるやつ）

$$x+y=k$$であるような点は$$k+1$$個あるので、それより前の$$0 \leq x+y \leq k$$となる点は全部で$$\sum_{i=0}^k i = k(k+1)/2$$個ある。その続きに$$(k,0), (k-1,1), \dots ,(0,k)$$の順に数えるとすると、次のように番号が付けられる。

```haskell
xy2i :: Int -> Int -> Int
xy2i x y = let k = x + y in k * succ k `div` 2 + y
```

逆変換も考える。が、きれいな一般解はなくて、$$k(k+1)/2 \leq i < (k+1)(k+2)/2$$つまり$$k(k+1) \leq 2i < (k+1)(k+2)$$となる$$k$$を二分探索で発見し、そこから$$y = i - k(k+1)/2, x = k - y$$と求めるしかないような。

$$k^2 + k - 2i \leq 0$$, $$\displaystyle k = \frac{-1 - \sqrt{1 + 8i}}{2} \leq k \leq \frac{-1 + \sqrt{1 + 8i}}{2}$$

この範囲の最大の$$k$$を求めてもいいけれど、整数平方根を二分探索で求めるとき実質同じ計算になる。Doubleで計算すると誤差の不安がつきまとう。

