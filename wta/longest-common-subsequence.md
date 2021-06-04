---
description: Longest Common Subsequence
---

# 33.最長共通部分列

２つの（文字）列が与えられたとき、同じ長さでそれぞれの部分列を取り出して等しい文字列を作ったときの最大の長さ、あるいはそのような部分列を求める問題。

## お話

### 素朴すぎる実現

あえて素朴なアルゴリズムから話を始める。簡単のため列の長さのみを求める。  
２つの文字列`as`, `bs`の最長共通部分列の長さ`lcs as bs`を求める。  
いずれかの列が空列であれば、答えは0である。

```haskell
lcs _ [] = 0
lcs [] _ = 0
```

両方の列の先頭の文字が異なるものであるとき、いずれかを除いた組に対する長さふたつのうち、大きいほうが結果である。

```haskell
lcs aas@(a:as) bbs@(b:bs)
  | a /= b = lcs as bbs `max` lcs aas bs
```

両方の列の先頭の文字が等しいものであるとき、上の２つに加え、両方を除いた組に対する長さに1を加えたものが選択肢に加わる。

```haskell
  | a == b = lcs as bbs `max` lcs aas bs `max` succ (lcs as bs)
```

この再帰的な定義は明確ではあるが、特に引数が短くなった再帰の深いところで、同じ引数に対する計算を何度も行うため非常に無駄が多い。このような場合は動的プログラミングの出番である。

### 動的プログラミング

単純なメモ化でなく、この問題特有のやり方がある。  
両方の引数の文字をそれぞれ`i`文字、`j`文字除いた列に対する`lcs`を配列の`(i,j)`要素に格納する。すなわち`arr ! (i,j) = lcs (drop i as, drop j bs)`である。再帰呼び出しの代わりにこの配列を参照することで再計算を避ける。

```haskell
lcs as bs = arr ! (0,0)
  where
    al = length as
    bl = length bs
    arr = array ((0,0),(al, bl))
          [((i,j), f i j) | i <- [0..al], j <- [0..bl]]
    f i j
      | i == al || j == bl = 0 -- 空文字列
      | a /= b = (arr ! (i1,j)) `max` (arr ! (i,j1))
      | a == b = (arr ! (i1,j)) `max` (arr ! (i,j1)) `max`
                 (succ $ arr ! (i1,j1))
      where
        a = as !! i
        b = bs !! j
        i1 = succ i
        j1 = succ j
```

右端の列、下端の行は0で固定であり、それ以外のマスの値はその下、右、右下隣の値から決まる。つまり、この配列は常にその全体が必要ということはなく、全て0の`bl`行めから始めて、ひとつ下の行の内容からその上の行の内容を作る仕事を`al`回繰り返すとき、それより下の行の内容は既に必要ない。このように考えると、配列なしにリストのみで計算することができる。

### 配列レス実装

さらに、上の実装の表を上下左右反転させて、左端列と上端行が0固定、それ以外のマスはその上、左、左上の値から決まる、という向きに計算をひっくり返す。  
すると、上の行と、この行の左端（先頭）の値0から、それより右の値を順次構成できる。  
行の先頭は必ず0で、これを含めるとasの各要素との対応がずれるので、今回はこれを除く。  
上の行をline、今回の行に対応するbsの文字をbとすると、一つのマスの値を求めるには、そのマスに対応するasの文字、左上のマスすなわち上の行の一つ手前のマス、真上のマス、この行の左のマスがそれぞれ必要である。これらはそれぞれ`f`の `a`, `c00`, `c01`, `c10`引数に渡される。  
`line1`が等式の両辺に現れているところがポイントである。

```haskell
line1 = tail line11
  where
    line11 = 0 : zipWith4 f1 as (0:line0) line0 line11
    f1 a c00 c01 c10
      | a /= b = c01 `max` c10
      | a == b = c01 `max` c10 `max` succ c00
```

この計算をbsのそれぞれの文字に繰り返して、最後に得られた列の末尾が答えとなる。

```haskell
import Data.List

-- @gotoki_no_joe
lcslen :: Eq a => [a] -> [a] -> Int
lcslen as bs = last (last lines)
  where
    line0 = replicate (length as) 0
    lines = line0 : zipWith linef bs lines
    linef b line = last line11 `seq` tail line11
      where
        line11 = 0 : zipWith4 f as (0:line) line line11
        f a c00 c01 c10
          | a /= b = c01 `max` c10
          | a == b = c01 `max` c10 `max` succ c00
```

対策をしないと、遅延評価により全ての`lines`がサンクとして積み上げられてから`linef`の計算が始まる。これらは結局必要になるので、`linef`に`seq`を仕掛けることで行の計算を強制させている。これをしないとかなり遅くなってしまう。（スペースリーク）

### 共通列の復元

よくある解説では、動的プログラミングで作成した配列を参照することで共通列を復元する。  
ここでは、最長となる選択を探すときに同時に、その共通列の候補を記録していく。最長共通部分列は複数ありうるので、結果はリストのリストとなる。

`max`で最長の候補を選択する際に、長い方の候補を保持する。長さが等しければ両方を維持する。`a == b` の場合は全ての候補に`a`を続ける。Haskellのリストの事情で、計算中は逆順に保存することにした。

```haskell
import Data.List

-- @gotoki_no_joe
lcs :: Eq a => [a] -> [a] -> (Int,[[a]])
lcs as bs = fmap (map reverse) $ last $ last lines
  where
    zero = (0,[[]])
    line0 = replicate (length as) zero
    lines = line0 : zipWith linef bs lines
    linef b line = last line11 `seq` tail line11
      where
        line11 = zero : zipWith4 f as (zero:line) line line11
        f a c00 c01 c10
          | a /= b = c01 `maxx` c10
          | a == b = c01 `maxx` c10 `maxx` succx a c00

maxx (a,bs) (c,ds) =
  case compare a c of
    LT -> (c,ds)
    EQ -> (a,nub (bs ++ ds))
    GT -> (a,bs)

succx c (a,bs) = (succ a, map (c :) bs)
```

### 関連問題

AtCoder: "Educational DP Contest - F問題: LCS [ACコード](https://atcoder.jp/contests/dp/submissions/23167812) 要求はlcsの任意の一つなので、全てのlcsを算出する部分を簡略化している。  
CODE FESTIVAL 2015 あさぷろ Middle B [ACコード](https://atcoder.jp/contests/code-festival-2015-morning-middle/submissions/23168078)



