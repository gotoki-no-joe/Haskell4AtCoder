# 最大公約数

[https://algo-logic.info/euclidean-algolithm/](https://algo-logic.info/euclidean-algolithm/)

ユークリッドの互除法の話。Prelude.gcdにあるしなぁ。

続きがある。

[https://algo-logic.info/extend-gcd/](https://algo-logic.info/extend-gcd/)

拡張ユークリッドの互除法

$$ax + by = gcd(a,b)$$を解く、ということは、$$x,y$$を求めるということ。  
少々読みにくい命令型の疑似コードを読み解く。

```c
// aとbに具体的な値を、xとyに答えを入れる変数の参照を渡す
// 戻り値はaとbのgcdで、xとyに解が入れられる
int extend_gcd(int a, int b, int *x, int *y) {
  if (b == 0) {
    *x = 1;
    *y = 0;
    return a;
  } else {
    d = extend_gcd(b, a % b, y, x); // ここでxとyを入れ替えるのがポイント
    *y -= a / b * *x;
    return d;
  }
}
```

Haskell化するには、戻り値を\(d,x,y\)のタプルにすればよいのだろうか。

```haskell
extend_gcd a b = snd $ loop a b
  where
    loop a 0 = (a,(1,0))
    loop a b = let (d,(y,x)) = loop b (mod a b)
               in  (d,(x,y - (a `div` b) * x))
```

で、これが求められると何ができるのだっけ？→mod演算の逆元 だった。

