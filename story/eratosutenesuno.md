# エラトステネスの篩

エラトステネスの篩は、フラグの配列を使うアルゴリズムである。これは、インデックスの数がある数の倍数であるとわかったとき立てられる。  
添字は2以上のみ関心があり、順に調べていくとき、フラグが立っていなければそのインデックスは素数であるとわかる。素数として取り出し、またその全ての倍数のフラグを立てる。これで、配列の上限までの値が素数判定できる。

というように、書き換え可能な配列があることを前提としたアルゴリズムなので、Haskellとの相性はきわめてよくない。その本質は、新たに素数pが見つかったとき、全てのpの倍数npを候補から除くことにある。

フラグ配列に素数の倍数を記録する操作と、そのフラグ配列の値を確認する動作を同時に行うようにしてみる。素数pが見つかったときに、p-1個のFalseの次に1個のTrueがきて、また先頭から繰り返すリスト`cycle (replicate (pred p) False ++ [True])`をたぐると、今注目している値がpの倍数であるかを確認できる。素数が見つかる度にこのリストが増えるが、それらの論理和をとってしまって問題ない。これは配列無しのエラトステネスの篩の実現といえる。シンプルであるが残念なことに効率はすぐに悪化する。

```haskell
primes1 :: [Int]
primes1 = 2 : loop 3 (cycle [False, True])
  where
    loop k (True :bs) =     loop (succ k) bs
    loop p (False:bs) = p : loop (succ p) bs1
      where
        bp = cycle (replicate (pred p) False ++ [True])
        bs1 = zipWith (||) bs bp
```

Haskellの教科書によくあるやり方は、新たに素数pが見つかったとき、pで割り切れない数だけ残すフィルタ ``filter (\k -> k `mod` p /= 0)``を追加するものである。エラトステネスの篩はpでの除算をしないでpの倍数だけを考えることが長所なのでこれでは台無しである。

```haskell
primes2 :: [Int]
primes2 = 2 : sieve [3,5..]
  where
    f p = \k -> k `mod` p /= 0
    sieve (p:ns) = p : sieve (filter (f p) ns)
```

そこで、`mod`でpの倍数か判定する代わりに、「次のpの倍数np」と遭遇したらこれを弾く、遭遇するかそれを超える値が来たら「次の次のpの倍数\(n+1\)p」に監視対象を繰り上げる、というロジックにする。svの第2引数が現在のnp、\(n+1\)pを作るためにpも第1引数で持つ。

```haskell
primes3 :: [Int]
primes3 = 2 : sieve [3,5..]
  where
    sieve (p:ps) = p : sieve (sv p (p+p) ps)
    sv p np xxs@(x:xs) =
      case compare np x of
        LT ->     sv p (np+p) xxs
        EQ ->     sv p (np+p)  xs
        GT -> x : sv p  np     xs
```

フィルタsvはnpを増やしつつ停止することなく実行し続ける。（サンクが残り続ける。）  
様々な素数pに対するnpが小さい順に取り出せればよいので、これらを優先度付きキューで管理することにする。キューから取り出されて消費されたnpは、\(n+1\)pに増えてまたキューに投入される。このキューへの挿入は挿入が終われば消えるので、サンクの肥大化を避けられる。

```haskell
primes4 :: [Int]
primes4 = 2 : 3 : sieve q0 [5,7..]
  where
    q0 = insertIQ 9 3 emptyIQ             -- キューが空の状態を回避
    sieve q xxs@(x:xs) =
      case compare np x of
        LT ->     sieve q1 xxs
        EQ ->     sieve q1  xs
        GT -> x : sieve q2  xs
      where
        (np,p) = getIQ q
        q1 = insertIQ (np+p) p (deleteIQ q)
        q2 = insertIQ (x+x)  x q
```

この実装では、IntMapによる優先度つきキューを用いている。  
以前は、Data.List.insert によるとりあえずの実装と、独自に部分順序付き木を実装したものを使う版を示していたが、冗長なのでここで終わる。

