# \(obsolete\) 素数、素因数分解

エラトステネスの篩\(5.\)で作り直したので、こちらはゴミ箱へ送るべき。  
素因数分解についても、素数なしで奇数で割ってもなんとかなるという実践的な情報もある。\(4.\)  
単純に約数を求めるなら、ここで示した素因数分解からの再合成をしなくても、\(3.\)で十分。  
元の数がない状態で素因数分解から約数を復元する場合には使えるので、その部分を取り出す作業待ち。

&lt;hr/&gt;

Data.Numbers.Primesが使えないときに使う簡単な代用品。

```haskell
import Data.List

primes = 2 : 3 : 5 : sieve [(9,3),(10,5)] [7,9..]

sieve qpqps@((q,p):qps) xxs@(x:xs) =
  case compare q x of
    LT -> sieve qps1 xxs
    EQ -> sieve qps1 xs
    GT -> x : sieve (insert (x+x,x) qpqps) xs
  where
    qps1 = insert (q+p,p) qps
-- このinsertをヒープに変えるともっと速くなる。

primeFactors n = loop n primes
  where
    loop 1 _ = []
    loop n pps@(p:ps)
      | r == 0 = p : loop q pps
      | q < p  = [n]
      | True   = loop n ps
      where (q,r) = divMod n p
```

素因数分解から約数を作るには、素因数とその個数にして、$$p^0 \sim p^n$$までのバリエーションを作っていくことでできる。

```haskell
compute :: Int -> [Int]
compute n = foldl loop [1] pfcs
  where
    pfcs = map (\xs -> (head xs, length xs)) $ group $ primeFactors n

loop xs (p,c) = foldl1 merge $ take (succ c) $ iterate (map (p *)) xs

merge xxs@(x:xs) yys@(y:ys) = case compare x y of
  LT -> x : merge xs yys
  EQ -> x : merge xs  ys
  GT -> y : merge xxs ys
merge xs [] = xs
merge [] ys = ys
```

Data.List.Insertを使う代わりに、よくあるfilter mod == 0 の効率的な版をどんどん挟み込む版。重くなる速度が早い。

```haskell
primes = 2 : 3 : 5 : sieve (sv 10 5 $ sv 9 3 [7,9..])
  where
    sieve (p:ps) = p : sieve (sv (p+p) p ps)
    sv q p xxs@(x:xs) =
      case compare q x of
        LT -> sv (q+p) p xxs
        EQ -> sv (q+p) p xs
        GT -> x : sv q p xs
```

