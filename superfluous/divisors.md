# 約数列挙

よりHaskellらしい\(?\)が、タプルのリストを一時的に作るために遅そうな実装も思いついた。  
しかしzipizで毎回a==bを調べるのは無駄もいいところなので取り下げる。

```haskell
divisors :: Int -> [Int]
divisors n =
  zipiz [] $
  [ (k, p)
  | k <- takeWhile (\k -> k*k <= n) [1..]
  , let (p,q) = divMod n k
  , q == 0
  ]

-- zipiz ps = map fst ps ++ reverse (map snd ps)
zipiz :: [(a,a)] -> [a]
zipiz rs [] = rs
zipiz rs [(a,b)]
  | a == b = a : rs
  | True   = a : b : rs
zipiz rs ((a,b):abs) = a : zipiz (b:rs) abs
```

整数平方根も使えば、takeWhileでなく上限のあるレンジで一気にkを生成できるが、どちらが速いだろうか。

