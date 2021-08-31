# 木を扱う

無向木はどこから始めても木。AtCoderでは頂点が1からN、それらの間の無向辺の情報リストで与えられる。

まずその情報を、頂点に対する辺の接続先のリストの配列にすることで扱いやすくする。

```haskell
tree = accumArray (flip (:)) [] (1,n)
       [p | (a:b:_) <- abs, p <- [(a,b),(b,a)]]
```

この情報を使って木を探索する際には、間違って先祖返りしないように、現在位置だけでなく親は誰かを覚えておく必要がある。

```haskell
foldTree :: (Int -> [a] -> a) -> Int -> a
foldTree f i = recur 0 i
  where
    recur p i = f i $ map (recur i) cs
      where
        cs = delete p $ tree ! i
```



