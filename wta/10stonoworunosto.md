# 10.ソートの大小関係を操る（多次元配列のソート）(2)

特に「多次元配列の」とか狭めることなく一般化して、任意の大小関係でのソートが

```haskell
Data.List.sortBy :: (a -> a -> Ordering) -> [a] -> [a]
```

でできる。例えば降順のソートは `sortBy (flip compare)` でよい。\
また、

```haskell
Data.Function.on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
```

も組み合わせると、

```haskell
sortBy (compare `on` fst) -- 対の左側だけを比較
sortBy (compare `on` (!! 1)) -- リストのリストを1番目の要素で比較
```

などとできる。

### 関連問題

ABC128B [ACコード](https://atcoder.jp/contests/abc128/submissions/22763098)\
キーエンス プログラミング コンテスト 2020 B Robot Arms\
ABC113C [ACコード](https://atcoder.jp/contests/abc113/submissions/22769268) 別アプローチの、Pごとに選り分けてからyで整列することを出題者は意図していそうな。
