# 6.総当り \(BIT全探索\) \(5\)

### 英語でなんて言うの？

「総当り戦」はround-robinというらしいが、計算機関係ではこれは別の意味があるので使えない。「総当たり攻撃」bruite-force attackは計算機関係の用語であるが、これもね…

### 説明

$$n$$とおりからひとつを選ぶ選択肢が$$k$$個あるとき、全ての組み合わせは$$n^k$$とおりとなる。特に$$n=2$$の場合の全ての組み合わせが、$$k$$ビットの2進数を0...0から1...1までのカウントアップで生成できることから、BIT全探索と呼ぶ界隈もあるようだが、自分は好きではない。

選択をひとつ行うごとに状態sが$$n$$倍に増える。その選択を行うための情報aは$$k$$要素のリストで与えることができる。

```haskell
exhaustiveSearch :: (a -> s -> [s]) -> s -> [a] -> [s]
exhaustiveSearch step ini = foldl (\s a -> concatMap (step a) s) [ini]
```

BIT全探索と呼ぶものは、選択は2とおりで、選択しない場合は状態は変化しない、という条件があるものを指しているような感じ。

```haskell
bitSearch :: (a -> s -> s) -> s -> [a] -> [s]
bitSearch f ini = foldl step [ini]
  where
    step ss a = ss ++ map (f a) ss

-- bitSearch f = exhaustiveSearch (\a s -> [s, f a s])
```

### 関連問題

ABC167C [ACコード](https://atcoder.jp/contests/abc167/submissions/22739273) bitSearch  
ABC182C [ACコード](https://atcoder.jp/contests/abc182/submissions/22739342) bitSearch この問題は[より特化した別解](https://atcoder.jp/contests/abc182/submissions/18003556)がある  
ABC190C [ACコード](https://atcoder.jp/contests/abc190/submissions/22739391) exhaustiveSearch  
ARC114A  
ABC197C  
ABC128C  
ABC147C  
ABC104C

