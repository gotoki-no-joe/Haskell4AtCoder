# Applicative使うやり方

[AtCoder Beginners Selectionの practice A](https://atcoder.jp/contests/abs/tasks/practice\_1#Haskell)に、tanakh氏監修のテンプレートがあった。

```haskell
import Control.Applicative

main :: IO ()
main = do
    -- 整数の入力
    a <- readLn
    -- スペース区切り整数の入力
    [b, c] <- map read . words <$> getLine
    -- 文字列の入力
    s <- getLine
    -- 出力
    putStrLn $ show (a + b + c) ++ " " ++ s

```

​確かに `<$>` を使えば1行で書けて簡潔になるけど、全て全く見た目が違うせいで、「1行にデータ1つ」「複数」「文字列のまま」で全部やり方が違うように見えるのもどうかと。

複数行を `getContents` で読み込んで `lines` で区切った後に、行ごとのparseをmapすればいいという場合にも使えないし。

```haskell
import Control.Applicative

main = do
-- 値が1つのとき
  n <- readLn -- read <$> getLine
-- 複数の値のとき
  [a,b,c] <- map read . words <$> getLine
-- 文字列のまま読むとき
  s <- getLine
-- (残り)全部読んで行の内容のリストのリストに一度に読み込む
  xys = map (map read . words) . lines <$> getContents
-- 本体
  let ans = compute n a b c xys
-- 結果が数ひとつのとき
  print ans
-- 結果が文字列のとき
  putStrLn ans
-- 結果がYes/Noのとき
  putStrLn $ if ans then "Yes" else "No"
-- 1行に空白区切りで出力するとき
  putStrLn $ unwords $ map show ans
-- ↑の高速版、Data.Listが必要
  putStrLn $ foldr ($) "" $ intersperse (' ' :) $ map shows ans
-- 1つ1行で出力するとき
  mapM_ print ans

compute :: Int -> [Int] -> [[Int]] -> [Int Bool]
compute n a b c xys =
```

あまり変わらなかった。
