# 説明

## 基本

競技プログラミングでは、参加者がプログラミング言語を自由に選択できるようにするため、プログラムが処理するべき入力データはファイルまたは標準入力からテキストで読み込み、処理結果の出力データもテキストで出力するようになっていることが多い。

Haskellで入出力を扱うには、IOモナドを使う。ざっくり、`do` ブロックの中の行が、順に実行される。最も素朴な方法としては、以下のコードをテンプレートとし、問題の入力形式に合わせて使用できる。

```haskell
main = do
-- 標準入力から1行読み、String型の値をl1に束縛する
  l1 <- getLine
  l2 <- getLine
-- 標準入力から（残り）全てを読み込み、String型の値をcoに束縛する
  co <- getContents
-- l1をRead型クラスの型の値として解釈する。純粋な計算はletで行う
  let x = read l1
  let [y,z] = map read $ words l2
  let ass = map (map read . words) $ lines co
-- 各引数の型はcomputeのシグネチャで指定すると見通しがよい
  let ans = compute x y z ass
-- Show型クラスの値を標準出力にテキストで出力する
  print ans
-- Yes/Noで答える問題は、Boolで返してこれで出力する
  putStrLn $ if ans then "Yes" else "No"
-- String型は、ダブルクオートがつかないようにこちらで出力する
  putStrLn ans
-- 同じ型の値を複数、空白を挟んで一行に表示する
  putStrLn $ unwords $ map show ans
-- 同じ型の値を複数、１行に一つずつ表示する
  mapM_ print ans

-- AtCoderでよくある型に編集しやすくしている
compute :: Int -> [Int] -> [[Int]] -> [Int Bool]
compute n a b xys =
```

「二つの値が書かれた行がn行続く」のような場合、この二つの値をリストでなくタプルで扱いたくなるが、そこは割り切った方がよい。その変換は単純なタイムロスになる。

## Applicative

ここで、`Control.Applicative`の機能を使うと、モナドの結果にさらに純粋な関数を適用できる。これにより、上のコードの読み込み部分がコンパクトにできる。

```haskell
import Control.Applicative

main = do
  x <- read <$> getLine
  [y,z] <- map read . words <$> getLine
  ass <- map (map read . words) . lines <$> getContents
  let ans = compute x y z ass
-- Show型クラスの値を標準出力にテキストで出力する
  print ans
-- Yes/Noで答える問題は、Boolで返してこれで出力する
  putStrLn $ if ans then "Yes" else "No"
-- String型は、ダブルクオートがつかないようにこちらで出力する
  putStrLn ans
-- 同じ型の値を複数、空白を挟んで一行に表示する
  putStrLn $ unwords $ map show ans
-- 同じ型の値を複数、１行に一つずつ表示する
  mapM_ print ans

-- AtCoderでよくある型に編集しやすくしている
compute :: Int -> [Int] -> [[Int]] -> [Int Bool]
compute n a b xys =
```

## ByteString

Haskellにおいて、文字列を文字のリストとして扱うやり方は、Preludeの関数で処理できるためとっつきやすいが、処理効率の観点からは絶望的である。$$2 \times 10^5$$個のデータの組を読み込む、というような場面で上のテンプレートを用いると、読み込みでかなりの時間を消費する。

`Data.ByteString`ライブラリを利用すると、この問題に対処できる。出力側が問題になることはあまりないのでそのままにしている。

```haskell
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.List

compute :: Int -> [Int] -> [[Int]] -> [Int Bool]
compute n as xys = ...

main = do
  Just (n,_) <- BS.readInt li <$> BS.getLine
  [x,y] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  ass <- map (unfoldr (BS.readInt . BS.dropWhile isSpace)) . BS.lines <$> BS.getContents
-- 本体
  let ans = compute n as xys
-- 結果が数ひとつのとき
  print ans
-- 結果が文字列のとき
  putStrLn ans
-- 結果がYes/Noのとき
  putStrLn $ if ans then "Yes" else "No"
-- 1行に空白区切りで出力するとき
  putStrLn $ foldr ($) "" $ intersperse (' ' :) $ map shows ans
-- 1つ1行で出力するとき
  mapM_ print ans
```
