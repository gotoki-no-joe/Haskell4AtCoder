---
description: basic template for main
---

# 基本テンプレート

### コード

```haskell
main = do
-- 1行読んで
  li <- getLine
-- 値が1つのとき
  let n = read li
-- 複数の値のとき
  let [a,b,c] = map read $ words li
-- (残り)全部読んで
  co <- getContents
-- 行の内容のリストのリストに一度に読み込む
  let xys = map (map read . words) $ lines co
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

### 説明

入力するデータ量が少ない場合のための、Stringを用いた基本部分。\
コピペして、問題の形式に合わせて読み込み部をトリミングし、出力部を選ぶ。

`compute`の型を宣言することで`read`の型を固定する。

AtCoderでときどきある、"Yes"か"No"を出力するタイプの問題は、Boolで返して19行めを使う。

### もっと説明

とりあえずこれがあれば動くというテンプレート。

競技プログラミングは参加者が言語を自由に選択できるようにするため、作成するプログラムは自立する必要がある。つまり、計算に必要なデータを標準入力の文字列から取り込み、計算結果を文字で標準出力に送り出すことができなくてはならない。

HaskellでそれをするにはIOモナドを扱う必要がある。このスニペットはそのあたりの面倒を処理するボイラプレートのテンプレートである。

HaskellのString型は重いため、入力データがある程度を超えると読み込み時間がネックになる。このスニペットと同等に使える[ByteStringで読み込む版](bytestring-template/)を用意した。
