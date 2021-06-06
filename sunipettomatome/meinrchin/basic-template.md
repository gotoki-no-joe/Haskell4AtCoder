---
description: basic template for main
---

# 基本テンプレート

言語を自由に選択できるように、独立したプログラムであることが求められる。つまり、計算に必要なデータを標準入力の文字列から取り込むこと、計算結果を文字で標準出力に送り出すことである。HaskellだとそのためにはIOモナドを扱う必要がある。

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
-- 1つ1行で出力するとき
  mapM_ print ans

compute :: Int -> [Int] -> [[Int]] -> [Int Bool]
compute n a b c xys =
```

とりあえずこれがあれば動くというテンプレート。  
`compute`の型を宣言することで`read`の型を固定する。  
AtCoderでときどきある、"Yes"か"No"を出力するタイプの問題は、Boolで返して20行めを使う。

HaskellのString型は重いため、入力データがある程度を超えると読み込み時間がネックになる。同等に使える[ByteStringで読み込む版](bytestring-template.md)を用意した。

