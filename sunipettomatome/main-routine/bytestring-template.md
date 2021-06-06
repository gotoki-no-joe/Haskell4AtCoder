---
description: 'template for main, fast read by ByteString'
---

# ByteStringテンプレート

基本テンプレートと同じことができるByteString版のテンプレート  
入力データが大きい場合、読み込み時間がかなり節約される。

```haskell
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.List

main = do
-- 1行読んで
  li <- BS.getLine
-- 値が1つのとき
  let Just (n,_) = BS.readInt li
-- 複数の値のとき
  let as = unfoldr (BS.readInt . BS.dropWhile isSpace) li
-- (残り)全部読んで
  co <- BS.getContents
-- 行の内容のリストのリストに一度に読み込む
  let xys = map (unfoldr (BS.readInt . BS.dropWhile isSpace)) $ BS.lines co
-- 本体
  let ans = compute n as xys
-- 結果が数ひとつのとき
  print ans
-- 結果が文字列のとき
  putStrLn ans
-- 結果がYes/Noのとき
  putStrLn $ if ans then "Yes" else "No"
-- 1行に空白区切りで出力するとき
  putStrLn $ ($ "") $ foldr1 (.) $ intersperse (' ' :) $ map shows ans
-- 1つ1行で出力するとき
  mapM_ print ans

compute :: Int -> [Int] -> [[Int]] -> [Int Bool]
compute n as xys =
```



