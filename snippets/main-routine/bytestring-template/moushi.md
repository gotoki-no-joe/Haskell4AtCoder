# もう少し関数化

```haskell
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.List
import Control.Monad
import Data.Array
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as MVU
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Set as S
import qualified Data.IntSet as IS
import Data.Bits
import qualified Data.Sequence as Q
import qualified Data.Heap as H

compute :: Int -> [Int] -> [[Int]] -> [Int Bool]
compute n as xys = ...

-- bootstrap

main = do
-- 値が1つのとき
  n <- getInt
-- 複数の値のとき
  as <- getInts
-- n行の整数の列
  xys <- replicateM n getInts
-- (残り)全部読んで整数の列に
  pqrs <- getIntsAll
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

-- 整数ひとつの行
getInt :: IO Int
getInt = do
  li <- BS.getLine
  let Just (n,_) = BS.readInt li
  return n

-- 複数の整数からなる行
getInts :: IO [Int]
getInts = do
  li <- BS.getLine
  return $ unfoldr (BS.readInt . BS.dropWhile isSpace) li

-- 残り全てを、複数の整数からなる行として読む
getIntsAll :: IO [[Int]]
getIntsAll = do
  co <- BS.getContents
  return $ map (unfoldr (BS.readInt . BS.dropWhile isSpace)) $ BS.lines co

```
