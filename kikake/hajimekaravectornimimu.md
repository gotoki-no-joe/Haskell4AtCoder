# はじめからVectorに読み込む

たいていの場合は入力データをリストに読み込んで問題ないが、後でvectorに入れるなら初めからそうしてしまえばよい。

1行のByteStringにN個の整数が並んでいるときに、それらをN+1要素のimmutable vectorの1からNに入れる

```haskell
-- ByteStringを読んで1からNに収めた長さN+1のVectorを作る
readIntV1N :: Int -> BS.ByteString -> IO (V.Vector Int)
readIntV1N n bs = do
    v <- MV.new (succ n)
    loop v 1 bs
    V.freeze(v) -- これをなくせばmutableのまま得られる
  where
    loop v i bs =
      case BS.readInt (BS.dropWhile isSpace bs) of
        Just (x,bs1) -> do { MV.write v i x; loop v (succ i) bs1 }
        Nothing -> return ()
```

MV.createを使ってIO アクションでないままやれたらいいのだけど、なんかうまくいかない。

典型90の34を作る途中で作ったもの。

できた。

```haskell
import qualified Data.ByteString.Char8 as BS
import Data.Char

import Control.Monad.ST (ST)
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Unboxed as V

main = do
  ...
  let as = readIntsV1N  n bytestr
  bs    <- readIntsMV1N n bytestr
  ...

-- ByteStringを読んで1からNに収めた長さN+1のVectorを作る
readIntsV1N :: Int -> BS.ByteString -> V.Vector Int
readIntsV1N n bs = V.create action
  where
    action :: ST s (MV.MVector s Int)
    action = do
      v <- MV.new (succ n)
      loop v 1 bs
    loop v i bs =
      case BS.readInt (BS.dropWhile isSpace bs) of
        Just (x,bs1) -> do { MV.write v i x; loop v (succ i) bs1 }
        Nothing -> return v

-- ByteStringを読んで1からNに収めた長さN+1のMutable Vectorを作る
readIntsMV1N :: Int -> BS.ByteString -> IO (MV.IOVector Int)
readIntsMV1N n bs =
  do
    v <- MV.new (succ n)
    loop v 1 bs
  where
    loop v i bs =
      case BS.readInt (BS.dropWhile isSpace bs) of
        Just (x,bs1) -> do { MV.write v i x; loop v (succ i) bs1 }
        Nothing -> return v
```



