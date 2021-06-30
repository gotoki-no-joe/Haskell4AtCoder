# Data.List.Split

Haskell Platformには含まれないが、時々便利。

[https://hackage.haskell.org/package/split-0.2.3.3/docs/Data-List-Split.html](https://hackage.haskell.org/package/split-0.2.3.3/docs/Data-List-Split.html)

```haskell
splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOneOf :: Eq a => [a] -> [a] -> [[a]]
splitWhen :: (a -> Bool) -> [a] -> [[a]]
endBy :: Eq a => [a] -> [a] -> [[a]]
endByOneOf :: Eq a => [a] -> [a] -> [[a]]
wordsBy :: (a -> Bool) -> [a] -> [[a]]
linesBy :: (a -> Bool) -> [a] -> [[a]]
chunksOf :: Int -> [e] -> [[e]]
splitPlaces :: Integral a => [a] -> [e] -> [[e]]
splitPlacesBlanks :: Integral a => [a] -> [e] -> [[e]]
chop :: ([a] -> (b, [a])) -> [a] -> [b]
divvy :: Int -> Int -> [a] -> [[a]]
-- divvy 5 2 [1..10] == [[1,2,3,4,5],[3,4,5,6,7],[5,6,7,8,9]]
```

さらに高度な分割器をつくるDSLもあるがスルー。

