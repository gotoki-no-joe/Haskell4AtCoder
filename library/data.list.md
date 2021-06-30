# Data.List

覚えていて当たり前な関数は省略し、時々必要になる便利な関数の覚え書き。

```haskell
-- 間に挟み込む ex) unwords = intersperse ' '
intersperse ::  a  ->  [a]  -> [a]
intercalate :: [a] -> [[a]] -> [a]

-- 部分列
-- subsequences "abc" = ["","a","b","ab","c","ac","bc","abc"]
subsequences :: [a] -> [[a]]

-- 順列
-- permutations "abc" = ["abc","bac","cba","bca","cab","acb"]
permutations :: [a] -> [[a]]

-- 分割
-- span p xs = (takeWhile p xs, dropWhile p xs)
span :: (a -> Bool) -> [a] -> ([a], [a])

-- 選り分け
-- partition p xs == (filter p xs, filter (not . p) xs)
partition :: (a -> Bool) -> [a] -> ([a], [a])

-- 特別な整列
-- sortOn f = sortBy (comparing f)
sortOn :: Ord b => (a -> b) -> [a] -> [a]
```

