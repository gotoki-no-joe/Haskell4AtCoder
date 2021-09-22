# トライ木

アルファベットの並びからなる語を登録したり、語が登録されているかを判別したりするための木構造。アルファベットは言葉どおり `Char` である必要はなく、 `Ord` であればよい。

```haskell
import qualified Data.Map as M

-- @gotoki_no_joe
data Trie a = Trie Bool (M.Map a Trie)

-- 空の木
emptyTrie :: Trie a
emptyTrie = Trie False IM.empty

-- 語を登録
insertTrie :: Ord a => [a] -> Trie a -> Trie a
insertTrie []     (Trie _ m) = Trie True m
insertTrie (c:cs) (Trie b m) =
    Trie b (M.insert c (insertTrie cs s) m)
  where
    s = M.findWithDefault emptyTrie c m

-- 語が登録済みか判定
existTrie :: Ord a => [a] -> Trie a -> Bool
existTrie []     (Trie b _) = b
existTrie (c:cs) (Trie _ m) =
  case IM.lookup c m of
    Nothing -> False
    Just t  -> existTrie cs t
--  Data.Maybe.maybe False (existTrie cs) (IM.lookup c m)
```

典型90問の27より。

単に Data.Set を使っても似たようなことはできるが、列全体をキーにするよりアルファベット単位のTrieの方が軽いと思うので、適材適所で。

