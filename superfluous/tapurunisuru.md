# タプルにする



タプルにする

1行の情報をリストで解析したので、そのままリストにしておく方が計算時間は節約されるが、本来はタプルであるべき情報ならタプルにしたいと思うかもしれない。

computeの型をきちんと指定することで、タプルの組数を明記しないで済むやり方。

```haskell
class L2T t where
  l2t :: [a] -> t

instance L2T (a,a) where
  l2t [x,y] = (x,y)

instance L2T (a,a,a) where
  l2t [x,y,z] = (x,y,z)
```

いやこんなの絶対動くわけないやんな。どうせここから必要な行だけコピペすればいいのだから、

```haskell
l2t2 (a:b:_)       = (a,b)
l2t3 (a:b:c:_)     = (a,b,c)
l2t4 (a:b:c:d:_)   = (a,b,c,d)
l2t5 (a:b:c:d:e:_) = (a,b,c,d,e)
```

これで十分。

