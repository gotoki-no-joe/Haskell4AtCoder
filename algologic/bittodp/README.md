# ビットDP

[https://algo-logic.info/bit-dp/](https://algo-logic.info/bit-dp/)

「ビット」は本質的でなく、集合の要素がそこそこ数え上げられる程度の対象であること、「DP」では何が解けるのかわからないから、もう少し名前を考えるべき。

対象の全体集合 Aを考える。その任意の部分集合 $$S \subseteq A$$に対して、Sの要素の順列に関して最適化したスコアをDPな配列dで保持する。そのとき、配列の添え字をSにできないので、全体集合の各要素に番号iを振れば、Sを自然数$$\sum_{i \in S} 2^i$$で表せる、というところが「ビット」の所以。

Sに対してさらにもう一つ要素 $$v \; (v \notin S, v \in A)$$を加えた結果のコストが、次のような漸化式で表せるように設計することが通例らしい。  
$$d[S \cup \{v\}] = d[S] + \textrm{cost}(S,v)$$

---

この書き方は「配るDP」の漸化式なので、手前を明示するスタイルにすると

$$d[S] = \min \{ d[S - \{v\}] \,|\, v \in S \}$$  
$$d[\emptyset] = c_0$$ または何らかの最小の状態に対する初期値配分

（なんか完全には対応してないねぇ。）
