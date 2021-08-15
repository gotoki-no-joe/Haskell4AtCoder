---
description: directive for strict evaluation
---

# 正格評価ディレクティブ

### コード

```haskell
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
```

### 説明

遅延評価のためにサンクが溜まって無駄に重いような気がする場合、コンパイラディレクティブの指定で、可能な箇所を先行評価するように指示できる。

### もっと説明

大抵の場合は単なる思いすごしで、それほどの効果は得られない。  
というかGHCは充分に速い。

