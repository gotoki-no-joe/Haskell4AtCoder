# 剰余演算

#### 同値性

$$(a \mod n) \mod n = a \mod n$$  
$$n^x \mod n = 0$$ただし$$x > 0$$は整数  
$$ab^{n-1} \mod n = a \mod n$$ ただし$$n$$は素数で$$b$$の約数でない

#### 逆数

$$((-a \mod n) + (a \mod n)) \mod n = 0$$

#### 分配則

$$(a + b) \mod n = ((a \mod n) + (b \mod n)) \mod n$$  
$$ab \mod n = ((a \mod n) (b \mod n)) \mod n$$

#### モジュラ逆数

モジュラ逆数$$b^{-1} \mod n$$の定義  
$$((b^{-1} \mod n) (b \mod n)) \mod n = 1$$  
乗算  
$$((ab \mod n) (b^{-1} \mod n)) \mod n = a \mod n$$  
分数の定義（右辺が定義できる場合）  
$$\displaystyle \frac{a}{b} \mod n = ((a \mod n)(b^{-1} \mod n)) \mod n$$



