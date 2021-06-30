# アフィン変換

二次元のアフィン変換 $$a, b, c, d$$が線形変換行列、$$t_x, t_y$$が平行移動$$\displaystyle \left ( \begin{array}{l}x' \\ y' \end{array} \right ) = \left ( \begin{array}{ll}a & b\\ c & d\end{array} \right ) \left ( \begin{array}{l}x \\ y \end{array} \right ) + \left ( \begin{array}{l}t_x \\ t_y \end{array} \right )$$  
を一つの行列で表現すると  
$$\displaystyle \left ( \begin{array}{l}x' \\ y' \\ 1\end{array} \right ) = \left ( \begin{array}{lll}a & b & t_x \\ c & d & t_y \\ 0 & 0 & 1 \end{array} \right ) \left ( \begin{array}{l}x \\ y \\ 1\end{array} \right ) $$

X軸に$$p$$倍、Y軸に$$q$$倍する、原点中心の拡大  
$$\left ( \begin{array}{ll}p & 0 \\ 0 & q \end{array} \right )$$

原点中心の回転行列  
$$\left ( \begin{array}{ll}\cos \theta & \sin \theta \\ -\sin \theta & \cos \theta \end{array} \right )$$

せん断  
水平に右に$$m$$傾ける$$\left ( \begin{array}{ll}1 & m \\ 0 & 1 \end{array} \right )$$、垂直に上にm傾ける$$\left ( \begin{array}{ll}1 & 0 \\ m & 1 \end{array} \right )$$

