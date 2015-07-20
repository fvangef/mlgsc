import Data.Text
import Data.Tree

import NewickParser
import NewickDumper
import NeighborJ

(Right t1) = parseNewickTree "(A,B,C,D,E);"

t2 = joinNeighbors t1 (pack "A") (pack "B") (pack "f")
t3 = joinNeighbors t2 (pack "C") (pack "f") (pack "g")
