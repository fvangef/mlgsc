module NewickDumper (treeToNewick) where

import qualified Data.Text as T
import qualified Data.List as L
import Data.Tree

treeToNewick :: Tree T.Text -> T.Text
treeToNewick tree = T.snoc (nodeToNewick tree) ';'

nodeToNewick :: Tree T.Text -> T.Text
nodeToNewick (Node label [])    = label
nodeToNewick (Node _ children)  = T.cons '(' $ T.snoc innerNw ')'
    where   innerNw     = T.intercalate (T.pack ",") childrenNw
            childrenNw  = L.map nodeToNewick children
