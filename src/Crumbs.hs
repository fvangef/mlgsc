module Crumbs (follow) where

import Data.Tree

type Crumbs = [Int]

-- Follows a list of crumbs. No error recovery!

follow :: Crumbs -> Tree a -> a
follow (c:cs) (Node _ kids) = follow cs (kids !! c)
follow [] node = rootLabel node
