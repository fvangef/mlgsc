-- module Crumbs (followCrumbs) where
module Crumbs (
    Crumbs,
    followCrumbs,
    dropCrumbs,
    bestByWithIndex, 
    empty
    ) where

import Data.Tree
import Data.List
import Control.Monad.Writer


type Crumbs = [Int]

empty = (undefined, -1)

-- Follows a list of crumbs. No error recovery!

followCrumbs :: Crumbs -> Tree a -> a
followCrumbs (c:cs) (Node _ kids) = followCrumbs cs (kids !! c)
followCrumbs [] node = rootLabel node

-- A wrapper around the monadic dropCrumbsM below

dropCrumbs :: (Ord b) => (a -> b) -> Tree a -> (b, Crumbs)
dropCrumbs m tree = runWriter $ dropCrumbsM m tree

-- Given a tree and some metric m, generates a list of crumbs by recursively
-- applying m to a node's children and calling itself on the best-scoring child,
-- until a leaf is reached. Returns the score of that leaf, as well as a list of
-- crumbs followed to reach it, as a Writer monad.

dropCrumbsM :: (Ord b) => (a -> b) -> Tree a -> Writer [Int] b
dropCrumbsM m (Node rl []) = return $ m rl
dropCrumbsM m (Node rl kids) = do
    let (bestKid, bestNdx) = bestByWithIndex kids m'
    tell [bestNdx]
    dropCrumbsM m $ bestKid
    where m' (Node rl _) = m rl




-- finds the (first) object in a list that maximizes some metric m, returns
-- that object and its index in the list. Not efficient, but should be ok for
-- short lists.

bestByWithIndex :: (Ord b) => [a] -> (a -> b) -> (a, Int)
bestByWithIndex [] m    = empty
bestByWithIndex objs m  = (bestObj, head ndx) 
    where   bestObj     = objs !! (head ndx)
            ndx         = elemIndices max_metric obj_metrics 
            max_metric  = maximum obj_metrics
            obj_metrics = map m objs
