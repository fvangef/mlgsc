-- module Crumbs (followCrumbs) where
module Crumbs (
    followCrumbs,
    dropCrumbs,
    dropCrumbsM,
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

-- Produces a Crumbs trail, given a tree and a metric m for scoring nodes

dropCrumbs :: Tree a -> (a -> Int) -> Crumbs
dropCrumbs tree m = dropCrumbs' tree m []

dropCrumbs' :: Tree a -> (a -> Int) -> Crumbs -> Crumbs
dropCrumbs' (Node _ []) m crumbs = crumbs
dropCrumbs' (Node _ kids) m crumbs = dropCrumbs' bestKid m (bestKidIdx:crumbs)
    where   (bestKid, bestKidIdx) = bestByWithIndex kids m'
            m' (Node rl _) = m rl

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
