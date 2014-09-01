-- module Crumbs (followCrumbs) where
module Crumbs where

import Data.Tree
import Data.List

type Crumbs = [Int]

-- Follows a list of crumbs. No error recovery!

followCrumbs :: Crumbs -> Tree a -> a
followCrumbs (c:cs) (Node _ kids) = followCrumbs cs (kids !! c)
followCrumbs [] node = rootLabel node

-- Produces a Crumbs trail, given a tree and a function for scoring nodes

dropCrumbs :: Tree a -> (Tree a -> Int) -> Crumbs
dropCrumbs tree f = dropCrumbs' tree f []

dropCrumbs' :: Tree a -> (Tree a -> Int) -> Crumbs -> Crumbs
dropCrumbs' (Node _ []) f crumbs = crumbs
dropCrumbs' (Node _ kids) f crumbs = dropCrumbs' bestKid f (bestKidIdx:crumbs)
    where   bestKid = undefined
            bestKidIdx = undefined

-- finds the (first) object in a list that maximizes some metric m, returns
-- that object and its index in the list. Not efficient, but should be ok for
-- short lists.

bestByWithIndex :: (Ord b) => [a] -> (a -> b) -> (a, Int)
bestByWithIndex objs m = (bestObj, head ndx) 
    where   bestObj     = objs !! (head ndx)
            ndx         = elemIndices max_metric obj_metrics 
            max_metric  = maximum obj_metrics
            obj_metrics = map m objs

arbf :: String -> Int
arbf "A" = -1
arbf "a" = 1
arbf "r" = 6
arbf "c" = 2
arbf "Z" = 5

objs = ["Z", "r", "a", "r", "c"]
