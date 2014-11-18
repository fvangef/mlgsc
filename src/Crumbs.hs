-- module Crumbs (followCrumbs) where
module Crumbs (
    Crumbs,
    followCrumbs,
    followCrumbsWithTrail,
    dropCrumbs,
    dropExtendedCrumbsM,
    bestByWithIndex, 
    empty,
    ) where

import Data.Tree
import Data.List
import Control.Monad.Writer


type Crumb = Int
type Crumbs = [Crumb]

empty = (undefined, -1)
emptyExt = (undefined, -1, 1, 1)

-- Follows a list of crumbs. Returns the rootLabel of the node corresponding to
-- the last crumb. No error recovery!

followCrumbs :: Crumbs -> Tree a -> a
followCrumbs (c:cs) (Node _ kids) = followCrumbs cs (kids !! c)
followCrumbs [] node = rootLabel node

-- As above, but returns a list containing the rootLabels of all nodes visited
-- on the path through the tree.

followCrumbsWithTrail :: Crumbs -> Tree a -> [a]
followCrumbsWithTrail (c:cs) (Node rl kids) =
    rl : (followCrumbsWithTrail cs (kids !! c))
followCrumbsWithTrail [] node = [rootLabel node]

-- A wrapper around the monadic dropCrumbsM below

dropCrumbs :: (Ord b) => (a -> b) -> Tree a -> (b, Crumbs)
dropCrumbs m tree = runWriter $ dropCrumbsM m tree

-- Given a tree and some metric m (which will typically be a function that
-- scores a sequence according to a model held at the tree node), generates a
-- list of crumbs by recursively applying m to a node's children and calling
-- itself on the best-scoring child, until a leaf is reached. Returns the score
-- of that leaf, as well as a list of crumbs followed to reach it, as a Writer
-- monad.

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

{-
 - Extended Crumbs: these crumbs not only track the _index_ of the "best" child
 - according to the metric m (again, think of a sequence scoring function), but
 - also record the two best values of on a set of sibling nodes. This enables to
 - compute e.g. the log odds ratio of the best to the next best, etc.
 - -}

data ExtendedCrumb = ExtendedCrumb {
        siblingIndex    :: Int,
        bestValue       :: Int,
        secondBestValue :: Int
        }

type ExtendedCrumbTrail = [ExtendedCrumb]

-- A wrapper around the monadic dropExtendedCrumbsM below

dropExtendedCrumbs :: (Ord b) => (a -> b) -> Tree a -> (b, Crumbs)
dropExtendedCrumbs m tree = runWriter $ dropCrumbsM m tree

-- like dropCrumbsM, but with extended crumbs.

dropExtendedCrumbsM m (Node rl []) = return $ m rl
dropExtendedCrumbsM m (Node rl kids) = do
    let (bestKid, bestNdx, bestScore, secondBestScore) = bestByExtended kids m'
    tell [(bestNdx, bestScore, secondBestScore)] 
    dropExtendedCrumbsM m $ bestKid
    where m' (Node rl _) = m rl

-- finds the (first) object in a list that maximizes some metric m (think score
-- of a sequence according to a model), returns that object and its index in
-- the list, as well as the best score and second-best score themselves. Not
-- efficient, but should be ok for short lists.

bestByExtended :: Ord b => [a] -> (a -> b) -> (a, Int, b, b)
bestByExtended objs m = (bestObj, bestNdx, bestMetricValue, secondBestMetricValue)
    where   sorted = reverse $ sort $ metricValues
            metricValues = map m objs
            bestMetricValue = sorted !! 0
            secondBestMetricValue = sorted !! 1
            bestNdx = head $ elemIndices bestMetricValue metricValues
            bestObj = objs !! bestNdx
            
