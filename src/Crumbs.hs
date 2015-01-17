-- module Crumbs (followCrumbs) where
module Crumbs (
    dropExtendedCrumbs,
    followExtendedCrumbsWithTrail,
    ) where

import Data.Tree
import Data.List
import Control.Monad.Writer

import CladeModel

data ScoredPathStep = ScoredPathStep {
                childIndex  :: Int
                , gold      :: Int  -- best score
                , silver    :: Int  -- second-best score (no bronze...)
                }

type ScoredPath = [ScoredPathStep]

type ExtCrumb = (Int, Int, Int)
type ExtCrumbs = [ExtCrumb]

{-
 - Extended Crumbs: these crumbs not only track the _index_ of the "best" child
 - according to the metric m (again, think of a sequence scoring function), but
 - also record the two best values of on a set of sibling nodes. This enables to
 - compute e.g. the log odds ratio of the best to the next best, etc.
 - -}

-- A wrapper around the monadic dropExtendedCrumbsM below

-- dropExtendedCrumbs :: (Ord b) => (a -> b) -> Tree a -> (b, Crumbs)
dropExtendedCrumbs m tree = runWriter $ dropExtendedCrumbsM m tree

-- like dropCrumbsM, but with extended crumbs.

dropExtendedCrumbsM :: (CladeModel -> Int) -> Tree CladeModel -> Writer ExtCrumbs Int
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
            
-- Follows an extended crumbs trail, returning labels and score ratios

-- followExtendedCrumbsWithTrail :: Ord b => [(Int, b, b)] -> Tree a -> [a]
followExtendedCrumbsWithTrail ((ndx, best, secBest):cs) (Node rl kids) =
    (rl, best, secBest) : (followExtendedCrumbsWithTrail cs (kids !! ndx))
followExtendedCrumbsWithTrail [] node = [(rootLabel node, 1, 1)]
