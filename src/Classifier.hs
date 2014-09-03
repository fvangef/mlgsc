module Classifier (buildNucClassifier, scoreCrumbs) where

import Data.Tree
import qualified Data.Map.Strict as M
import qualified Data.List as L

import MlgscTypes
import CladeModel
import NucModel

buildNucClassifier  :: SmallProb -> ScaleFactor
                    -> OTUToAlnMap -> OTUTree -> Tree NucModel
buildNucClassifier smallprob scale map tree =
    fmap (alnToNucModel smallprob scale) treeOfAlns
    where   treeOfAlns      = mergeAlns treeOfLeafAlns
            treeOfLeafAlns  = fmap (\k -> M.findWithDefault [] k map) tree

-- TODO: is this ever used?
otuLookup :: OTUToAlnMap -> OTUName -> Alignment
otuLookup map otu = undefined

scoreCrumbs :: (CladeModel mod) => Sequence -> mod -> Int
scoreCrumbs seq mod = scoreSeq mod seq -- isn't this flip scoreSeq?

-- produces a new tree of which each node's data is a concatenation of its
-- children node's data. Meant to be called on a Tree Alignment  whose inner
-- nodes are empty. To see it in action, do
--   putStrLn $ drawTree $ fmap show $ mergeAlns treeOfLeafAlns
-- in GHCi.

mergeAlns :: Tree Alignment -> Tree Alignment
mergeAlns leaf@(Node _ []) = leaf
mergeAlns (Node _ kids) = Node mergedKidAlns mergedKids
    where   mergedKids = L.map mergeAlns kids
            mergedKidAlns = concatMap rootLabel mergedKids

