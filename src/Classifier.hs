module Classifier (NucClassifier, buildNucClassifier, scoreCrumbs) where

import Data.Tree
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Binary (Binary, put, get, Get)
import Data.Text.Binary

import MlgscTypes
import CladeModel
import NucModel

data NucClassifier = NucClassifier {
                        otuTree :: OTUTree,
                        modTree :: Tree NucModel
                        }

instance Binary NucClassifier where
    put classifier = do
        put $ otuTree classifier
        put $ modTree classifier
    
    get = do
        otuTree <- get :: Get OTUTree
        modTree <- get :: Get (Tree NucModel)
        return $ NucClassifier otuTree modTree
        
-- TODO: make NucClassifier and instance of Data.Binary.
--
buildNucClassifier  :: SmallProb -> ScaleFactor
                    -> OTUToAlnMap -> OTUTree -> NucClassifier
buildNucClassifier smallprob scale map otuTree = NucClassifier otuTree modTree
    where   modTree         = fmap (alnToNucModel smallprob scale) treeOfAlns
            treeOfAlns      = mergeAlns treeOfLeafAlns
            treeOfLeafAlns  = fmap (\k -> M.findWithDefault [] k map) otuTree

-- TODO: is this ever used?
--otuLookup :: OTUToAlnMap -> OTUName -> Alignment
--otuLookup map otu = undefined

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

