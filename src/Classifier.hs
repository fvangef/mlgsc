module Classifier (
    Classifier(..),
    buildClassifier,
    classifySequence,
    leafOTU) where

import Data.Tree
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Binary (Binary, put, get, Get)
import Data.Text.Binary
import qualified Data.Text.Lazy as LT
import qualified Data.Text as ST
import Data.Ord

import MlgscTypes
-- import CladeModel
import Alignment
import NucModel
import PepModel
import CladeModel (CladeModel(..), scoreSeq, cladeName)

data Classifier = Classifier OTUTree (Tree CladeModel)
                deriving (Show, Eq)

instance Binary Classifier where
    put (Classifier otuTree modTree) = do
        put otuTree
        put modTree

    get = do
        otuTree <- get :: Get OTUTree
        modTree <- get :: Get (Tree CladeModel)
        return $ Classifier otuTree modTree

buildClassifier :: Molecule -> SmallProb -> ScaleFactor ->
    AlnMap -> OTUTree -> Classifier
buildClassifier mol smallProb scale alnMap otuTree 
    = case mol of
        DNA -> buildNucClassifier smallProb scale alnMap otuTree
        Prot -> buildPepClassifier smallProb scale alnMap otuTree

{-
buildNucClassifier  :: SmallProb -> ScaleFactor
                    -> AlnMap -> OTUTree -> Classifier
buildNucClassifier smallprob scale map otuTree = Classifier otuTree modTree
    where   modTree         = fmap (NucCladeModel . alnToNucModel smallprob scale) treeOfAlns
            treeOfAlns      = mergeAlns treeOfLeafAlns
            treeOfLeafAlns  = fmap (\k -> M.findWithDefault [] k map) otuTree
-}

-- TODO: these two are almost identical: refactor and pass the alnt-to-model
-- function as a parameter in the case clause of buildClassifier above.

buildNucClassifier  :: SmallProb -> ScaleFactor -> AlnMap -> OTUTree
    -> Classifier
buildNucClassifier smallprob scale map otuTree =
    Classifier otuTree cladeModTree
    where   cladeModTree = fmap NucCladeModel modTree
            modTree = fmap (\(name, aln) -> 
                            alnToNucModel smallprob scale name aln)
                            treeOfNamedAlns
            treeOfNamedAlns = mergeNamedAlns treeOfLeafNamedAlns
            treeOfLeafNamedAlns =
                fmap (\k -> (k, M.findWithDefault [] k map)) otuTree

buildPepClassifier  :: SmallProb -> ScaleFactor -> AlnMap -> OTUTree
    -> Classifier
buildPepClassifier smallprob scale map otuTree =
    Classifier otuTree cladeModTree
    where   cladeModTree = fmap PepCladeModel modTree
            modTree = fmap (\(name, aln) -> 
                            alnToPepModel smallprob scale name aln)
                            treeOfNamedAlns
            treeOfNamedAlns = mergeNamedAlns treeOfLeafNamedAlns
            treeOfLeafNamedAlns =
                fmap (\k -> (k, M.findWithDefault [] k map)) otuTree

-- TODO: OutputData seems too complex, as the score is actually found in the
-- trail.

classifySequence :: Classifier -> Sequence -> Trail
classifySequence (Classifier _ modTree) seq = scoreSequence seq modTree

scoreSequence :: Sequence -> Tree CladeModel -> Trail
scoreSequence seq (Node model []) = []
scoreSequence seq (Node model kids) = 
    (bestKidName, bestKidScore, sndBestKidScore)  : (scoreSequence seq bestKid)
    where   bestKidName = cladeName $ rootLabel bestKid
            (bestKid, (Down bestKidScore)) = orderedKids !! 0
            (sndBestKid, (Down sndBestKidScore)) = orderedKids !! 1
            orderedKids = L.sortBy (comparing snd) $ zip kids (map Down scores)
            scores = map (flip scoreSeq seq . rootLabel) kids

{-
scoreSequenceM :: (CladeModel -> Int) -> Tree CladeModel -> Writer ExtCrumbs Int
scoreSequenceM scoreFunction (Node rl []) = return $ scoreFunction rl
scoreSequenceM scoreFunction (Node rl kids) = do
    let (bestKid, bestNdx, bestScore, secondBestScore) = bestByExtended kids scoreFunction'
    tell [(bestNdx, bestScore, secondBestScore)] 
    dropExtendedCrumbsM scoreFunction $ bestKid
    where scoreFunction' (Node rl _) = scoreFunction rl
-}

-- finds the (first) object in a list that maximizes some metric m (think score
-- of a sequence according to a model), returns that object and its index in
-- the list, as well as the best score and second-best score themselves. Not
-- efficient, but should be ok for short lists.

-- TODO: if we no longer need the indices, this is way to complicated.
bestByExtended :: Ord b => [a] -> (a -> b) -> (a, Int, b, b)
bestByExtended objs m = (bestObj, bestNdx, bestMetricValue, secondBestMetricValue)
    where   sorted = reverse $ L.sort $ metricValues
            metricValues = map m objs
            bestMetricValue = sorted !! 0
            secondBestMetricValue = sorted !! 1
            bestNdx = head $ L.elemIndices bestMetricValue metricValues
            bestObj = objs !! bestNdx

-- produces a new tree of which each node's data is a concatenation of its
-- children node's data. Meant to be called on a Tree Alignment  whose inner
-- nodes are empty. To see it in action, do
--   putStrLn $ drawTree $ fmap show $ mergeAlns treeOfLeafAlns
-- in GHCi.

{-
mergeAlns :: Tree Alignment -> Tree Alignment
mergeAlns leaf@(Node _ []) = leaf
mergeAlns (Node _ kids) = Node mergedKidAlns mergedKids
    where   mergedKids = L.map mergeAlns kids
            mergedKidAlns = concatMap rootLabel mergedKids
-}

mergeNamedAlns :: Tree (CladeName, Alignment) -> Tree (CladeName, Alignment)
mergeNamedAlns leaf@(Node _ []) = leaf
mergeNamedAlns (Node (name,_) kids) = Node (name,mergedKidAlns) mergedKids
    where   mergedKids = L.map mergeNamedAlns kids
            mergedKidAlns = concatMap (snd . rootLabel) mergedKids

leafOTU :: Trail -> OTUName
leafOTU trail = otuName
    where (otuName, _, _) = last trail
