module Classifier (
    Classifier(..),
    buildClassifier,
    classifySequenceWithExtendedTrail,
    classifySequence,
    scoreSeq,
    scseq,          -- TODO: streamline exports
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
import SimplePepModel
import Crumbs
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
        -- Prot -> buildPepClassifier smallProb scale alnMap otuTree
        Prot -> buildSimplePepClassifier smallProb scale alnMap otuTree

buildNucClassifier  :: SmallProb -> ScaleFactor
                    -> AlnMap -> OTUTree -> Classifier
buildNucClassifier smallprob scale map otuTree = Classifier otuTree modTree
    where   modTree         = fmap (NucCladeModel . alnToNucModel smallprob scale) treeOfAlns
            treeOfAlns      = mergeAlns treeOfLeafAlns
            treeOfLeafAlns  = fmap (\k -> M.findWithDefault [] k map) otuTree

buildPepClassifier  :: SmallProb -> ScaleFactor
                    -> AlnMap -> OTUTree -> Classifier
buildPepClassifier smallprob scale map otuTree = Classifier otuTree modTree
    where   modTree         = fmap (PepCladeModel . alnToPepModel smallprob scale) treeOfAlns
            treeOfAlns      = mergeAlns treeOfLeafAlns
            treeOfLeafAlns  = fmap (\k -> M.findWithDefault [] k map) otuTree

buildSimplePepClassifier  :: SmallProb -> ScaleFactor -> AlnMap -> OTUTree -> Classifier
buildSimplePepClassifier smallprob scale map otuTree =
    Classifier otuTree cladeModTree
    where   cladeModTree = fmap SimplePepCladeModel modTree
            modTree = fmap (\(name, aln) -> 
                            alnToSimplePepModel smallprob scale name aln)
                            treeOfNamedAlns
            treeOfNamedAlns = mergeNamedAlns treeOfLeafNamedAlns
            treeOfLeafNamedAlns =
                fmap (\k -> (k, M.findWithDefault [] k map)) otuTree

-- TODO: OutputData seems too complex, as the score is actually found in the
-- trail.
--
classifySequence :: Classifier -> Sequence -> OutputData
classifySequence (Classifier _ modTree) seq = OutputData trail 1
    where trail = scoreSequence (flip scoreSeq seq) modTree

-- dropExtendedCrumbs :: (Ord b) => (a -> b) -> Tree a -> (b, Crumbs)
scoreSequence :: (CladeModel -> Int) -> Tree CladeModel -> Trail
scoreSequence scoreFunction tree = scoreSequence' scoreFunction tree []

scoreSequence' :: (CladeModel -> Int) -> Tree CladeModel -> Trail -> Trail
scoreSequence' scoreFunction (Node model []) trail =
    (ST.empty, scoreFunction model, 1) : trail
scoreSequence' scoreFunction (Node model kids) trail =  
    scoreSequence' scoreFunction bestKid ((cladeName model, bestScore, secondBestScore) : trail)
    where   (bestKid, bestNdx, bestScore, secondBestScore) = bestByExtended kids scoreFunction'
            scoreFunction' (Node rl _) = scoreFunction rl


-- the use of Down is to reverse the order according to sortBy
--
scseq :: Sequence -> Tree CladeModel -> Int
scseq seq (Node model []) = scoreSeq model seq
scseq seq (Node _ kids) = scseq seq bestKid
    where   bestKid = fst $ head orderedKids
            orderedKids = L.sortBy (comparing snd) $ zip kids (map Down scores)
            scores = map (flip scoreSeq seq . rootLabel) kids

-- like dropCrumbsM, but with extended crumbs.

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

-- Classifies a Sequence according to a Classifier, yielding an OutputData

classifySequenceWithExtendedTrail :: Classifier -> Sequence -> OutputData
classifySequenceWithExtendedTrail classifier@(Classifier otuTree _) query
    = OutputData trail score
    where   trail = followExtendedCrumbsWithTrail crumbs otuTree
            (score, crumbs) = scoreSequenceWithExtendedCrumbs classifier query  

-- Same thing, but with ExtCrumbs

scoreSequenceWithExtendedCrumbs (Classifier _ modTree) seq =
    dropExtendedCrumbs (scoreCrumbs seq) modTree

-- Passsed a Sequence, returns a function CladeModel -> Int that can itelf be
-- passed to dropCrumbs, thereby scoring said sequence according to a classifier
-- and obtaining a crumbs trail. IOW, this is _meant_ to be called with only one
-- argument.

scoreCrumbs :: Sequence -> CladeModel -> Int
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

mergeNamedAlns :: Tree (CladeName, Alignment) -> Tree (CladeName, Alignment)
mergeNamedAlns leaf@(Node _ []) = leaf
mergeNamedAlns (Node (name,_) kids) = Node (name,mergedKidAlns) mergedKids
    where   mergedKids = L.map mergeNamedAlns kids
            mergedKidAlns = concatMap (snd . rootLabel) mergedKids

leafOTU :: OutputData -> OTUName
leafOTU od = otuName
    where (otuName, _, _) = last $ trail od
