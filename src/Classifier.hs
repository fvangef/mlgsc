module Classifier (
    Classifier(..),
    buildClassifier,
    classifySequence,
    classifySequenceMulti,
    leafOTU) where

import Data.Tree
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Binary (Binary, put, get, Get)
import Data.Ord

import MlgscTypes
import Alignment
import NucModel
import PepModel
import PWMModel (PWMModel(..), scoreSeq, cladeName)

-- TODO: rename CladeModel to PWMModel, and the Classifier c'tor to
-- PWMClassifier. This  should have the scale factor as a second argument. In
-- this way we can add other kinds of models, e.g. if we add a k-mer model, it
-- could look like:
-- data Classifier = PWMClassifier (Tree PWMModel) ScaleFactor
--                 | KmerClassifier (Tree KmerModel)
--                 

data Classifier = PWMClassifier (Tree PWMModel) ScaleFactor
                deriving (Show, Eq)

instance Binary Classifier where
    put (PWMClassifier modTree scaleFactor) = do
        put modTree
        put scaleFactor

    get = do
        modTree <- get :: Get (Tree PWMModel)
        scaleFactor <- get :: Get ScaleFactor
        return $ PWMClassifier modTree scaleFactor

buildClassifier :: Molecule -> SmallProb -> ScaleFactor ->
    AlnMap -> OTUTree -> Classifier
buildClassifier mol smallProb scale alnMap otuTree 
    = case mol of
        DNA -> buildNucClassifier smallProb scale alnMap otuTree
        Prot -> buildPepClassifier smallProb scale alnMap otuTree


-- TODO: these two are almost identical: refactor and pass the alnt-to-model
-- function as a parameter in the case clause of buildClassifier above.

buildNucClassifier  :: SmallProb -> ScaleFactor -> AlnMap -> OTUTree
    -> Classifier
buildNucClassifier smallprob scale map otuTree =
    PWMClassifier cladeModTree scale
    where   cladeModTree = fmap NucPWMModel modTree
            modTree = fmap (\(name, aln) -> 
                            alnToNucModel smallprob scale name aln)
                            treeOfNamedAlns
            treeOfNamedAlns = mergeNamedAlns treeOfLeafNamedAlns
            treeOfLeafNamedAlns =
                fmap (\k -> (k, M.findWithDefault [] k map)) otuTree

buildPepClassifier  :: SmallProb -> ScaleFactor -> AlnMap -> OTUTree
    -> Classifier
buildPepClassifier smallprob scale map otuTree =
    PWMClassifier cladeModTree scale
    where   cladeModTree = fmap PepPWMModel modTree
            modTree = fmap (\(name, aln) -> 
                            alnToPepModel smallprob scale name aln)
                            treeOfNamedAlns
            treeOfNamedAlns = mergeNamedAlns treeOfLeafNamedAlns
            treeOfLeafNamedAlns =
                fmap (\k -> (k, M.findWithDefault [] k map)) otuTree

-- The Int parameter is the log_10(ER) cutoff (the support value of nodes in the
-- path in the default output). 

classifySequence :: Classifier -> Int -> Sequence -> Trail
classifySequence (PWMClassifier modTree scale) log10ERcutoff seq =
    chooseSubtree modTree scale log10ERcutoff seq

chooseSubtree :: Tree PWMModel -> ScaleFactor -> Int -> Sequence -> Trail
chooseSubtree (Node _ []) _ _ _ = []
chooseSubtree (Node model kids) scale cutoff seq
    | diff < (round scale * cutoff)   = []
    | otherwise     = PWMStep bestKidName bestKidScore
                        sndBestKidScore log10ER
                        : chooseSubtree bestKid scale cutoff seq
    where   diff = bestKidScore - sndBestKidScore
            bestKidName = cladeName $ rootLabel bestKid
            (bestKid, Down bestKidScore) = orderedKids !! 0
            (sndBestKid, Down sndBestKidScore) = orderedKids !! 1
            orderedKids = L.sortBy (comparing snd) $ zip kids (map Down scores)
            scores = map (flip scoreSeq seq . rootLabel) kids
            log10ER = log10evidenceRatio (round scale) bestKidScore sndBestKidScore

-- TODO: this one should be named "all" instead of multi, as it explores _all_
-- branches of the tree. Intended mainly for debugging, as it enables to see a
-- query's score at every node of the tree, and therefore allows identifying
-- where the classifier chooses the wrong branch. The recursion starts at the
-- root (rather than at its children), so we get rid of the Trail's head (hence
-- the call to map tail).

classifySequenceMulti :: Classifier -> Int -> Sequence -> [Trail]
classifySequenceMulti (PWMClassifier modTree scale) log10ERcutoff seq =
    map tail $ chooseSubtrees modTree scale log10ERcutoff seq bestScore
        where bestScore = maximum $ map (flip scoreSeq seq . rootLabel) (subForest modTree)

chooseSubtrees :: Tree PWMModel -> ScaleFactor -> Int -> Sequence -> Score -> [Trail]
chooseSubtrees (Node model []) scale _ seq bestScore = [[PWMStep name score (-1) log10ER]]
    where   name = cladeName model
            score = scoreSeq model seq
            log10ER = log10evidenceRatio (round scale) bestScore score
chooseSubtrees (Node model kids) scale cutoff seq bestScore = 
    map (thisstep:) $ concat $ map (\kid -> chooseSubtrees kid scale cutoff seq bestKidScore) kids
    where   thisstep = PWMStep (cladeName model) score (-1) log10ER
            score = scoreSeq model seq
            log10ER = log10evidenceRatio (round scale) bestScore score
            bestKidScore = maximum $ map (flip scoreSeq seq . rootLabel) kids

paths :: OTUTree -> [[OTUName]]
paths (Node name []) = [[name]]
paths (Node name kids) = map (name:) $ foldl1 (++) $ map paths kids
-- finds the (first) object in a list that maximizes some metric m (think score
-- of a sequence according to a model), returns that object and its index in
-- the list, as well as the best score and second-best score themselves. Not
-- efficient, but should be ok for short lists.

-- TODO: if we no longer need the indices, this is way to complicated.
bestByExtended :: Ord b => [a] -> (a -> b) -> (a, Int, b, b)
bestByExtended objs m = (bestObj, bestNdx, bestMetricValue, secondBestMetricValue)
    where   sorted = L.sortBy (flip compare) metricValues
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
leafOTU trail = otuName $ last trail

-- Computes the base-10 log of the evidence ratio, i.e. log_10 (exp(delta-AIC /
-- 2)), except that I use delta-AIC' (in which the factor 2 is dropped, so I
-- avoid having to multiply by 2 only to divide by 2 again just after).

log10evidenceRatio :: Int -> Int -> Int -> Double
log10evidenceRatio scaleFactor bestScore secondBestScore = logBase 10 er
    where   l_min = scoreTologLikelihood scaleFactor bestScore
            l_sec = scoreTologLikelihood scaleFactor secondBestScore
            er = exp(deltaAIC' l_min l_sec) 

-- Converts a model score (which is a scaled, rounded log-likelihood (log base
-- 10)) to a log-likelihood (log base e, i.e. ln). To do this, we _divide_ by
-- the scale factor to get an unscaled log10-likelihood, and then divide by
-- log10(e) to get a ln-based likelihood.

scoreTologLikelihood :: Int -> Int -> Double
scoreTologLikelihood scaleFactor score = log10Likelihood / logBase 10 e
    where   log10Likelihood = fromIntegral score / fromIntegral scaleFactor
            e = exp 1.0
            
-- Computes the difference in AIC of two log-likelihoods, taking into account
-- that the number of parameters k is in our case the same in any two models,
-- and this cancels out, i.e. delta AIC = AIC1 - AIC2 = 2k -2 ln (L_1) - (2k -
-- 2 ln(L_2)) = -2 (ln (L_1) - ln (L_2)). Since the arguments are already _log_
-- likelihoods, the expression simplifies to -2 (l_1 - l_2), where l_1 =
-- ln(L_1), etc. I also drop the constant 2, since we'd be dividing by 2 right
-- away in evidenceRatio anyway.

deltaAIC' :: Double -> Double -> Double
deltaAIC' l1 l2 = l1 - l2 
