module Classifier (
    NucClassifier,
    otuTree, modTree,
    buildClassifier,
    scoreSequenceWithCrumbs,
    scoreSequenceWithExtendedCrumbs) where

import Data.Tree
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Binary (Binary, put, get, Get)
import Data.Text.Binary

import MlgscTypes
-- import CladeModel
import Alignment
import NucModel
import PepModel
import Crumbs

data Classifier = NucClassifier OTUTree (Tree NucModel)
                | PepClassifier OTUTree (Tree PepModel)
                deriving (Show, Eq)

buildClassifier :: Molecule -> SmallProb -> ScaleFactor ->
    AlnMap -> OTUTree -> Classifier
buildClassifier mol smallProb scale alnMap otuTree 
    = case mol of
        DNA -> buildNucClassifier smallProb scale alnMap otuTree
        Prot -> buildPepClassifier smallProb scale alnMap otuTree

instance Binary Classifier where
    put (NucClassifier otuTree nucModTree) = do
        put DNA
        put otuTree
        put nucModTree
    put (PepClassifier otuTree pepModTree) = do
        put Prot
        put otuTree
        put pepModTree
    
    get = do
        mol <- get :: Get Molecule
        otuTree <- get :: Get OTUTree
        case mol of
            DNA -> do
                nucModTree <- get :: Get (Tree NucModel)
                return $ NucClassifier otuTree nucModTree
            Prot -> do
                pepModTree <- get :: Get (Tree NucModel)
                return $ NucClassifier otuTree pepModTree

-- TODO: factor out these two

buildNucClassifier  :: SmallProb -> ScaleFactor
                    -> AlnMap -> OTUTree -> Classifier
buildNucClassifier smallprob scale map otuTree = NucClassifier otuTree modTree
    where   modTree         = fmap (alnToNucModel smallprob scale) treeOfAlns
            treeOfAlns      = mergeAlns treeOfLeafAlns
            treeOfLeafAlns  = fmap (\k -> M.findWithDefault [] k map) otuTree

buildPepClassifier  :: SmallProb -> ScaleFactor
                    -> AlnMap -> OTUTree -> Classifier
buildPepClassifier smallprob scale map otuTree = PepClassifier otuTree modTree
    where   modTree         = fmap (alnToPepModel smallprob scale) treeOfAlns
            treeOfAlns      = mergeAlns treeOfLeafAlns
            treeOfLeafAlns  = fmap (\k -> M.findWithDefault [] k map) otuTree

scoreSequenceWithCrumbs :: Classifier -> Sequence -> (Int, Crumbs)
scoreSequenceWithCrumbs (NucClassifier _ nucModTree) seq =
    dropCrumbs (scoreCrumbs seq) nucModTree

scoreSequenceWithExtendedCrumbs (NucClassifier _ nucModTree) seq =
    dropExtendedCrumbs (scoreCrumbs seq) nucModTree

-- Passsed a Sequence, returns a function (CladeModel mod) => mod -> int that
-- can itelf be passed to dropCrumbs, thereby scoring said sequence according to
-- a classifier and obtaining a crumbs trail. IOW, this is _meant_ to be called
-- with only one argument.

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

