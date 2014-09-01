import qualified Data.Text.Lazy as LT
import qualified Data.Map.Strict as M
import Data.Tree

import MlgscTypes
import NewickParser
import Classifier
import FastA
import Crumbs

-- Toy data for ghci

fastaInput = unlines [
    ">Aeromonas", 
    "ACGTACGT",
    ">Aeromonas", 
    "ACGTACGT",
    ">Aeromonas", 
    "ACGTACGT",
    ">Aeromonas", 
    "ACGTACGT",
    ">Aeromonas", 
    "ACGTACGT",
    ">Bacillus", 
    "BCGTACGT",
    ">Bacillus", 
    "BCGTACGT",
    ">Bacillus", 
    "BCGTACGT",
    ">Clostridium", 
    "CCGTACGT",
    ">Clostridium", 
    "CCGTACGT",
    ">Clostridium", 
    "CCGTACGT",
    ">Clostridium", 
    "CCGTACGT"
    ]

newick = "(Aeromonas,(Bacillus,Clostridium));"
(Right tree) = parseNewickTree newick

fastaRecs2 = fastATextToRecords $ LT.pack fastaInput
fastAMap = fastARecordsToAlnMap fastaRecs2

-- maps alignments to OTU names within a tree
treeOfLeafAlns = fmap (\k -> M.findWithDefault [] k fastAMap) tree

-- produces a new tree of which each node's data is a concatenation of its
-- children node's data. Meant to be called on a Tree Alignment  whose inner
-- nodes are empty. To see it in action, do
--   putStrLn $ drawTree $ fmap show $ mergeAlns treeOfLeafAlns
-- in GHCi.

mergeAlns :: Tree Alignment -> Tree Alignment
mergeAlns leaf@(Node _ []) = leaf
mergeAlns (Node _ kids) = Node mergedKidAlns mergedKids
    where   mergedKids = map mergeAlns kids
            mergedKidAlns = concatMap rootLabel mergedKids


treeOfAlns = mergeAlns treeOfLeafAlns

-- TODO: covert treeOfAlns into a tree of NucModels, by fmapping the NucModel
-- constructor. Them it should be possible to score a sequence by using
-- dropCrumbs.
