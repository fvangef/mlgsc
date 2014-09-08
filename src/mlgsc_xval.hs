-- General Sequence Classifier

-- Cross-validates a model

-- module Main where

import System.Environment (getArgs)
--import System.Console.GetOpt
--import System.IO
--
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.Text.Lazy as LT
import qualified Data.Text as ST
import qualified Data.Text.IO as STIO

import Data.Binary (decodeFile)
import Data.Tree
--import System.Directory
--
import MlgscTypes
import FastA
import Crumbs (dropCrumbs, followCrumbs)
import NucModel
import Classifier (NucClassifier, buildNucClassifier)
import NewickParser
import NewickDumper

--import Trees
--import Align
--import Cdf

main :: IO ()
main = do
    let smallProb = 0.0001
    let scaleFactor = 1000
    [alnFname, newickFname] <- getArgs
    newickString <- readFile newickFname
    let (Right tree) = parseNewickTree newickString
    fastAInput <-  LTIO.readFile alnFname
    let fastARecs = fastATextToRecords fastAInput
    let (trainSet, testSet) = splitAln fastARecs
    let otuAlnMap = fastARecordsToAlnMap trainSet
    let classifier = buildNucClassifier smallProb scaleFactor otuAlnMap tree
    -- Remove OTUs from the test set if they're not in the tree - can't be used
    -- for testing
    let testInTree = dropOTUsNotInTree tree testSet
    -- Also remove those not in train set - no chance of identifying them
    putStrLn "The following are retained"
    let testInTrainSet = dropOTUsNotInTrainSet otuAlnMap testInTree
    mapM_ (LTIO.putStrLn . header) testInTrainSet


dumpAlnMap :: OTUToAlnMap -> [String]
dumpAlnMap otuAlnMap = map f $ M.assocs otuAlnMap
    where f (k, v) = otu ++ " (" ++ num ++ " seq)"
            where   otu = ST.unpack k
                    num = show $ length v 

-- Splits the FastA input into a training set and a test set (half and half).
-- This should work unless two genera happen to occur mixed with each other,
-- in perfect alternation. Not very likely, I surmise.

splitAln :: [FastA] -> ([FastA], [FastA])
splitAln (rec1:rec2:rest) = (rec1:rest1, rec2:rest2)
    where (rest1, rest2) = splitAln rest
    

-- Takes a list of FastA records (meant to be the test set) drops any
-- records whose OTU is not found in the tree.

dropOTUsNotInTree :: OTUTree -> [FastA] -> [FastA]
dropOTUsNotInTree otuTree testFastARecs =
    filter (\fasta -> S.member (LT.toStrict $ header fasta) treeOtus) testFastARecs
        where   treeOtus = S.fromList $ fringe otuTree

dropOTUsNotInTrainSet :: OTUToAlnMap -> [FastA] -> [FastA]
dropOTUsNotInTrainSet otuAlnMap testFastARecs =
    filter (\fasta -> M.member (LT.toStrict $ header fasta) otuAlnMap) testFastARecs
