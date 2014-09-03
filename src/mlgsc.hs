-- General Sequence Classifier

-- Classifies a set of sequences using a classifier produced by gsc_mk.

-- module Main where

--import System.Environment (getArgs)
--import System.Console.GetOpt
--import System.IO
--
import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.Text.Lazy as LT
import Data.Binary (decodeFile)
import Data.Tree
--import System.Directory
--
--import MlgscTypes
import FastA
import Crumbs (dropCrumbs, followCrumbs)
import NucModel
import Classifier (scoreCrumbs)
--import Trees
--import Align
--import Cdf

main :: IO ()
main = do
    queryFastA <- LTIO.readFile "../data/firmicutes_gt1.aln"
    let queryRecs = fastATextToRecords queryFastA
    classifier <- (decodeFile "classifier.bcls") :: IO (Tree NucModel)
    let query = LT.toStrict $ FastA.sequence $ head queryRecs
    let (score, crumbs) = dropCrumbs (scoreCrumbs $ query) classifier
    let otu = followCrumbs 
    -- TODO: add original newick tree to the model
    return ()

