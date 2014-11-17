-- General Sequence Classifier

-- Classifies a set of sequences using a classifier produced by gsc_mk.

-- module Main where

import System.Environment (getArgs)
--import System.Console.GetOpt
--import System.IO
--
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.Text as ST
import qualified Data.Text.IO as STIO

import Data.Binary (decodeFile)
import Data.Tree
--import System.Directory
--
import MlgscTypes
import FastA
import Crumbs (dropCrumbs, followCrumbs, followCrumbsWithTrail)
import NucModel
import Classifier (NucClassifier, otuTree, modTree, scoreSequenceWithCrumbs)

--import Trees
--import Align
--import Cdf

main :: IO ()
main = do
    [queriesFname, classifierFname] <- getArgs   
    queryFastA <- LTIO.readFile queriesFname
    let queryRecs = fastATextToRecords queryFastA
    classifier <- (decodeFile classifierFname) :: IO NucClassifier
    let query = LT.toStrict $ FastA.sequence $ queryRecs !! 2
    let otu = classifySequenceWithTrail classifier query
    STIO.putStrLn otu

-- classifySequence :: NucClassifier -> Sequence -> LT.Text
classifySequence classifier query = otu
    where   (score, crumbs) = scoreSequenceWithCrumbs classifier query  
            otu = followCrumbs crumbs $ otuTree classifier

-- classifySequence :: NucClassifier -> Sequence -> LT.Text
classifySequenceWithTrail classifier query = taxo
    where   (score, crumbs) = scoreSequenceWithCrumbs classifier query  
            taxo = ST.intercalate (ST.pack "; ") $ followCrumbsWithTrail crumbs $ otuTree classifier
