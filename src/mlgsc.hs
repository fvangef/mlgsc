-- General Sequence Classifier

-- Classifies a set of sequences using a classifier produced by gsc_mk.

-- module Main where

import System.Environment (getArgs)
--import System.Console.GetOpt
--import System.IO
--
import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.Text.Lazy as LT
import qualified Data.Text.IO as STIO

import Data.Binary (decodeFile)
import Data.Tree
--import System.Directory
--
--import MlgscTypes
import FastA
import Crumbs (dropCrumbs, followCrumbs)
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
    let (score, crumbs) = scoreSequenceWithCrumbs classifier query  
    let otu = followCrumbs crumbs $ otuTree classifier
    STIO.putStrLn otu

