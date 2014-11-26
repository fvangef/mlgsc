-- General Sequence Classifier

-- Classifies a set of sequences using a classifier produced by gsc_mk.

-- module Main where

import System.Environment (getArgs)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.Text as ST
import qualified Data.Text.IO as STIO
import Text.Printf

import Data.Binary (decodeFile)
import Data.Tree
import MlgscTypes
import FastA
import Crumbs (dropCrumbs, followCrumbs, followCrumbsWithTrail,
    followExtendedCrumbsWithTrail)
import NucModel
import Classifier (NucClassifier, otuTree, modTree, scoreSequenceWithCrumbs,
        scoreSequenceWithExtendedCrumbs)
import Output

main :: IO ()
main = do
    [queriesFname, classifierFname] <- getArgs   
    queryFastA <- LTIO.readFile queriesFname
    let queryRecs = fastATextToRecords queryFastA
    classifier <- (decodeFile classifierFname) :: IO NucClassifier
    let headers = map FastA.header queryRecs
    let predictions = map (classifySequenceWithExtendedTrail classifier .
                            LT.toStrict . FastA.sequence) queryRecs
    mapM_ STIO.putStrLn $ zipWith output headers predictions
