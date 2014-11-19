-- General Sequence Classifier

-- Classifies a set of sequences using a classifier produced by gsc_mk.

-- module Main where

import System.Environment (getArgs)
import Control.Applicative
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
import Crumbs (dropCrumbs, followCrumbs, followCrumbsWithTrail,
    followExtendedCrumbsWithTrail)
import NucModel
import Classifier (NucClassifier, otuTree, modTree, scoreSequenceWithCrumbs,
        scoreSequenceWithExtendedCrumbs)

--import Trees
--import Align
--import Cdf

main :: IO ()
main = do
    [queriesFname, classifierFname] <- getArgs   
    queryFastA <- LTIO.readFile queriesFname
    let queryRecs = fastATextToRecords queryFastA
    classifier <- (decodeFile classifierFname) :: IO NucClassifier
    mapM_ STIO.putStrLn $ map (classifySequenceWithExtendedTrail classifier .
                            LT.toStrict . FastA.sequence) queryRecs

-- classifySequence :: NucClassifier -> Sequence -> LT.Text
classifySequence classifier query = otu
    where   (score, crumbs) = scoreSequenceWithCrumbs classifier query  
            otu = followCrumbs crumbs $ otuTree classifier

-- classifySequence :: NucClassifier -> Sequence -> LT.Text
classifySequenceWithTrail classifier query = taxo
    where   (score, crumbs) = scoreSequenceWithCrumbs classifier query  
            taxo = ST.intercalate (ST.pack "; ") $ followCrumbsWithTrail crumbs $ otuTree classifier

-- classifySequence :: NucClassifier -> Sequence -> LT.Text

classifySequenceWithExtendedTrail classifier query = trailToTaxo trail
    where   (score, crumbs) = scoreSequenceWithExtendedCrumbs classifier query  
            -- extendedTaxo = ST.intercalate (ST.pack ";dd.. ") taxoList
            trail = followExtendedCrumbsWithTrail crumbs $ otuTree classifier

trailToExtendedTaxo trail = ST.intercalate (ST.pack "; ") eLbl
    where   labels = tail $ map (\(lbl,_,_) -> lbl) trail
            confid = init $ map (\(_,best,second) -> best - second) trail
            eLbl = getZipList $ toElbl <$> ZipList labels <*> ZipList confid
            toElbl lbl conf = ST.concat [lbl,
                                         ST.pack " (", 
                                         ST.pack $ show conf,
                                         ST.pack ")"]

trailToTaxo trail = ST.intercalate (ST.pack "; ") $ map phyloNode2Text trail
 
phyloNode2Text (lbl, best, second) = ST.concat [
                                     lbl,
                                     ST.pack $ show best
                                     ]
 
