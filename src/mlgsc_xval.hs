-- General Sequence Classifier

-- Cross-validates a model

-- module Main where

import System.Environment (getArgs)
import System.Random
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import qualified Data.Set as S
import Data.Foldable (toList)
import Data.Sequence ((><), Seq)
import qualified Data.Sequence as Sq
import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.Text.Lazy as LT
import qualified Data.Text as ST
import qualified Data.Text.IO as STIO

import Data.Binary (decodeFile)
import Data.Tree

import MlgscTypes
import FastA
import Alignment
import Crumbs (dropCrumbs, followCrumbs)
import NucModel
import Classifier (NucClassifier, otuTree, 
        buildNucClassifier, scoreSequenceWithCrumbs)
import NewickParser
import NewickDumper
import Weights
import Shuffle
import Output

main :: IO ()
main = do
    let smallProb = 0.0001
    let scaleFactor = 1000
    [alnFname, newickFname] <- getArgs
    newickString <- readFile newickFname
    let (Right tree) = parseNewickTree newickString
    fastAInput <-  LTIO.readFile alnFname
    let fastARecs = Sq.fromList $ fastATextToRecords fastAInput
    let bounds = (0, Sq.length fastARecs)
    gen <- getStdGen
    let randomIndices = take 100 $ shuffleList gen $ validIndices fastARecs
    putStrLn ("Performing LOO X-val on indices " ++ (show randomIndices))
    mapM_ (STIO.putStrLn . leaveOneOut tree fastARecs) randomIndices

-- Return a list of indices of valid FastA records (index in the records
-- Sequence). A record is valid, for LOO purposes, if it pertains to an OTU with
-- at least 2 members - otherwise, if one would remove the single instance of a
-- given OTU, there is no way the classifier can recognize it, as it won't be
-- represented in the training set.

validIndices :: Seq FastA -> [Int]
validIndices fastARecs = map snd validFreqIdxPairs
    where   validFreqIdxPairs = filter
                        (\(freq, index) -> freq > 1) freqIdxPairs
            freqIdxPairs = toList $ Sq.mapWithIndex toFreqIdxPair fastARecs
            otu2freq = M.fromListWith (+) [(otu, 1) | otu <- fastAOTUs] 
            fastAOTUs = toList $ fmap fastAOTU fastARecs
            toFreqIdxPair idx fasta = (freq, idx)
                where   freq = otu2freq ! (fastAOTU fasta)
                        

scoreQuery :: NucClassifier -> FastA -> String
scoreQuery classifier query =
    (LT.unpack $ FastA.header query) ++ " -> " ++ (ST.unpack otu) ++ " (" 
    ++ (show score) ++ ")"
    where   otu = followCrumbs crumbs $ otuTree classifier
            (score, crumbs) = scoreSequenceWithCrumbs classifier querySeq 
            querySeq = LT.toStrict $ FastA.sequence query

-- Given any Seq, returns its nth element as well as all the others, in the
-- same order. Note: this is a Data.Sequence.Seq, not a (biomolecular)
-- Sequence. I just use a Seq instead of a list because it's faster.

spliceElemAt :: Seq a -> Int -> (a, Seq a)
spliceElemAt seq n = (elem, head >< tail)
    where   (head, rest) = Sq.splitAt n seq
            elem = Sq.index rest 0
            tail = Sq.drop 1 rest

-- Given a Newick tree, a Seq of FastA records, and the index of one of the
-- records, builds a test set consisting of that record, and a training set
-- consisting of all the rest, then trains a classifier on the test set and the
-- newick tree, and finally scores the test sequence on the classifier. IOW,
-- does one leave-one-out test.

-- TODO: rmove the hard-coded constants below!

leaveOneOut :: OTUTree -> Seq FastA -> Int -> ST.Text
leaveOneOut tree fastaRecs n = ST.concat [header, ST.pack " -> ", prediction]
    where   header = LT.toStrict $ FastA.header testRec
            prediction = classifySequenceWithExtendedTrail classifier testSeq
            testSeq = LT.toStrict $ FastA.sequence testRec
            classifier = buildNucClassifier smallProb scaleFactor otuAlnMap tree
            smallProb = 0.0001
            scaleFactor = 1000
            otuAlnMap = alnToAlnMap wtOtuAln
            wtOtuAln = henikoffWeightAln otuAln
            otuAln = fastARecordsToAln trainSet
            trainSet = Data.Foldable.toList trainSetSeq
            (testRec, trainSetSeq) = spliceElemAt fastaRecs n

