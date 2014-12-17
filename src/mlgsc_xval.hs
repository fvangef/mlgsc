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
import Options.Applicative

import Data.Binary (decodeFile)
import Data.Tree

import MlgscTypes
import FastA
import Alignment
import Align
import Crumbs (dropCrumbs, followCrumbs)
import CladeModel
import NucModel
import Classifier (NucClassifier, otuTree, modTree,
        buildNucClassifier, scoreSequenceWithCrumbs)
import NewickParser
import NewickDumper
import Weights
import Shuffle
import Output

data Params = Params {
                optSmallProb        :: Double
                , optScaleFactor    :: Double
                , optNbRounds       :: Int
                , optSeed           :: Int
                , alnFname          :: String
                , treeFname         :: String
                }

parseSmallProb :: Parser Double
parseSmallProb = option auto
                    (long "small-prob"
                    <> short 'p'
                    <> metavar "SMALL_PROB"
                    <> value 0.0001
                    <> help "small probability for absent residues")

parseScaleFactor :: Parser Double
parseScaleFactor = option auto
                    (long "scale-factor"
                    <> short 's'
                    <> metavar "SCALE_FACTOR"
                    <> value 1000.0
                    <> help "scale factor for log(frequencies)")

parseNbRounds :: Parser Int
parseNbRounds = option auto
                    (long "nb_rounds"
                    <> short 'r'
                    <> metavar "NB_ROUNDS"
                    <> value 100
                    <> help "number of rounds of LOO")

parseSeed :: Parser Int
parseSeed = option auto
                    (long "random-seed"
                    <> short 'R'
                    <> metavar "RANDOM_SEED"
                    <> value (-1)
                    <> help "seed for the random number generator")

parseOptions :: Parser Params
parseOptions = Params
                <$> parseSmallProb
                <*> parseScaleFactor
                <*> parseNbRounds
                <*> parseSeed
                <*> argument str (metavar "<alignment file>")
                <*> argument str (metavar "<tree file>")

parseOptionsInfo :: ParserInfo Params
parseOptionsInfo = info (helper <*> parseOptions) 
                    ( fullDesc
                    <> progDesc "perform leave-one-out cross-validation"
                    <> Options.Applicative.header
                        "mlgsc_xval - cross-validate model from alignment and tree")

main :: IO ()
main = do
    params <- execParser parseOptionsInfo
    let smallProb = optSmallProb params
    let scaleFactor = optScaleFactor params
    let nbRounds = optNbRounds params
    newickString <- readFile $ treeFname params
    let (Right tree) = parseNewickTree newickString
    fastAInput <-  LTIO.readFile $ alnFname params
    let fastARecs = Sq.fromList $ fastATextToRecords fastAInput
    let bounds = (0, Sq.length fastARecs)
    gen <- getGen $ optSeed params
    let randomIndices = take nbRounds $ shuffleList gen $ validIndices fastARecs
    putStrLn ("Performing LOO X-val on indices " ++ (show randomIndices))
    mapM_ (STIO.putStrLn .
        leaveOneOut smallProb scaleFactor tree fastARecs) randomIndices

-- gets a random number generator. If the seed is negative, gets the global
-- generator, else use the seed.

getGen :: Int -> IO StdGen
getGen seed
    | seed < 0 = getStdGen
    | otherwise = return $ mkStdGen seed

-- Return a list of indices of valid FastA records (index in the records
-- Sequence). A record is valid, for LOO purposes, if it pertains to an OTU with
-- at least 2 members - otherwise, if one would remove the single instance of a
-- given OTU, there is no way the classifier can recognize it, as it won't be
-- represented in the training set.

validIndices :: Seq FastA -> [Int]
validIndices fastARecs = map snd validFreqIdxPairs
    where   validFreqIdxPairs = filter
                        (\(freq, index) -> freq > 3) freqIdxPairs
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

leaveOneOut :: SmallProb -> ScaleFactor -> OTUTree -> Seq FastA -> Int -> ST.Text
leaveOneOut smallProb scaleFactor tree fastaRecs n = ST.concat [header, ST.pack " -> ", prediction]
    where   header = LT.toStrict $ FastA.header testRec
            prediction = classifySequenceWithExtendedTrail
                classifier alignedTestSeq
            alignedTestSeq = msalign scoringScheme rootMod $ degap testSeq
            scoringScheme = ScoringScheme (-2)
                (scoringSchemeMap (absentResScore rootMod))
            rootMod = rootLabel $ modTree classifier 
            testSeq = LT.toStrict $ FastA.sequence testRec
            classifier = buildNucClassifier smallProb scaleFactor otuAlnMap tree
            otuAlnMap = alnToAlnMap wtOtuAln
            wtOtuAln = henikoffWeightAln otuAln
            otuAln = fastARecordsToAln trainSet
            trainSet = Data.Foldable.toList trainSetSeq
            (testRec, trainSetSeq) = spliceElemAt fastaRecs n

degap :: Sequence -> Sequence
degap = ST.replace gap ST.empty
    where gap = ST.pack "-"
