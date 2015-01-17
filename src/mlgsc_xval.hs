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
import CladeModel
import NucModel
import Classifier (Classifier(..), buildClassifier, classifySequenceWithExtendedTrail)
import NewickParser
import NewickDumper
import Weights
import Shuffle
import Output
import OutputFormatStringParser

data Params = Params {
                optSmallProb        :: Double
                , optScaleFactor    :: Double
                , optNbRounds       :: Int
                , optSeed           :: Int
                , optMinSeqInOTU    :: Int
                , optVerbosity      :: Int
                , optNoHenikoffWt   :: Bool
                , optOutFmtString   :: String
                , molType           :: Molecule
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

parseMinSeqInOTU :: Parser Int
parseMinSeqInOTU = option auto
                    (long "min-seq-ino-OTU"
                    <> short 'm'
                    <> metavar "MIN_SEQ_IN_OTU"
                    <> value (3)
                    <> help "minimum #member seqs in an OTU")

parseVerbosity :: Parser Int
parseVerbosity = option auto
                    (long "verbositsy"
                    <> short 'v'
                    <> metavar "VERBOSITY_LEVEL (0-2)"
                    <> value (1)
                    <> help "verbosity (0: quiet, 1: normal, 2: verbose)")

parseOptions :: Parser Params
parseOptions = Params
                <$> parseSmallProb
                <*> parseScaleFactor
                <*> parseNbRounds
                <*> parseSeed
                <*> parseMinSeqInOTU
                <*> parseVerbosity
                <*> switch (
                        short 'W' <> long "no-Henikoff-weighting"
                        <> help "don't perform Henikoff weighting of input aln")
                <*> option str
                    (long "output-format"
                    <> short 'f'
                    <> metavar "OUTPUT FORMAT STRING"
                    <> value "%h (%l) -> %p"
                    <> help "printf-like format string for output")
                <*> argument auto (metavar "<DNA|Prot>")
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
    let mol = molType params
    let noHWt = optNoHenikoffWt params
    putStrLn $ runInfo params randomIndices gen
    mapM_ (STIO.putStrLn .
        leaveOneOut (optOutFmtString params) mol noHWt smallProb
        scaleFactor tree fastARecs) randomIndices

-- gets a random number generator. If the seed is negative, gets the global
-- generator, else use the seed.

getGen :: Int -> IO StdGen
getGen seed
    | seed < 0 = getStdGen
    | otherwise = return $ mkStdGen (seed - 1)

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

runInfo :: Params -> [Int] -> StdGen -> String
runInfo params randomIndices gen
    | (optVerbosity params <= 1) = ""
    | otherwise = unlines [
        ("Performing " ++ (show $ optNbRounds params) ++  " rounds of LOO"),
        ("alignment:\t" ++ alnFname params),
        ("phylogeny:\t" ++ treeFname params),
        ("seed:\t" ++ (head $ words $ show gen)),
        ("indices:\t" ++ (show randomIndices)),
        ("min #nb seq / OTU:\t" ++ (show $ optMinSeqInOTU params)),
        ("Henikoff weighting:\t" ++ (show $ not $ optNoHenikoffWt params))

        ]

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
-- TODO: refactor, passing params as a single argument, or using a Reader monad

leaveOneOut :: FmtString -> Molecule -> Bool -> SmallProb -> ScaleFactor ->
    OTUTree -> Seq FastA -> Int -> ST.Text
leaveOneOut fmtString mol noHWt smallProb scaleFactor tree fastaRecs n =
    formatResult fmtString origRec alignedTestSeq prediction 
    where   header = LT.toStrict $ FastA.header testRec
            prediction = classifySequenceWithExtendedTrail classifier alignedTestSeq
            alignedTestSeq = msalign scoringScheme rootMod testSeq
            scoringScheme = ScoringScheme (-2)
                (scoringSchemeMap (absentResScore rootMod))
            rootMod = rootLabel modTree 
            testSeq = LT.toStrict $ FastA.sequence origRec
            classifier@(Classifier _ modTree) =
                buildClassifier mol smallProb scaleFactor otuAlnMap tree
            otuAlnMap = alnToAlnMap wtOtuAln
            wtOtuAln = if noHWt
                then otuAln
                else henikoffWeightAln otuAln
            otuAln = fastARecordsToAln trainSet
            trainSet = Data.Foldable.toList trainSetSeq
            (testRec, trainSetSeq) = spliceElemAt fastaRecs n
            -- we also need the ungapped sequence, e.g. for output and to make
            -- sure alignment works.
            origRec = FastA.degap testRec

oneRoundLOO :: Params -> OTUTree -> Seq FastA -> Int -> ST.Text
oneRoundLOO params otuTree fastARecs testRecNdx = 
    runReader (looReader otuTree fastARecs testRecNdx) params


looReader :: OTUTree -> Seq FastA -> Int -> Reader Params ST.Text
looReader otuTree fastaRecs testRecNdx = undefined
