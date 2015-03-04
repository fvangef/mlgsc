-- General Sequence Classifier

-- Cross-validates a model

-- module Main where

import System.Environment (getArgs)
import System.Random
import System.IO
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Data.Maybe
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
import Classifier (Classifier(..), buildClassifier, classifySequence, leafOTU)
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
                , optOnlyFalse      :: Bool
                , optIndices        :: String
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
                <*> switch (
                        short 'x' <> long "only-wrong"
                        <> help "only show wrong classifications")
                <*> option str
                    (long "aln-seq-indices"
                    <> short 'i'
                    <> value ""
                    <> help "whitespace-separated list of indices of sequences to use [testing]")
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
    newickString <- readFile $ treeFname params
    let (Right tree) = parseNewickTree newickString
    fastAInput <-  LTIO.readFile $ alnFname params
    let fastARecs = Sq.fromList $ fastATextToRecords fastAInput
    let bounds = (0, Sq.length fastARecs)
    gen <- getGen $ optSeed params
    seqIndices <- getSeqIndices gen params fastARecs
    let mol = molType params
    putStrLn $ runInfo params seqIndices gen
    mapM_ STIO.putStrLn $ catMaybes $
        map (oneRoundLOO params tree fastARecs) seqIndices

warn :: String -> IO ()
warn msg = hPutStrLn stderr $ "WARNING: " ++ msg

{- Determine which sequences in the input FastA will be used as queries for LOO
 - runs, and return their indices in the Sequence of FastA records (that's a
 - Data.Sequence, not a molecular sequence, of course...). Not all sequences are
 - eligible: if an OTU contains only one sequence, and we remove it from the
 - training set, then the modeling step will be unable to model that OTU at all,
 - and the removed sequence, when used as a LOO query, will always be
 - misclassified. Therefore, a for a sequence to be used as an LOO query, there
 - must be at least one other sequence in the same OTU, IOW there is a minimum
 - OTU size of 2. Even this is rather small, because of the potential for
 - overfitting when using a single sequence as a training set for a given OTU.
 - Therefore, the default minimal OTU size is 3 (and can be overridden with
 - option -m).
 - OTUs with sufficient sizes and sequences that belong to such OTUs are called
 - 'valid'.
 - The behaviour is as follows:
 - (1) if the user supplies indices directly (option -i), those indices are
 - used. NO CHECK IS PERFORMED, whether of sequence validity, or of bounds. This
 - option should only be used for testing; it has the advantage of being
 - portable.
 - (2) if the user specifies a number n of LOO rounds (option -r), then n
 - indices are drawn at random (without replacement) from the valid indices,
 - UNLESS n is larger than (or equal to) the number of valid indices. In that
 - case, all valid sequences are used, and without permutation (i.e., in the
 - order in which they appear in the FastA).
 - (3) otherwise, a default of 100 is used, i.e. the program behaves as in (2),
 - as if the user had specified 100 rounds. (in particular, if there are fewer
 - than 100 valid sequences, then they are all used in turn). 
 -}

getSeqIndices :: StdGen -> Params -> Seq FastA -> IO [Int]
getSeqIndices gen params fastARecs = do
        let nbRounds = optNbRounds params
        let outputWarnings = (optVerbosity params > 0)
        let validNdx = validIndices fastARecs
        if null $ optIndices params 
        then do if nbRounds > length validNdx
                then do
                    if outputWarnings
                        then warn $ "More LOO rounds (" ++ show nbRounds ++ ") than valid indices (" ++ show (length validNdx) ++ ")."
                        else return ()
                    return validNdx
                else return $ take nbRounds $ shuffleList gen $ validNdx
        else do
            if outputWarnings
                then warn "User-specified indices are not checked."
                else return ()
            return $ map read $ words $ optIndices params

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
runInfo params seqIndices gen
    | (optVerbosity params <= 1) = ""
    | otherwise = unlines [
        ("Performing " ++ (show $ length seqIndices) ++  " rounds of LOO"),
        ("alignment:\t" ++ alnFname params),
        ("phylogeny:\t" ++ treeFname params),
        ("seed:\t" ++ (head $ words $ show gen)),
        ("indices:\t" ++ (show seqIndices)),
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

{-
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
-}

-- These two perform the same as leaveOneOut, but using a Reader monad for
-- passing the parameters. It is essentially a matter of style, both functions
-- do the same thing. The first has nine arguments, which seems a lot; the
-- second has four, which seems more manageable. The fact that it relies on
-- "hidden" arguments is visible from its signature (Reader Params). 
-- A third possibility would be to simply pass the Params object directly. I'm
-- not sure which is best.

oneRoundLOO :: Params -> OTUTree -> Seq FastA -> Int -> Maybe ST.Text
oneRoundLOO params otuTree fastARecs testRecNdx = 
    runReader (looReader otuTree fastARecs testRecNdx) params


looReader :: OTUTree -> Seq FastA -> Int -> Reader Params (Maybe ST.Text)
looReader otuTree fastaRecs testRecNdx = do
    fmtString <- asks optOutFmtString
    noHwt <- asks optNoHenikoffWt
    let wtOtuAln = if noHwt
            then otuAln
            else henikoffWeightAln otuAln
    let otuAlnMap = alnToAlnMap wtOtuAln
    mol <- asks molType
    smallProb <- asks optSmallProb
    scaleFactor <- asks optScaleFactor
    let classifier@(Classifier modTree) =
            buildClassifier mol smallProb scaleFactor otuAlnMap otuTree
    let rootMod = rootLabel modTree 
    let scoringScheme =
            ScoringScheme (-2) (scoringSchemeMap (absentResScore rootMod))
    let alignedTestSeq = msalign scoringScheme rootMod testSeq
    let prediction = classifySequence classifier alignedTestSeq
    onlyFalse <- asks optOnlyFalse
    if  (onlyFalse && 
         (leafOTU prediction) == (LT.toStrict $ fastAOTU testRec))
        then return Nothing
        else return $ Just $ formatResult fmtString origRec alignedTestSeq prediction
    where   header = LT.toStrict $ FastA.header testRec
            testSeq = LT.toStrict $ FastA.sequence origRec
            otuAln = fastARecordsToAln trainSet
            trainSet = Data.Foldable.toList trainSetSeq
            (testRec, trainSetSeq) = spliceElemAt fastaRecs testRecNdx
            origRec = FastA.degap testRec
