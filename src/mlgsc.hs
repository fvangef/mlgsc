-- General Sequence Classifier

-- Classifies a set of sequences using a classifier produced by gsc_mk.

-- module Main where

import System.Environment (getArgs)
import System.Console.GetOpt
import System.IO

import qualified Data.List as L
import qualified Data.Text.Lazy as T
import qualified Data.Text.IO as IOT
import System.Directory

import MlgscTypes
import Model
import FastA
import Trees
import Align
import Cdf

usage = unlines [
	"mlgsc - maximum-likelihood general sequence classifier",
	"",
	"Usage: mlgsc [-Aasvx] <queries> <model>",
	"",
	"'queries' is the name of a FastA file containing the sequences to classify, ",
	"and 'model' is the name of a file containing a model produced by mlgsc_train.",
	"",
	"Output",
	"......",
	"",
	"The program ouputs the query label, predicted OTU label, score, and cumulative",
	"probability; if option -s is passed the aligned sequence is also shown. Example:",
	"",
	"\tA0Q0B0_CLONN\tClostridium\t-110611\t0.98",
	"\tA0RIG4_BACAH\tBacillus\t-120641\t0.81",
	"\tA1HTQ9_9FIRM\tClostridium\t-184297\t0.63",
	"\t...",
	"",
	"Where `A0Q0B0_CLONN` is classified as Clostridium, etc. The",
	"score is the log-likelihood of the most likely matrix (multiplied by a constant",
	"and rounded to the nearest integer, to speed up computation and avoid",
	"underflows).  The cumulative probability is the fraction of sequences in the",
	"training alignment that have a lower score, e.g. a value of 0.75 means that",
	"one-fourth of the training sequences yield a higher score.",
	"",
	"Options",
	".......",
	"",
	"\t-A: don't align the sequences (faster, but the sequences should already",
	"\t    be aligned)",
	"\t-s: show the aligned sequence",
	"\t-x: cross-validate - show the classification iff the predicted OTU does",
	"\t    NOT match the sequence label. Used e.g. to find misclassified",
	"\t    sequences. Only makes sense if the query sequences are labeled",
	"\t    with the OTU names of the training set."
	]

type ScoreTuple = (T.Text, T.Text, Int, T.Text)

data Options = Options {
        optResFilter    :: (ScoreTuple -> Bool)
        , optAlign      :: Bool
        , optWithSeq    :: Bool
        , optStar       :: Bool -- -> star topology (seemingly not very useful)
		, optHelp		:: Bool
        } 

defOptions :: Options
defOptions = Options { 
        optResFilter  = (\_ -> True)  -- keep all results
        , optAlign      = True
        , optWithSeq    = False 
        , optStar       = False
        , optHelp       = False
        }

options :: [OptDescr (Options -> Options)]
options = [
        Option ['A'] ["dont-align"]     (NoArg setDontAlign)
            "cross-validate: show only mismatches",
        Option ['s'] ["with-sequence"]  (NoArg setWithSeq)
            "show aligned sequence",
        Option ['h'] ["help"]  			(NoArg setHelp)
			"show help message and exit",
        Option ['x'] ["cross-validate"] (NoArg setOnlyWrong)
            "cross-validate: show only mismatches",
        Option ['a'] ["star-shaped"]    (NoArg setToStar) "convert tree to star topology"
        ]

keepOnlyWrong :: ScoreTuple -> Bool
keepOnlyWrong (hdr, cls, _, _) = (hdr /= cls)

setOnlyWrong :: Options -> Options
setOnlyWrong opt = opt { optResFilter = keepOnlyWrong }

setDontAlign :: Options -> Options
setDontAlign opt = opt { optAlign = False }

setWithSeq :: Options -> Options
setWithSeq opt = opt { optWithSeq = True }

setToStar :: Options -> Options
setToStar opt = opt { optStar = True }

setHelp :: Options -> Options
setHelp opt = opt { optHelp = True }

runOpts :: [String] -> IO (Options, [String])
runOpts argv =
        case getOpt RequireOrder options argv of
                -- no error: ofs is a list of functions that transform an
                -- Options record -> apply their composition to the record of
                -- default options. 
                (ofs,args,[]) -> return (options, args)
                        where   options = fp defOptions
                                -- prefix "id", otherwise fold will fail if ofs
                                -- is empty.
                                fp = foldl1 (.) (id:ofs)
                -- error(s): warn then quit
                (_,_,errs) -> ioError (userError (concat errs ++ usageInfo usage options ++ ")"))

main = do
        (options, args) <- getArgs >>= runOpts
        if ((2 /= length args) || (optHelp options))
                then putStrLn usage
                else do
                        -- queries must be ALIGNED to the model
                        let [qFname,modFname] = args
                        process options qFname modFname

process :: Options -> String -> String -> IO ()
process opts qFname modFname = do
        queries <- parseFastAFile qFname
        (Classifier model distrib) <- loadClassifier modFname
        let classifier = if optStar opts
                                then toStarTree model
                                else model
        let alignedQueries = if optAlign opts
                                then map (align classifier) queries
                                else queries
        let scoreTuples = L.map (scoreQuery classifier) alignedQueries
        let filteredScoreTuples = L.filter (optResFilter opts) scoreTuples
        let scoreStrings = L.map (formatter opts distrib) filteredScoreTuples
        mapM_ (IOT.putStrLn . T.toStrict) scoreStrings

-- TODO: this should go into FastA
parseFastAFile :: String -> IO [FastA]
parseFastAFile fname = do
        fh <- openFile fname ReadMode
        fasta <- hGetContents fh
        let records = fastATextToRecords $ T.pack fasta
        return records

align :: RoseTree Model -> FastA -> FastA
align hm fasta = fasta { FastA.sequence = aln_sequence }
        where   aln_sequence = msalign defScoring isl (FastA.sequence fasta)
                isl = (matrix . rTreeData) hm

scoreQuery hm q = (header q, fst scoreTuple, snd scoreTuple, FastA.sequence q)
        where   scoreTuple = tightModelRTreeScore hm (FastA.sequence q)

formatter :: Options -> EmpiricalCDF Int -> ScoreTuple -> T.Text
formatter opts distrib (hdr, cls, scr, seq) =
        T.intercalate (T.pack "\t") field_list
        where   field_list = if optWithSeq opts
                                then [hdr, cls, T.pack $ show scr, T.pack $ show prob, seq]
                                else [hdr, cls, T.pack $ show scr, T.pack $ show prob]
                prob = ecdf distrib scr              
