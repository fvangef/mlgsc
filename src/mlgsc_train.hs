-- General Sequence Classifier
-- Model maker

module Main where

import System.Environment (getArgs)
import System.Console.GetOpt
import System.IO
import System.Random
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import qualified Data.Text.IO as TIO
import System.Random


import MlgscTypes
import Trees
import Model
import NewickParser
import FastA
import Cdf
import Weights
import Shuffle

-- smallprob = 0.0001

usage = unlines [ 
	"mlgsc_train - train a model for the mlgsc classifier",
	"",
	"Usage: mlgsc_train [-h] <alignment> <tree>",
	"",
	"'alignment' is the name of a file containing a multiple alignment of",
	"sequences from the classifying region, in multiple (gapped) FastA. Each sequence",
	"must be labeled by the OTU it belongs to, e.g.:",
	"",
	">Sporosarcina",
	"atatgca-ccga--tcgatcgatc...",
	">Sporosarcina",
	"atacgca-tcga--tcgatcaatc...",
	">Bacillus",
	"gcatgcatccga--tcg-tcgatt...",
	"...",
	"",
	"'tree' is the name of a file containing a phylogeny of the reference OTUs, in",
	"Newick format, e.g.",
	"",
	"((Bacillus,Sporosarcina),(Clostridium,Dorea));",
	"",
	"The output is a mlgsc model, and is printed on stdout.",
	"",
	"Options",
	"-------",
	"",
	"\t-B: do not balance inner node matrices - each OTU sequence has",
    "\t    equal weight. Default is to give equal weight to children matrices",
    "\t    instead of sequences.",
	"\t-h: print this help message on stdout and exit successfully.",
    "\t-w <o|n>: weight the sequences by OTU or not at all, respectively.",
    "\t   Default is to apply Henikoff weights globally, to all sequences.",
    "\t   With -w o, Henikoff weights are computed per-OTU; with -w n the",
    "\t   sequences are not weighted."
	]



data Mode = Default | LeaveOneOut
data WeightingScheme = None | Global | OTU

data Options = Options {
        optMode         :: Mode
        , optVerbose    :: Bool
        , optBalanced   :: TreeBuilder -- a function that returns a model tree
        , optSmallProb  :: Float
		, optHelp		:: Bool
        , optScale      :: Int
        , optWeighting  :: WeightingScheme
        } 

defOptions :: Options
defOptions = Options { 
        optMode         = Default
        , optVerbose    = False	-- currently unused
        , optBalanced   = balancedModelRTree
        , optSmallProb  = 0.0001
		, optHelp		= False
        , optScale      = 1000
        , optWeighting  = Global
        }

options :: [OptDescr (Options -> Options)]
options = [
           Option ['B'] ["balanced"]  (NoArg setUnbalanced) "unbalanced"
		 , Option ['h'] ["help"] (NoArg setHelp) "help message"
         , Option ['l']   ["leave-one-out"]    (NoArg setLeaveOneOut)
            "leave-one out: model on stdin, test set on stderr"
         , Option ['v'] ["verbose"]   (NoArg setVerbose) "verbose"
         , Option ['w'] ["weight"]    (ReqArg setWeighting "weighting")
            "[o]tu | [n]o Henikoff weights"
        ]

setVerbose :: Options -> Options
setVerbose opt = opt { optVerbose = True }

setHelp :: Options -> Options
setHelp opt = opt { optHelp = True }

setLeaveOneOut :: Options -> Options
setLeaveOneOut opt = opt { optMode = LeaveOneOut }

setUnbalanced :: Options -> Options
setUnbalanced opt = opt { optBalanced = pairs2tightModelRTree }

setWeighting :: String -> Options -> Options
setWeighting arg opt = case head arg of
    'o' -> opt { optWeighting = OTU }
    'n' -> opt { optWeighting = None }
    otherwise -> error ("Unrecognized option to -w: " ++ arg)


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
                (_,_,errs) -> ioError (userError
                        (concat errs ++ usageInfo usage options ++ ")"))


main :: IO ()
main = do
    argv <- getArgs
    (opts, args)  <- runOpts argv
    if ((length args) /= 2 || (optHelp opts == True))
        then putStrLn usage
        else do
            let seqFileName = args !! 0
            rawOtuSeqPairs <- getOtuSeqPairs seqFileName
            gen <- getStdGen
            let sampledOtuSeqPairs = case (optMode opts) of
                    Default     -> rawOtuSeqPairs
                    LeaveOneOut -> snd $ leaveOneOut gen rawOtuSeqPairs
            -- weights (or not) the input sequences
            let otuSeqPairs = case (optWeighting opts) of
                    None   -> sampledOtuSeqPairs
                    Global -> henikoffWeightSeqs sampledOtuSeqPairs
                    OTU    -> henikoffWeightSeqsByOTU sampledOtuSeqPairs
            tree <- getTree $ args !! 1
            defaultMode opts otuSeqPairs tree
            case optMode opts of
                Default     -> return ()
                LeaveOneOut -> TIO.hPutStrLn stderr $ T.toStrict $
                    leftOutPairs2FastA $ fst $ leaveOneOut gen rawOtuSeqPairs

defaultMode :: Options -> [(T.Text,T.Text)] -> RoseTree T.Text -> IO ()
defaultMode opts otuSeqPairs tree = do
    let classifier = (optBalanced opts) -- this is a _function_
                        (optSmallProb opts)
                        (optScale opts)
                        otuSeqPairs
                        tree
    let ecd = scoreDist classifier otuSeqPairs
    case emptyMatrices classifier of
            Just list -> TIO.hPutStrLn stderr $ T.toStrict $
                T.concat [
                    T.pack "Some OTUs in the tree (",
                    list,
                    T.pack ") have no sequences - aborting."
                    ]
            Nothing ->  do
                    putStrLn $ show (Classifier classifier ecd)

{-
leaveOneOutMode :: Options -> [(String,String)] -> RoseTree String -> IO ()
leaveOneOutMode opts otuSeqPairs tree = do
        let (classifier, trainingSet) =
		pairs2Leave1Out (optSmallProb opts) (optScale opts) otuSeqPairs tree
        let ecd = scoreDist classifier otuSeqPairs
        case emptyMatrices classifier of
                Just list -> hPutStrLn stderr ("Some OTUs in the tree (" ++ list ++ ") have no sequences - aborting.")
                Nothing ->  do
                        hPutStr stderr trainingSet
                        putStrLn $ show (Classifier classifier ecd)
-}


getOtuSeqPairs :: String -> IO [(T.Text, Sequence)]
getOtuSeqPairs fname = do
        fastARecords <- parseFastAFile fname
        return $ map (\s -> (FastA.header s, FastA.sequence s)) fastARecords

-- This should probably go into FastA
parseFastAFile :: String -> IO [FastA]
parseFastAFile fname = do
        fh <- openFile fname ReadMode
        fasta <- hGetContents fh
        let records = fastATextToRecords $ T.pack fasta
        return records

getTree :: String -> IO (RoseTree T.Text)
getTree fname = do 
        fh <- openFile fname ReadMode
        newick <- hGetContents fh
        let spcTree = parseRoseTree newick
        case spcTree of 
                Left err -> error $ show err
                Right tree -> return tree

emptyMatrices :: RoseTree Model -> Maybe T.Text
emptyMatrices classifier
        | T.null list = Nothing
        | otherwise = Just list :: Maybe T.Text
        where list = T.intercalate (T.pack ", ") $ map name $ filter (null . matrix)
                     $ fringe classifier

scoreDist :: RoseTree Model -> [(T.Text,Sequence)] -> EmpiricalCDF Int
scoreDist classifier otuSeqPairs = empiricalCdf scores
        where scores = map (snd . tightModelRTreeScore classifier . snd) otuSeqPairs

-- Some functions for weighting alignments

-- computes integer weights, suitable for repeating sequences

pairs2IntWeights :: [(T.Text,T.Text)] -> [Int]
pairs2IntWeights pairs = map (round . (/min)) norm_weights
    where   norm_weights = normalize $ alnRawWeights aln
            min = minimum norm_weights
            aln = map snd pairs

-- takes a (name, sequence) list and a list of corresponding integer weights,
-- and repeats each sequence as many times as its weight, e.g. a sequence with
-- weight 4 is output 4 times.

applyWeights :: [Int] -> [(T.Text,T.Text)]  -> [(T.Text,T.Text)]
applyWeights weights pairs =
    concat $ zipWith (\n p -> take n $ repeat p) weights pairs

henikoffWeightSeqs :: [(T.Text, T.Text)] -> [(T.Text, T.Text)]
henikoffWeightSeqs pairs = applyWeights weights pairs
    where weights = pairs2IntWeights pairs

henikoffWeightSeqsByOTU :: [(T.Text, T.Text)] -> [(T.Text, T.Text)]
henikoffWeightSeqsByOTU pairs = concatMap henikoffWeightSeqs otuSeqList
    where   otuSeqList = zipWith list2pairs otus seqLists
            seqLists = M.elems otu2pairs    
            otus = M.keys otu2pairs
            otu2pairs = M.fromListWith (++) $ map (\(n, s) -> (n,[s])) pairs

list2pairs :: T.Text -> [Sequence] -> [(T.Text,T.Text)]
list2pairs name seqs = map (\seq -> (name, seq)) seqs

-- Some functions for leave-one-out

-- Takes the usual (tag, sequence) pair list, groups them by tag into a list of
-- lists (each inner list has pairs with the same tag), then shuffles the inner
-- lists.

shufflePairs :: StdGen -> [(T.Text, T.Text)] -> [[(T.Text, T.Text)]]
shufflePairs gen pairs  = zipWith shuffleList gens byOtu 
    where   gens        = randomGens gen $ length byOtu
            byOtu       = M.elems otu2pairs
            otu2pairs   = M.fromListWith (++) $ map (\p@(t,s) -> (t,[p])) pairs


leaveOneOut :: StdGen -> [(T.Text, T.Text)] -> (
    [(T.Text, T.Text)], -- the pairs left out of the training set (one per OTU)
    [(T.Text, T.Text)]  -- the pairs remaining in the set
    )
leaveOneOut gen pairs = (leftOutPairs, keptPairs)
    where   leftOutPairs = map head keptOTUs
            keptPairs = concatMap tail keptOTUs 
            keptOTUs = L.filter (\l -> (length l > 1)) $ shufflePairs gen pairs

leftOutPairs2FastA :: [(T.Text,T.Text)] -> T.Text
leftOutPairs2FastA pairs = T.concat $ map pairs2fasta pairs
    where   pairs2fasta (tag, seq) = T.concat [
                  T.pack ">",
                  tag, T.pack "\n",
                  T.replace (T.pack "-") (T.pack "") seq
                ]

-- Some toy data


toy_pairs = [
    ("otu1", "AAA"),
    ("otu1", "CCC"),
    ("otu1", "GGG"),
    ("otu1", "TTT"),
    ("otu2", "AAA"),
    ("otu2", "CCC"),
    ("otu2", "GGG"),
    ("otu2", "TTT"),
    ("otu3", "AAA"),
    ("otu3", "CCC"),
    ("otu3", "GGG"),
    ("otu3", "TTT")
    ]
{-
shufflePairs :: StdGen -> [(String, String)] -> [[(String, String)]]
shufflePairs pairs = zipWith shuffleList gens seqLists
    where   gens = randomGens $ length seqLists
            seqLists = M.elems otu2pairs
            otu2pairs = M.fromListWith (++) $ map (\p@(n,s) -> (n,[p])) pairs

-- toy data

pairs1 = [
    ("otu1", "AAA"),
    ("otu1", "CCC"),
    ("otu1", "AAA"),
    ("otu1", "CCC"),

-}
