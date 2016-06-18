-- General Sequence Classifier

module Main (main) where

-- Classifies a set of sequences using a classifier produced by gsc_mk.

-- module Main where

import System.Environment (getArgs)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.Text as ST
import qualified Data.Text.IO as STIO
import Text.Printf
import qualified Text.Parse as TP
import Control.Applicative
import Control.Monad.Reader
import Options.Applicative

import Codec.Compression.GZip (decompress)

import qualified Data.ByteString.Lazy as LB
import Data.Map (Map, fromList, findWithDefault)
import Data.Binary (decodeFile, decode)
import Data.Tree
import Data.Char
import Data.List
import Data.Ord

import MlgscTypes
import FastA
import Trim (trimSeq, maskFlaky)
import PWMModel
import Align
import Classifier (Classifier(..), classifySequence, classifySequenceMulti,
                    classifySequenceAll, StoredClassifier(..))
import Output

-- distinguish between MlgscAlignMode (which includes no alignment) from
-- AlignMode (which is exported by Align), in which no alignment makes little
-- sense.

data MlgscAlignMode = DoAlignment AlignMode | NoAlignment
data MaskMode = None | Trim | MaskFlaky

data TreeTraversalMode = BestTraversal
                       | FullTraversal
                       | RecoverTraversal Int
                       | SingleNodeTraversal CladeName
    
data Params = Params {
                optTreeTraversalMode    :: TreeTraversalMode
                , optNoAlign            :: Bool
                , optOutFmtString       :: String
                , optStepFmtString      :: String
                , optERCutoff           :: Int   -- for Best mode (TODO: could be an argument to the BestTraversal c'tor)
                , optMaskMode           :: MaskMode
                , optAlnMode               :: MlgscAlignMode
                , queryFname            :: String
                , clsfrFname            :: String
                }

parseTreeTraversal :: Monad m => String -> m TreeTraversalMode
parseTreeTraversal optString
    | 'b' == initC = return BestTraversal
    | 'a' == initC = return FullTraversal
    | 'r' == initC = do
                        let (Right num,_) = TP.runParser TP.parseDec $ tail optString
                        -- TODO: handle bad parse
                        return $ RecoverTraversal num
    where initC = toLower $ head optString

parseMaskMode :: Monad m => String -> m MaskMode
parseMaskMode optString
    | 't' == initC = return Trim
    | 'n' == initC = return None
    | 'f' == initC = return MaskFlaky
    | otherwise = return None -- TODO: warn about unrecognized opt
    where initC = toLower $ head optString

parseAlignMode :: Monad m => String -> m MlgscAlignMode
parseAlignMode optString
    | 'g' == initC = return $ DoAlignment AlignGlobal
    | 's' == initC = return $ DoAlignment AlignSemiglobal
    | 'n' == initC = return NoAlignment
    | otherwise = error ("invalid alignment mode: " ++ optString)
    where initC = toLower $ head optString

parseOptions :: Parser Params
parseOptions = Params
                <$> option (str >>= parseTreeTraversal)
                    (long "traversal-mode"
                    <> short 'm'
                    <> help "tree traversal mode (b|a|r<int>)"
                    <> value BestTraversal)
                -- NOTE: the -A switch is deprecated; use -a n instead.
                -- We keep it for compatibility
                <*> switch
                    (long "no-align"
                    <> short 'A'
                    <> help "do not align query sequences")
                <*> option str
                    (long "output-format"
                    <> short 'f'
                    <> help "printf-like format string for output."
                    <> value "%h -> %p")
                <*> option str
                    (long "step-format"
                    <> short 's'
                    <> help "printf-like format string for step (path element)"
                    <> value "%t (%s)")
                <*> option auto
                    (long "ER-cutoff"
                    <> short 'e'
                    <> help "drop clades with ER lower than this"
                    <> value 0)
                <*> option (str >>= parseMaskMode)
                    (long "mask-mode"
                    <> short 'M'
                    <> help "mask aligned query: n)one* | t)trim | mask f)laky"
                    <> value None)
                <*> option (str >>= parseAlignMode)
                    (long "align-mode"
                    <> short 'a'
                    <> help "alignment mode: g)lobal* | s)emiglobal | n)one"
                    <> value (DoAlignment AlignGlobal))
                <*> argument str (metavar "<query seq file>")
                <*> argument str (metavar "<classifier file>")

parseOptionsInfo :: ParserInfo Params
parseOptionsInfo = info (helper <*> parseOptions) 
                    ( fullDesc
                    <> progDesc "classify sequences according to a model"
                    <> Options.Applicative.header
                        "mlgsc - maximum-likelihood general sequence classifier")


-- Some common format options have names (e.g. "simple" -> "%h -> %P"). These
-- must be translated using the following map (some short forms are possible)

fmtMap :: Map String String
fmtMap = fromList [
                    ("minimal", "%i\t%P"),
                    ("min", "%i\t%P"),
                    ("m", "%i\t%P"),
                    ("simple", "%h -> %P (%s)"),
                    ("s", "%h -> %P (%s)")
                ]

translateFmtKw :: Params -> IO Params
translateFmtKw params = do
    let origFmt = optOutFmtString params
    let fmt = findWithDefault origFmt origFmt fmtMap
    return params { optOutFmtString = fmt }
                

main :: IO ()
main = do
    params <- execParser parseOptionsInfo >>= translateFmtKw
    queryFastA <- LTIO.readFile $ queryFname params
    let queryRecs = fastATextToRecords queryFastA
    let clsfrFn = clsfrFname params
    storedClassifier <- if isSuffixOf ".gz" clsfrFn
        then do
            z <- LB.readFile clsfrFn
            return (decode $ decompress z) :: IO StoredClassifier
        else (decodeFile clsfrFn) :: IO StoredClassifier
    let (StoredClassifier classifier@(PWMClassifier modTree scale) _) = storedClassifier
    let rootMod = rootLabel modTree
    -- TODO: replace the magic "2" below by a meaningful constant/param
    let scoringScheme = ScoringScheme (-2) (scoringSchemeMap (absentResScore rootMod))
    let headers = map FastA.header queryRecs
    let processedQueries =
            map (
                -- All the transformations from Fasta record to ready-to-score
                -- query. Note that some (mb*) depend on params among other
                -- things.
                mbMask params
                . mbAlign params scoringScheme rootMod
                . ST.toUpper
                . LT.toStrict
                . FastA.sequence
            ) queryRecs
    let outlines =
            case optTreeTraversalMode params of
                BestTraversal -> bestTraversal params classifier queryRecs processedQueries
                (RecoverTraversal _) -> recoverTraversal params classifier queryRecs processedQueries
                FullTraversal -> fullTraversal params classifier queryRecs processedQueries

    mapM_ STIO.putStrLn outlines

-- Returns an alignment step (technically, a ST.Text -> ST.Text function), or
-- just id if option 'optNoAlign' is set. NOTE: for now there are two ways of
-- spcifying 'no alignment', because we introduced semiglobal, yet either
-- alignment mode is incompatible with 'no alignment', so in reality it's either
-- global, semiglobal, or not at all, which are all governed by -a.
-- We could just drop -A, but this could break existing code. For now we just
-- handle both here, in the future there should be a distinction between CLI
-- params and run params, with the former essentially the same as the current
-- Params, and a function to map them to the latter.

mbAlign params scsc rootMod =
    if optNoAlign params
            then id
            else case (optAlnMode params) of
                NoAlignment         -> id
                (DoAlignment mode)  -> msalign mode scsc rootMod

-- Returns a masking step (a ST.Text -> ST.Text function), or just id if no
-- masking was requested. TODO: this function could be called  during
-- command-line argument parsing...

mbMask params =
    case optMaskMode params of
        None -> id
        Trim -> trimSeq
        MaskFlaky -> maskFlaky

bestTraversal :: Params -> Classifier -> [FastA] -> [Sequence] -> [ST.Text]
bestTraversal params classifier queryRecs processedQueries =
    getZipList $ (formatResultWrapper params)
        <$> ZipList queryRecs
        <*> ZipList processedQueries
        <*> ZipList predictions 
    where
        log10ER = optERCutoff params
        predictions = map (classifySequence classifier log10ER) processedQueries

recoverTraversal :: Params -> Classifier -> [FastA] -> [Sequence] -> [ST.Text]
recoverTraversal params classifier queryRecs processedQueries =
    getZipList $ (formatResultWrapper params)
        <$> ZipList queryRecs
        <*> ZipList processedQueries
        <*> ZipList bestPredictions
    where
        (RecoverTraversal tieThreshold)  = optTreeTraversalMode params
        bestPredictions = map getBestPrediction predictions
        predictions = map (classifySequenceMulti classifier tieThreshold) processedQueries
    
getBestPrediction :: [Trail] -> Trail
getBestPrediction trails = maximumBy (comparing lastScore) trails

lastScore :: Trail -> Score
lastScore trail = bestScore $ last trail 

fullTraversal :: Params -> Classifier -> [FastA] -> [Sequence] -> [ST.Text]
fullTraversal params classifier queryRecs processedQueries =
    concat $ getZipList $ (fullTraversalFmt1Query params)
        <$> ZipList queryRecs
        <*> ZipList processedQueries
        <*> ZipList predictions
    where
        predictions = map (classifySequenceAll classifier) processedQueries
    
fullTraversalFmt1Query :: Params -> FastA -> Sequence -> [Trail] -> [ST.Text]
fullTraversalFmt1Query params queryRec processQuery trails =
    map (formatResultWrapper params queryRec processQuery) trails

{- I like to apply the output formatter in aplicative style to the lists of
 - arguments. However, I'm not sure how to make this play with the Reader monad,
 - except by using a wrapper like below. -}

formatResultWrapper :: Params -> FastA -> Sequence -> Trail -> ST.Text  
formatResultWrapper params query alnQry trail =
    runReader (formatResultReader query alnQry trail) params

{- This works, but it's not quite clear what should go in Output,
 - OutputFormatStringParser, or just plain here in Main. -}

formatResultReader :: FastA -> Sequence -> Trail -> Reader Params ST.Text 
formatResultReader query alnQry trail = do
    fmtString    <- asks optOutFmtString
    stepFmtString <- asks optStepFmtString
    let (Right format) = parseOutputFormatString fmtString 
    let (Right stepfmt) = parseStepFormatString stepFmtString
    return $ ST.concat $ map (evalFmtComponent query alnQry trail stepfmt) format

