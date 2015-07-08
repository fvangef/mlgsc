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

import Data.Binary (decodeFile)
import Data.Tree
import Data.Char
import Data.List
import Data.Ord
import MlgscTypes
import FastA
import PWMModel
import Align
import Classifier (Classifier(..), classifySequence, classifySequenceMulti)
import Output

data TreeTraversalMode = BestTraversal | FullTraversal | RecoverTraversal Int
    
data Params = Params {
                optTreeTraversalMode   :: TreeTraversalMode
                , optNoAlign           :: Bool
                , optOutFmtString      :: String
                , optStepFmtString     :: String
                , optERCutoff          :: Int   -- for Best mode
                , queryFname           :: String
                , clsfrFname           :: String
                }

parseTreeTraversal :: Monad m => String -> m TreeTraversalMode
parseTreeTraversal optString
    | 'b' == initC = return BestTraversal
    | 'a' == initC = return FullTraversal
    | otherwise    = do
                        let (Right num,_) = TP.runParser TP.parseDec optString
                        -- TODO: handle bad parse
                        return $ RecoverTraversal num
    where initC = toLower $ head optString

parseOptions :: Parser Params
parseOptions = Params
                <$> option (str >>= parseTreeTraversal)
                    (long "traversal-mode"
                    <> short 'm'
                    <> help "tree traversal mode (b|a|<int>)"
                    <> value BestTraversal)
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
                <*> argument str (metavar "<query seq file>")
                <*> argument str (metavar "<classifier file>")

parseOptionsInfo :: ParserInfo Params
parseOptionsInfo = info (helper <*> parseOptions) 
                    ( fullDesc
                    <> progDesc "classify sequences according to a model"
                    <> Options.Applicative.header
                        "mlgsc - maximum-likelihood general sequence classifier")

main :: IO ()
main = do
    params <- execParser parseOptionsInfo
    queryFastA <- LTIO.readFile $ queryFname params
    let queryRecs = fastATextToRecords queryFastA
    classifier@(PWMClassifier modTree scale) <- (decodeFile $ clsfrFname params) :: IO Classifier
    let rootMod = rootLabel modTree
    let scoringScheme = ScoringScheme (-2) (scoringSchemeMap (absentResScore rootMod))
    let processQuery = if (optNoAlign params)
                            then id
                            else (msalign scoringScheme rootMod)
    let headers = map FastA.header queryRecs
    let processedQueries =
            map (processQuery . ST.toUpper .
                LT.toStrict. FastA.sequence) queryRecs
    let outlines =
            case optTreeTraversalMode params of
                BestTraversal -> bestTraversal params classifier queryRecs processedQueries
                (RecoverTraversal _) -> recoverTraversal params classifier queryRecs processedQueries
                FullTraversal -> fullTraversal params classifier queryRecs processedQueries

    mapM_ STIO.putStrLn outlines

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
        
        predictions = map (classifySequenceMulti classifier (-1)) processedQueries
    
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

