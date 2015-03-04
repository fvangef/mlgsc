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
import Control.Applicative
import Control.Monad.Reader
import Options.Applicative

import Data.Binary (decodeFile)
import Data.Tree
import MlgscTypes
import FastA
import CladeModel
import NucModel
import Align
import Classifier (Classifier(..), classifySequence)
import Output
import OutputFormatStringParser

data Params = Params {
                optNoAlign          :: Bool
                , optOutFmtString   :: String
                , optERCutoff       :: Int
                , queryFname        :: String
                , clsfrFname        :: String
                }

parseOptions :: Parser Params
parseOptions = Params
                <$> switch
                    (long "no-align"
                    <> short 'A'
                    <> help "do not align query sequences")
                <*> option str
                    (long "output-format"
                    <> short 'f'
                    <> help "printf-like format string for output. OVERRIDES -e"
                    <> value "%h -> %p")
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
    classifier@(Classifier _ modTree) <- (decodeFile $ clsfrFname params) :: IO Classifier
    let rootMod = rootLabel modTree
    let scoringScheme = ScoringScheme (-2) (scoringSchemeMap (absentResScore rootMod))
    let processQuery = if (optNoAlign params)
                            then id
                            else (msalign scoringScheme rootMod)
    let headers = map FastA.header queryRecs
    let processedQueries =
            map (processQuery . ST.toUpper .
                LT.toStrict. FastA.sequence) queryRecs
    let predictions = map (classifySequence classifier) processedQueries
    let outLines = getZipList $ (formatResultWrapper params)
                                <$> ZipList queryRecs
                                <*> ZipList processedQueries
                                <*> ZipList predictions
    mapM_ STIO.putStrLn outLines

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
   minER        <- asks optERCutoff 
   let (Right format) = parseOutputFormatString fmtString 
   return $ ST.concat $ map (evalFmtComponent minER query alnQry trail) format
