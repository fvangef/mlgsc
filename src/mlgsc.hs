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
import Options.Applicative

import Data.Binary (decodeFile)
import Data.Tree
import MlgscTypes
import FastA
import Crumbs (followExtendedCrumbsWithTrail)
import CladeModel
import NucModel
import Align
import Classifier (Classifier(..), classifySequenceWithExtendedTrail)
import Output

data Params = Params {
                optNoAlign          :: Bool
                , optOutFmtString   :: String
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
                    <> help "printf-like format string for output"
                    <> value "%h -> %p")
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
    let predictions = map (classifySequenceWithExtendedTrail classifier) processedQueries
    let outLines = getZipList $ (formatResult $ optOutFmtString params)
                                <$> ZipList queryRecs
                                <*> ZipList processedQueries
                                <*> ZipList predictions
    mapM_ STIO.putStrLn outLines
