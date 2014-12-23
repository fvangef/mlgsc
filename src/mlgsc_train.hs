module Main where

import Options.Applicative
import System.FilePath.Posix
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Binary (encodeFile)
import qualified Data.Text as ST
import qualified Data.Text.IO as STIO
import qualified Data.Text.Lazy.IO as LTIO

import MlgscTypes
import NewickParser
import NewickDumper
import FastA
import Classifier (buildClassifier)
import Weights
import Alignment

data Params = Params {
                optSmallProb        :: Double
                , optScaleFactor    :: Double
                , optOutFName       :: String
                , optVerbosity      :: Int
                , molType           :: Molecule
                , alnFName          :: String
                , treeFName         :: String
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

parseOutFName :: Parser String
parseOutFName = option str
                    (long "output-file"
                    <> short 'o'
                    <> metavar "OUTFILE"
                    <> value ""
                    <> help "name of the output classifier (default: derived from alignment)")

parseVerbosityLevel :: Parser Int
parseVerbosityLevel = option auto
                    (long "verbosity"
                    <> short 'v'
                    <> metavar "VERBOSITY LEVEL"
                    <> value 1
                    <> help "0: quiet, 1: normal, 2: verbose")

parseOptions :: Parser Params
parseOptions = Params
                <$> parseSmallProb
                <*> parseScaleFactor
                <*> parseOutFName
                <*> parseVerbosityLevel
                <*> argument auto (metavar "<DNA|Pep>")
                <*> argument str (metavar "<alignment file>")
                <*> argument str (metavar "<tree file>")

parseOptionsInfo :: ParserInfo Params
parseOptionsInfo = info (helper <*> parseOptions) 
                    ( fullDesc
                    <> progDesc "train model from alignment and tree"
                    <> Options.Applicative.header
                        "mlgsc_train - model trainer for the ML general sequence classifier")


main :: IO ()
main = do
    params <- execParser parseOptionsInfo
    newickString <- readFile $ treeFName params
    let (Right tree) = parseNewickTree newickString
    fastAInput <-  LTIO.readFile $ alnFName params
    let fastaRecs = fastATextToRecords fastAInput
    let otuAln = (henikoffWeightAln . fastARecordsToAln) fastaRecs
    let otuAlnMap = alnToAlnMap otuAln
    putStrLn "The following tree OTUs are NOT found in the alignment:"
    mapM_ STIO.putStrLn $ treeOTUsNotInALn tree otuAlnMap
    putStrLn "The following alignment OTUs are NOT found in the tree:"
    mapM_ STIO.putStrLn $ alnOTUsNotInTree tree otuAlnMap

    let classifier = buildClassifier
                        (molType params)
                        (optSmallProb params) (optScaleFactor params)
                        otuAlnMap tree
    let outputFileName = outFName (optOutFName params)
                                  (alnFName params)
    encodeFile outputFileName classifier

dumpAlnMap :: AlnMap -> [String]
dumpAlnMap otuAlnMap = map f $ M.assocs otuAlnMap
    where f (k, v) = otu ++ " (" ++ num ++ " seq)"
            where   otu = ST.unpack k
                    num = show $ length v 

dumpAlnRow :: AlnRow -> ST.Text
dumpAlnRow (AlnRow lbl seq wt) = ST.unwords [lbl, ST.pack $ show wt]

-- returns a list of tree OTUs not found in the alignment

treeOTUsNotInALn :: OTUTree -> AlnMap -> [ST.Text]
treeOTUsNotInALn otuTree otuAlnMap =
    filter (\otu -> M.notMember otu otuAlnMap) $ fringe otuTree

-- returns a list of alignment OTUs not found in the tree

alnOTUsNotInTree :: OTUTree -> AlnMap -> [ST.Text]
alnOTUsNotInTree otuTree otuAlnMap =
    filter (\otu -> S.notMember otu treeOTUSet) $ M.keys otuAlnMap
    where   treeOTUSet = S.fromList $ fringe otuTree
    
-- Computes the name of the output file, based on the value of the "optOutFName"
-- option and possibly of the alignment file name.

outFName :: String -> String -> String
outFName optOutFN alnFN 
      | optOutFN == ""  = replaceExtension alnFN "bcls"
      | otherwise       = optOutFN
