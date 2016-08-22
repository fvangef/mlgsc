module Main where

import Options.Applicative
import System.FilePath.Posix
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Binary (encodeFile, encode)
import Data.ByteString.Lazy (ByteString, unpack)
import qualified Data.Text as ST
import qualified Data.Text.IO as STIO
import qualified Data.Text.Lazy.IO as LTIO
import System.Environment (getArgs, getProgName)
import Data.Digest.Pure.CRC32

import MlgscTypes
import NewickDumper
import FastA
import Classifier (buildClassifier, Classifier, 
        StoredClassifier(..), Metadata(..))
import Alignment
import IDTree
-- TODO should replace (most of) the above with:
import API (rawTree, fastaRecsAndTree, fastaRecsAndTree',
    otuAlignmentMap, parsePhyloFormat)

data Params = Params {
                optSmallProb        :: Double
                , optScaleFactor    :: Double
                , optOutFName       :: String
                , optVerbosity      :: Int
                , optNoHenikoffWt   :: Bool
                , optIDtree         :: Bool
                , optPhyloFormat    :: PhyloFormat 
                , molType           :: Molecule
                , posParams         :: [String]
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

parseWeighting :: Parser Bool
parseWeighting =  switch (
                    short 'W'
                    <> long "no-Henikoff-weighting"
                    <> help "don't perform Henikoff weighting of input aln")

parseOptions :: Parser Params
parseOptions = Params
                <$> parseSmallProb
                <*> parseScaleFactor
                <*> parseOutFName
                <*> parseVerbosityLevel
                <*> parseWeighting
                <*> switch (
                    short 'I'
                    <> long "id-tree"
                    <> help "input tree labeled by seq ID, not taxa")
                <*> option (str >>= parsePhyloFormat) (
                    short 'T'
                    <> long "tree-file-format"
                    <> help "T)axonomy or N)ewick (default)"
                    <> value Newick )
                <*> argument auto (metavar "<DNA|Prot>")
                <*> many (argument str (metavar "<alignment file> [tree file]"))

parseOptionsInfo :: ParserInfo Params
parseOptionsInfo = info (helper <*> parseOptions) 
                    ( fullDesc
                    <> progDesc "train model from alignment and tree"
                    <> Options.Applicative.header
                        "mlgsc_train - model trainer for the ML general sequence classifier")


main :: IO ()
main = do
    params <- execParser parseOptionsInfo
    let alnFName = head $ posParams params
    let treeFName = last $ posParams params
    treeString <- readFile $ treeFName
    fastAInput <-  LTIO.readFile $ alnFName
    let (fastaRecs, tree) = fastaRecsAndTree
                                (optIDtree params)
                                (fastATextToRecords fastAInput)
                                (rawTree (optPhyloFormat params) treeString)
    let otuAlnMap = otuAlignmentMap (optNoHenikoffWt params) fastaRecs
    let outputFileName = outFName (optOutFName params) alnFName
    runInfo params tree outputFileName
    possibleWarnings params tree otuAlnMap
    let classifier = buildClassifier
                        (molType params)
                        (optSmallProb params) (optScaleFactor params)
                        otuAlnMap tree
    cmdln <- getCmdLine
    -- TODO: the classifier gets encoded twice... this is inefficient.
    let cksum = crc32 $ encode classifier
    let sc = StoredClassifier classifier (Metadata cmdln cksum)
    encodeFile outputFileName sc

runInfo :: Params -> NewickTree -> String -> IO ()
runInfo params tree outFname
    | optVerbosity params < 2   = do return ()
    | otherwise                 = do
        putStrLn $ unlines [
            "mlgsc_train - building model ",
            ("input alignment:  " ++ (head $ posParams params)),
            ("input tree: " ++ (last $ posParams params)),
            ("output: " ++ outFname),
            ("molecule: " ++ (show $ molType params)),
            ("small prob: " ++ (show $ optSmallProb params)),
            ("scale factor: " ++ (show $ optScaleFactor params)),
            ("Henikoff weighting: " ++ (show $ not $ optNoHenikoffWt params)),
            ("Tree format: " ++ (show $ optPhyloFormat params)),
            ("ID tree: " ++ (show $ optIDtree params)),
            if (optIDtree params)
                then "Relabeled tree: " ++ (ST.unpack $ treeToNewick tree)
                else ""
            ]

getCmdLine :: IO String
getCmdLine = do
            prog <- getProgName
            args <- getArgs
            return $ L.intercalate " " $ prog : args

possibleWarnings :: Params -> OTUTree -> AlnMap -> IO ()
possibleWarnings params tree otuAlnMap =
    if (optVerbosity params) > 0
        then do
            if not $ null $ treeOTUsNotInALn tree otuAlnMap
                then do
                    putStrLn "The following tree taxa  are NOT found in the alignment:"
                    mapM_ STIO.putStrLn $ treeOTUsNotInALn tree otuAlnMap
                else return ()
            if not $ null $ alnOTUsNotInTree tree otuAlnMap
                then do
                    putStrLn "The following alignment taxa  are NOT found in the tree:"
                    mapM_ STIO.putStrLn $ alnOTUsNotInTree tree otuAlnMap
                else return ()
        else return ()

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
