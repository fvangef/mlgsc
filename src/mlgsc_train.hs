import System.Environment (getArgs)
import Options.Applicative
--import System.Console.GetOpt
--import System.IO
--import System.Random
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Binary (encodeFile)
import qualified Data.Text as ST
import qualified Data.Text.IO as STIO
import qualified Data.Text.Lazy.IO as LTIO
--import qualified Data.Text.IO as TIO
--import System.Random
--
--
import MlgscTypes
import NewickParser
import NewickDumper
import FastA
import Classifier
import Weights
import Alignment

data Molecule = DNA | Pep
    deriving (Show, Eq, Read)

data Params = Params {
                optSmallProb        :: Double
                , optScaleFactor    :: Double
                , optOutFname       :: String
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

parseOutFname :: Parser String
parseOutFname = option auto
                    (long "output-file"
                    <> short 'o'
                    <> metavar "OUTFILE"
                    <> value ""
                    <> help "name of the output classifier (default: derived from alignment)")
                    
parseOptions :: Parser Params
parseOptions = Params
                <$> parseSmallProb
                <*> parseScaleFactor
                <*> parseOutFname
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
    newickString <- readFile $ treeFname params
    let (Right tree) = parseNewickTree newickString
    fastAInput <-  LTIO.readFile $ alnFname params
    let fastaRecs = fastATextToRecords fastAInput
    -- putStrLn "Weight dump (short)"
    -- mapM_ (STIO.putStrLn . dumpAlnRow) $ take 10 rawOtuAln
    let otuAln = (henikoffWeightAln . fastARecordsToAln) fastaRecs
    let otuAlnMap = alnToAlnMap otuAln
    -- Show this if verbose
    -- mapM_ putStrLn $ dumpAlnMap otuAlnMap
    -- Show this unless quiet or list is empty
    putStrLn "The following tree OTUs are NOT found in the alignment:"
    mapM_ STIO.putStrLn $ treeOTUsNotInALn tree otuAlnMap
    putStrLn "The following alignment OTUs are NOT found in the tree:"
    mapM_ STIO.putStrLn $ alnOTUsNotInTree tree otuAlnMap

    let classifier = buildNucClassifier
                        (optSmallProb params) (optScaleFactor params)
                        otuAlnMap tree
    encodeFile "classifier.bcls" classifier

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
    
