import System.Environment (getArgs)
--import System.Console.GetOpt
--import System.IO
--import System.Random
--import qualified Data.List as L
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
--import Trees
--import Model
import NewickParser
import NewickDumper
import FastA
import Classifier
--import Cdf
--import Weights
--import Shuffle

main :: IO ()
main = do
    let smallProb = 0.0001
    let scaleFactor = 1000
    [alnFname, newickFname] <- getArgs
    newickString <- readFile newickFname
    let (Right tree) = parseNewickTree newickString
    fastAInput <-  LTIO.readFile alnFname
    let fastaRecs = fastATextToRecords fastAInput
    let otuAlnMap = fastARecordsToAlnMap fastaRecs
    -- Show this if verbose
    -- mapM_ putStrLn $ dumpAlnMap otuAlnMap
    -- Show this unless quiet or list is empty
    putStrLn "The following tree OTUs are NOT found in the alignment:"
    mapM_ STIO.putStrLn $ checkOtuNames tree otuAlnMap
    let classifier = buildNucClassifier smallProb scaleFactor
                    otuAlnMap tree
    encodeFile "classifier.bcls" classifier

dumpAlnMap :: OTUToAlnMap -> [String]
dumpAlnMap otuAlnMap = map f $ M.assocs otuAlnMap
    where f (k, v) = otu ++ " (" ++ num ++ " seq)"
            where   otu = ST.unpack k
                    num = show $ length v 

-- compares OTU names in the tree to those in the FastA map
checkOtuNames :: OTUTree -> OTUToAlnMap -> [ST.Text]
checkOtuNames otuTree otuAlnMap =
    filter (\otu -> M.notMember otu otuAlnMap) $ fringe otuTree
