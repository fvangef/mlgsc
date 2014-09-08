import System.Environment (getArgs)
--import System.Console.GetOpt
--import System.IO
--import System.Random
--import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Binary (encodeFile)
import qualified Data.Text as ST
import qualified Data.Text.Lazy.IO as LTIO
--import qualified Data.Text.IO as TIO
--import System.Random
--
--
import MlgscTypes
--import Trees
--import Model
import NewickParser
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
    mapM_ putStrLn $ dumpAlnMap otuAlnMap
    let classifier = buildNucClassifier smallProb scaleFactor
                    otuAlnMap tree
    encodeFile "classifier.bcls" classifier

dumpAlnMap :: OTUToAlnMap -> [String]
dumpAlnMap otuAlnMap = map f $ M.assocs otuAlnMap
    where f (k, v) = otu ++ " (" ++ num ++ " seq)"
            where   otu = ST.unpack k
                    num = show $ length v 
