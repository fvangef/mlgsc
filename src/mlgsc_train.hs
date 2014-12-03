import System.Environment (getArgs)
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

main :: IO ()
main = do
    let smallProb = 0.0001
    let scaleFactor = 1000
    [alnFname, newickFname] <- getArgs
    newickString <- readFile newickFname
    let (Right tree) = parseNewickTree newickString
    fastAInput <-  LTIO.readFile alnFname
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

    let classifier = buildNucClassifier smallProb scaleFactor
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
    
