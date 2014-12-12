
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
import Data.Tree

import MlgscTypes
import NewickParser
import NewickDumper
import FastA
import Classifier
import Weights
import Alignment

main :: IO ()
main = do
    [newickFname] <- getArgs
    newickString <- readFile newickFname
    let (Right tree) = parseNewickTree newickString
    mapM_ STIO.putStrLn $ map pathToText $ paths tree
    return ()

-- Generates all the paths through the tree - in just two lines (I'm sure
-- Haskell pros can whittle this down even further, but still, the contrast with
-- tree2taxo.lua (which is 23 lines long and of course relies also on nw_luaed)
-- is rather striking. This could serve as the basis for a mechanism that
-- explores multiple branches in the tree if there is any doubt that the best
-- score is the only reasonable choice.

paths :: OTUTree -> [[OTUName]]
paths (Node name []) = [[name]]
paths (Node name kids) = map (name:) $ foldl1 (++) $ map paths kids

pathToText :: [OTUName] -> ST.Text
pathToText = ST.intercalate (ST.pack "; ")
