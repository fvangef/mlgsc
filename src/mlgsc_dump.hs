-- General Sequence Classifier

-- Dumps info about a classifier

-- module Main where

import System.Environment (getArgs)

--import qualified Data.Text.Lazy.IO as LTIO
--import qualified Data.Text.Lazy as LT
import qualified Data.Text.IO as STIO
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Data.Binary (decodeFile)
import Data.Tree
--import System.Directory
--
--import MlgscTypes
--import Crumbs (dropCrumbs, followCrumbs)
import NucModel
import Classifier (NucClassifier, otuTree, modTree)
import NewickDumper

--import Trees
--import Align
--import Cdf

main :: IO ()
main = do
    [classifierFname] <- getArgs   
    classifier <- (decodeFile classifierFname) :: IO NucClassifier
    let otutree = otuTree classifier
    let modtree = modTree classifier
    STIO.putStrLn $ treeToNewick otutree 
    putStrLn $ drawTree $ fmap show otutree
    putStrLn $ drawTree $ fmap mod2String modtree
    return ()

mod2String :: NucModel -> String
mod2String mod = show $ U.length $ V.head $ matrix mod

