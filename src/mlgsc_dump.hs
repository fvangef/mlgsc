-- General Sequence Classifier

-- Dumps info about a classifier

-- module Main where

import System.Environment (getArgs)

import qualified Data.Text.IO as STIO
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Data.Binary (decodeFile)
import Data.Tree
--
import MlgscTypes
import PWMModel (cladeName, prettyPrint)
import Classifier (Classifier(..), classifySequence,
    StoredClassifier(..), Metadata(..))
import NewickDumper

main :: IO ()
main = do
    [clsfrFname] <- getArgs   
    storedClassifier <- decodeFile $ clsfrFname :: IO StoredClassifier
    let (StoredClassifier classifier@(PWMClassifier modTree scale) 
            metadata) = storedClassifier
    putStrLn "Date:\t<add>"
    putStrLn "User:\t<add>"
    putStr "CRC32:\t" 
    print $ checksum metadata
    putStrLn $ heading '-' "Command line"
    putStrLn $ cmdLine metadata
    putStrLn ""
    let taxonTree = fmap cladeName modTree 
    putStrLn $ heading '-' "Taxon tree"
    STIO.putStrLn $ treeToNewick taxonTree 
    putStrLn ""
    putStrLn $ heading '-' "Models"
    putStrLn $ foldMap prettyPrint modTree 
    return ()

heading :: Char -> String -> String
heading c s = s ++ "\n" ++ (replicate l c) ++ "\n"
        where l = length s
