-- General Sequence Classifier

-- Dumps info about a classifier

-- module Main where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

import qualified Data.Text.IO as STIO
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Foldable (find)

import Options.Applicative

import Data.Binary (decodeFile)
import Data.Tree
--
import MlgscTypes
import Classifier (Classifier(..), classifySequence,
    StoredClassifier(..), Metadata(..))
import PWMModel (PWMModel, cladeName, tablePrint, prettyPrint, tablePrint)
import NewickDumper

data Params = Params {
                optModelPrintFct    :: PWMModel -> String
                , posParams         :: [String]
                }

parseModelStyle :: Monad m => String -> m (PWMModel -> String)
parseModelStyle s = if 'T' == head s
                                then return tablePrint
                                else return prettyPrint

parseOptions :: Parser Params
parseOptions = Params
                <$> option (str >>= parseModelStyle) (
                    short 's'
                    <> long "model-style"
                    <> value prettyPrint )
                <*> many (argument str (metavar "<classifier filename> [clade name]"))

parseOptionsInfo :: ParserInfo Params
parseOptionsInfo = info (helper <*> parseOptions) 
                    ( fullDesc
                    <> progDesc "dump classifier in text form"
                    <> Options.Applicative.header
                        "mlgsc_dump - text dumper for classifiers")

warn :: String -> IO ()
warn s = hPutStrLn stderr s

main :: IO ()
main = do
    params <- execParser parseOptionsInfo
    let positionalParams =  posParams params
    let clsfrFname = head positionalParams
    classifier@(PWMClassifier modTree scale) <-
        (decodeFile clsfrFname) :: IO Classifier
    case tail positionalParams of
        [] -> do
            let taxonTree = fmap cladeName modTree 
            putStrLn $ heading '-' "Taxon tree"
            STIO.putStrLn $ treeToNewick taxonTree 
            putStrLn ""
            putStrLn $ heading '-' "Models"
            putStrLn $ foldMap (optModelPrintFct params) modTree 
        [tgtCladeName] -> do
            case find (\m -> cladeName m == T.pack tgtCladeName) modTree of
                Nothing -> warn ("clade '" ++ tgtCladeName ++ "' not found.")
                (Just mod) -> putStrLn $ (optModelPrintFct params) mod

heading :: Char -> String -> String
heading c s = s ++ "\n" ++ (replicate l c) ++ "\n"
        where l = length s

