-- General Sequence Classifier

module Main (main) where

-- Classifies a set of sequences using a classifier produced by gsc_mk.  
-- module Main where

import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.Text as ST
import qualified Data.Text.IO as STIO
import Text.Printf
import qualified Text.Parse as TP
import Control.Applicative
import Control.Monad.Reader
import Options.Applicative

import Data.Maybe
import Data.Map (Map, fromList, findWithDefault)
import Data.Binary (decodeFile)
import Data.Tree
import Data.Char
import Data.List
import Data.Ord

import MlgscTypes
import FastA
import Trim
import PWMModel
import Align
import Classifier (Classifier(..), classifySequence,
    classifySequenceMulti, classifySequenceAll,
    StoredClassifier(..))
import Output

data TreeTraversalMode = BestTraversal
                       | FullTraversal
                       | RecoverTraversal Int

data MaskMode = None | Trim

data SingleNodeScope = NodeItself | NodesChildren
    
data Params = Params {
                optTreeTraversalMode    :: TreeTraversalMode
                , optNoAlign            :: Bool
                , optOutFmtString       :: String
                , optStepFmtString      :: String
                , optERCutoff           :: Int   -- for Best mode (TODO: could be an argument to the BestTraversal c'tor)
                , optMaskMode           :: MaskMode
                , opt1NodeScope         :: SingleNodeScope
                , posParams             :: [String]
                }

parseTreeTraversal :: Monad m => String -> m TreeTraversalMode
parseTreeTraversal optString
    | 'b' == initC = return BestTraversal
    | 'a' == initC = return FullTraversal
    | otherwise    = do
                        let (Right num,_) = TP.runParser TP.parseDec optString
                        -- TODO: handle bad parse
                        return $ RecoverTraversal num
    where initC = toLower $ head optString

parseMaskMode :: Monad m => String -> m MaskMode
parseMaskMode optString
    | 't' == initC = return Trim
    | 'n' == initC = return None
    | otherwise = return None -- TODO: warn about unrecognized opt
    where initC = toLower $ head optString

parseSingleNodeScope :: Monad m => String -> m SingleNodeScope
parseSingleNodeScope optString
    | 's' == initC = return NodeItself
    | 'c' == initC = return NodesChildren
    -- TODO: handle others
    where initC = toLower $ head optString

parseOptions :: Parser Params
parseOptions = Params
                <$> option (str >>= parseTreeTraversal)
                    (long "traversal-mode"
                    <> short 'm'
                    <> help "tree traversal mode (b|a|<int>)"
                    <> value BestTraversal)
                <*> switch
                    (long "no-align"
                    <> short 'A'
                    <> help "do not align query sequences")
                <*> option str
                    (long "output-format"
                    <> short 'f'
                    <> help "printf-like format string for output."
                    <> value "%h -> %p")
                <*> option str
                    (long "step-format"
                    <> short 's'
                    <> help "printf-like format string for step (path element)"
                    <> value "%t (%s)")
                <*> option auto
                    (long "ER-cutoff"
                    <> short 'e'
                    <> help "drop clades with ER lower than this"
                    <> value 0)
                <*> option (str >>= parseMaskMode)
                    (long "mask-mode"
                    <> short 'M'
                    <> help "mask aligned query: n)one* | t)trim"
                    <> value None)
                <*> option (str >>= parseSingleNodeScope)
                    (long "single-node-scope"
                    <> short 'S'
                    <> help "only score vs s) single node or its c)hildren"
                    <> value NodeItself)
                <*> many (argument str (metavar "<query seq file> <classifier filename> [clade name]"))

parseOptionsInfo :: ParserInfo Params
parseOptionsInfo = info (helper <*> parseOptions) 
                    ( fullDesc
                    <> progDesc "classify sequences according to a model"
                    <> Options.Applicative.header
                        "mlgsc - maximum-likelihood general sequence classifier")


-- Some common format options have names (e.g.  "simple" -> "%h -> %P").
-- These must be translated using the following map (some short forms are
-- possible)

fmtMap :: Map String String
fmtMap = fromList [
                    ("minimal", "%i\t%P"),
                    ("min", "%i\t%P"),
                    ("m", "%i\t%P"),
                    ("simple", "%h -> %P (%s)"),
                    ("s", "%h -> %P (%s)")
                ]

translateFmtKw :: Params -> IO Params
translateFmtKw params = do
    let origFmt = optOutFmtString params
    let fmt = findWithDefault origFmt origFmt fmtMap
    return params { optOutFmtString = fmt }
                

main :: IO ()
main = do
    params <- execParser parseOptionsInfo >>= translateFmtKw
    let positionalParams = posParams params
    let (queryFname, clsfrFname, singleNode) =
            case positionalParams of
                [q,c,n] -> (q,c,Just n)
                [q,c]   -> (q,c,Nothing)
    queryFastA <- LTIO.readFile queryFname
    let queryRecs = fastATextToRecords queryFastA
    storedClassifier <- (decodeFile clsfrFname) :: IO StoredClassifier
    let (StoredClassifier
            origClassifier@(PWMClassifier modTree scale)
            _) = storedClassifier
    let rootMod = rootLabel modTree
    -- Some options reduce the classifier to a subclade
    let classifier =
            case singleNode of
                 Nothing -> origClassifier
                 Just node -> case subClsf of
                                  Nothing -> error "node not found"
                                  (Just c) -> c
                        where subClsf = subsetClassifier origClassifier (ST.pack node) (opt1NodeScope params) 
    -- TODO: replace the magic "-2" below by a meaningful
    -- constant/param
    let scoringScheme =
            ScoringScheme (-2) (scoringSchemeMap
            (absentResScore rootMod))
    let headers = map FastA.header queryRecs
    let processedQueries =
            map (
                -- All the transformations from Fasta record to
                -- ready-to-score query. Note that some (mb*)
                -- depend on params among other things.
                mbMask params
                . mbAlign params scoringScheme rootMod
                . ST.toUpper
                . LT.toStrict
                . FastA.sequence
            ) queryRecs
    let outlines =
            case optTreeTraversalMode params of
                BestTraversal ->
                    bestTraversal params classifier queryRecs
                    processedQueries
                (RecoverTraversal _) ->
                    recoverTraversal params classifier queryRecs
                    processedQueries
                FullTraversal -> fullTraversal params classifier
                    queryRecs processedQueries

    mapM_ STIO.putStrLn outlines

subsetClassifier :: Classifier -> CladeName -> SingleNodeScope ->
    Maybe Classifier
subsetClassifier (PWMClassifier modtree scale) name scope = 
            case subtree of
                (Just s) -> Just $ PWMClassifier s scale
                Nothing  -> Nothing
            where subtree = subTreeNamed name modtree 

subTreeNamed :: CladeName -> (Tree PWMModel) -> Maybe (Tree PWMModel)
subTreeNamed name subtree@(Node model kids)
    | name == cladeName model = Just $ trim subtree
    | otherwise =
        case kids of
           [] -> Nothing
           (k:ks) -> listToMaybe $ catMaybes $ map (subTreeNamed name) kids

trim:: Tree a -> Tree a
trim leaf@(Node _ []) = leaf
trim (Node x kids) = Node x $ map toLeaf kids

toLeaf :: Tree a -> Tree a
toLeaf n = n {subForest = []}

-- Returns an alignment step (technically, a ST.Text -> ST.Text
-- function), or just id if option 'optNoAlign' is set. The parens in the
-- signature are not strictly necessary, but emphasize the fact that we
-- return a function.

mbAlign :: Params -> ScoringScheme -> PWMModel -> (Sequence -> Sequence)
mbAlign params scsc rootMod =
    if optNoAlign params
            then id
            else msalign scsc rootMod

-- Returns a masking step (a ST.Text -> ST.Text function),
-- or just id if no masking was requested.

mbMask :: Params -> (ST.Text -> ST.Text)
mbMask params =
    case optMaskMode params of
        None -> id
        Trim -> trimSeq

bestTraversal :: Params -> Classifier -> [FastA] -> [Sequence] -> [ST.Text]
bestTraversal params classifier queryRecs processedQueries =
    getZipList $ (formatResultWrapper params)
        <$> ZipList queryRecs
        <*> ZipList processedQueries
        <*> ZipList predictions 
    where
        log10ER = optERCutoff params
        predictions = map (classifySequence classifier log10ER) processedQueries

recoverTraversal :: Params -> Classifier -> [FastA] -> [Sequence] -> [ST.Text]
recoverTraversal params classifier queryRecs processedQueries =
    getZipList $ (formatResultWrapper params)
        <$> ZipList queryRecs
        <*> ZipList processedQueries
        <*> ZipList bestPredictions
    where
        (RecoverTraversal tieThreshold)  = optTreeTraversalMode params
        bestPredictions = map getBestPrediction predictions
        predictions = map (classifySequenceMulti classifier tieThreshold) processedQueries
    
getBestPrediction :: [Trail] -> Trail
getBestPrediction trails = maximumBy (comparing lastScore) trails

lastScore :: Trail -> Score
lastScore trail = bestScore $ last trail 

fullTraversal :: Params -> Classifier -> [FastA] -> [Sequence] -> [ST.Text]
fullTraversal params classifier queryRecs processedQueries =
    concat $ getZipList $ (fullTraversalFmt1Query params)
        <$> ZipList queryRecs
        <*> ZipList processedQueries
        <*> ZipList predictions
    where
        predictions = map (classifySequenceAll classifier) processedQueries
    
fullTraversalFmt1Query :: Params -> FastA -> Sequence -> [Trail] -> [ST.Text]
fullTraversalFmt1Query params queryRec processQuery trails =
    map (formatResultWrapper params queryRec processQuery) trails

{- I like to apply the output formatter in aplicative style to the lists of
- arguments. However, I'm not sure how to make this play with the Reader
- monad, except by using a wrapper like below. -}

formatResultWrapper :: Params -> FastA -> Sequence -> Trail -> ST.Text  
formatResultWrapper params query alnQry trail =
    runReader (formatResultReader query alnQry trail) params

{- This works, but it's not quite clear what should go in Output,
 - OutputFormatStringParser, or just plain here in Main. -}

formatResultReader :: FastA -> Sequence -> Trail -> Reader Params ST.Text 
formatResultReader query alnQry trail = do
    fmtString    <- asks optOutFmtString
    stepFmtString <- asks optStepFmtString
    let (Right format) = parseOutputFormatString fmtString 
    let (Right stepfmt) = parseStepFormatString stepFmtString
    return $ ST.concat $ map (evalFmtComponent query alnQry trail stepfmt) format

