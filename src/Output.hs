module Output where

-- TODO: check imports

import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT
import Text.ParserCombinators.Parsec

import MlgscTypes
import FastA

-- formats an output line according to a format string (à la printf). ARguments
-- are: format string, original query as a FastA record, query sequence after
-- alignment, and path through the tree (as returned by trailToExtendedTaxo).
-- TODO: shouldn't the parsing of the fmt string be done just once? The format
-- itself is constant, even though the resulting string is different for every
-- query.

-- TODO: pass only the argments that vary from query to query, and the rest
-- using a Reader monad. See  formatResultReader in mlgsc.hs

formatResult :: FmtString -> StepFmtString -> FastA -> Sequence -> Trail
    -> ST.Text
formatResult fmtString stepFmtString query alnQry trail = 
    ST.concat $ map (evalFmtComponent query alnQry trail stepfmt) format
        where   (Right format) = parseOutputFormatString fmtString
                (Right stepfmt) = parseStepFormatString stepFmtString


data FmtComponent = Literal Char
                    | ID
                    | Header
                    | Path
                    | UPath     -- path with "unclassified"
                    | QueryLength
                    | Query
                    | AlignedQuery
                    | Score
                    deriving (Show)
                       
type Format = [FmtComponent]
type FmtString = String

literalChar :: Parser FmtComponent
literalChar = do
    c <- noneOf "%"
    return $ Literal c

escape :: Parser FmtComponent
escape = do
    char '%'
    f <- oneOf "%ahilpqsu"
    return $ case f of
                '%' -> Literal '%'
                'a' -> AlignedQuery
                'l' -> QueryLength
                'h' -> Header
                'i' -> ID
                'p' -> Path
                'q' -> Query
                's' -> Score
                'u' -> UPath

fmtComponent :: Parser FmtComponent
fmtComponent = literalChar <|> escape

format :: Parser [FmtComponent]
format = many fmtComponent

parseOutputFormatString :: String -> Either ParseError (Format)
parseOutputFormatString fmt = (parse format "format" fmt)

evalFmtComponent :: FastA
                 -> Sequence
                 -> Trail
                 -> StepFormat
                 -> FmtComponent
                 -> ST.Text
evalFmtComponent query alnQry trail stepfmt component = case component of
    Header          -> LT.toStrict $ FastA.header query
    QueryLength     -> ST.pack $ show $ LT.length $ FastA.sequence query
    ID              -> LT.toStrict $ fastAId query
    Query           -> LT.toStrict $ FastA.sequence query
    AlignedQuery    -> alnQry
    Path            -> trailToPath stepfmt trail 
    UPath           -> trailToUPath stepfmt trail 
    Score           -> ST.pack $ show $ bestScore $ last trail
    (Literal c)     -> ST.pack [c]

trailToPath :: StepFormat -> Trail -> ST.Text
trailToPath stepfmt trail = ST.intercalate (ST.pack "; ") $
                    map (formatStep stepfmt) trail

trailToUPath = undefined



data StepFmtComponent = SLiteral Char   -- 'Literal' already taken...
                        | TaxonName
                        | SupportVal
                        | BestScore
                        deriving (Show)

type StepFormat = [StepFmtComponent]
type StepFmtString = String

-- A Literal character, for a Step component (literalChar is already used for
-- output format components)

sLiteralChar :: Parser StepFmtComponent
sLiteralChar = do
    c <- noneOf "%"
    return $ SLiteral c

-- Same idea for escape

sEscape :: Parser StepFmtComponent
sEscape = do
    char '%'
    f <- oneOf "%bst"
    return $ case f of
                '%' -> SLiteral '%'
                't' -> TaxonName
                's' -> SupportVal
                'b' -> BestScore

stepFmtComponent :: Parser StepFmtComponent
stepFmtComponent = sLiteralChar <|> sEscape

stepFormat :: Parser StepFormat
stepFormat = many stepFmtComponent


parseStepFormatString :: String -> Either ParseError (StepFormat)
parseStepFormatString sfmt = (parse stepFormat "stepformat" sfmt)

run :: Show a => Parser a -> String -> IO ()
run p input 
        = case (parse p "" input) of
                Left err -> do  { putStr "parse error at "
                                ; print err
                                }
                Right x -> print x

formatStep :: StepFormat -> Step -> ST.Text
formatStep stepFormat step =
    ST.concat $ map (evalStepFmtComponent step) stepFormat

evalStepFmtComponent :: Step -> StepFmtComponent -> ST.Text
evalStepFmtComponent step component = case component of
    (SLiteral char) -> ST.pack [char]
    TaxonName       -> otuName step
    SupportVal      -> ST.pack $ if abs(log10ER step) < 1000
                            then show $ round $ log10ER step
                            else "*"
    BestScore       -> ST.pack $ show $ bestScore step
