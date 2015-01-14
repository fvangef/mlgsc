module OutputFormatStringParser (
    FmtComponent(..),
    Format,
    FmtString,
    parseOutputFormatString) where


import Text.ParserCombinators.Parsec
import qualified Data.Text as T

data FmtComponent = Literal Char
                    | ID
                    | Header
                    | Path
                    | QueryLength
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
    f <- oneOf "%ahilps"
    return $ case f of
                '%' -> Literal '%'
                'a' -> AlignedQuery
                'l' -> QueryLength
                'h' -> Header
                'i' -> ID
                'p' -> Path
                's' -> Score

fmtComponent :: Parser FmtComponent
fmtComponent = literalChar <|> escape

format :: Parser [FmtComponent]
format = many fmtComponent

parseOutputFormatString :: String -> Either ParseError (Format)
parseOutputFormatString fmt = (parse format "format" fmt)

run :: Show a => Parser a -> String -> IO ()
run p input 
	= case (parse p "" input) of
		Left err -> do 	{ putStr "parse error at "
				; print err
				}
		Right x	-> print x
