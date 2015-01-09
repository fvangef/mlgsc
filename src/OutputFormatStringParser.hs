module OutputFormatStringParser (
    FmtComponent(..),
    Format,
    parseOutputFormatString) where


import Text.ParserCombinators.Parsec
import qualified Data.Text as T

data FmtComponent = Literal Char
                        | Header
                        | Path
                        | Length
                        | Sequence
                        deriving (Show)
                       
type Format = [FmtComponent]

literalChar :: Parser FmtComponent
literalChar = do
    c <- noneOf "%"
    return $ Literal c

escape :: Parser FmtComponent
escape = do
    char '%'
    f <- oneOf "hpl%"
    return $ case f of
                'h' -> Header
                'p' -> Path
                'l' -> Length
                's' -> Sequence
                '%' -> Literal '%'

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