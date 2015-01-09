module OutputFormatStringParser (parseOutputFormatString) where


import Text.ParserCombinators.Parsec
import qualified Data.Text as T

data FmtStringComponent = Literal Char
                        | Header
                        | Path
                        | Length
                        deriving (Show)
                       
literalChar :: Parser FmtStringComponent
literalChar = do
    c <- noneOf "%"
    return $ Literal c

escape :: Parser FmtStringComponent
escape = do
    char '%'
    f <- oneOf "hpl%"
    return $ case f of
                'h' -> Header
                'p' -> Path
                'l' -> Length
                '%' -> Literal '%'

fmtStringComponent :: Parser FmtStringComponent
fmtStringComponent = literalChar <|> escape

fmtString :: Parser [FmtStringComponent]
fmtString = many fmtStringComponent

parseOutputFormatString :: String -> Either ParseError ([FmtStringComponent])
parseOutputFormatString fmt = (parse undefined "" fmt)

run :: Show a => Parser a -> String -> IO ()
run p input 
	= case (parse p "" input) of
		Left err -> do 	{ putStr "parse error at "
				; print err
				}
		Right x	-> print x
