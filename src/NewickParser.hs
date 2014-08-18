module NewickParser where

import Text.ParserCombinators.Parsec
import qualified Data.Text as T
import Data.Tree


identifier :: Parser String
identifier = many (letter <|> digit <|> char '_') <?> "identifier"

oParen :: Parser ()
oParen = char '(' >> return ()

cParen :: Parser ()
cParen = char ')' >> return ()

semicolon :: Parser ()
semicolon = char ';' >> return ()

comma :: Parser ()
comma = char ',' >> return ()

nwLeaf :: Parser (Tree String)
nwLeaf = do
    id <- identifier
    return $ Node id []

nwNode :: Parser (Tree String)
nwNode = do	{ oParen
			  ; children <- nwNode `sepBy` comma
			  ; cParen
			  ; return $ Node "" children
			}
		 <|> nwLeaf

newickTree :: Parser (Tree String)
newickTree = do
    tree <- nwNode
    semicolon
    return tree

parseNewickTree :: String -> Either ParseError (Tree String)
parseNewickTree newick = (parse newickTree "" newick)

run :: Show a => Parser a -> String -> IO ()
run p input 
	= case (parse p "" input) of
		Left err -> do 	{ putStr "parse error at "
				; print err
				}
		Right x	-> print x
