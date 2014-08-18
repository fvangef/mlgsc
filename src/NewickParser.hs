module NewickParser (parseNewickTree) where

-- Hm, for now it will parse String, not Data.Text (but the trees should be very
-- small compared to the alignments, so it should make no difference).

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

nwLeaf :: Parser (Tree T.Text)
nwLeaf = do
    id <- identifier
    return $ Node (T.pack id) []

nwNode :: Parser (Tree T.Text)
nwNode = do	{ oParen
			  ; children <- nwNode `sepBy` comma
			  ; cParen
			  ; return $ Node T.empty children
			}
		 <|> nwLeaf

newickTree :: Parser (Tree T.Text)
newickTree = do
    tree <- nwNode
    semicolon
    return tree

parseNewickTree :: String -> Either ParseError (Tree T.Text)
parseNewickTree newick = (parse newickTree "" newick)

run :: Show a => Parser a -> String -> IO ()
run p input 
	= case (parse p "" input) of
		Left err -> do 	{ putStr "parse error at "
				; print err
				}
		Right x	-> print x
