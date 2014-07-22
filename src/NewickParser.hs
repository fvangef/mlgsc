module NewickParser where

import Text.ParserCombinators.Parsec
import qualified Data.Text.Lazy as T
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

{-
leaf :: Parser (BinaryTree String)
leaf = do { i <- identifier
			; return (Leaf i)
		}

node :: Parser (BinaryTree String)
node = do 	{ oParen
			; left <- node
			; comma
			; right <- node
			; cParen
			; return (Node "" left right)
			}
		<|> leaf

bifurcatingTree :: Parser (BinaryTree String)
bifurcatingTree =
		do
			t <- node
			semicolon
			return t
-}

leaf :: Parser (Tree T.Text)
leaf = do 
		i <- identifier
		return (Node (T.pack i) [])

node :: Parser (Tree T.Text)
node = do {
		oParen ;
		l <-node `sepBy1` (char ',') ;
		cParen ;
		return (Node (T.pack "") l)
		}
	      <|> leaf

roseTree :: Parser (Tree T.Text)
roseTree = do
		t <- node
		semicolon
		return t

{-
parseBifurcatingTree :: String -> Either ParseError (BinaryTree String)
parseBifurcatingTree newick = (parse bifurcatingTree "" newick) 
-}

parseTree :: String -> Either ParseError (Tree T.Text)
parseTree newick = (parse roseTree "" newick)

run :: Show a => Parser a -> String -> IO ()
run p input 
	= case (parse p "" input) of
		Left err -> do 	{ putStr "parse error at "
				; print err
				}
		Right x	-> print x
