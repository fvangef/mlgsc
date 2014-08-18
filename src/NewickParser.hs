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

-- TODO: write parser for group of nodes
nwNode = Parser (Tree String)
nwNode = do nwLeaf
            <|> 
    
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

roseLeaf :: Parser (RoseTree T.Text)
roseLeaf = do 
		i <- identifier
		return (RoseTree (T.pack i) [])

roseNode :: Parser (RoseTree T.Text)
roseNode = do {
		oParen ;
		l <-roseNode `sepBy1` (char ',') ;
		cParen ;
		return (RoseTree (T.pack "") l)
		}
	      <|> roseLeaf

roseTree :: Parser (RoseTree T.Text)
roseTree = do
		t <- roseNode
		semicolon
		return t

parseBifurcatingTree :: String -> Either ParseError (BinaryTree String)
parseBifurcatingTree newick = (parse bifurcatingTree "" newick) 
-}

newickTree :: Parser (Tree T.Text)
newickTree = undefined

parseNewickTree :: String -> Either ParseError (Tree T.Text)
parseNewickTree newick = (parse newickTree "" newick)

run :: Show a => Parser a -> String -> IO ()
run p input 
	= case (parse p "" input) of
		Left err -> do 	{ putStr "parse error at "
				; print err
				}
		Right x	-> print x
