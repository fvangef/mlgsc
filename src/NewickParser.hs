module NewickParser (parseNewickTree) where

-- Hm, for now it will parse String, not Data.Text (but the trees should be very
-- small compared to the alignments, so it should make no difference).

import Text.ParserCombinators.Parsec
import qualified Data.Text as T
import Data.Tree


identifier :: Parser String
identifier = many (letter <|> digit <|> char '_') <?> "identifier"

-- Note: these "number" parsers actually return (), because they are used to
-- recognize branch lengths in Newick, which are not used in the classifier.
-- They are only needed to allow phylograms to parse (otherwise only cladograms
-- would be accepted).

digits :: Parser ()
digits = many digit >> return ()

period :: Parser ()
period = char '.' >> return ()

number :: Parser ()
number = digits >> option () (period >> digits)

oParen :: Parser ()
oParen = char '(' >> return ()

cParen :: Parser ()
cParen = char ')' >> return ()

colon :: Parser ()
colon = char ':' >> return ()

semicolon :: Parser ()
semicolon = char ';' >> return ()

comma :: Parser ()
comma = char ',' >> return ()

nwLeaf :: Parser (Tree T.Text)
nwLeaf = do
    id <- identifier
    length <- option () (colon >> number)
    return $ Node (T.pack id) []

nwNode :: Parser (Tree T.Text)
nwNode = do {oParen
              ; children <- nwNode `sepBy` comma
              ; cParen
              ; label <- option "" identifier 
              ; length <- option () (colon >> number)
              ; return $ Node (T.pack label) children
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
        Left err -> do  { putStr "parse error at "
                ; print err
                }
        Right x -> print x
