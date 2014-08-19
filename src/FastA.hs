module FastA (FastA(..), fastATextToRecords) where

import qualified Data.Text.Lazy as T

data FastA = FastA { header :: T.Text, sequence :: T.Text } deriving (Show)

-- takes a FastA input (one or more records) and returns a list of FastA
-- records.

fastATextToRecords :: T.Text -> [FastA]
fastATextToRecords fasta = map chunk2FastA chunks 
    where chunks = T.splitOn (T.pack "\n>") $ T.tail fasta

chunk2FastA :: T.Text -> FastA
chunk2FastA chunk = FastA hdr seq
    where   hdr = head chunk_lines
            seq = T.concat $ tail chunk_lines 
            chunk_lines = T.lines chunk



-- To test in GHCi, e.g.:
--
-- parse fastaRawRecord "(unknown)" fasta
--

{-
-- Fault-tolerant parameters. Only assumes that the first char is a '>'.

toFastARecords :: String -> [FastA]
toFastARecords fasta = toFastaRecords' "\n>" laxStr2list ('\n':fasta)

{-
 - Takes a FastA input (usually file contents) and returns a list of FastA data
 - objects (see above). 'delim' is what separates records: usually '>' works,
 - but then the '>' character must never appear elsewhere than as the first
 - char of a header line (e.g. ">myId 56->57" would not parse, due to another
 - '>' within the header). Otherwise, use "\n>", but then prefix the whole
 - input with '\n' (because presumably it starts with '>').  The contents of
 - the records (i.e., the strings between delimiters) is further split into
 - lists of lines using one of strictStr2list or laxStr2list. The former is
 - faster but expects all lines to be '\n' - terminated, while the latter is
 - slower but will accept that the _last_ line has no '\n'.
 -}

toFastaRecords' :: String -> (String -> [String]) -> String -> [FastA]
toFastaRecords' delim str2list text =
	map (strToRecord str2list) $ (tail . splitOn delim) text

{- Takes a string representing a FastA record, and returns a FastA object. The
 - string (i.e., what's between the headers' '>'), is split into lines. The
 - first line becomes the 'header' field of the FastA record, while all other
 - lines are concatenated to become the 'sequence' field). --}

strToRecord :: (String -> [String]) -> String -> FastA
strToRecord str2list str = FastA header sequence
		where	header		= head list
			sequence	= foldr1 (++) $ tail list
			list		= str2list str

-- Splits a FastA string into lines, assuming ALL lines are '\n' - terminated.

strictStr2list :: String -> [String]
strictStr2list str = init $ splitOn "\n" str

-- Splits a FastA string into lines - does NOT assume ALL lines are '\n' -
-- terminated, but has to filter the empty lines.

laxStr2list :: String -> [String]
laxStr2list str = filter (/= "") $ splitOn "\n" str
-}
