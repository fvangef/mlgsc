module FastA (FastA(..), fastATextToRecords) where

import qualified Data.Map.Strict as M -- most likely going to use all values
import qualified Data.Text.Lazy as LT
import qualified Data.Text as ST
import qualified Data.List as L

import MlgscTypes

data FastA = FastA { header :: LT.Text, sequence :: LT.Text } deriving (Show)

-- takes a FastA input (one or more records) and returns a list of FastA
-- records.

fastATextToRecords :: LT.Text -> [FastA]
fastATextToRecords fasta = map chunk2FastA chunks 
    where chunks = LT.splitOn (LT.pack "\n>") $ LT.tail fasta

chunk2FastA :: LT.Text -> FastA
chunk2FastA chunk = FastA hdr seq
    where   hdr = head chunk_lines
            seq = LT.concat $ tail chunk_lines 
            chunk_lines = LT.lines chunk


fastARecordsToAlnMap :: [FastA] -> M.Map  SciName Alignment
fastARecordsToAlnMap = undefined

-- Some data to play around with in GHCi

fasta = unlines [
    ">Genus_A",
    "AXGCAT",
    "GCGTGT",
    ">Genus_B",
    "BXG",
    "CAT",
    "GCG",
    "TGC",
    ">Genus_A",
    "AYGC",
    "ATGC",
    "GTGC",
    ">Genus_C",
    "CXGCATGCATGC",
    ">Genus_B",
    "BYGCATGCATAC",
    ">Genus_C",
    "CYGCATGCATGC",
    ">Genus_C",
    "CZGCTTGCATGC"
    ]

fastaRecs = fastATextToRecords $ LT.pack fasta

headers = map (LT.toStrict . header) fastaRecs

-- initial map, with one empty list per genus:

initMap = L.foldl (\map otu -> M.insert otu [] map) M.empty headers

-- how to insert one element into the map, using Cons (:) instead of (++), i.e.
-- O(+)

mapw1 = M.insertWith (\s a -> (head s):a) (ST.pack "Genus_A") [ST.pack "XAGT"] initMap

hsTuples = L.map (\r -> (LT.toStrict $ header r, [LT.toStrict $ FastA.sequence r])) fastaRecs

-- TODO: produce final map by folding hsTuples on initMap. Abstract the line
-- with insertWith above into a function

finalMap = L.foldl f initMap hsTuples

f :: (M.Map SciName Alignment) -> (SciName, [Sequence]) -> (M.Map SciName Alignment)
f map (otu, singleton) = M.insertWith g otu singleton map
    where g new acc = (head new):acc


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
