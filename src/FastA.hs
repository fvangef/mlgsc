module FastA (FastA(..), fastATextToRecords) where

import qualified Data.Map.Strict as M -- most likely going to use all values
import qualified Data.Text.Lazy as LT
import qualified Data.Text as ST
import qualified Data.List as L

import MlgscTypes

data FastA = FastA { header :: LT.Text, sequence :: LT.Text } deriving (Show)

-- takes a FastA input (one or more records) and returns a list of FastA
-- records. This uses lazy text.

fastATextToRecords :: LT.Text -> [FastA]
fastATextToRecords fasta = map chunk2FastA chunks 
    where chunks = LT.splitOn (LT.pack "\n>") $ LT.tail fasta

chunk2FastA :: LT.Text -> FastA
chunk2FastA chunk = FastA hdr seq
    where   hdr = head chunk_lines
            seq = LT.concat $ tail chunk_lines 
            chunk_lines = LT.lines chunk


-- Produces a (otu -> alignment) map. This map is strict, also in values (maps
-- are always strict in keys, as I understand).

-- This works as follows: a list of (otu-name, [sequence]) tuples (hsTuples) is
-- folded over by the updateMap function, using an initial (otu-name -> []) map
-- (initMap). That is, the initial map is updated for each of the tuples. The
-- updating results in the new tuples's sequence member being cons'ed (:) onto
-- the existing value in the map (which is empty in the initial map). Note that
-- the tuples have the sequence as a singleton list, because the insertWith
-- function wants its two arguments to have the same type as its result, which
-- is a list. Therefore, I just call head on the new singleton and cons (:) it
-- onto the existing list, yielding the new value. 

-- It is possible to build the map in fewer steps using (++) instead of (:), but
-- it seems that head and (:) work in constant time, while (++) is O(n).

-- Note also that due to the cons'ing, the _last_ sequence in the FastA records
-- list is the head of the list that constitutes the alignment of any given OTU. 

fastARecordsToAlnMap :: [FastA] -> M.Map  SciName Alignment
fastARecordsToAlnMap fastaRecs = L.foldl updateMap initMap hsTuples
    where   initMap = L.foldl (\map otu -> M.insert otu [] map) M.empty headers
            headers = L.map (LT.toStrict . header) fastaRecs
            hsTuples = L.map (\r -> (LT.toStrict $ header r, [LT.toStrict $ FastA.sequence r])) fastaRecs
            updateMap map (otu, singleton) =
                M.insertWith prepend otu singleton map
                where prepend new acc = (head new):acc

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


