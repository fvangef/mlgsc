module FastA (
    FastA(..),
    fastATextToRecords,
    fastAId,
    fastAOTU,
    degap,
    idToTaxonMap,
    utax2taxo
    ) where

import qualified Data.Text.Lazy as LT
import Data.Map (Map, fromList)

data FastA = FastA { header :: LT.Text, sequence :: LT.Text } deriving (Show)

-- takes a FastA input (one or more records) and returns a list of FastA
-- records. This uses lazy text.

fastATextToRecords :: LT.Text -> [FastA]
fastATextToRecords fasta = map chunk2FastA chunks 
    where chunks = LT.splitOn (LT.pack "\n>") $ LT.tail fasta

chunk2FastA :: LT.Text -> FastA
chunk2FastA chunk = FastA hdr seq
    where   hdr = head chunk_lines
            seq = (LT.toUpper . LT.concat) $ tail chunk_lines 
            chunk_lines = LT.lines chunk

fastAId :: FastA -> LT.Text
fastAId = head . LT.words . FastA.header

fastAOTU :: FastA -> LT.Text
fastAOTU = (!! 1) . LT.words . FastA.header

degap :: FastA -> FastA
degap (FastA hdr seq) = FastA hdr $  LT.replace (LT.pack "-") LT.empty seq

idToTaxonMap :: [FastA] -> Map LT.Text LT.Text
idToTaxonMap records = fromList $ zip ids taxa
    where   ids     = map fastAId records
            taxa    = map fastAOTU records

-- Returns the taxonomy part of a Fasta record with Utax-formatted header
utax2taxo :: FastA -> LT.Text
utax2taxo rec =
    LT.replace (LT.pack ",") (LT.pack "; ") $ last $ LT.splitOn (LT.pack "=") $ header rec
