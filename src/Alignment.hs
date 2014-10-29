module Alignment (Alignment, AlnRow(..), rowId, rowOTU,
    fastARecordsToAln,
    alnToAlnMap, AlnMap) where

import qualified Data.Text.Lazy as LT
import qualified Data.Text as ST
import qualified Data.Map.Strict as M
import qualified Data.List as L

import FastA

data AlnRow = AlnRow {
                rowLabel    :: ST.Text, -- <- FastA header
                rowSeq      :: ST.Text,
                rowWeight   :: Int
                } deriving (Show)

type Alignment = [AlnRow]

type Label = ST.Text
type AlnMap = M.Map Label Alignment

-- By convention, the ID is the first word of the label (itself typically just
-- the FastA header).

rowId :: AlnRow -> ST.Text
rowId = (head . ST.words . rowLabel)

-- and the OTU (if any) is the second word, else ""
--
rowOTU :: AlnRow -> ST.Text
rowOTU row
    | length lblWords >= 2 = lblWords !! 1
    | otherwise = ST.empty
    where lblWords = ST.words $ rowLabel row



-- Produces an Alignment from a list of FastA records. The row weights are set
-- to 1.

fastARecordsToAln :: [FastA] -> Alignment
fastARecordsToAln = L.map (\(FastA hdr seq) ->
    AlnRow (LT.toStrict hdr) (LT.toStrict seq) 1)

-- Produces a (otu -> (sub)Alignment) map from an Alignment. This map is strict,
-- also in values (maps are always strict in keys, as I understand).

alnToAlnMap :: Alignment -> AlnMap
alnToAlnMap aln =
    M.fromListWith (++) $ L.map (\row -> (rowOTU row, [row])) aln
