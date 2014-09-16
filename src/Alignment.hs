module Alignment (Alignment, AlnRow(..), fastARecordsToAln, alnToAlnMap, AlnMap) where

import qualified Data.Text.Lazy as LT
import qualified Data.Text as ST
import qualified Data.Map.Strict as M
import qualified Data.List as L

import FastA

data AlnRow = AlnRow {
                rowLabel    :: ST.Text, -- e.g., OTU name
                rowSeq      :: ST.Text,
                rowWeight   :: Int
                }

type Alignment = [AlnRow]

type Label = ST.Text
type AlnMap = M.Map Label Alignment

-- Produces an Alignment from a list of FastA records. The row weights are set
-- to 1.

fastARecordsToAln :: [FastA] -> Alignment
fastARecordsToAln = L.map (\(FastA hdr seq) ->
    AlnRow (LT.toStrict hdr) (LT.toStrict seq) 1)

-- Produces a (otu -> (sub)Alignment) map from an Alignment. This map is strict,
-- also in values (maps are always strict in keys, as I understand).

alnToAlnMap :: Alignment -> AlnMap
alnToAlnMap aln =
    M.fromListWith (++) $ L.map (\row -> (rowLabel row, [row])) aln
