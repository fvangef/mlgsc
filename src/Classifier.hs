module Classifier where

import qualified Data.Map.Strict as M -- most likely going to use all values
import qualified Data.Text.Lazy as LT
import qualified Data.Text as ST
import qualified Data.List as L

import MlgscTypes
import FastA

fastARecords2alnMap :: [FastA] -> M.Map  SciName Alignment
fastARecords2alnMap = undefined

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
