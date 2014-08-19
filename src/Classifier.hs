module Classifier where

import qualified Data.Map.Strict as M -- most likely going to use all values
import qualified Data.Text.Lazy as T
import qualified Data.List as L

import MlgscTypes
import FastA

fastARecords2alnMap :: [FastA] -> M.Map  SciName Alignment
fastARecords2alnMap = undefined

-- Some data to play around with in GHCi

fasta = unlines [
    ">Genus_A",
    "ATGCAT",
    "GCGTGT",
    ">Genus_B",
    "ATG",
    "CAT",
    "GCG",
    "TGC",
    ">Genus_A",
    "ATGC",
    "ATGC",
    "GTGC",
    ">Genus_C",
    "XTGCATGCATGC",
    ">Genus_B",
    "GTGCATGCATAC",
    ">Genus_C",
    "YTGCATGCATGC",
    ">Genus_C",
    "NTGCTTGCATGC"
    ]

fastaRecs = fastATextToRecords $ T.pack fasta

headers = map header fastaRecs

-- initial map, with one empty list per genus:

initMap = L.foldl (\map otu -> M.insert otu [] map) M.empty headers

-- how to insert one element into the map, using Cons (:) instead of (++), i.e.
-- O(+)

mapw1 = M.insertWith (\s a -> (head s):a) (T.pack "Genus_A") [T.pack "XAGT"] initMap

hsTuples = L.map (\r -> (header r, [FastA.sequence r])) fastaRecs

-- TODO: produce final map by folding hsTuples on initMap. Abstract the line
-- with insertWith above into a function
