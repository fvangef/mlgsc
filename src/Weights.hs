module Weights where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text.Lazy as T

import MlgscTypes

-- TODO: introduce a new Alignment type, being more than just a list of Sequence
-- but rather a list of AlnRow, which should be something like:

data AlnRow = SimpleAlnRow OTUName Sequence
              | WeightedAlnRow OTUName Ssequence Int
-- or in fact just use weighted rows, and set a weight of 1 for unweighted ones.

-- This example is taken from
-- http://www.cs.umd.edu/class/fall2011/cmsc858s/Weights.pdf.
-- The code below was checked against the weights given in that document.
-- Everything is ok.

henikoffWeightAln :: Alignment -> WeightedAln
henikoffWeightAln aln = zip aln roundNormweights
    where   roundNormWeights = normalize weights
            weights = map (weightSeq aln) aln

normalize :: [Double] -> [Int]
normalize rawWts = L.map (round . (/ minw)) rawWts
    where   minw = minimum rawWts

weightSeq :: Alignment -> Sequence -> Float
weightSeq aln seq = sum $ map (weightMap !) $ T.unpack seq 


-- The Henikoff weight of a residue R (in a given column) is 1 / n s, where n is
-- the number of _different_ residues in the column, and s is the number of
-- sequences which have residue R in that column. For example, for a column
-- consisting of G, G, and C the value of n is 2 (two different residues, G and
-- C). The value of n is 2 for G and 1 for C, as there are two Gs and one C in
-- the column. The weight of G at that column is therefore 1 / (2 * 2) = 1/4,
-- and that for C is 1 / (2 * 1) = 1/2.

resWeight :: Int -> Int -> Float
resWeight num_res num_seq_with_res = 1.0 / (n * s)
    where   s = fromIntegral num_seq_with_res
            n = fromIntegral num_res 

-- Constructs the (residue -> weight) map for a given column. The keys are the
-- different residues in the column, e.g. for a column consisting of G, G, C the
-- map would have a key for G (with value 1/4) and a key for C (with value 1/2).

colToWeightsMap :: Column -> M.Map Char Float
colToWeightsMap col = M.map (res_weight $ M.size res_count_map) res_count_map
    where res_count_map = M.fromListWith (+) $ zip (T.unpack col) $ repeat 1

-- Maps all residues in a column to their weight, e.g. ['G', 'G', 'C'] -> [1/4,
-- 1/4, 1/2].

resColToWeightCol :: Column -> [Float]
resColToWeightCol col = L.map ((M.!) weightsMap) (T.unpack col)
    where weightsMap = colToWeightsMap col

-- The raw sequence weights are the sum of the residue weights over the
-- sequence. This fuction returns the raw weights of each sequence in an
-- alignment.

alnRawWeights :: Alignment -> [Float]
alnRawWeights aln =
    L.map sum $ L.transpose $ L.map resColToWeightCol $ T.transpose aln

normalize :: [Float] -> [Float]
normalize xs = L.map (/ (sum xs)) xs
