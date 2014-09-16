module Weights where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import Control.Applicative

import MlgscTypes
import Alignment

-- This example is taken from
-- http://www.cs.umd.edu/class/fall2011/cmsc858s/Weights.pdf.
-- The code below was checked against the weights given in that document.
-- Everything is ok.

henikoffWeightAln :: Alignment -> Alignment
henikoffWeightAln aln = liftA2 updateWeight aln normWeights
    where   normWeights = normalize $ alnRawWeights aln 
            updateWeight row weight = row {rowWeight = weight} 

normalize :: [Double] -> [Int]
normalize rawWts = L.map (round . (/ minw)) rawWts
    where   minw = minimum rawWts

-- The raw sequence weights are the sum of the residue weights over the
-- sequence. This fuction returns the raw weights of each sequence in an
-- alignment.

alnRawWeights :: Alignment -> [Double]
alnRawWeights aln =
    L.map sum $ L.transpose $ L.map resColToWeightCol $ T.transpose $
        L.map Alignment.rowSeq aln

-- the number of _different_ residues in the column, and s is the number of
-- sequences which have residue R in that column. For example, for a column
-- consisting of G, G, and C the value of n is 2 (two different residues, G and
-- C). The value of n is 2 for G and 1 for C, as there are two Gs and one C in
-- the column. The weight of G at that column is therefore 1 / (2 * 2) = 1/4,
-- and that for C is 1 / (2 * 1) = 1/2.

resWeight :: Int -> Int -> Double
resWeight num_res num_seq_with_res = 1.0 / (n * s)
    where   s = fromIntegral num_seq_with_res
            n = fromIntegral num_res 

-- Constructs the (residue -> weight) map for a given column. The keys are the
-- different residues in the column, e.g. for a column consisting of G, G, C the
-- map would have a key for G (with value 1/4) and a key for C (with value 1/2).

colToWeightsMap :: Column -> M.Map Char Double
colToWeightsMap col = M.map (resWeight $ M.size res_count_map) res_count_map
    where res_count_map = M.fromListWith (+) $ zip (T.unpack col) $ repeat 1

-- Maps all residues in a column to their weight, e.g. ['G', 'G', 'C'] -> [1/4,
-- 1/4, 1/2].

resColToWeightCol :: Column -> [Double]
resColToWeightCol col = L.map ((M.!) weightsMap) (T.unpack col)
    where weightsMap = colToWeightsMap col
