-- A model for a conserved region of nucleic acid of length L. 

{- This model uses a 2D array (more precisely, a Vector of Vectors). The outer
 - vector must be boxed, as it contains other vectors; the inner vectors can be
 - unboxed, since they contain numbers (i.e. positional scores of nucleotides).
 -
 - Since unboxed vectors are more efficient than boxed ones, (see e.g.
 - http://www.haskell.org/haskellwiki/Numeric_Haskell:_A_Vector_Tutorial#Boxed_Arrays:_Data.Vector),
 - I will have a short outer vector (one per nucleotide) of long inner
 - vectors (indexed by position) of integers. 
 -
 - E.g. (where '<>' denote vectors):
 -  <
 -   <0,3,5,2>, -- A
 -   <3,6,6,3>, -- C
 -   <1,3,5,2>, -- G
 -   <2,6,8,1>  -- T
 -  >
 -}

module NucModel (NucModel, alnToNucModel, probOf, colToMap) where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Map.Strict as M
import qualified Data.Text as T


import MlgscTypes
import CladeModel

data NucModel = NucModel {
                    matrix :: V.Vector (U.Vector Int)
                }

-- Builds a NucMOdel from a list of aligned sequences. Residues other than A, C,
-- G, T are ignored, but gaps (-) are modelled.

-- TODO: transpose alignment into columns, map colToMap to each col, then make 5
-- lists of 
alnToNucModel :: Double -> Int -> Alignment -> NucModel
alnToNucModel smallProb scale aln = undefined
    where   scoreMapList = fmap (freqMapToScoreMap scale
                                . countsMapToRelFreqMap size
                                . colToCountsMap ) $ T.transpose aln
            size = length aln   -- number of sequences

probOf :: NucModel -> Residue -> Position -> Int
probOf mod res pos = undefined

-- TODO: convert to relative frequencies, then log thereof, then scale and
-- round. Can maybe be done in another function

-- NOTE: functions below here should not be exported.

-- Takes a Column (the residues of each sequence at a given position) and
-- returns a map of absolute frequency (i.e., counts) of each residue.

colToCountsMap :: Column -> M.Map Residue Int
colToCountsMap col = M.fromListWith (+) [(res, 1) | res <- (T.unpack col)] 

-- Scales the absolute frequency into a relative frequency, using the "height"
-- of the alignment (= number of sequences).

countsMapToRelFreqMap :: Int -> M.Map Residue Int -> M.Map Residue Double
countsMapToRelFreqMap size = fmap (\n -> (fromIntegral n) / size)

-- Transforms a Residue -> Relative frequency map into a Residue -> Score map,
-- where the score is a rounded, scaled logarithm of the relative frequency.

freqMapToScoreMap :: Int -> M.Map Residue Double -> M.Map Residue Int
freqMapToScoreMap scale = fmap (round . (* scale) . logBase 10)

