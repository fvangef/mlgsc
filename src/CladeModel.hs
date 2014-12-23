-- A type class for clade models, i.e., a conserved sequence regions that should
-- be able to recognize a clade (be it a OTU, or a species, or whatever "rank").

module CladeModel where

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import MlgscTypes

class CladeModel a where
    -- The score of a residue at a position. This is the log observed frequency
    -- of said residue at that position, scaled and rounded.
    scoreOf ::  a -> Residue -> Position -> Int
    -- The score of a sequence according to the model
    scoreSeq :: a -> Sequence -> Int
    -- The length (in aligned positions) of the model
    modLength :: a -> Int
    -- Score of a residue not found at some position
    absentResScore :: a -> Int

-- some functions useful for building clade models from alignents

-- Converts a residue probability (estimated by its relative frequency) to a
-- rounded, scaled, logarithm.

probToScore :: Double -> Double -> Int
probToScore scale prob = round (scale * (logBase 10 prob))

-- Takes a Column (a vertical slice through an alignment, IOW the residues of
-- each sequence at a given single position) and returns a map of absolute
-- frequency (i.e., counts) of each residue.

colToCountsMap :: Column -> M.Map Residue Int
colToCountsMap col = M.fromListWith (+) [(res, 1) | res <- (T.unpack col)] 

-- Computes a residue -> counts map for a column, weighted by sequence weight,
-- e.g. if a sequence has 'A' at that position, and the sequence's weight is 2,
-- then for that sequence 'A' is added twice to the total count of 'A'.

weightedColToCountsMap :: [Int] -> Column -> M.Map Residue Int
weightedColToCountsMap weights col =
    M.fromListWith (+) $ zip (T.unpack col) weights

-- Scales the absolute frequency into a relative frequency, by dividing the
-- (possibly weighted) counts by the (posibly weighted) number of sequences

countsMapToRelFreqMap :: Double -> M.Map Residue Int -> M.Map Residue Double
countsMapToRelFreqMap size = fmap (\n -> (fromIntegral n) / size)

-- Transforms a Residue -> Relative frequency map into a Residue -> Score map,
-- where the score is a rounded, scaled logarithm of the relative frequency.

freqMapToScoreMap :: Double -> M.Map Residue Double -> M.Map Residue Int
freqMapToScoreMap scale = fmap $ probToScore scale
