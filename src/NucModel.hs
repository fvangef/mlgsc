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

-- TODO: once it works, restrict exports to this:
module NucModel (NucModel, alnToNucModel) where

-- module  NucModel where -- 

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Binary
import Data.Vector.Binary


import MlgscTypes
import CladeModel

data NucModel = NucModel {
                    matrix :: V.Vector (U.Vector Int)
                    , smallScore :: Int
                } deriving (Show, Eq)

instance CladeModel NucModel where
    --Remember: sequence positions start -- at 1, but vector indexes (sensibly)
    -- start at 0.
    scoreOf nm res pos
        | res == 'A'    = (mat V.! 0) U.! (pos - 1)
        | res == 'C'    = (mat V.! 1) U.! (pos - 1)
        | res == 'G'    = (mat V.! 2) U.! (pos - 1)
        | res == 'T'    = (mat V.! 3) U.! (pos - 1)
        | res == '-'    = (mat V.! 4) U.! (pos - 1)
        | otherwise     = smallScore nm
        where mat = matrix nm

    scoreSeq nm seq = sum $ map (\(c,i) -> scoreOf nm c i) seqWithPos
        where seqWithPos = zip (T.unpack seq) [1..] -- eg [('A',1), ...], etc.

    -- just return the length of the 'A' vector (they're all the same length
    -- anyway)
    modLength nm = U.length vA
        where   vA = mat V.! 0
                mat = matrix nm

    absentResScore = smallScore

instance Binary NucModel where
    put nm = do
        put $ matrix nm
        put $ smallScore nm

    get = do
        mat <- get :: Get (V.Vector (U.Vector Int))
        smallScore <- get :: Get Int
        return $ NucModel mat smallScore

-- Builds a NucMOdel from a list of aligned sequences. Residues other than A, C,
-- G, T are ignored, but gaps (-) are modelled.

-- TODO: transpose alignment into columns, map colToMap to each col, then make 5
-- lists of 
alnToNucModel :: Double -> Double -> Alignment -> NucModel
alnToNucModel smallProb scale aln = 
    NucModel (scoreMapListToVectors smallScore scoreMapList) smallScore
    where   scoreMapList = fmap (freqMapToScoreMap scale
                                . countsMapToRelFreqMap size
                                . colToCountsMap ) $ T.transpose aln
            size = fromIntegral $ length aln   -- number of sequences
            smallScore = round (scale * (logBase 10 smallProb))


-- NOTE: functions below here should not be exported.

-- Converts a residue probability (estimated by its relative frequency) to a
-- rounded, scaled, logarithm.

probToScore :: Double -> Double -> Int
probToScore scale prob = round (scale * (logBase 10 prob))

-- Takes a Column (a vertical slice through an alignment, IOW the residues of
-- each sequence at a given single position) and returns a map of absolute
-- frequency (i.e., counts) of each residue.

colToCountsMap :: Column -> M.Map Residue Int
colToCountsMap col = M.fromListWith (+) [(res, 1) | res <- (T.unpack col)] 

-- Scales the absolute frequency into a relative frequency, using the "height"
-- of the alignment (= number of sequences).

countsMapToRelFreqMap :: Double -> M.Map Residue Int -> M.Map Residue Double
countsMapToRelFreqMap size = fmap (\n -> (fromIntegral n) / size)

-- Transforms a Residue -> Relative frequency map into a Residue -> Score map,
-- where the score is a rounded, scaled logarithm of the relative frequency.

freqMapToScoreMap :: Double -> M.Map Residue Double -> M.Map Residue Int
freqMapToScoreMap scale = fmap $ probToScore scale

-- TODO: uncomment scoreMapListToVectors :: Int -> [M.Map Residue Int] -> undefined
scoreMapListToVectors smallScore sml = V.fromList [vA, vC, vG, vT, vD] 
    where   vA = U.fromList $ map (M.findWithDefault smallScore 'A') sml
            vC = U.fromList $ map (M.findWithDefault smallScore 'C') sml
            vG = U.fromList $ map (M.findWithDefault smallScore 'G') sml
            vT = U.fromList $ map (M.findWithDefault smallScore 'T') sml
            vD = U.fromList $ map (M.findWithDefault smallScore '-') sml

-- Some data to play around with in GHCi.
-- TODO: remove this when tests ok.


aln1 = [
    T.pack "ATGC-G",
    T.pack "AACG-G",
    T.pack "AACGGG",
    T.pack "ATG--G",
    T.pack "ATAATT"
    ]

sml = fmap (freqMapToScoreMap 1000.0 . countsMapToRelFreqMap 5 . colToCountsMap) $ T.transpose aln1

nm1 = alnToNucModel 0.0001 1000 aln1