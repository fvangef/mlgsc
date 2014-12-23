-- A model for a conserved region of protein (peptidic) of length L. 

{- This model uses a boxed vector of residue -> score maps. There is one map per
 - position in the modeled region (corresponding to columns in the multiple
 - alignments the model is made from).  -
 -}

-- TODO: once it works, restrict exports to the minimal needed set.
module PepModel (PepModel, matrix, alnToPepModel) where

-- module  PepModel where -- 

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Binary
import Data.Vector.Binary


import MlgscTypes
import Alignment
import CladeModel

data PepModel = PepModel {
                    matrix :: V.Vector (M.Map Char Int)
                    , smallScore :: Int
                } deriving (Show, Eq)

instance CladeModel PepModel where
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

    scoreSeq nm seq
        | (U.null $ V.head $ matrix nm)   = minBound :: Int
        | otherwise = sum $ map (\(c,i) -> scoreOf nm c i) seqWithPos
        where seqWithPos = zip (T.unpack seq) [1..] -- eg [('A',1), ...], etc.

    -- just return the length of the 'A' vector (they're all the same length
    -- anyway)
    modLength nm = U.length vA
        where   vA = mat V.! 0
                mat = matrix nm

    absentResScore = smallScore

instance Binary PepModel where
    put nm = do
        put $ matrix nm
        put $ smallScore nm

    get = do
        mat <- get :: Get (V.Vector (U.Vector Int))
        smallScore <- get :: Get Int
        return $ PepModel mat smallScore

-- Builds a PepMOdel from a (weighted) Alignment
-- G, T are ignored, but gaps (-) are modelled.

alnToPepModel :: SmallProb -> ScaleFactor -> Alignment -> PepModel
alnToPepModel smallProb scale aln = PepModel scoreMapList smallScore
    where   scoreMapList = fmap (freqMapToScoreMap scale
                                . countsMapToRelFreqMap wsize
                                . weightedColToCountsMap weights)
                                $ T.transpose sequences
            smallScore = round (scale * (logBase 10 smallProb))
            wsize = fromIntegral $ sum weights
            sequences = map rowSeq aln
            weights = map rowWeight aln

