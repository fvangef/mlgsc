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
import CladeModelAux

data PepModel = PepModel {
                    matrix :: V.Vector (M.Map Residue Int)
                    , smallScore :: Int
                    , modelLength :: Int
                } deriving (Show, Eq)

--Remember: sequence positions start -- at 1, but vector indexes (sensibly)
-- start at 0.
scoreOf mod res pos = M.findWithDefault (smallScore mod) res posMap
    where posMap = (matrix mod) V.! (pos - 1)

-- TODO: try to rewrite this in applicative style
scoreSeq mod seq = sum $ map (\(res,pos) -> scoreOf mod res pos) seqWithPos
    where seqWithPos = zip (T.unpack seq) [1..] -- eg [('A',1), ...], etc.

-- just return the length of the 'A' vector (they're all the same length
-- anyway)
modLength = modelLength

absentResScore = smallScore

instance Binary PepModel where
    put mod = do
        put $ matrix mod
        put $ smallScore mod
        put $ modelLength mod

    get = do
        mat <- get :: Get (V.Vector (M.Map Residue Int))
        smallScore <- get :: Get Int
        modelLength <- get :: Get Int
        return $ PepModel mat smallScore modelLength

-- Builds a PepMOdel from a (weighted) Alignment
-- G, T are ignored, but gaps (-) are modelled.

alnToPepModel :: SmallProb -> ScaleFactor -> Alignment -> PepModel
alnToPepModel smallProb scale aln = PepModel scoreMapVector smallScore length
    where   scoreMapVector = V.fromList scoreMapList
            scoreMapList = fmap (freqMapToScoreMap scale
                                . countsMapToRelFreqMap wsize
                                . weightedColToCountsMap weights)
                                $ T.transpose sequences
            smallScore = round (scale * (logBase 10 smallProb))
            wsize = fromIntegral $ sum weights
            sequences = map rowSeq aln
            weights = map rowWeight aln
            length = T.length $ rowSeq $ head aln 
