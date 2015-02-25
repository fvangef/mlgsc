-- A model for a conserved region of protein (peptidic) of length L. 

{- This model uses a boxed vector of residue -> score maps. There is one map per
 - position in the modeled region (corresponding to columns in the multiple
 - alignments the model is made from).  -
 -}

-- TODO: once it works, restrict exports to the minimal needed set.
module SimplePepModel where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Binary
import Data.Vector.Binary
import Data.Text.Binary


import MlgscTypes
import Alignment
import CladeModelAux

data SimplePepModel = SimplePepModel {
                    cladeName       :: CladeName
                    , matrix        :: V.Vector (M.Map Residue Int)
                    , smallScore    :: Int
                    , modelLength   :: Int
                } deriving (Show, Eq)

--Remember: sequence positions start -- at 1, but vector indexes (sensibly)
-- start at 0.
simplePepScoreOf :: SimplePepModel -> Residue -> Position -> Int
simplePepScoreOf (SimplePepModel _ _ smallScore 0) _ _ = smallScore -- empty models
simplePepScoreOf mod res pos = M.findWithDefault (smallScore mod) res posMap
    where posMap = (matrix mod) V.! (pos - 1)

-- TODO: try to rewrite this in applicative style
simplePepScoreSeq :: SimplePepModel -> Sequence -> Int
simplePepScoreSeq (SimplePepModel _ _ smallScore 0) seq = smallScore * T.length seq
simplePepScoreSeq mod seq = sum $ map (\(res,pos) -> simplePepScoreOf mod res pos) seqWithPos
    where seqWithPos = zip (T.unpack seq) [1..] -- eg [('A',1), ...], etc.

simplePepModLength = modelLength

simplePepAbsentResScore = smallScore

instance Binary SimplePepModel where
    put mod = do
        put $ cladeName mod
        put $ matrix mod
        put $ smallScore mod
        put $ modelLength mod

    get = do
        cladeName <- get :: Get CladeName
        mat <- get :: Get (V.Vector (M.Map Residue Int))
        smallScore <- get :: Get Int
        modelLength <- get :: Get Int
        return $ SimplePepModel cladeName mat smallScore modelLength

-- Builds a PepMOdel from a (weighted) Alignment
-- G, T are ignored, but gaps (-) are modelled.

alnToPepModel :: SmallProb -> ScaleFactor -> CladeName -> Alignment
    -> SimplePepModel
alnToPepModel smallProb scale name [] = SimplePepModel name V.empty smallScore 0
    where   smallScore = round (scale * (logBase 10 smallProb))
alnToPepModel smallProb scale name aln = SimplePepModel name scoreMapVector smallScore length
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
