-- A model for a conserved region of protein (peptidic) of length L. 

{- This model uses a boxed vector of residue -> score maps. There is one map per
 - position in the modeled region (corresponding to columns in the multiple
 - alignments the model is made from).  -
 -}

-- TODO: once it works, restrict exports to the minimal needed set.
module PepModel where

import Data.Vector ((!))
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Set as S
import Data.List (intersperse, intercalate, sortBy, map)
import Data.Ord (comparing)
import Data.Int
import Data.Binary
import Data.Vector.Binary
import Data.Text.Binary
import Text.Printf
import Numeric.LinearAlgebra ((<>), svd)
import Numeric.LinearAlgebra.Data (Z, R, fromColumns, toColumns, fromZ, toZ,
            rows, cols, diagRect, tr)
import qualified Numeric.LinearAlgebra.Data as LA


import MlgscTypes
import Alignment
import PWMModelAux

data PepModel = PepModel {
                    clade           :: CladeName
                    , matrix        :: V.Vector (M.Map Residue Int)
                    , smallScore    :: Int
                    , modelLength   :: Int
                } deriving (Show, Eq)

--Remember: sequence positions start -- at 1, but vector indexes (sensibly)
-- start at 0.
pepScoreOf :: PepModel -> Residue -> Position -> Int
pepScoreOf (PepModel _ _ smallScore 0) _ _ = smallScore -- empty models
pepScoreOf _ '.' _ = 0 -- masked residues have a score of 0
pepScoreOf mod res pos = M.findWithDefault (smallScore mod) res posMap
    where posMap = (matrix mod) V.! (pos - 1)

-- TODO: try to rewrite this in applicative style
pepScoreSeq :: PepModel -> Sequence -> Int
pepScoreSeq (PepModel _ _ smallScore 0) seq = smallScore * T.length seq
pepScoreSeq mod seq = sum $ map (\(res,pos) -> pepScoreOf mod res pos) seqWithPos
    where seqWithPos = zip (T.unpack seq) [1..] -- eg [('A',1), ...], etc.

pepModLength = modelLength

pepAbsentResScore = smallScore

pepCladeName = clade

pepPrettyPrint :: PepModel -> String
pepPrettyPrint mod = intercalate "\n" [taxonName, modStr, "\n"]
    where   taxonName = if T.null $ clade mod
                            then "(unnamed)"
                            else T.unpack $ clade mod
            modStr = prettyPrintWM mod

prettyPrintWM :: PepModel -> String
prettyPrintWM mod = concatMap (\n -> ppMatPos n) [1 .. modelLength mod]   
    where   ppMatPos n = printf "%3d | %s\n" n posScores
                     where  posScores = intercalate ", " $
                                map ppTuple sortedScores :: String
                            ppTuple (res, score) = 
                                printf "%c: %d" res score
                            sortedScores = reverse $ sortBy (comparing snd) $
                                            M.assocs pMap 
                            pMap = (matrix mod) ! (n-1)

pepTablePrint :: PepModel -> String
pepTablePrint mod = intercalate "\n" [taxonName, modHdr, modStr, "\n"]
    where   taxonName = if T.null $ clade mod
                            then "(unnamed)"
                            else T.unpack $ clade mod
            modHdr = "pos | " ++ (intersperse ' ' $ S.toList amino_acids)
            modStr = tablePrintWM mod

tablePrintWM :: PepModel -> String
tablePrintWM mod = concatMap (\n -> ppMatPos n) [1 .. modelLength mod]   
    where   ppMatPos n = printf "%3d | %s\n" n posScores
                where   posScores = intercalate " " $
                            map matScore $ S.toAscList amino_acids
                        matScore aa = show $ case M.lookup aa pMap of
                            Just score -> score
                            Nothing -> smallScore mod
                        pMap = (matrix mod) ! (n-1)

instance Binary PepModel where
    put mod = do
        put $ clade mod
        put $ matrix mod
        put $ smallScore mod
        put $ modelLength mod

    get = do
        cladeName <- get :: Get CladeName
        mat <- get :: Get (V.Vector (M.Map Residue Int))
        smallScore <- get :: Get Int
        modelLength <- get :: Get Int
        return $ PepModel cladeName mat smallScore modelLength

-- Builds a PepMOdel from a (weighted) Alignment
-- G, T are ignored, but gaps (-) are modelled.

alnToPepModel :: SmallProb -> ScaleFactor -> CladeName -> Alignment
    -> PepModel
alnToPepModel smallProb scale name [] = PepModel name V.empty smallScore 0
    where   smallScore = round (scale * (logBase 18 smallProb))
alnToPepModel smallProb scale name aln =
    PepModel name denoisedScoreMapVector smallScore length
    where   denoisedScoreMapVector = denoise 0 scoreMapVector
            scoreMapVector = V.fromList scoreMapList
            scoreMapList = fmap (freqMapToScoreMap scale
                                . countsMapToRelFreqMap wsize
                                . weightedColToCountsMap weights)
                                $ T.transpose sequences
            smallScore = round (scale * (logBase 10 smallProb))
            wsize = fromIntegral $ sum weights
            sequences = map rowSeq aln
            weights = map rowWeight aln
            length = T.length $ rowSeq $ head aln 


-- WARNING: there are two kinds of Vector here (though possibly the same behind
-- the scences): LA.Vector and V.Vector.

-- Attempts to reduce noise in the PWM by applying singular value decomposition
-- and setting the `remove` smallest singular values to zero.

denoise :: Int -> V.Vector (M.Map Residue Int) -> V.Vector (M.Map Residue Int)
denoise remove mat = matrix2posMapVec $ reduceNoise remove $  posMapVec2Matrix mat (-4000)

reduceNoise :: Int -> LA.Matrix R -> LA.Matrix R
reduceNoise remove rMat = u <> sigma' <> tr(v)
                    where   (u, sv, v) = svd rMat
                            sigma' = diagRect 0 sv' r c
                            sv' = trimSingularValues sv remove
                            r = rows rMat
                            c = cols rMat

-- replaces the last `remove` singular values (in `sv`) by zero, in the hope of
-- reducing noise
trimSingularValues :: LA.Vector R -> Int -> LA.Vector R
trimSingularValues sv remove = LA.fromList (svs ++ zeroes)
                        where   svs = take (length svl - remove) svl
                                zeroes = take remove $ repeat 0.0
                                svl = LA.toList sv

posMapVec2Matrix :: V.Vector (M.Map Residue Int) -> Int -> LA.Matrix R
posMapVec2Matrix mat smallScore = fromZ $
    fromColumns $ map (posMap2LAVector smallScore) $ V.toList mat


posMap2LAVector :: Int -> M.Map Residue Int -> LA.Vector Z
posMap2LAVector smallScore posMap = LA.fromList $
    map (\res -> fromIntegral $ M.findWithDefault smallScore res posMap) $
        S.toAscList amino_acids

matrix2posMapVec :: LA.Matrix R -> V.Vector (M.Map Residue Int)
matrix2posMapVec = V.fromList . map laVector2PosMap . toColumns . toZ

laVector2PosMap :: LA.Vector Z -> M.Map Residue Int
laVector2PosMap col = M.fromList $ zip residues scores 
                    where   residues = S.toAscList amino_acids 
                            scores = map fromIntegral $ LA.toList col :: [Int]
