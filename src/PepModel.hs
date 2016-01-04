-- A model for a conserved region of protein (peptidic) of length L. 

{- This model uses a boxed vector of residue -> score maps. There is one map per
 - position in the modeled region (corresponding to columns in the multiple
 - alignments the model is made from).  -
 -}

-- TODO: once it works, restrict exports to the minimal needed set.
module PepModel (
    PepModel,
    pepScoreOf,
    pepScoreSeq,
    pepModLength,
    pepAbsentResScore,
    pepCladeName,
    pepPrettyPrint,
    pepTablePrint,
    pepResidues,
    pepScaleFactor
    ) where

import Data.Vector ((!))
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Set as S
import Data.List (intersperse, intercalate, sortBy)
import Data.Ord (comparing)
import Data.Binary
import Data.Vector.Binary
import Data.Text.Binary
import Text.Printf


import MlgscTypes
import Alignment
import PWMModelAux

data PepModel = PepModel {
                    clade           :: CladeName
                    , matrix        :: V.Vector (M.Map Residue Int)
                    , smallScore    :: Int
                    , modelLength   :: Int
                    , scaleFactor   :: ScaleFactor
                } deriving (Show, Eq)

--Remember: sequence positions start -- at 1, but vector indexes (sensibly)
-- start at 0.
pepScoreOf :: PepModel -> Residue -> Position -> Int
pepScoreOf (PepModel _ _ smallScore 0 _) _ _ = smallScore -- empty models
pepScoreOf _ '.' _ = 0 -- masked residues have a score of 0
pepScoreOf mod res pos = M.findWithDefault (smallScore mod) res posMap
    where posMap = (matrix mod) V.! (pos - 1)

-- TODO: try to rewrite this in applicative style
pepScoreSeq :: PepModel -> Sequence -> Int
pepScoreSeq (PepModel _ _ smallScore 0 _) seq = smallScore * T.length seq
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
                            map matScore $ S.toList amino_acids
                        matScore aa = show $ case M.lookup aa pMap of
                            Just score -> score
                            Nothing -> smallScore mod
                        pMap = (matrix mod) ! (n-1)

pepResidues mod = amino_acids

pepScaleFactor = scaleFactor

instance Binary PepModel where
    put mod = do
        put $ clade mod
        put $ matrix mod
        put $ smallScore mod
        put $ modelLength mod
        put $ scaleFactor mod

    get = do
        cladeName <- get :: Get CladeName
        mat <- get :: Get (V.Vector (M.Map Residue Int))
        smallScore <- get :: Get Int
        modelLength <- get :: Get Int
        scale <- get :: Get ScaleFactor
        return $ PepModel cladeName mat smallScore modelLength scale

-- Builds a PepMOdel from a (weighted) Alignment
-- G, T are ignored, but gaps (-) are modelled.

alnToPepModel :: SmallProb -> ScaleFactor -> CladeName -> Alignment
    -> PepModel
alnToPepModel smallProb scale name [] =
    PepModel name V.empty smallScore 0 scale
    where   smallScore = round (scale * (logBase 10 smallProb))
alnToPepModel smallProb scale name aln = PepModel name scoreMapVector smallScore length scale
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
