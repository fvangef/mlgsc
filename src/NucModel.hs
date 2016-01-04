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

-- TODO: once it works, restrict exports to the minimal needed set.
module NucModel (NucModel,
                    nucScoreOf,
                    nucScoreSeq,
                    nucModLength,
                    nucAbsentResScore,
                    nucCladeName,
                    nucPrettyPrint,
                    nucResidues,
                    nucScaleFactor,
                    matrix,
                    alnToNucModel) where

-- module  NucModel where -- 

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Binary
import Data.Vector.Binary
import Data.Text.Binary


import MlgscTypes
import Alignment
import PWMModelAux

data NucModel = NucModel {
                    clade           :: CladeName
                    , matrix        :: V.Vector (U.Vector Int)
                    , smallScore    :: Int
                    , scaleFactor   :: ScaleFactor
                } deriving (Show, Eq)

--Remember: sequence positions start -- at 1, but vector indexes (sensibly)
-- start at 0.
nucScoreOf _ '.' _  = 0 -- masked residues have a score of zero
nucScoreOf nm res pos
    | res == 'A'    = (mat V.! 0) U.! (pos - 1)
    | res == 'C'    = (mat V.! 1) U.! (pos - 1)
    | res == 'G'    = (mat V.! 2) U.! (pos - 1)
    | res == 'T'    = (mat V.! 3) U.! (pos - 1)
    | res == '-'    = (mat V.! 4) U.! (pos - 1)
    | otherwise     = smallScore nm
    where mat = matrix nm

nucScoreSeq nm seq
    | (U.null $ V.head $ matrix nm)   = smallScore nm * T.length seq
    | otherwise = sum $ map (\(c,i) -> nucScoreOf nm c i) seqWithPos
    where seqWithPos = zip (T.unpack seq) [1..] -- eg [('A',1), ...], etc.

-- just return the length of the 'A' vector (they're all the same length
-- anyway)
nucModLength nm = U.length vA
    where   vA = mat V.! 0
            mat = matrix nm

nucAbsentResScore = smallScore

nucCladeName = clade

nucPrettyPrint = undefined

nucResidues mod = nucleotides

nucScaleFactor = scaleFactor

instance Binary NucModel where
    put nm = do
        put $ clade nm
        put $ matrix nm
        put $ smallScore nm
        put $ scaleFactor nm

    get = do
        clade <- get :: Get CladeName
        mat <- get :: Get (V.Vector (U.Vector Int))
        smallScore <- get :: Get Int
        scaleFactor <- get :: Get ScaleFactor
        return $ NucModel clade mat smallScore scaleFactor

-- Builds a NucModel from a (weighted) Alignment
-- G, T are ignored, but gaps (-) are modelled.

alnToNucModel :: SmallProb -> ScaleFactor -> CladeName -> Alignment
    -> NucModel
alnToNucModel smallProb scale name aln = 
    NucModel name (scoreMapListToVectors smallScore scoreMapList)
                smallScore scale
    where   scoreMapList = fmap (freqMapToScoreMap scale
                                . countsMapToRelFreqMap wsize
                                . weightedColToCountsMap weights)
                                $ T.transpose sequences
            smallScore = round (scale * (logBase 10 smallProb))
            wsize = fromIntegral $ sum weights
            sequences = map rowSeq aln
            weights = map rowWeight aln


scoreMapListToVectors :: Int -> [M.Map Residue Int] -> V.Vector (U.Vector Int)
scoreMapListToVectors smallScore sml = V.fromList [vA, vC, vG, vT, vD] 
    where   vA = U.fromList $ map (M.findWithDefault smallScore 'A') sml
            vC = U.fromList $ map (M.findWithDefault smallScore 'C') sml
            vG = U.fromList $ map (M.findWithDefault smallScore 'G') sml
            vT = U.fromList $ map (M.findWithDefault smallScore 'T') sml
            vD = U.fromList $ map (M.findWithDefault smallScore '-') sml
