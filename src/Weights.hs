module Weights where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Array.Unboxed (UArray, (!))
import Control.Applicative
import Data.Char

import MlgscTypes
import Alignment

type HenikoffMatrix = UArray (Int, Int) Double

henikoffWeightAln :: Alignment -> Alignment
henikoffWeightAln aln = getZipList $
    updateWeight <$> ZipList aln <*> ZipList normWeights
    where   normWeights = normalize $ alnRawWeights aln 
            updateWeight row weight = row {rowWeight = weight} 

normalize :: [Double] -> [Int]
normalize rawWts = L.map (round . (/ (minimum rawWts))) rawWts

-- The raw sequence weights are the sum of the residue weights over the
-- sequence. This fuction returns the raw weights of each sequence in an
-- alignment.

alnRawWeights :: Alignment -> [Double]
alnRawWeights aln = L.map (henikoffWeightRow hMat) aln
    where hMat = henikoffMatrix aln

henikoffWeightRow :: HenikoffMatrix -> AlnRow -> Double  
henikoffWeightRow hMat (AlnRow _ seq _) = henikoffWeightSeq hMat seq

henikoffWeightSeq :: HenikoffMatrix -> Sequence -> Double
henikoffWeightSeq hMat seq = henikoffWeightSeq' hMat seq 0

henikoffWeightSeq' :: HenikoffMatrix -> Sequence -> Int -> Double
henikoffWeightSeq' hMat seq pos
    | T.empty == seq = 0
    | otherwise     = headWeight + restWeight
    where   headWeight = hMat ! (charIndex $ T.head seq, pos)
            restWeight = henikoffWeightSeq' hMat (T.tail seq) (pos + 1)
    
henikoffMatrix :: Alignment -> HenikoffMatrix
henikoffMatrix = undefined -- ST monad here

charIndex :: Char -> Int
charIndex c = case toUpper c of
    'A' -> 0
    'C' -> 1
    'G' -> 2
    'T' -> 3
    otherwise -> 4
