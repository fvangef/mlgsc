module Weights where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import Control.Applicative

import MlgscTypes
import Alignment

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
alnRawWeights aln = L.Map (henikoffWeightRow hMat) aln
    where hMat = henikoffMatrix aln

henikoffWeightRow :: HenikoffMatrix -> ALnRow -> Double  
henikoffWeightRow hMat (AlnRow _ seq _) = henikoffWeightSeq hMat seq

henikoffWeightSeq :: HenikoffMatrix -> Sequence -> Double
henikoffWeightSeq = undefined



