module Weights where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT

import Data.Array.Unboxed
import Data.Array.ST
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Char

import MlgscTypes
import Alignment

type HenikoffMatrix = UArray (Int, Int) Double
type CountMatrix = UArray (Int, Int) Int

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
    | ST.empty == seq    = 0
    | otherwise         = headWeight + restWeight
    where   headWeight = hMat ! (charIndex $ ST.head seq, pos)
            restWeight = henikoffWeightSeq' hMat (ST.tail seq) (pos + 1)

henikoffMatrix :: Alignment -> HenikoffMatrix
henikoffMatrix aln = countMat2HenikoffMat countMat 
    where countMat = lazyTextFreqArray $ map (LT.fromStrict . rowSeq) aln

-- Produces a Henikoff weight matrix from a count matrix.

countMat2HenikoffMat :: CountMatrix -> HenikoffMatrix
countMat2HenikoffMat mat = array (bounds mat) $ map (divBy tCounts) $ assocs mat
    where   tCounts = typeCounts mat
            divBy tCounts ((i,j), c) = ((i,j), hWeight c (tCounts ! j))

hWeight :: Int -> Int -> Double
hWeight 0 _ = 0
hWeight c tc = 1.0 / fromIntegral (c * tc)

-- Returns the number of different residues (see typeCount) at each position of
-- a frequency (count) matrix, as a 1D array.
typeCounts :: UArray (Int, Int) Int -> UArray Int Int
typeCounts mat = listArray (0,len) $ map (typeCountAt mat) [0..len]
    where len = snd $ snd $ bounds mat

-- Returns the number of different residues (see typeCount) in column j of
-- count matrix 'mat'.
typeCountAt :: (IArray a1 a, Ix t1, Ix i, Ord a, Num a, Num i) =>
       a1 (i, t1) a -> t1 -> Int
typeCountAt mat j = typeCount $ col mat j

-- Returns the number of different residue types at a column, e.g. if a column
-- has A, C, C, A, C, T, there are 3 different types: A, C, T.
typeCount :: (IArray a1 a, Ix i, Ord a, Num a) => a1 i a -> Int
typeCount col = length $ filter (> 0) $ elems col

-- Returns the j-th column of a 2D array, as a 1D array.
col :: (IArray a e, Ix t1, Ix t, Num t) => a (t, t1) e -> t1 -> a t e
col ary j = ixmap (0,4) (\i -> (i,j)) ary

lazyTextFreqArray :: [LT.Text] -> CountMatrix
lazyTextFreqArray lines = runSTUArray $ do
    let lineLen = fromIntegral $ LT.length $ head lines
    array <- newArray ((0,0), (4, lineLen)) 0 :: ST s (STUArray s (Int, Int) Int)
    forM_ lines $ \line -> do
        countChar array line 0 
    return array

countChar array line index
    | line == LT.empty  = return ()
    | otherwise = do
        let c = LT.head line
        let ci = charIndex c
        count <- readArray array (ci, index)
        writeArray array (ci, index) (count + 1)
        countChar array (LT.tail line) (index + 1)

charIndex :: Char -> Int
charIndex c = case toUpper c of
    'A' -> 0
    'C' -> 1
    'G' -> 2
    'T' -> 3
    otherwise -> 4

counts = listArray ((0,0), (4,7)) [
    0, 1, 0, 1, 0, 1, 1, 2,    -- A
    1, 1, 0, 0, 1, 0, 0, 1,    -- C
    2, 1, 3, 0, 0, 1, 2, 0,    -- G
    0, 0, 0, 2, 2, 1, 0, 0,    -- T
    0, 0, 0, 0, 0, 0, 0, 0     -- -
    ] :: CountMatrix
