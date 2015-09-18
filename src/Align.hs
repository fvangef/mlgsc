module Align (msalign,
    ScoringScheme(..), defScScheme, scoringSchemeMap) where

import Data.Array
import qualified Data.Array.Base as B
import qualified Data.Array.Unboxed as UBA
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U

import MlgscTypes
import PWMModel (PWMModel, scoreOf, modLength)


data Direction = None | Diag | Down | Righ -- 'Right' is defined by Either
    deriving (Show, Eq)
    
data DPCell = DPCell {
        val ::Int,
        dir :: Direction
    } deriving (Show, Eq)

instance Ord DPCell where
     (DPCell int1 _) <= (DPCell int2 _) = int1 <= int2

dirSym :: DPCell -> Char
dirSym c = case dir c of
    None -> '.'
    Diag -> '\\'    -- how the f*ck does one get a literal '\'?
    Down -> '|'
    Righ -> '-'

type VSequence = U.Vector Char

type DPMatrix = Array (Int,Int) DPCell

type MADPMat = UBA.UArray (Int,Int) Int

type GapPenalty = Int

-- A ScoringScheme has two components: a gap opening penality (gap penalties are
-- linear, so there is no distinct gap extension penalty); and a -(model score,
-- -> alignment score) map. A residue's positional score according to a
-- PWMModel is translated into an alignment score using Map.lookupGE. In the
-- 'defScheme' below, for instance, a model score of -4000 yields a score of -1,
-- between -3999 (included) and -800, the score is 1, etc, until the score is 3
-- from -199 to 0. Note: these particular values have been tested (see
-- TestAlign.hs) and found to work, but better ones may exist; in particular the
-- best vales clearly depend on the model's score for an absent residue
-- (smallProb). This is the purpose of the scoringSchemeMap function

data ScoringScheme = ScoringScheme {
                            gapOP :: Int
                            , scThresholds :: M.Map Int Int
                            }

defScScheme = ScoringScheme (-2) (M.fromList [(-4000,-1),(-800,1),(-400,2),(0,3)])

-- With a smallScore of -4000, yields same threshold map as in defScScheme.

scoringSchemeMap :: Int -> M.Map Int Int
scoringSchemeMap smallScore = M.fromList $ zip thresholds [-1, 1, 2, 3]
    where   thresholds = map (round . (sscFrac /)) [1.0, 5.0, 10.0] ++ [0]
            sscFrac = fromIntegral smallScore

-- First step of sequence-to-prob-matrix alignment.  Fills a dynamic programming
-- matrix (DPMatrix), with a scoring scheme, an ISLProbMatrix and a Sequence as
-- inputs. The result can be used for backtracking. Like Needleman-Wunsch, but
-- with adaptations to matching sequences to a matrix, i.e. the first column is
-- 0 instead of (i * insertion penalty), etc. -- think of the ISLProbMatrix as
-- horizontal, and of the sequence as vertical. Uses an immutable array filled
-- with a list.

-- TODO: the gap opening penalty and scoring map for scoreModVseq should be
-- parameters, not hard-coded.

msdpmatIA :: ScoringScheme -> PWMModel -> VSequence -> DPMatrix
msdpmatIA scsc hmod vseq  = dpmat
    where   dpmat = array ((0,0), (seq_len, mat_len)) [((i,j), cell i j) | i <- [0..seq_len], j <- [0..mat_len]]
            seq_len = U.length vseq
            mat_len = modLength hmod
            cell i j
                | i == 0 && j == 0  = DPCell 0 None
                | i == 0        = DPCell (j * penalty) Righ
                | j == 0        = DPCell 0 Down
                | otherwise = maximum [
                        DPCell match Diag,
                        DPCell hGap Down,
                        DPCell vGap Righ ]
                where
                    match = val (dpmat!(i-1,j-1)) + match_score
                    hGap  = val (dpmat!(i-1,  j)) + penalty
                    vGap  = val (dpmat!(i  ,j-1)) + penalty
                    penalty = gapOP scsc
                    match_score = scoreModVseq (scThresholds scsc) hmod vseq i j 

-- A version with a mutable array.

-- Int constants for backtracking in the same array as NW scores, but different
-- cells. I have to use an ADT to be able to case ... of on it, but I need a
-- mapping to (and from) Int since I use unboxed arrays and these do not accept
-- any ADT.

left = 1
up = 2
both = 3

data Backtrack = GoLeft | GoUp | Both

bt2Int :: Backtrack -> Int
bt2Int GoLeft = 1
bt2Int GoUp   = 2
bt2Int Both = 3

int2bt :: Int -> Backtrack
int2bt 1    = GoLeft
int2bt 2    = GoUp
int2bt 3    = Both


twoDto1D :: Int -> (Int, Int) -> Int
twoDto1D width (i,j) = i * width + j

msdpmat :: ScoringScheme -> PWMModel -> VSequence -> MADPMat
msdpmat scsc hmod vseq = runSTUArray $ do
                let seq_len = U.length vseq
                let mat_len = modLength hmod
                let penalty = gapOP scsc
                let array_width = 2 * (mat_len + 1)
                -- dpmat has the scores and backtracking info in two contiguous
                -- arrays (STUArray does not support arrays of tuples). The
                -- left-hand array (0 <= j <= mat_len) is for the scores, and
                -- the right-hand array (mat_len+1 <= j <= 2 * mat_len  + 1) is
                -- for backtracking symbols, coded as integers (see above).
                dpmat <- newArray ((0,0), (seq_len, (2 * mat_len) + 1)) 0 :: ST s (STUArray s (Int, Int) Int)
                -- initialize leftmost columns (j = 0 and j = mat_len+1)
                forM_ [1..seq_len] $ \i -> do
                    B.unsafeWrite dpmat (twoDto1D array_width (i, 0)) 0
                    B.unsafeWrite dpmat (twoDto1D array_width (i, mat_len + 1)) up
                -- initialize top row of left array
                forM_ [0..mat_len] $ \j ->
                    B.unsafeWrite dpmat (twoDto1D array_width (0, j)) (j * penalty)
                -- and of right array
                forM_ [mat_len+1 .. ((2*mat_len)+1)] $ \j ->
                    B.unsafeWrite dpmat (twoDto1D array_width (0, j)) left
                -- now fill rest of matrices
                forM_ [1..seq_len] $ \i ->
                    forM_ [1..mat_len] $ \j -> do
                        let match_score = scoreModVseq (scThresholds scsc) hmod vseq i j               
                        match_cell_val <- B.unsafeRead dpmat (twoDto1D array_width (i-1,j-1))
                        hGap_cell_val  <- B.unsafeRead dpmat (twoDto1D array_width (i-1, j))
                        vGap_cell_val  <- B.unsafeRead dpmat (twoDto1D array_width (i, j-1)) 
                        let match_sc = match_cell_val + match_score
                        let hGap_sc  = hGap_cell_val  + penalty
                        let vGap_sc  = vGap_cell_val  + penalty
                        let best     =  maximum [match_sc, hGap_sc, vGap_sc]
                        B.unsafeWrite dpmat (twoDto1D array_width (i,j)) best
                        if best == hGap_sc
                            then
                                B.unsafeWrite dpmat (twoDto1D array_width (i,j+mat_len+1)) up
                            else if best == vGap_sc
                                then
                                    B.unsafeWrite dpmat (twoDto1D array_width (i,j+mat_len+1)) left
                                else 
                                    B.unsafeWrite dpmat (twoDto1D array_width (i,j+mat_len+1)) both
                return dpmat 


-- A score function for seq-vs-mat (ISLProbMatrix)

-- NOTE: a residue that is rare but not unknown should not have a penalty
-- stronger than a gap penalty, or strange things will happen.
-- It might be better to take gap penalties into account when designing these
-- score functions.

-- I prefix 'mod' with 'h' for 'horizontal', and 'seq' with 'v', which stands
-- for both 'vertical' and 'vector'. This reminds us that the DP is asymmetric,
-- with the model horizontally (on top) and the sequence vertically (on the left
-- side), when visualizing the process. This also reflects the fact that the
-- sequence has O(1) indexing (and so should the model).

{-
seqISLMatScore :: (PWMModel mod) => mod -> VSequence -> Position -> Position -> Int
seqISLMatScore hmod vseq i j
    | prob == -4000 = -1        -- not found at that position
    | prob == 0         = 3
    | prob > -300       = 2     -- ~ 1000 * log10(0.5)
    | prob > -600       = 1     -- ~ 1000 * log10(0.25)
    | otherwise = 0
    where   prob    = scoreOf hmod res j
            res     = vseq ! (i-1)
-}

-- A new version of the above, which simply uses a Map. TODO: the map values
-- should at least depend on the PWMModel, e.g. the model's smallScore should
-- be the lowest in the map (with zero the highest).

scoreModVseq :: M.Map Int Int -> PWMModel -> VSequence -> Position -> Position -> Int
scoreModVseq scThr hmod vseq i j =
    case M.lookupGE modScore scThr of
        Nothing -> 0    -- shouldn't happen
        Just (k, v) -> v
    where   modScore    = scoreOf hmod res j
            res         = vseq U.! (i-1)
                        

{-
topCell :: Array (Int,Int) DPCell -> (Int,Int)
topCell mat = fst $ maximumBy cellCmp (assocs mat)
        where cellCmp (ix1, (DPCell val1 _)) (ix2, (DPCell val2 _))
                | val1 > val2   = GT
                | otherwise     = LT
-}

msalignIA :: ScoringScheme -> PWMModel -> Sequence -> Sequence
msalignIA scsc mat seq = T.pack $ nwMatBacktrackIA (msdpmatIA scsc mat vseq) vseq
    where vseq = U.fromList $ T.unpack seq 

-- mutable-array - based version

msalign :: ScoringScheme -> PWMModel -> Sequence -> Sequence
msalign scsc mat seq = T.pack $ nwMatBacktrack (msdpmat scsc mat vseq) vseq
    where vseq = U.fromList $ T.unpack seq 
{-
nwMatPath :: RawProbMatrix -> String -> String
nwMatPath hm vs = toPathMatrix (fmap dirSym (nw seqMatScore (-1) hra vra))
        where   hra = Mat hm (length hm)
                vra = Seq vs (length vs)
-}

-- TODO: according to DP matrix graph, this function may actually be
-- superfluous: if we just start from the lower right corner, we'll end up with
-- the same aligned sequence and spare ourselves a O(n^2) search for the best
-- cell.

topCellInLastCol :: Array (Int,Int) DPCell -> (Int,Int)
topCellInLastCol mat = fst $ maximumBy cellCmp $
    map (\ix -> (ix, mat ! ix)) lastCol 
    where   (lv,lh) = snd $ bounds mat
            lastCol = [(i,lh) | i <- [0..lv]] 
            cellCmp (ix1, DPCell val1 _) (ix2, DPCell val2 _)
                | val1 > val2   = GT
                | otherwise = LT

-- Traces the path back from corner to corner (NW matrix), but the horizontal
-- "sequence" is a matrix (the vertical sequence is still a sequence, though);
-- yields only the aligned sequence (not the matrix).

nwMatBacktrackIA :: DPMatrix -> VSequence -> String
nwMatBacktrackIA mat v = reverse va
    where va = nwMatBacktrackIA' mat topLastCol v
          topLastCol = topCellInLastCol mat

nwMatBacktrackIA' :: DPMatrix -> (Int,Int) -> VSequence -> String
nwMatBacktrackIA' _ (0,0) _ = ""
-- These two cases may actually be covered by the general case
nwMatBacktrackIA' mat (0,j) vseq = '-':vRest
    where vRest = nwMatBacktrackIA' mat (0,j-1) vseq
nwMatBacktrackIA' mat (i,0) vseq = ""
    where vRest = nwMatBacktrackIA' mat (i-1,0) vseq
nwMatBacktrackIA' mat (i,j) vseq = 
    case dir (mat!(i,j)) of
        Diag -> (vseq U.! (i-1)):vRest
            where vRest = nwMatBacktrackIA' mat (i-1,j-1) vseq
        Righ -> '-':vRest
            where vRest = nwMatBacktrackIA' mat (i, j-1) vseq
        Down -> vRest
            where vRest = nwMatBacktrackIA' mat (i-1, j) vseq

-- Same, but for the mutable array version (though the array here is immutable,
-- it is derived from a mutable array, see msdpmatMA).

nwMatBacktrack :: MADPMat -> VSequence -> String
nwMatBacktrack mat vseq = reverse trace
    where   trace = nwMatBacktrack' mat bottomright vseq
            bottomright = snd $ UBA.bounds mat

nwMatBacktrack' :: MADPMat -> (Int,Int) -> VSequence -> String
nwMatBacktrack' mat (i,j) vseq
    | (i == 0) && (j == k)  = "" -- isn't this a special case of #3?
    | (i == 0)              = '-':nwMatBacktrack' mat (0,j-1) vseq
    |             (j == k)  = ""    
    | otherwise             = if (mat UBA.! (i,j)) == left
                                then '-':nwMatBacktrack' mat (i,j-1) vseq
                                else if (mat UBA.! (i,j)) == up
                                    then nwMatBacktrack' mat (i-1,j) vseq
                                    else (vseq U.! (i-1)):nwMatBacktrack' mat (i-1,j-1) vseq
                                
                                
    -- TODO: k is constant - should it not be computed before the recursion, or
    -- can the compiler figure this one out?
    where   k = mat_len + 1
            mat_len = (array_width - 1) `div` 2
            array_width = snd $ snd $ UBA.bounds mat


-- These are for debugging
-- TODO: uncomment (and adapt) when switch to ByteString works
--
-- prints out a 2D array (such as a DP matrix)
