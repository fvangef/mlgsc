module Trim (trimSeq) where

import Learning.HMM
import Data.Random.Distribution.Categorical as C

import Data.Map ((!))
import Data.Map as M
import Data.Text (Text)
import qualified Data.Text as ST

-- An HMM for trimming leading and trailing unaligned regions from aligned
-- sequences that are much shorter than the model

data State = Pre | Aln | Post
  deriving (Show, Ord, Eq)

type Path = [State]

-- Shouldn't these be called Emission, or somethng?
data Output = Gap | Residue
  deriving (Show, Eq)

-- A simple HMM that models the alignment of a short sequence to a
-- "good" alignment (i.e., with no (or only few) unaligned blocks). This assumes
-- the short sequence has a well-aligned part (Aln state in the model),
-- possibly preceded by a leading gapped region (missing leading sequence,
-- Pre state in the model), and possibly followed by a trailing gapped region
-- (missing trailing sequence, Post state in the model).

trimHmmStates = [Pre, Aln, Post]

trimHmmOutputs = [Gap, Residue]

trimHmmInitProb = C.fromList [(0.9, Pre), (0.1, Aln), (0.0, Post)]

trimHmmTransProb :: State -> Categorical Double State
trimHmmTransProb Pre = C.fromList [(0.99, Pre), (0.01, Aln), (0.0, Post)]
trimHmmTransProb Aln = C.fromList [(0.0, Pre), (0.99, Aln), (0.01, Post)]
trimHmmTransProb Post = C.fromList [(0.0, Pre), (0.0, Aln), (1.0, Post)]

trimHmmEmProb Pre = C.fromList [(0.95, Gap), (0.05, Residue)]
trimHmmEmProb Aln = C.fromList [(0.05, Gap), (0.95, Residue)]
trimHmmEmProb Post = C.fromList [(0.95, Gap), (0.05, Residue)]

trimHmm = HMM trimHmmStates trimHmmOutputs
    trimHmmInitProb trimHmmTransProb trimHmmEmProb

-- trimSeq should be replaced by trimSeqGeneric trimHmm
trimSeq :: Text -> Text
trimSeq seq = ST.pack $ zipWith translate path seqAsStr
    where   (path, _)   = viterbi trimHmm $ alnSeqToHMMSym seqAsStr
            seqAsStr    = ST.unpack seq    

translate :: State -> Char -> Char
translate state c = case state of
                        Pre -> '.'
                        Aln -> c
                        Post -> '.'

-- A generalized version of trimSeq, works for any HMM. I don't change existing
-- names so as not to break code, but trimSeq should be the name of this
-- function.

trimSeqGeneric :: HMM State Output -> Text -> Text
trimSeqGeneric hmm seq = ST.pack $ zipWith translate path seqAsStr
    where   (path, _)   = viterbi hmm $ alnSeqToHMMSym seqAsStr
            seqAsStr    = ST.unpack seq

alnSeqToHMMSym :: String -> [Output]
alnSeqToHMMSym = Prelude.map (\c -> if c == '-' then Gap else Residue) 

transitionsPos :: Path -> (Int, Int)
transitionsPos path = (nbPre, nbPre + nbAln)
    where counts = M.fromListWith (+) $ zip path (repeat 1) :: Map State Int
          nbPre = counts ! Pre
          nbAln = counts ! Aln

boundaries :: HMM State Output -> String -> (Int, Int)
boundaries hmm seq = transitionsPos path
    where (path, _) = viterbi hmm $ alnSeqToHMMSym seq

seq1 = [Gap, Gap, Gap, Gap, Gap, Gap, Gap, Gap, Gap, Gap, Gap, Gap, Gap, Gap, Residue, Residue, Residue, Residue, Residue, Residue, Residue, Residue, Residue, Residue, Residue, Residue, Residue, Residue, Residue, Residue, Residue, Residue, Residue, Residue, Residue, Residue, Residue, Residue, Gap, Gap, Gap, Gap, Gap, Gap, Gap, Gap, Gap]

alnSeq1 = "-------------cgacgagcgacttacgagcagcgatg-------------"

(outs, _) = viterbi hmm $ alnSeqToHMMSym alnSeq1

