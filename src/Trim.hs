module Trim (
    trimSeq
    , maskFlaky
    ) where

import Learning.HMM
import Data.Random.Distribution.Categorical as C

import Data.Map ((!))
import Data.Map as M
import Data.Text (Text)
import qualified Data.Text as ST

-- An HMM for trimming leading and trailing unaligned regions from aligned
-- sequences that are much shorter than the model

data State = Pre | Aln | Unaln | Post
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

-- Another HMM, for cases where the sequence may be hard to align on large
-- portions of the model (e.g. when it has insertions, or when the model itself
-- has some flaky regions). This is even simpler than the first: we have an
-- 'aligned' state and an 'unaligned' one. The reason I keep two different
-- models is that in this one we have no a priori belief about the initial
-- state, whereas for the "trim" one we strongly favour starting in Pre. 

flakyHmmStates = [Aln, Unaln]

flakyHmmOutputs = [Gap, Residue]

flakyHmmInitProb = C.fromList [(0.5, Aln), (0.5, Unaln)]

flakyHmmTransProb :: State -> Categorical Double State
flakyHmmTransProb Aln = C.fromList [(0.95, Aln), (0.05, Unaln)]
flakyHmmTransProb Unaln = C.fromList [(0.95, Unaln), (0.05, Aln)]

flakyHmmEmProb :: State -> Categorical Double Output
flakyHmmEmProb Aln = C.fromList [(0.95, Residue), (0.05, Gap)]
flakyHmmEmProb Unaln = C.fromList [(0.95, Gap), (0.05, Residue)]

flakyHmm = HMM flakyHmmStates flakyHmmOutputs flakyHmmInitProb
    flakyHmmTransProb flakyHmmEmProb
--
-- trimSeq should be replaced by maskSeqGeneric trimHmm
trimSeq :: Text -> Text
trimSeq = maskSeqGeneric trimHmm 

maskFlaky :: Text -> Text
maskFlaky = maskSeqGeneric flakyHmm

translate :: State -> Char -> Char
translate state c = case state of
                        Pre     -> '.'
                        Aln     -> c
                        Post    -> '.'
                        Unaln   -> '.'

-- A generalized version of trimSeq, works for any HMM. I don't change existing
-- names so as not to break code, but trimSeq should be the name of this
-- function.

maskSeqGeneric :: HMM State Output -> Text -> Text
maskSeqGeneric hmm seq = ST.pack $ zipWith translate path seqAsStr
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
alnSeq2 = "-----------a-cg-cgagcgacttacgagcagcga-g--ct---------"

alnSeq3 = "cacgatgcagcat----------cagatgcatgcat----cgatgcagcacg"
alnSeq4 = "cacgatgcagc-tca-------c-agatgcatgca-t---cgatgcagcacg"

alnSeq5 = "-----cacgatgcagcat----------cagatgcatgcat-----------"
alnSeq6 = "--c-tcacgatgcagc-tca-------c-agatgcatgca-t---ca-----"

(outs, _) = viterbi trimHmm $ alnSeqToHMMSym alnSeq1

