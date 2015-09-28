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

hmmStates = [Pre, Aln, Post]

hmmOutputs = [Gap, Residue]

initProb = C.fromList [(0.9, Pre), (0.1, Aln), (0.0, Post)]

transProb :: State -> Categorical Double State
transProb Pre = C.fromList [(0.99, Pre), (0.01, Aln), (0.0, Post)]
transProb Aln = C.fromList [(0.0, Pre), (0.99, Aln), (0.01, Post)]
transProb Post = C.fromList [(0.0, Pre), (0.0, Aln), (1.0, Post)]

emProb Pre = C.fromList [(0.95, Gap), (0.05, Residue)]
emProb Aln = C.fromList [(0.05, Gap), (0.95, Residue)]
emProb Post = C.fromList [(0.95, Gap), (0.05, Residue)]

hmm = HMM hmmStates hmmOutputs initProb transProb emProb

trimSeq :: Text -> Text
trimSeq seq = ST.pack $ zipWith translate path seqAsStr
    where   (path, _)   = viterbi hmm $ alnSeqToHMMSym seqAsStr
            seqAsStr    = ST.unpack seq    
            translate state c = case state of
                                    Pre -> '.'
                                    Aln -> c
                                    Post -> '.'

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

