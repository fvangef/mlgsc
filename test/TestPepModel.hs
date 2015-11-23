{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import qualified Data.Text as T
import Data.Binary
  
import TestFileUtils
import CladeModel
import PepModel
import NucModel
import PWMModel
import Alignment

-- Numeric testing of PepModels:

small_prob = 0.0001 :: Double -- need to be specific, else logBase complains
small_score = round (scale_factor * (logBase 10 small_prob))
scale_factor = 1000 :: Double

test_aln1 = [
    AlnRow "my-OTU" "VIKD-" 1,
    AlnRow "my-OTU" "VARE-" 1,
    AlnRow "my-OTU" "VARNX" 1,
    AlnRow "my-OTU" "VIK--" 1,
    AlnRow "my-OTU" "VIHQY" 1
    ]

aln1Mod = alnToPepModel small_prob scale_factor "my-OTU" test_aln1 
-- TODO: remove when devel done
aln1Mat = PepModel.matrix aln1Mod
aln1RMat = posMapVec2Matrix aln1Mat small_score 
aln1RMatRN = reduceNoise 0 aln1RMat
aln1Mat' = matrix2posMapVec aln1RMatRN

-- These tests are calques from TestNucModel (this is also why the test numbers
-- are discontinuous), but while for nucleotides it is practical to test all
-- five possibilities (ACTG-), for the 20 amino acids we won't go all that way.
-- We test the residues that are found at a given position, plus perhaps one (or
-- the gap) that is not.
--
-- There are 5 out of 5 'V's in the first column, hence 5/5 ... 1
test_1 = "A@1(aln1)" ~: round (scale_factor * (logBase 10 (5/5)))
    ~?= pepScoreOf aln1Mod 'V' 1
-- There are 0 out of 5 'C's in the first column: use small_prob
test_2 = "C@1(aln1)" ~: (round (scale_factor * (logBase 10 small_prob)))
    ~?= pepScoreOf aln1Mod 'C' 1
-- 'D' is for 'dash'
test_5 = "D@1(aln1)" ~: (round (scale_factor * (logBase 10 small_prob)))
    ~?= (pepScoreOf aln1Mod '-' 1)

test_6  = "A@2(aln1)" ~: (round (scale_factor * (logBase 10 (2/5))))
    ~?= (pepScoreOf aln1Mod 'A' 2)
test_8  = "G@2(aln1)" ~: (round (scale_factor * (logBase 10 small_prob)))
    ~?= (pepScoreOf aln1Mod 'G' 2)
test_9  = "T@2(aln1)" ~: (round (scale_factor * (logBase 10 (3/5))))
    ~?= (pepScoreOf aln1Mod 'I' 2)

test_11 = "A@3(aln1)" ~: (round (scale_factor * (logBase 10 (1/5))))
    ~?= (pepScoreOf aln1Mod 'H' 3)
test_12 = "C@3(aln1)" ~: (round (scale_factor * (logBase 10 (2/5))))
    ~?= (pepScoreOf aln1Mod 'R' 3)
test_13 = "G@3(aln1)" ~: (round (scale_factor * (logBase 10 (2/5))))
    ~?= (pepScoreOf aln1Mod 'K' 3)
test_14 = "T@3(aln1)" ~: (round (scale_factor * (logBase 10 small_prob)))
    ~?= (pepScoreOf aln1Mod 'T' 3)

test_16 = "A@4(aln1)" ~: (round (scale_factor * (logBase 10 (1/5))))
    ~?= (pepScoreOf aln1Mod 'D' 4)
test_17 = "C@4(aln1)" ~: (round (scale_factor * (logBase 10 (1/5))))
    ~?= (pepScoreOf aln1Mod 'E' 4)
test_18 = "G@4(aln1)" ~: (round (scale_factor * (logBase 10 (1/5))))
    ~?= (pepScoreOf aln1Mod 'N' 4)
test_19 = "T@4(aln1)" ~: (round (scale_factor * (logBase 10 (1/5))))
    ~?= (pepScoreOf aln1Mod 'Q' 4)
test_20 = "D@4(aln1)" ~: (round (scale_factor * (logBase 10 (1/5))))
    ~?= (pepScoreOf aln1Mod '-' 4)

test_21 = "A@5(aln1)" ~: (round (scale_factor * (logBase 10 small_prob)))
    ~?= (pepScoreOf aln1Mod 'A' 5)
-- we _always_ divide by the number of sequences in the aln, even when non-{ATGC-} residues occur (like N)
test_24 = "T@5(aln1)" ~: (round (scale_factor * (logBase 10 (1/5))))
    ~?= (pepScoreOf aln1Mod 'Y' 5)
test_25 = "D@5(aln1)" ~: (round (scale_factor * (logBase 10 (3/5))))
    ~?= (pepScoreOf aln1Mod '-' 5)

-- Test Binary functions, i.e. storage as binary file to disk, and reading from
-- it, then check every single value in the model, just as above. These cases
-- are all under the same test, because I don't want to write and read the file
-- every time.

test_26 = TestCase
    (do
        removeIfExists "aln1Mod.bcls"
        encodeFile "aln1Mod.bcls" aln1Mod
        aln2Mod <- decodeFile "aln1Mod.bcls"
        assertEqual "store-read" aln1Mod aln2Mod
        round (scale_factor * (logBase 10 (5/5)))
            @=? pepScoreOf aln1Mod 'V' 1
        round (scale_factor * (logBase 10 small_prob))
            @=? pepScoreOf aln1Mod 'C' 1
        round (scale_factor * (logBase 10 small_prob))
            @=? pepScoreOf aln1Mod '-' 1
        
        round (scale_factor * (logBase 10 (2/5)))
            @=? pepScoreOf aln1Mod 'A' 2
        round (scale_factor * (logBase 10 small_prob))
            @=? pepScoreOf aln1Mod 'G' 2
        round (scale_factor * (logBase 10 (3/5)))
            @=? pepScoreOf aln1Mod 'I' 2
        
        round (scale_factor * (logBase 10 (1/5)))
            @=? pepScoreOf aln1Mod 'H' 3
        round (scale_factor * (logBase 10 (2/5)))
            @=? pepScoreOf aln1Mod 'R' 3
        round (scale_factor * (logBase 10 (2/5)))
            @=? pepScoreOf aln1Mod 'K' 3
        round (scale_factor * (logBase 10 small_prob))
            @=? pepScoreOf aln1Mod 'T' 3
        
        round (scale_factor * (logBase 10 (1/5)))
            @=? pepScoreOf aln1Mod 'D' 4
        round (scale_factor * (logBase 10 (1/5)))
            @=? pepScoreOf aln1Mod 'E' 4
        round (scale_factor * (logBase 10 (1/5)))
            @=? pepScoreOf aln1Mod 'N' 4
        round (scale_factor * (logBase 10 (1/5)))
            @=? pepScoreOf aln1Mod 'Q' 4
        round (scale_factor * (logBase 10 (1/5)))
            @=? pepScoreOf aln1Mod '-' 4
        
        round (scale_factor * (logBase 10 small_prob))
            @=? pepScoreOf aln1Mod 'A' 5
        round (scale_factor * (logBase 10 (1/5)))
            @=? pepScoreOf aln1Mod 'Y' 5
        round (scale_factor * (logBase 10 (3/5)))
            @=? pepScoreOf aln1Mod '-' 5
        )

-- Computes the expected score from the list of positional frequencies. Short
-- name to save some space.
--
es ps = sum $ map (round . (scale_factor *) . (logBase 10)) ps

test_27 = "AAAAA" ~: (pepScoreSeq aln1Mod "AAAAA") ~?= (es [small_prob, 2/5, small_prob, small_prob, small_prob])
test_28 = "-----" ~: (pepScoreSeq aln1Mod "-----") ~?= (es [small_prob, small_prob, small_prob, 1/5, 3/5])
test_29 = "VIRQY" ~: (pepScoreSeq aln1Mod "VIRQY") ~?= (es [5/5, 3/5, 2/5, 1/5, 1/5])
test_30 = "VIK--" ~: (pepScoreSeq aln1Mod "VIK--") ~?= (es [5/5, 3/5, 2/5, 1/5, 3/5])
-- A dot stands for a masked position, and has a score of 0
test_33 = "mask" ~: (pepScoreSeq aln1Mod ".....") ~?= 0
test_34 = "pmask" ~: (pepScoreSeq aln1Mod ".IRQ.") ~?= (es [3/5, 2/5, 1/5])
                             
-- Test model length

test_31 = "modLength" ~: (pepModLength aln1Mod) ~?= 5

-- Tests for empty alignments (as they can appear if some taxon is mentioned in
-- a tree but no representatives are found in the training alignment).
--
aln2Mod = alnToPepModel small_prob scale_factor "anonymous" []

test_32 = "emptyAln score" ~: (pepScoreSeq aln2Mod "LEHVN") ~?= 5 * small_score

-- Tests models with weighted sequences
-- NOTE: I will leave A, C, G, and T for these tests - just think of them as
-- alanine, cysteine, glycine, and threonine :-)

-- This is exactly the same as aln1, so we can happiliy re-use those tests.
-- TODO: once the test pass, remove this aln - aln1 and aln3 are now of the same
-- type, apart from having the same value.

test_aln3 = [
    AlnRow "my_OTU" "ATGC-" 1,
    AlnRow "my_OTU" "AACG-" 1,
    AlnRow "my_OTU" "AACTN" 1,
    AlnRow "my_OTU" "ATG--" 1,
    AlnRow "my_OTU" "ATAAT" 1
    ]

aln3Mod = alnToPepModel small_prob scale_factor "my_OTU" test_aln3 

test_40 = "A@1(aln3)" ~: (round (scale_factor * (logBase 10 (5/5)))) ~?= (pepScoreOf aln3Mod 'A' 1)
test_41 = "C@1(aln3)" ~: (round (scale_factor * (logBase 10 small_prob))) ~?= (pepScoreOf aln3Mod 'C' 1)
test_42 = "G@1(aln3)" ~: (round (scale_factor * (logBase 10 small_prob))) ~?= (pepScoreOf aln3Mod 'G' 1)
test_43 = "T@1(aln3)" ~: (round (scale_factor * (logBase 10 small_prob))) ~?= (pepScoreOf aln3Mod 'T' 1)
test_44 = "D@1(aln3)" ~: (round (scale_factor * (logBase 10 small_prob))) ~?= (pepScoreOf aln1Mod '-' 1)

-- Now, we use non-unit weights.

test_aln4 = [
    AlnRow "my_OTU_wght" "ATGC-" 2,
    AlnRow "my_OTU_wght" "AACG-" 1,
    AlnRow "my_OTU_wght" "AACTN" 3,
    AlnRow "my_OTU_wght" "ATG--" 2,
    AlnRow "my_OTU_wght" "ATAAT" 1
    ]

aln4Mod = alnToPepModel small_prob scale_factor "my_OTU_wght" test_aln4 


test_51 = "A@1(aln4)" ~: (round (scale_factor * (logBase 10 (9/9)))) ~?= (pepScoreOf aln4Mod 'A' 1)
test_52 = "C@1(aln4)" ~: (round (scale_factor * (logBase 10 small_prob))) ~?= (pepScoreOf aln4Mod 'C' 1)
test_53 = "G@1(aln4)" ~: (round (scale_factor * (logBase 10 small_prob))) ~?= (pepScoreOf aln4Mod 'G' 1)
test_54 = "T@1(aln4)" ~: (round (scale_factor * (logBase 10 small_prob))) ~?= (pepScoreOf aln4Mod 'T' 1)
test_55 = "D@1(aln4)" ~: (round (scale_factor * (logBase 10 small_prob))) ~?= (pepScoreOf aln4Mod '-' 1)

test_56  = "A@2(aln4)" ~: (round (scale_factor * (logBase 10 (4/9)))) ~?= (pepScoreOf aln4Mod 'A' 2)
test_57  = "C@2(aln4)" ~: (round (scale_factor * (logBase 10 small_prob))) ~?= (pepScoreOf aln4Mod 'C' 2)
test_58  = "G@2(aln4)" ~: (round (scale_factor * (logBase 10 small_prob))) ~?= (pepScoreOf aln4Mod 'G' 2)
test_59  = "T@2(aln4)" ~: (round (scale_factor * (logBase 10 (5/9)))) ~?= (pepScoreOf aln4Mod 'T' 2)
test_60 = "D@2(aln4)" ~: (round (scale_factor * (logBase 10 small_prob))) ~?= (pepScoreOf aln4Mod '-' 2)

test_61 = "A@3(aln4)" ~: (round (scale_factor * (logBase 10 (1/9)))) ~?= (pepScoreOf aln4Mod 'A' 3)
test_62 = "C@3(aln4)" ~: (round (scale_factor * (logBase 10 (4/9)))) ~?= (pepScoreOf aln4Mod 'C' 3)
test_63 = "G@3(aln4)" ~: (round (scale_factor * (logBase 10 (4/9)))) ~?= (pepScoreOf aln4Mod 'G' 3)
test_64 = "T@3(aln4)" ~: (round (scale_factor * (logBase 10 small_prob))) ~?= (pepScoreOf aln4Mod 'T' 3)
test_65 = "D@3(aln4)" ~: (round (scale_factor * (logBase 10 small_prob))) ~?= (pepScoreOf aln4Mod '-' 3)

test_66 = "A@4(aln4)" ~: (round (scale_factor * (logBase 10 (1/9)))) ~?= (pepScoreOf aln4Mod 'A' 4)
test_67 = "C@4(aln4)" ~: (round (scale_factor * (logBase 10 (2/9)))) ~?= (pepScoreOf aln4Mod 'C' 4)
test_68 = "G@4(aln4)" ~: (round (scale_factor * (logBase 10 (1/9)))) ~?= (pepScoreOf aln4Mod 'G' 4)
test_69 = "T@4(aln4)" ~: (round (scale_factor * (logBase 10 (3/9)))) ~?= (pepScoreOf aln4Mod 'T' 4)
test_70 = "D@4(aln4)" ~: (round (scale_factor * (logBase 10 (2/9)))) ~?= (pepScoreOf aln4Mod '-' 4)

test_71 = "A@5(aln4)" ~: (round (scale_factor * (logBase 10 small_prob))) ~?= (pepScoreOf aln4Mod 'A' 5)
test_72 = "C@5(aln4)" ~: (round (scale_factor * (logBase 10 small_prob))) ~?= (pepScoreOf aln4Mod 'C' 5)
test_73 = "G@5(aln4)" ~: (round (scale_factor * (logBase 10 small_prob))) ~?= (pepScoreOf aln4Mod 'G' 5)
test_74 = "T@5(aln4)" ~: (round (scale_factor * (logBase 10 (1/9)))) ~?= (pepScoreOf aln4Mod 'T' 5)
test_75 = "D@5(aln4)" ~: (round (scale_factor * (logBase 10 (5/9)))) ~?= (pepScoreOf aln4Mod '-' 5)


-- Test the scoring function on a model made from a weighted alignment. 
--
-- Computes the expected score from the list of positional frequencies. Short
-- name to save some space.

test_80 = "AAAAA" ~: (pepScoreSeq aln4Mod "AAAAA") ~?= (es [9/9, 4/9, 1/9, 1/9, small_prob])
test_81 = "-----" ~: (pepScoreSeq aln4Mod "-----") ~?= (es [small_prob, small_prob, small_prob, 2/9, 5/9])
test_82 = "ATCG-" ~: (pepScoreSeq aln4Mod "ATCG-") ~?= (es [9/9, 5/9, 4/9, 1/9, 5/9])
test_83 = "ATCGN" ~: (pepScoreSeq aln4Mod "ATCGN") ~?= (es [9/9, 5/9, 4/9, 1/9, 3/9])
                             
-- Test model length

tests = TestList [
            TestLabel "pepScoreOf" test_1
            , TestLabel "pepScoreOf" test_2
            , TestLabel "pepScoreOf" test_5
            , TestLabel "pepScoreOf" test_6
            , TestLabel "pepScoreOf" test_8
            , TestLabel "pepScoreOf" test_9
            , TestLabel "pepScoreOf" test_11
            , TestLabel "pepScoreOf" test_12
            , TestLabel "pepScoreOf" test_13
            , TestLabel "pepScoreOf" test_14
            , TestLabel "pepScoreOf" test_16
            , TestLabel "pepScoreOf" test_17
            , TestLabel "pepScoreOf" test_18
            , TestLabel "pepScoreOf" test_19
            , TestLabel "pepScoreOf" test_20
            , TestLabel "pepScoreOf" test_21
            , TestLabel "pepScoreOf" test_24
            , TestLabel "pepScoreOf" test_25
            , TestLabel "pepScoreOf" test_26
            , TestLabel "pepScoreSeq" test_27
            , TestLabel "pepScoreSeq" test_28
            , TestLabel "pepScoreSeq" test_29
            , TestLabel "pepScoreSeq" test_30
            , TestLabel "pepScoreSeq" test_31
            , TestLabel "pepScoreSeq" test_32
            , TestLabel "pepScoreSeq" test_33
            , TestLabel "pepScoreSeq" test_34
            , TestLabel "wgt pepScoreOf" test_40
            , TestLabel "wgt pepScoreOf" test_41
            , TestLabel "wgt pepScoreOf" test_42
            , TestLabel "wgt pepScoreOf" test_43
            , TestLabel "wgt pepScoreOf" test_44
            , TestLabel "wgt pepScoreOf" test_51
            , TestLabel "wgt pepScoreOf" test_52
            , TestLabel "wgt pepScoreOf" test_53
            , TestLabel "wgt pepScoreOf" test_54
            , TestLabel "wgt pepScoreOf" test_55
            , TestLabel "wgt pepScoreOf" test_56
            , TestLabel "wgt pepScoreOf" test_57
            , TestLabel "wgt pepScoreOf" test_58
            , TestLabel "wgt pepScoreOf" test_59
            , TestLabel "wgt pepScoreOf" test_60
            , TestLabel "wgt pepScoreOf" test_61
            , TestLabel "wgt pepScoreOf" test_62
            , TestLabel "wgt pepScoreOf" test_63
            , TestLabel "wgt pepScoreOf" test_64
            , TestLabel "wgt pepScoreOf" test_65
            , TestLabel "wgt pepScoreOf" test_66
            , TestLabel "wgt pepScoreOf" test_67
            , TestLabel "wgt pepScoreOf" test_68
            , TestLabel "wgt pepScoreOf" test_69
            , TestLabel "wgt pepScoreOf" test_70
            , TestLabel "wgt pepScoreOf" test_71
            , TestLabel "wgt pepScoreOf" test_72
            , TestLabel "wgt pepScoreOf" test_73
            , TestLabel "wgt pepScoreOf" test_74
            , TestLabel "wgt pepScoreOf" test_75
            , TestLabel "pepScoreSeq" test_80
            , TestLabel "pepScoreSeq" test_81
            , TestLabel "pepScoreSeq" test_82
            , TestLabel "pepScoreSeq" test_83
        ]

main = do
    runTestTT tests
