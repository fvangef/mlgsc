{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import Data.Binary
import qualified Data.Text as T

import TestFileUtils
import CladeModel
import NucModel
import Alignment

-- Numeric testing of NucModels:

small_prob = 0.0001 :: Double -- need to be specific, else logBase complains
scale_factor = 1000 :: Double

test_aln1 = [
	AlnRow "my-OTU" "ATGC-" 1,
	AlnRow "my-OTU" "AACG-" 1,
	AlnRow "my-OTU" "AACTN" 1,
	AlnRow "my-OTU" "ATG--" 1,
	AlnRow "my-OTU" "ATAAT" 1
	]

aln1Mod = alnToNucModel small_prob scale_factor test_aln1 

-- There are 5 out of 5 'A's in the first column, hence 5/5 ... 1
test_1 = TestCase (assertEqual "A@1(aln1)" (round (scale_factor * (logBase 10 (5/5)))) (nucScoreOf aln1Mod 'A' 1))
-- There are 0 out of 5 'C's in the first column: use small_prob
test_2 = TestCase (assertEqual "C@1(aln1)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln1Mod 'C' 1))
-- and so on...
test_3 = TestCase (assertEqual "G@1(aln1)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln1Mod 'G' 1))
test_4 = TestCase (assertEqual "T@1(aln1)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln1Mod 'T' 1))
-- 'D' is for 'dash'
test_5 = TestCase (assertEqual "D@1(aln1)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln1Mod '-' 1))

test_6  = TestCase (assertEqual "A@2(aln1)" (round (scale_factor * (logBase 10 (2/5)))) (nucScoreOf aln1Mod 'A' 2))
test_7  = TestCase (assertEqual "C@2(aln1)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln1Mod 'C' 2))
test_8  = TestCase (assertEqual "G@2(aln1)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln1Mod 'G' 2))
test_9  = TestCase (assertEqual "T@2(aln1)" (round (scale_factor * (logBase 10 (3/5)))) (nucScoreOf aln1Mod 'T' 2))
test_10 = TestCase (assertEqual "D@2(aln1)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln1Mod '-' 2))

test_11 = TestCase (assertEqual "A@3(aln1)" (round (scale_factor * (logBase 10 (1/5)))) (nucScoreOf aln1Mod 'A' 3))
test_12 = TestCase (assertEqual "C@3(aln1)" (round (scale_factor * (logBase 10 (2/5)))) (nucScoreOf aln1Mod 'C' 3))
test_13 = TestCase (assertEqual "G@3(aln1)" (round (scale_factor * (logBase 10 (2/5)))) (nucScoreOf aln1Mod 'G' 3))
test_14 = TestCase (assertEqual "T@3(aln1)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln1Mod 'T' 3))
test_15 = TestCase (assertEqual "D@3(aln1)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln1Mod '-' 3))

test_16 = TestCase (assertEqual "A@4(aln1)" (round (scale_factor * (logBase 10 (1/5)))) (nucScoreOf aln1Mod 'A' 4))
test_17 = TestCase (assertEqual "C@4(aln1)" (round (scale_factor * (logBase 10 (1/5)))) (nucScoreOf aln1Mod 'C' 4))
test_18 = TestCase (assertEqual "G@4(aln1)" (round (scale_factor * (logBase 10 (1/5)))) (nucScoreOf aln1Mod 'G' 4))
test_19 = TestCase (assertEqual "T@4(aln1)" (round (scale_factor * (logBase 10 (1/5)))) (nucScoreOf aln1Mod 'T' 4))
test_20 = TestCase (assertEqual "D@4(aln1)" (round (scale_factor * (logBase 10 (1/5)))) (nucScoreOf aln1Mod '-' 4))

test_21 = TestCase (assertEqual "A@5(aln1)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln1Mod 'A' 5))
test_22 = TestCase (assertEqual "C@5(aln1)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln1Mod 'C' 5))
test_23 = TestCase (assertEqual "G@5(aln1)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln1Mod 'G' 5))
-- we _always_ divide by the number of sequences in the aln, even when non-{ATGC-} residues occur (like N)
test_24 = TestCase (assertEqual "T@5(aln1)" (round (scale_factor * (logBase 10 (1/5)))) (nucScoreOf aln1Mod 'T' 5))
test_25 = TestCase (assertEqual "D@5(aln1)" (round (scale_factor * (logBase 10 (3/5)))) (nucScoreOf aln1Mod '-' 5))

-- Test Binary functions, i.e. storage as binary file to disk, and reading from
-- it, then check every single value in the model, just as above. These cases
-- are all under the same test, because I don't want to write and read the file
-- every time.

test_26 = TestCase (do
                        removeIfExists "aln1Mod.bmod"
                        encodeFile "aln1Mod.bmod" aln1Mod
                        aln2Mod <- decodeFile "aln1Mod.bmod"
                        assertEqual "store-read" aln1Mod aln2Mod

                        assertEqual "A@1(aln2)" (round (scale_factor * (logBase 10 (5/5)))) (nucScoreOf aln2Mod 'A' 1)
                        assertEqual "C@1(aln2)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln2Mod 'C' 1)
                        assertEqual "G@1(aln2)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln2Mod 'G' 1)
                        assertEqual "T@1(aln2)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln2Mod 'T' 1)
                        assertEqual "D@1(aln2)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln2Mod '-' 1)
                        
                        assertEqual "A@2(aln2)" (round (scale_factor * (logBase 10 (2/5)))) (nucScoreOf aln2Mod 'A' 2)
                        assertEqual "C@2(aln2)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln2Mod 'C' 2)
                        assertEqual "G@2(aln2)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln2Mod 'G' 2)
                        assertEqual "T@2(aln2)" (round (scale_factor * (logBase 10 (3/5)))) (nucScoreOf aln2Mod 'T' 2)
                        assertEqual "D@2(aln2)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln2Mod '-' 2)
                        
                        assertEqual "A@3(aln2)" (round (scale_factor * (logBase 10 (1/5)))) (nucScoreOf aln2Mod 'A' 3)
                        assertEqual "C@3(aln2)" (round (scale_factor * (logBase 10 (2/5)))) (nucScoreOf aln2Mod 'C' 3)
                        assertEqual "G@3(aln2)" (round (scale_factor * (logBase 10 (2/5)))) (nucScoreOf aln2Mod 'G' 3)
                        assertEqual "T@3(aln2)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln2Mod 'T' 3)
                        assertEqual "D@3(aln2)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln2Mod '-' 3)
                        
                        assertEqual "A@4(aln2)" (round (scale_factor * (logBase 10 (1/5)))) (nucScoreOf aln2Mod 'A' 4)
                        assertEqual "C@4(aln2)" (round (scale_factor * (logBase 10 (1/5)))) (nucScoreOf aln2Mod 'C' 4)
                        assertEqual "G@4(aln2)" (round (scale_factor * (logBase 10 (1/5)))) (nucScoreOf aln2Mod 'G' 4)
                        assertEqual "T@4(aln2)" (round (scale_factor * (logBase 10 (1/5)))) (nucScoreOf aln2Mod 'T' 4)
                        assertEqual "D@4(aln2)" (round (scale_factor * (logBase 10 (1/5)))) (nucScoreOf aln2Mod '-' 4)
                        
                        assertEqual "A@5(aln2)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln2Mod 'A' 5)
                        assertEqual "C@5(aln2)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln2Mod 'C' 5)
                        assertEqual "G@5(aln2)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln2Mod 'G' 5)
                        assertEqual "T@5(aln2)" (round (scale_factor * (logBase 10 (1/5)))) (nucScoreOf aln2Mod 'T' 5)
                        assertEqual "D@5(aln2)" (round (scale_factor * (logBase 10 (3/5)))) (nucScoreOf aln2Mod '-' 5)

                        )

-- Test the scoring function. Here I use the short versions of assertEqual,
-- TestCase, etc (namely, -?=, etc.)
--
-- Computes the expected score from the list of positional frequencies. Short
-- name to save some space.
--
es ps = sum $ map (round . (scale_factor *) . (logBase 10)) ps

test_27 = "AAAAA" ~: (nucScoreSeq aln1Mod "AAAAA") ~?= (es [5/5, 2/5, 1/5, 1/5, small_prob])
test_28 = "-----" ~: (nucScoreSeq aln1Mod "-----") ~?= (es [small_prob, small_prob, small_prob, 1/5, 3/5])
test_29 = "ATCG-" ~: (nucScoreSeq aln1Mod "ATCG-") ~?= (es [5/5, 3/5, 2/5, 1/5, 3/5])
test_30 = "ATCGN" ~: (nucScoreSeq aln1Mod "ATCGN") ~?= (es [5/5, 3/5, 2/5, 1/5, small_prob])
                             
-- Test model length

test_31 = "modLength" ~: (nucModLength aln1Mod) ~?= 5

-- Tests for empty alignments
--
aln2Mod = alnToNucModel small_prob scale_factor []

test_32 = "emptyAln score" ~: (nucScoreSeq aln2Mod "AATGC") ~?= (minBound :: Int)

-- Tests models with weighted sequences
--

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

aln3Mod = alnToNucModel small_prob scale_factor test_aln3 

test_40 = TestCase (assertEqual "A@1(aln3)" (round (scale_factor * (logBase 10 (5/5)))) (nucScoreOf aln3Mod 'A' 1))
test_41 = TestCase (assertEqual "C@1(aln3)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln3Mod 'C' 1))
test_42 = TestCase (assertEqual "G@1(aln3)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln3Mod 'G' 1))
test_43 = TestCase (assertEqual "T@1(aln3)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln3Mod 'T' 1))
test_44 = TestCase (assertEqual "D@1(aln3)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln1Mod '-' 1))

-- Now, we use non-unit weights.

test_aln4 = [
	AlnRow "my_OTU_wght" "ATGC-" 2,
	AlnRow "my_OTU_wght" "AACG-" 1,
	AlnRow "my_OTU_wght" "AACTN" 3,
	AlnRow "my_OTU_wght" "ATG--" 2,
	AlnRow "my_OTU_wght" "ATAAT" 1
	]

aln4Mod = alnToNucModel small_prob scale_factor test_aln4 


test_51 = TestCase (assertEqual "A@1(aln4)" (round (scale_factor * (logBase 10 (9/9)))) (nucScoreOf aln4Mod 'A' 1))
test_52 = TestCase (assertEqual "C@1(aln4)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln4Mod 'C' 1))
test_53 = TestCase (assertEqual "G@1(aln4)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln4Mod 'G' 1))
test_54 = TestCase (assertEqual "T@1(aln4)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln4Mod 'T' 1))
test_55 = TestCase (assertEqual "D@1(aln4)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln4Mod '-' 1))

test_56  = TestCase (assertEqual "A@2(aln4)" (round (scale_factor * (logBase 10 (4/9)))) (nucScoreOf aln4Mod 'A' 2))
test_57  = TestCase (assertEqual "C@2(aln4)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln4Mod 'C' 2))
test_58  = TestCase (assertEqual "G@2(aln4)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln4Mod 'G' 2))
test_59  = TestCase (assertEqual "T@2(aln4)" (round (scale_factor * (logBase 10 (5/9)))) (nucScoreOf aln4Mod 'T' 2))
test_60 = TestCase (assertEqual "D@2(aln4)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln4Mod '-' 2))

test_61 = TestCase (assertEqual "A@3(aln4)" (round (scale_factor * (logBase 10 (1/9)))) (nucScoreOf aln4Mod 'A' 3))
test_62 = TestCase (assertEqual "C@3(aln4)" (round (scale_factor * (logBase 10 (4/9)))) (nucScoreOf aln4Mod 'C' 3))
test_63 = TestCase (assertEqual "G@3(aln4)" (round (scale_factor * (logBase 10 (4/9)))) (nucScoreOf aln4Mod 'G' 3))
test_64 = TestCase (assertEqual "T@3(aln4)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln4Mod 'T' 3))
test_65 = TestCase (assertEqual "D@3(aln4)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln4Mod '-' 3))

test_66 = TestCase (assertEqual "A@4(aln4)" (round (scale_factor * (logBase 10 (1/9)))) (nucScoreOf aln4Mod 'A' 4))
test_67 = TestCase (assertEqual "C@4(aln4)" (round (scale_factor * (logBase 10 (2/9)))) (nucScoreOf aln4Mod 'C' 4))
test_68 = TestCase (assertEqual "G@4(aln4)" (round (scale_factor * (logBase 10 (1/9)))) (nucScoreOf aln4Mod 'G' 4))
test_69 = TestCase (assertEqual "T@4(aln4)" (round (scale_factor * (logBase 10 (3/9)))) (nucScoreOf aln4Mod 'T' 4))
test_70 = TestCase (assertEqual "D@4(aln4)" (round (scale_factor * (logBase 10 (2/9)))) (nucScoreOf aln4Mod '-' 4))

test_71 = TestCase (assertEqual "A@5(aln4)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln4Mod 'A' 5))
test_72 = TestCase (assertEqual "C@5(aln4)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln4Mod 'C' 5))
test_73 = TestCase (assertEqual "G@5(aln4)" (round (scale_factor * (logBase 10 small_prob))) (nucScoreOf aln4Mod 'G' 5))
test_74 = TestCase (assertEqual "T@5(aln4)" (round (scale_factor * (logBase 10 (1/9)))) (nucScoreOf aln4Mod 'T' 5))
test_75 = TestCase (assertEqual "D@5(aln4)" (round (scale_factor * (logBase 10 (5/9)))) (nucScoreOf aln4Mod '-' 5))


-- Test the scoring function on a model made from a weighted alignment. 
--
-- Computes the expected score from the list of positional frequencies. Short
-- name to save some space.

test_80 = "AAAAA" ~: (nucScoreSeq aln4Mod "AAAAA") ~?= (es [9/9, 4/9, 1/9, 1/9, small_prob])
test_81 = "-----" ~: (nucScoreSeq aln4Mod "-----") ~?= (es [small_prob, small_prob, small_prob, 2/9, 5/9])
test_82 = "ATCG-" ~: (nucScoreSeq aln4Mod "ATCG-") ~?= (es [9/9, 5/9, 4/9, 1/9, 5/9])
test_83 = "ATCGN" ~: (nucScoreSeq aln4Mod "ATCGN") ~?= (es [9/9, 5/9, 4/9, 1/9, small_prob])
                             
-- Test model length

tests = TestList [
		    TestLabel "nucScoreOf" test_1
		    , TestLabel "nucScoreOf" test_2
		    , TestLabel "nucScoreOf" test_3
		    , TestLabel "nucScoreOf" test_4
		    , TestLabel "nucScoreOf" test_5
		    , TestLabel "nucScoreOf" test_6
		    , TestLabel "nucScoreOf" test_7
		    , TestLabel "nucScoreOf" test_8
		    , TestLabel "nucScoreOf" test_9
		    , TestLabel "nucScoreOf" test_10
		    , TestLabel "nucScoreOf" test_11
		    , TestLabel "nucScoreOf" test_12
		    , TestLabel "nucScoreOf" test_13
		    , TestLabel "nucScoreOf" test_14
		    , TestLabel "nucScoreOf" test_15
		    , TestLabel "nucScoreOf" test_16
		    , TestLabel "nucScoreOf" test_17
		    , TestLabel "nucScoreOf" test_18
		    , TestLabel "nucScoreOf" test_19
		    , TestLabel "nucScoreOf" test_20
		    , TestLabel "nucScoreOf" test_21
		    , TestLabel "nucScoreOf" test_22
		    , TestLabel "nucScoreOf" test_23
		    , TestLabel "nucScoreOf" test_24
		    , TestLabel "nucScoreOf" test_25
		    , TestLabel "nucScoreOf" test_26
            , TestLabel "nucScoreSeq" test_27
            , TestLabel "nucScoreSeq" test_28
            , TestLabel "nucScoreSeq" test_29
            , TestLabel "nucScoreSeq" test_30
            , TestLabel "nucScoreSeq" test_31
            , TestLabel "nucScoreSeq" test_32
            , TestLabel "wgt nucScoreOf" test_40
            , TestLabel "wgt nucScoreOf" test_41
            , TestLabel "wgt nucScoreOf" test_42
            , TestLabel "wgt nucScoreOf" test_43
            , TestLabel "wgt nucScoreOf" test_44
            , TestLabel "wgt nucScoreOf" test_51
            , TestLabel "wgt nucScoreOf" test_52
            , TestLabel "wgt nucScoreOf" test_53
            , TestLabel "wgt nucScoreOf" test_54
            , TestLabel "wgt nucScoreOf" test_55
            , TestLabel "wgt nucScoreOf" test_56
            , TestLabel "wgt nucScoreOf" test_57
            , TestLabel "wgt nucScoreOf" test_58
            , TestLabel "wgt nucScoreOf" test_59
            , TestLabel "wgt nucScoreOf" test_60
            , TestLabel "wgt nucScoreOf" test_61
            , TestLabel "wgt nucScoreOf" test_62
            , TestLabel "wgt nucScoreOf" test_63
            , TestLabel "wgt nucScoreOf" test_64
            , TestLabel "wgt nucScoreOf" test_65
            , TestLabel "wgt nucScoreOf" test_66
            , TestLabel "wgt nucScoreOf" test_67
            , TestLabel "wgt nucScoreOf" test_68
            , TestLabel "wgt nucScoreOf" test_69
            , TestLabel "wgt nucScoreOf" test_70
            , TestLabel "wgt nucScoreOf" test_71
            , TestLabel "wgt nucScoreOf" test_72
            , TestLabel "wgt nucScoreOf" test_73
            , TestLabel "wgt nucScoreOf" test_74
            , TestLabel "wgt nucScoreOf" test_75
            , TestLabel "nucScoreSeq" test_80
            , TestLabel "nucScoreSeq" test_81
            , TestLabel "nucScoreSeq" test_82
            , TestLabel "nucScoreSeq" test_83
		]

main = do
    runTestTT tests
