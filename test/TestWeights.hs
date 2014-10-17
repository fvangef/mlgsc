
import Test.HUnit
import qualified Data.Text.Lazy as T
import qualified Data.Text as S
import qualified Data.Map.Strict as M

import MlgscTypes
import Weights
import Alignment

-- This example is taken from
-- http://www.cs.umd.edu/class/fall2011/cmsc858s/Weights.pdf.
-- The code below was checked against the weights given in that document.
-- Everything is ok.

aln = [
    AlnRow (S.pack "myOTU") (S.pack "GCGTTAGC") 1,
    AlnRow (S.pack "myOTU") (S.pack "GAGTTGGA") 1,
    AlnRow (S.pack "myOTU") (S.pack "CGGACTAA") 1
    ]

[wt1, wt2, wt3] = map rowWeight $ henikoffWeightAln aln

-- The alignment weights are integral, so I normalize the ones from the website,
-- i.e. I divide all by the smallest (namely, 0.28125)

test01 = "H wght seq1" ~: wt1 ~?= round (0.31250 / 0.28125)
test02 = "H wght seq2" ~: wt2 ~?= round (0.28125 / 0.28125)
test03 = "H wght seq3" ~: wt3 ~?= round (0.40625 / 0.28125)

-- Check the raw weights
[rawWt1, rawWt2, rawWt3] = alnRawWeights aln

test11 = "H raw wt seq1" ~: rawWt1 ~?= 2.5 
test12 = "H raw wt seq2" ~: rawWt2 ~?= 2.25 
test13 = "H raw wt seq3" ~: rawWt3 ~?= 3.25 

-- As it happens, the above sequences have relative weights of 1.11, 1.0, and
-- 1.444, all of which, after rounding, yield 1. The following has sequences
-- with final (rounded) weights different from 1.

aln2 = [
    AlnRow (S.pack "myOTU") (S.pack "AAAAAAAA") 1,
    AlnRow (S.pack "myOTU") (S.pack "AAAAAAAA") 1,
    AlnRow (S.pack "myOTU") (S.pack "AAAAAAAA") 1,
    AlnRow (S.pack "myOTU") (S.pack "AAAAAAAA") 1,
    AlnRow (S.pack "myOTU") (S.pack "CCCCCCCC") 1,
    AlnRow (S.pack "myOTU") (S.pack "CCCCCCCC") 1,
    AlnRow (S.pack "myOTU") (S.pack "TTTTTTTT") 1
    ]

-- Check the raw weights
[rawWt31, rawWt32, rawWt33, rawWt34, rawWt35, rawWt36, rawWt37] = alnRawWeights aln2

tolerance = 0.0001

test31 = ((rawWt31 - 0.6666) < tolerance) ~? "H raw wt seq31"
test32 = ((rawWt32 - 0.6666) < tolerance) ~? "H raw wt seq32"
test33 = ((rawWt33 - 0.6666) < tolerance) ~? "H raw wt seq33"
test34 = ((rawWt34 - 0.6666) < tolerance) ~? "H raw wt seq34"
test35 = ((rawWt35 - 1.3333) < tolerance) ~? "H raw wt seq35"
test36 = ((rawWt36 - 1.3333) < tolerance) ~? "H raw wt seq36"
test37 = ((rawWt37 - 2.6666) < tolerance) ~? "H raw wt seq37"

tests = TestList [
            TestLabel "Henikoff weights" test01
            , TestLabel "Henikoff weights" test02
            , TestLabel "Henikoff weights" test03
            , TestLabel "Henikoff weights" test11
            , TestLabel "Henikoff weights" test12
            , TestLabel "Henikoff weights" test13
            , TestLabel "Henikoff weights" test31
            , TestLabel "Henikoff weights" test32
            , TestLabel "Henikoff weights" test33
            , TestLabel "Henikoff weights" test34
            , TestLabel "Henikoff weights" test35
            , TestLabel "Henikoff weights" test36
            , TestLabel "Henikoff weights" test37
		]

main = do
	runTestTT tests

