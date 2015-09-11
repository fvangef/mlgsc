import Test.HUnit
import qualified Data.Text.Lazy as T
import qualified Data.Text as S
import qualified Data.Map.Strict as M

import MlgscTypes
import FastA

fastaInput = ">hdr1\naa\ntt\n>hdr2 len > 5\ngg\ncc\n>hdr3\ngaattc"

fastARecs = fastATextToRecords $ T.pack fastaInput

test1 = TestCase (assertEqual "FastA rec 1 hdr" (T.pack "hdr1") (FastA.header $ head fastARecs))
test2 = TestCase (assertEqual "FastA rec 1 seq" (T.pack "AATT") (FastA.sequence $ head fastARecs))

test3 = TestCase (assertEqual "FastA rec 2 hdr" (T.pack "hdr2 len > 5") (FastA.header $ fastARecs !! 1))
test4 = TestCase (assertEqual "FastA rec 2 seq" (T.pack "GGCC") (FastA.sequence $ fastARecs !! 1))

test5 = TestCase (assertEqual "FastA rec 3 hdr" (T.pack "hdr3") (FastA.header $ fastARecs !! 2))
test6 = TestCase (assertEqual "FastA rec 3 seq" (T.pack "GAATTC") (FastA.sequence $ fastARecs !! 2))

-- Check ID and OTU, by convention the first and second words of the header.

fastAInput2 = T.pack $ unlines [
    ">XAA01 Methanococcus",
    "accgatgctaatgtagcatgcagcatatgcg",
    "cgagcgatctagcacgagcatgcatg",
    "cgcaggtcatcgagagtc",
    ">ZAB089 Archaeoglobus",
    "cgatatcgagagcgatcatcatgcagcagcaggcat",
    "gcagcatgcatgcncgatcggatgcatgcnngcatcga"
    ]

-- by now I have learned about the short syntax :-)

fastARecs2 = fastATextToRecords fastAInput2

record10 = fastARecs2 !! 0
record11 = fastARecs2 !! 1

test7 = "FastA ID" ~: (T.pack "XAA01") ~=? (FastA.fastAId record10)
test8 = "FastA OTU" ~: (T.pack "Methanococcus") ~=? (FastA.fastAOTU record10)
test9 = "FastA ID" ~: (T.pack "ZAB089") ~=? (FastA.fastAId record11)
test10 = "FastA OTU" ~: (T.pack "Archaeoglobus") ~=? (FastA.fastAOTU record11)

tests = TestList [
            TestLabel "FastA parser" test1
            , TestLabel "FastA parser" test2
            , TestLabel "FastA parser" test3
            , TestLabel "FastA parser" test4
            , TestLabel "FastA parser" test5
            , TestLabel "FastA parser" test6
            , TestLabel "fastA header fields" test7
            , TestLabel "fastA header fields" test8
            , TestLabel "fastA header fields" test9
            , TestLabel "fastA header fields" test10
		]

main = do
	runTestTT tests

