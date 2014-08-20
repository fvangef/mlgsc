import Test.HUnit
import qualified Data.Text.Lazy as T
import qualified Data.Text as S
import qualified Data.Map.Strict as M

import MlgscTypes
import FastA

fastaInput = ">hdr1\naa\ntt\n>hdr2 len > 5\ngg\ncc\n>hdr3\ngaattc"

fastARecs = fastATextToRecords $ T.pack fastaInput

test1 = TestCase (assertEqual "FastA rec 1 hdr" (T.pack "hdr1") (FastA.header $ head fastARecs))
test2 = TestCase (assertEqual "FastA rec 1 seq" (T.pack "aatt") (FastA.sequence $ head fastARecs))

test3 = TestCase (assertEqual "FastA rec 2 hdr" (T.pack "hdr2 len > 5") (FastA.header $ fastARecs !! 1))
test4 = TestCase (assertEqual "FastA rec 2 seq" (T.pack "ggcc") (FastA.sequence $ fastARecs !! 1))

test5 = TestCase (assertEqual "FastA rec 3 hdr" (T.pack "hdr3") (FastA.header $ fastARecs !! 2))
test6 = TestCase (assertEqual "FastA rec 3 seq" (T.pack "gaattc") (FastA.sequence $ fastARecs !! 2))

-- Test of the OTU -> Alignment function

fastaInput2 = unlines [
    ">Genus_A",
    "AXGCAT",
    "GCGTGT",
    ">Genus_B",
    "BXG",
    "CAT",
    "GCG",
    "TGC",
    ">Genus_A",
    "AYGC",
    "ATGC",
    "GTGC",
    ">Genus_C",
    "CXGCATGCATGC",
    ">Genus_B",
    "BYGCATGCATAC",
    ">Genus_C",
    "CYGCATGCATGC",
    ">Genus_C",
    "CZGCTTGCATGC",
    ">Genus_D",
    "DXGCTTGCATGC"
    ]

fastaRecs2 = fastATextToRecords $ T.pack fastaInput2
fastAMap = fastARecordsToAlnMap fastaRecs2

(Just aln_A) = M.lookup (S.pack "Genus_A") fastAMap
(Just aln_B) = M.lookup (S.pack "Genus_B") fastAMap
(Just aln_C) = M.lookup (S.pack "Genus_C") fastAMap
(Just aln_D) = M.lookup (S.pack "Genus_D") fastAMap

test7 = "Genus A; seq 1" ~: (head aln_A) ~?= (S.pack "AYGCATGCGTGC")
test8 = "Genus A; seq 2" ~: (aln_A !! 1) ~?= (S.pack "AXGCATGCGTGT")
test9 = "Genus B; seq 1" ~: (head aln_B) ~?= (S.pack "BYGCATGCATAC")
test10 = "Genus B; seq 2" ~: (aln_B !! 1) ~?= (S.pack "BXGCATGCGTGC")
test11 = "Genus C; seq 1" ~: (head aln_C) ~?= (S.pack "CZGCTTGCATGC")
test12 = "Genus C; seq 2" ~: (aln_C !! 1) ~?= (S.pack "CYGCATGCATGC")
test13 = "Genus C; seq 3" ~: (aln_C !! 2) ~?= (S.pack "CXGCATGCATGC")
test14 = "Genus D; seq 1" ~: (head aln_D) ~?= (S.pack "DXGCTTGCATGC")

tests = TestList [
            TestLabel "FastA parser" test1
            , TestLabel "FastA parser" test2
            , TestLabel "FastA parser" test3
            , TestLabel "FastA parser" test4
            , TestLabel "FastA parser" test5
            , TestLabel "FastA parser" test6
            , TestLabel "OTU -> aln map" test7
            , TestLabel "OTU -> aln map" test8
            , TestLabel "OTU -> aln map" test9
            , TestLabel "OTU -> aln map" test10
            , TestLabel "OTU -> aln map" test11
            , TestLabel "OTU -> aln map" test12
            , TestLabel "OTU -> aln map" test13
            , TestLabel "OTU -> aln map" test14
		]

main = do
	runTestTT tests

