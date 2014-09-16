import Test.HUnit
import qualified Data.Text.Lazy as T
import qualified Data.Text as S
import qualified Data.Map.Strict as M

import MlgscTypes
import FastA
import Alignment


-- Test of the OTU -> Alignment function

fastaInput = unlines [
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

fastaRecs = fastATextToRecords $ T.pack fastaInput
aln = fastARecordsToAln fastaRecs
alnMap = alnToMap aln

test01 = "aln row label" ~: (rowLabel $ aln !! 0) ~? (S.pack "Genus_A")
test02 = "aln row label" ~: (rowLabel $ aln !! 1) ~? (S.pack "Genus_B")
test03 = "aln row label" ~: (rowLabel $ aln !! 2) ~? (S.pack "Genus_A")
test04 = "aln row label" ~: (rowLabel $ aln !! 3) ~? (S.pack "Genus_C")
test05 = "aln row label" ~: (rowLabel $ aln !! 4) ~? (S.pack "Genus_B")
test06 = "aln row label" ~: (rowLabel $ aln !! 5) ~? (S.pack "Genus_C")
test07 = "aln row label" ~: (rowLabel $ aln !! 6) ~? (S.pack "Genus_C")
test08 = "aln row label" ~: (rowLabel $ aln !! 7) ~? (S.pack "Genus_D")

test11 = "aln row seq" ~: (rowSeq $ aln !! 0) ~? (S.pack "AYGCATGCGTGC")
test12 = "aln row seq" ~: (rowSeq $ aln !! 1) ~? (S.pack "AXGCATGCGTGT")
test13 = "aln row seq" ~: (rowSeq $ aln !! 2) ~? (S.pack "BXGCATGCGTGC")
test14 = "aln row seq" ~: (rowSeq $ aln !! 3) ~? (S.pack "AYGCATGCGTGC")
test15 = "aln row seq" ~: (rowSeq $ aln !! 4) ~? (S.pack "CXGCATGCATGC")
test16 = "aln row seq" ~: (rowSeq $ aln !! 5) ~? (S.pack "BYGCATGCATAC")
test17 = "aln row seq" ~: (rowSeq $ aln !! 6) ~? (S.pack "CYGCATGCATGC")
test18 = "aln row seq" ~: (rowSeq $ aln !! 7) ~? (S.pack "CZGCTTGCATGC")

test21 = "aln row weight" ~: (rowWeight $ aln !! 0) ~? 1
test22 = "aln row weight" ~: (rowWeight $ aln !! 1) ~? 1
test23 = "aln row weight" ~: (rowWeight $ aln !! 2) ~? 1
test24 = "aln row weight" ~: (rowWeight $ aln !! 3) ~? 1
test25 = "aln row weight" ~: (rowWeight $ aln !! 4) ~? 1
test26 = "aln row weight" ~: (rowWeight $ aln !! 5) ~? 1
test27 = "aln row weight" ~: (rowWeight $ aln !! 6) ~? 1
test28 = "aln row weight" ~: (rowWeight $ aln !! 7) ~? 1

(Just aln_A) = M.lookup (S.pack "Genus_A") alnMap
(Just aln_B) = M.lookup (S.pack "Genus_B") alnMap
(Just aln_C) = M.lookup (S.pack "Genus_C") alnMap
(Just aln_D) = M.lookup (S.pack "Genus_D") alnMap

-- Note: due to the way the map is constructed, the order is reverses WRT the
-- original alignment.

test31 = "Genus A; seq 1" ~: (head aln_A) ~?= (S.pack "AYGCATGCGTGC")
test32 = "Genus A; seq 2" ~: (aln_A !! 1) ~?= (S.pack "AXGCATGCGTGT")
test33 = "Genus B; seq 1" ~: (head aln_B) ~?= (S.pack "BYGCATGCATAC")
test34 = "Genus B; seq 2" ~: (aln_B !! 1) ~?= (S.pack "BXGCATGCGTGC")
test35 = "Genus C; seq 1" ~: (head aln_C) ~?= (S.pack "CZGCTTGCATGC")
test36 = "Genus C; seq 2" ~: (aln_C !! 1) ~?= (S.pack "CYGCATGCATGC")
test37 = "Genus C; seq 3" ~: (aln_C !! 2) ~?= (S.pack "CXGCATGCATGC")
test38 = "Genus D; seq 1" ~: (head aln_D) ~?= (S.pack "DXGCTTGCATGC")

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

