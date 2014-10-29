import Test.HUnit
import qualified Data.Text.Lazy as T
import qualified Data.Text as S
import qualified Data.Map.Strict as M

import MlgscTypes
import FastA
import Alignment

-- Test of the OTU -> Alignment function

fastaInput = unlines [
    ">ID_01 Genus_A",
    "AXGCAT",
    "GCGTGT",
    ">ID_02 Genus_B",
    "BXG",
    "CAT",
    "GCG",
    "TGC",
    ">ID_03 Genus_A",
    "AYGC",
    "ATGC",
    "GTGC",
    ">ID_04 Genus_C",
    "CXGCATGCATGC",
    ">ID_05 Genus_B",
    "BYGCATGCATAC",
    ">ID_06 Genus_C",
    "CYGCATGCATGC",
    ">ID_07 Genus_C",
    "CZGCTTGCATGC",
    ">ID_08 Genus_D",
    "DXGCTTGCATGC"
    ]

fastaRecs = fastATextToRecords $ T.pack fastaInput
aln = fastARecordsToAln fastaRecs
alnMap = alnToAlnMap aln

test01 = "aln row label" ~: (rowLabel $ aln !! 0) ~?= (S.pack "ID_01 Genus_A")
test02 = "aln row label" ~: (rowLabel $ aln !! 1) ~?= (S.pack "ID_02 Genus_B")
test03 = "aln row label" ~: (rowLabel $ aln !! 2) ~?= (S.pack "ID_03 Genus_A")
test04 = "aln row label" ~: (rowLabel $ aln !! 3) ~?= (S.pack "ID_04 Genus_C")
test05 = "aln row label" ~: (rowLabel $ aln !! 4) ~?= (S.pack "ID_05 Genus_B")
test06 = "aln row label" ~: (rowLabel $ aln !! 5) ~?= (S.pack "ID_06 Genus_C")
test07 = "aln row label" ~: (rowLabel $ aln !! 6) ~?= (S.pack "ID_07 Genus_C")
test08 = "aln row label" ~: (rowLabel $ aln !! 7) ~?= (S.pack "ID_08 Genus_D")

test41 = "aln row Id" ~: (rowId $ aln !! 0) ~?= (S.pack "ID_01")
test42 = "aln row Id" ~: (rowId $ aln !! 1) ~?= (S.pack "ID_02")
test43 = "aln row Id" ~: (rowId $ aln !! 2) ~?= (S.pack "ID_03")
test44 = "aln row Id" ~: (rowId $ aln !! 3) ~?= (S.pack "ID_04")
test45 = "aln row Id" ~: (rowId $ aln !! 4) ~?= (S.pack "ID_05")
test46 = "aln row Id" ~: (rowId $ aln !! 5) ~?= (S.pack "ID_06")
test47 = "aln row Id" ~: (rowId $ aln !! 6) ~?= (S.pack "ID_07")
test48 = "aln row Id" ~: (rowId $ aln !! 7) ~?= (S.pack "ID_08")

test51 = "aln row OTU" ~: (rowOTU $ aln !! 0) ~?= (S.pack "Genus_A")
test52 = "aln row OTU" ~: (rowOTU $ aln !! 1) ~?= (S.pack "Genus_B")
test53 = "aln row OTU" ~: (rowOTU $ aln !! 2) ~?= (S.pack "Genus_A")
test54 = "aln row OTU" ~: (rowOTU $ aln !! 3) ~?= (S.pack "Genus_C")
test55 = "aln row OTU" ~: (rowOTU $ aln !! 4) ~?= (S.pack "Genus_B")
test56 = "aln row OTU" ~: (rowOTU $ aln !! 5) ~?= (S.pack "Genus_C")
test57 = "aln row OTU" ~: (rowOTU $ aln !! 6) ~?= (S.pack "Genus_C")
test58 = "aln row OTU" ~: (rowOTU $ aln !! 7) ~?= (S.pack "Genus_D")

test11 = "aln row seq" ~: (rowSeq $ aln !! 0) ~?= (S.pack "AXGCATGCGTGT")
test12 = "aln row seq" ~: (rowSeq $ aln !! 1) ~?= (S.pack "BXGCATGCGTGC")
test13 = "aln row seq" ~: (rowSeq $ aln !! 2) ~?= (S.pack "AYGCATGCGTGC")
test14 = "aln row seq" ~: (rowSeq $ aln !! 3) ~?= (S.pack "CXGCATGCATGC")
test15 = "aln row seq" ~: (rowSeq $ aln !! 4) ~?= (S.pack "BYGCATGCATAC")
test16 = "aln row seq" ~: (rowSeq $ aln !! 5) ~?= (S.pack "CYGCATGCATGC")
test17 = "aln row seq" ~: (rowSeq $ aln !! 6) ~?= (S.pack "CZGCTTGCATGC")
test18 = "aln row seq" ~: (rowSeq $ aln !! 7) ~?= (S.pack "DXGCTTGCATGC")

test21 = "aln row weight" ~: (rowWeight $ aln !! 0) ~?= 1
test22 = "aln row weight" ~: (rowWeight $ aln !! 1) ~?= 1
test23 = "aln row weight" ~: (rowWeight $ aln !! 2) ~?= 1
test24 = "aln row weight" ~: (rowWeight $ aln !! 3) ~?= 1
test25 = "aln row weight" ~: (rowWeight $ aln !! 4) ~?= 1
test26 = "aln row weight" ~: (rowWeight $ aln !! 5) ~?= 1
test27 = "aln row weight" ~: (rowWeight $ aln !! 6) ~?= 1
test28 = "aln row weight" ~: (rowWeight $ aln !! 7) ~?= 1

(Just aln_A) = M.lookup (S.pack "Genus_A") alnMap
(Just aln_B) = M.lookup (S.pack "Genus_B") alnMap
(Just aln_C) = M.lookup (S.pack "Genus_C") alnMap
(Just aln_D) = M.lookup (S.pack "Genus_D") alnMap

test31 = "Genus A; seq 1" ~: (rowSeq $ aln_A !! 0) ~?= (S.pack "AYGCATGCGTGC")
test32 = "Genus A; seq 2" ~: (rowSeq $ aln_A !! 1) ~?= (S.pack "AXGCATGCGTGT")
test33 = "Genus B; seq 1" ~: (rowSeq $ aln_B !! 0) ~?= (S.pack "BYGCATGCATAC")
test34 = "Genus B; seq 2" ~: (rowSeq $ aln_B !! 1) ~?= (S.pack "BXGCATGCGTGC")
test35 = "Genus C; seq 1" ~: (rowSeq $ aln_C !! 0) ~?= (S.pack "CZGCTTGCATGC")
test36 = "Genus C; seq 2" ~: (rowSeq $ aln_C !! 1) ~?= (S.pack "CYGCATGCATGC")
test37 = "Genus C; seq 3" ~: (rowSeq $ aln_C !! 2) ~?= (S.pack "CXGCATGCATGC")
test38 = "Genus D; seq 1" ~: (rowSeq $ aln_D !! 0) ~?= (S.pack "DXGCTTGCATGC")

tests = TestList [
            TestLabel "row label" test01,
            TestLabel "row label" test02,
            TestLabel "row label" test03,
            TestLabel "row label" test04,
            TestLabel "row label" test05,
            TestLabel "row label" test06,
            TestLabel "row label" test07,
            TestLabel "row label" test08,
            TestLabel "row seq" test11,
            TestLabel "row seq" test12,
            TestLabel "row seq" test13,
            TestLabel "row seq" test14,
            TestLabel "row seq" test15,
            TestLabel "row seq" test16,
            TestLabel "row seq" test17,
            TestLabel "row seq" test18,
            TestLabel "row weight" test21,
            TestLabel "row weight" test22,
            TestLabel "row weight" test23,
            TestLabel "row weight" test24,
            TestLabel "row weight" test25,
            TestLabel "row weight" test26,
            TestLabel "row weight" test27,
            TestLabel "row weight" test28,
            TestLabel "lookup" test31,
            TestLabel "lookup" test32,
            TestLabel "lookup" test33,
            TestLabel "lookup" test34,
            TestLabel "lookup" test35,
            TestLabel "lookup" test36,
            TestLabel "lookup" test37,
            TestLabel "lookup" test38,
            TestLabel "lookup" test41,
            TestLabel "lookup" test42,
            TestLabel "lookup" test43,
            TestLabel "lookup" test44,
            TestLabel "lookup" test45,
            TestLabel "lookup" test46,
            TestLabel "lookup" test47,
            TestLabel "lookup" test48,
            TestLabel "lookup" test51,
            TestLabel "lookup" test52,
            TestLabel "lookup" test53,
            TestLabel "lookup" test54,
            TestLabel "lookup" test55,
            TestLabel "lookup" test56,
            TestLabel "lookup" test57,
            TestLabel "lookup" test58
		]

main = do
	runTestTT tests
