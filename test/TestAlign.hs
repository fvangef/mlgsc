{-# LANGUAGE OverloadedStrings #-}
 
import Test.HUnit

import Align
import Alignment
import CladeModel
import NucModel
import MlgscTypes

scale_factor = 1000.0
small_prob = 0.0001

aln1 = [
        AlnRow "my-OTU" "AAATTTAAA" 1,
        AlnRow "my-OTU" "AAATTTAAA" 1,
        AlnRow "my-OTU" "AAATTTAAA" 1
        ]


aln1Mod = alnToNucModel small_prob scale_factor aln1
scsc = ScoringScheme (-2) (scoringSchemeMap (absentResScore aln1Mod))

input1 = "AAATTTAAA"
exp1 = input1
test1 = TestCase (assertEqual "ident" exp1 (msalign scsc aln1Mod input1))

input2 = "AAATTAAA"
exp2 = "AAATT-AAA"
test2 = TestCase (assertEqual "1 gap" exp2 (msalign scsc aln1Mod input2))

input3 = "GGGGGGGGGAAATTTAAACCCCCCCCCC"
exp3 = input1
test3 = TestCase (assertEqual "overhangs" exp3 (msalign scsc aln1Mod input3))

input4 = "GGGGGGGGGAAATAAACCCCCCCCCC"
exp4 = "AAAT--AAA"
test4 = TestCase (assertEqual "gap+overhangs" exp4 (msalign scsc aln1Mod input4))

-- Small, real example (protein)

{-
aln2 = [
	"CE--HQPSVIMKPFDMENLVHHIRQIHG",
	"MDIPKMPKIIVKPFDFVVFINRIRELVS",
	"MDIPKMPKIIVKPFDFVVFINRIRELVS",
	"MNLSPQPKIIMKPFDMEVLANRVRQLVG",
	"TPLSKRPLFIIKPFDMDVLVSRIRQLKN",
	"IERLRQPSVIMKPFDMENLTSHIRQVSG",
	"RE--KQPSVIMKPFDMENLVYHIRQVHG"
	]

scoring2 = ScoringScheme {
	scoreFunction = seqISLMatScore
	, gapOPenalty = -2
	}
	

rpm2 = tightRawProbMatrix 0.0001 aln2
isl2 = matrixMap (round . (*scale_factor) . (logBase 10)) rpm2

input5 = "MNLSPQPKIIMKPFDMEVLANRVRQLVG"
exp5 = input5
test5 = TestCase (assertEqual "small real 1" exp5 (msalign scoring isl2 input5))

input6 = "REKQPSVIMKPFDMENLVYHIRQVHG"
exp6 = "RE--KQPSVIMKPFDMENLVYHIRQVHG"
test6 = TestCase (assertEqual "small real 2" exp6 (msalign scoring2 isl2 input6))

input7 = "VAILGYMDIPKMPKIIVKPFDFVVFINRIRELVSPCWFL"
exp7 = "MDIPKMPKIIVKPFDFVVFINRIRELVS"
test7 = TestCase (assertEqual "small real 3" exp7 (msalign scoring2 isl2 input7))

input8 = "VAILGYREKQPSVIMKPFDMENLVYHIRQVHGPCWFL"
exp8 = "RE--KQPSVIMKPFDMENLVYHIRQVHG"
test8 = TestCase (assertEqual "small real 4" exp8 (msalign scoring isl2 input8))

input9 = "VAILGYMDIPKMPKPFDFVVFINRIRELVSPCWFL"
exp9 = "MDIPKMP----KPFDFVVFINRIRELVS"
test9 = TestCase (assertEqual "small real 5" exp9 (msalign scoring2 isl2 input9))

input10 = "VAILGYMDIPKMPKIIVKVFINRIRELVSPCWFL"
exp10 = "MDIPKMPKIIVK-----VFINRIRELVS"

test10 = TestCase (assertEqual "small real 6" exp10 (msalign scoring2 isl2 input10))

-- This very small matrix was used to find and fix a bug, captured in test cases
-- below.

aln3 = [
	"CE--HQPS",
	"MDIPKMPK",
	"MDIPKMPK",
	"MNLSPQPK",
	"TPLSKRPL",
	"IERLRQPS",
	"RE--KQPS"
	]

rpm3 = tightRawProbMatrix 0.0001 aln3
isl3 :: ISLProbMatrix
isl3 = matrixMap (round . (*scale_factor) . (logBase 10)) rpm3

input11 = "REKQPS"
-- This used to yield, wrongly, "-E--KQPS".
exp11 = "RE--KQPS"

test11 = TestCase (assertEqual "small real 7" exp11 (msalign scoring isl3 input11))


-- Real-life, full-length alignment (long enough to classify to genus in most
-- cases)

aln4 = [
	"EFCNILNDYLLNQRDIVVTGVAKDGIEALKLIQEKKPDLLILDIIMPHLDGLGVLEKINTMNIEKLPRVIVLSAVGQDKITQRAITLGADYYVVKPFDMDVFTKRIRQMFN-",
	"ELVSMLESYVAAQDDMEVIGTAYNGQECLNLLKDKQPDVLVLDIIMPHLDGLAVLEKMRHIERLRQPSVIMLTAFGQEDVTKKAVDLGASYFILKPFDMENLTSHIRQVSG-",
	"EFGDILYEYLNNQEDIEVVGVARDGIEAYELIVEKLPDIAILDIIMPHLDGLGVLEKIGATAISKRPLFIILSAVGQDKITQRALALGAEYYVVKPFDMEVLISRIRQLKN-",
	"EFAKLLKEYMNQYEDIEVLELAKDGLQAIEMIISKKPDVVVLDIIMPNLDGLGVLERLSTMQLEYRPIFIMLSAIGQDVFVQRAVNLGAEYYIIKPFDVEVLVTRIRQL--Y",
	"ELVTLLDEYISNQSDMEVIGTAYNGQDCLHMLEEKQPDILILDIIMPHLDGLAVLEKVRT-SFEHQPNVIMLTAFGQEDVTKKAVELGASYFILKPFDMENLVHHIRQIYG-",
	"DFCELLKEFINQQDDFVLVGIANNGLEALEIINSEAPDVMVLDIIMPHLDGIGVLEKISTGTVAHKPKVIMLTAFGQESVTSRAVELGADYYILKPFDFAVLATRIRQLAD-",
	"QFNMLLTEVFNSQPDFMVVGNSYDGVETLKVVEEQKPDLLMLDIIMPYLDGIGVIENLSTMK--TKPNIIVISAVGQENISQKAVNMGALYYFVKPFDLNVMIERVRQLV-F",
	"EFCNILSDYLLNQRDIMVTGIANDGVEALKLVEEKKPDLIILDIIMPHLDGLGVLEKLNSIDITPMPRVIVLSAVGQDKITQRAINLGADYYVVKPFDMDVFTKRIRQMFN-",
	"EFCNILNDYLLNQRDIMVTGIAKDGVEALKLIEEKRPDLVVLDIIMPHLDGLGVLEELNNMNMEPMPRIIVLSAVGQDKITQRAITLGADYYVVKPFDMDVFTQRIRQMFN-",
	"DFCDILKEYLQRQADIEVVGIAKDGLEAVELIQEMTPDLVVLDIIMPHLDGLGVLERLNKLNLESFPKVIILSAVGQDKITQRAINLGADYYVIKPFDFEVFIERIREMTS-"
	]

{- The genera are as follows:
>Clostridium
>Bacillus
>Clostridium
>Clostridium
>Geobacillus
>Desulfotomaculum
>Caldicellulosiruptor
>Clostridium
>Clostridium
>Alkaliphilus
-}

rpm4 = tightRawProbMatrix 0.0001 aln4
isl4 :: ISLProbMatrix
isl4 = matrixMap (round . (*scale_factor) . (logBase 10)) rpm4

input12 = "EFCNILNDYLLNQRDIVVTGVAKDGIEALKLIQEKKPDLLILDIIMPHLDGLGVLEKINTMNIEKLPRVIVLSAVGQDKITQRAITLGADYYVVKPFDMDVFTKRIRQMFN"
exp12 = "EFCNILNDYLLNQRDIVVTGVAKDGIEALKLIQEKKPDLLILDIIMPHLDGLGVLEKINTMNIEKLPRVIVLSAVGQDKITQRAITLGADYYVVKPFDMDVFTKRIRQMFN-"

test12 = TestCase (assertEqual "small real 7" exp12 (msalign scoring isl4 input12))

input13 = "ELVTLLDEYISNQSDMEVIGTAYNGQDCLHMLEEKQPDILILDIIMPHLDGLAVLEKVRTSFEHQPNVIMLTAFGQEDVTKKAVELGASYFILKPFDMENLVHHIRQIYG"
exp13 = "ELVTLLDEYISNQSDMEVIGTAYNGQDCLHMLEEKQPDILILDIIMPHLDGLAVLEKVRT-SFEHQPNVIMLTAFGQEDVTKKAVELGASYFILKPFDMENLVHHIRQIYG-"

test13 = TestCase (assertEqual "small real 7" exp13 (msalign scoring2 isl4 input13))

-- for this one the function yields a different alignment, but with an equal
-- score...
input14 = "EFAKLLKEYMNQYEDIEVLELAKDGLQAIEMIISKKPDVVVLDIIMPNLDGLGVLERLSTMQLEYRPIFIMLSAIGQDVFVQRAVNLGAEYYIIKPFDVEVLVTRIRQLY"
exp14 = "EFAKLLKEYMNQYEDIEVLELAKDGLQAIEMIISKKPDVVVLDIIMPNLDGLGVLERLSTMQLEYRPIFIMLSAIGQDVFVQRAVNLGAEYYIIKPFDVEVLVTRIRQLY--"

test14 = TestCase (assertEqual "small real 7" exp14 (msalign scoring2 isl4 input14))
-- A smaller version of aln4, used to debug some unexpected behaviour

aln5 = [
	"IRQMFN-",
	"IRQVSG-",
	"IRQLKN-",
	"IRQL--Y",
	"IRQIYG-",
	"IRQLAD-",
	"VRQLV-F",
	"IRQMFN-",
	"IRQMFN-",
	"IREMTS-"
	]

rpm5 = tightRawProbMatrix 0.0001 aln5
isl5 :: ISLProbMatrix
isl5 = matrixMap (round . (*scale_factor) . (logBase 10)) rpm5
-}

tests = TestList [
		TestLabel "msalign" test1
		, TestLabel "msalign" test2
		, TestLabel "msalign" test3
		, TestLabel "msalign" test4
		-- , TestLabel "msalign" test5
		-- , TestLabel "msalign" test6
		-- , TestLabel "msalign" test7
		-- , TestLabel "msalign" test8
		-- , TestLabel "msalign" test9
		-- , TestLabel "msalign" test10
		-- , TestLabel "msalign" test11
		-- , TestLabel "msalign" test12
		-- , TestLabel "msalign" test13
		-- , TestLabel "msalign" test14
		]

main = do
	runTestTT tests

