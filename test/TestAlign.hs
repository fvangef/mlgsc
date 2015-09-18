{-# LANGUAGE OverloadedStrings #-}
 
import Test.HUnit

import Align
import Alignment
import PWMModel
import PepModel
import NucModel
import MlgscTypes

scale_factor = 1000.0
small_prob = 0.0001

aln1 = [
        AlnRow "my-OTU" "AAATTTAAA" 1,
        AlnRow "my-OTU" "AAATTTAAA" 1,
        AlnRow "my-OTU" "AAATTTAAA" 1
        ]


aln1Mod = NucPWMModel $ alnToNucModel small_prob scale_factor "my-OTU" aln1
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

aln2 = [
    AlnRow "some-OTU" "CE--HQPSVIMKPFDMENLVHHIRQIHG" 1,
    AlnRow "some-OTU" "MDIPKMPKIIVKPFDFVVFINRIRELVS" 1,
    AlnRow "some-OTU" "MDIPKMPKIIVKPFDFVVFINRIRELVS" 1,
    AlnRow "some-OTU" "MNLSPQPKIIMKPFDMEVLANRVRQLVG" 1,
    AlnRow "some-OTU" "TPLSKRPLFIIKPFDMDVLVSRIRQLKN" 1,
    AlnRow "some-OTU" "IERLRQPSVIMKPFDMENLTSHIRQVSG" 1,
    AlnRow "some-OTU" "RE--KQPSVIMKPFDMENLVYHIRQVHG" 1
    ]

scoring = defScScheme

mod2 = PepPWMModel $ alnToPepModel small_prob scale_factor "some-OTU" aln2

input5 = "MNLSPQPKIIMKPFDMEVLANRVRQLVG"
exp5 = input5
test5 = TestCase (assertEqual "small real 1" exp5 (msalign scoring mod2 input5))

input6 = "REKQPSVIMKPFDMENLVYHIRQVHG"
exp6 = "RE--KQPSVIMKPFDMENLVYHIRQVHG"
test6 = TestCase (assertEqual "small real 2" exp6 (msalign scoring mod2 input6))

input7 = "VAILGYMDIPKMPKIIVKPFDFVVFINRIRELVSPCWFL"
exp7 = "MDIPKMPKIIVKPFDFVVFINRIRELVS"
test7 = TestCase (assertEqual "small real 3" exp7 (msalign scoring mod2 input7))

input8 = "VAILGYREKQPSVIMKPFDMENLVYHIRQVHGPCWFL"
exp8 = "RE--KQPSVIMKPFDMENLVYHIRQVHG"
test8 = TestCase (assertEqual "small real 4" exp8 (msalign scoring mod2 input8))

input9 = "VAILGYMDIPKMPKPFDFVVFINRIRELVSPCWFL"
exp9 = "MDIPKMPK----PFDFVVFINRIRELVS"
test9 = TestCase (assertEqual "small real 5" exp9 (msalign scoring mod2 input9))

input10 = "VAILGYMDIPKMPKIIVKVFINRIRELVSPCWFL"
exp10 = "MDIPKMPKIIVK-----VFINRIRELVS"

test10 = TestCase (assertEqual "small real 6" exp10 (msalign scoring mod2 input10))

-- This very small matrix was used to find and fix a bug, captured in test cases
-- below.

aln3 = [
    AlnRow "prob1" "CE--HQPS" 1,
    AlnRow "prob1" "MDIPKMPK" 1,
    AlnRow "prob1" "MDIPKMPK" 1,
    AlnRow "prob1" "MNLSPQPK" 1,
    AlnRow "prob1" "TPLSKRPL" 1,
    AlnRow "prob1" "IERLRQPS" 1,
    AlnRow "prob1" "RE--KQPS" 1
    ]

mod3 = PepPWMModel $ alnToPepModel small_prob scale_factor "prob1" aln3

input11 = "REKQPS"
-- This used to yield, wrongly, "-E--KQPS".
exp11 = "RE--KQPS"

test11 = TestCase (assertEqual "small real 7" exp11 (msalign scoring mod3 input11))


-- Real-life, full-length alignment (long enough to classify to genus in most
-- cases)

aln4 = [
    AlnRow "rltaxon" "EFCNILNDYLLNQRDIVVTGVAKDGIEALKLIQEKKPDLLILDIIMPHLDGLGVLEKINTMNIEKLPRVIVLSAVGQDKITQRAITLGADYYVVKPFDMDVFTKRIRQMFN-" 1,
    AlnRow "rltaxon" "ELVSMLESYVAAQDDMEVIGTAYNGQECLNLLKDKQPDVLVLDIIMPHLDGLAVLEKMRHIERLRQPSVIMLTAFGQEDVTKKAVDLGASYFILKPFDMENLTSHIRQVSG-" 1,
    AlnRow "rltaxon" "EFGDILYEYLNNQEDIEVVGVARDGIEAYELIVEKLPDIAILDIIMPHLDGLGVLEKIGATAISKRPLFIILSAVGQDKITQRALALGAEYYVVKPFDMEVLISRIRQLKN-" 1,
    AlnRow "rltaxon" "EFAKLLKEYMNQYEDIEVLELAKDGLQAIEMIISKKPDVVVLDIIMPNLDGLGVLERLSTMQLEYRPIFIMLSAIGQDVFVQRAVNLGAEYYIIKPFDVEVLVTRIRQL--Y" 1,
    AlnRow "rltaxon" "ELVTLLDEYISNQSDMEVIGTAYNGQDCLHMLEEKQPDILILDIIMPHLDGLAVLEKVRT-SFEHQPNVIMLTAFGQEDVTKKAVELGASYFILKPFDMENLVHHIRQIYG-" 1,
    AlnRow "rltaxon" "DFCELLKEFINQQDDFVLVGIANNGLEALEIINSEAPDVMVLDIIMPHLDGIGVLEKISTGTVAHKPKVIMLTAFGQESVTSRAVELGADYYILKPFDFAVLATRIRQLAD-" 1,
    AlnRow "rltaxon" "QFNMLLTEVFNSQPDFMVVGNSYDGVETLKVVEEQKPDLLMLDIIMPYLDGIGVIENLSTMK--TKPNIIVISAVGQENISQKAVNMGALYYFVKPFDLNVMIERVRQLV-F" 1,
    AlnRow "rltaxon" "EFCNILSDYLLNQRDIMVTGIANDGVEALKLVEEKKPDLIILDIIMPHLDGLGVLEKLNSIDITPMPRVIVLSAVGQDKITQRAINLGADYYVVKPFDMDVFTKRIRQMFN-" 1,
    AlnRow "rltaxon" "EFCNILNDYLLNQRDIMVTGIAKDGVEALKLIEEKRPDLVVLDIIMPHLDGLGVLEELNNMNMEPMPRIIVLSAVGQDKITQRAITLGADYYVVKPFDMDVFTQRIRQMFN-" 1,
    AlnRow "rltaxon" "DFCDILKEYLQRQADIEVVGIAKDGLEAVELIQEMTPDLVVLDIIMPHLDGLGVLERLNKLNLESFPKVIILSAVGQDKITQRAINLGADYYVIKPFDFEVFIERIREMTS-" 1
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

mod4 = PepPWMModel $  alnToPepModel small_prob scale_factor "real-life" aln4

input12 = "EFCNILNDYLLNQRDIVVTGVAKDGIEALKLIQEKKPDLLILDIIMPHLDGLGVLEKINTMNIEKLPRVIVLSAVGQDKITQRAITLGADYYVVKPFDMDVFTKRIRQMFN"
exp12 = "EFCNILNDYLLNQRDIVVTGVAKDGIEALKLIQEKKPDLLILDIIMPHLDGLGVLEKINTMNIEKLPRVIVLSAVGQDKITQRAITLGADYYVVKPFDMDVFTKRIRQMFN-"

test12 = TestCase (assertEqual "small real 7" exp12 (msalign scoring mod4 input12))

input13 = "ELVTLLDEYISNQSDMEVIGTAYNGQDCLHMLEEKQPDILILDIIMPHLDGLAVLEKVRTSFEHQPNVIMLTAFGQEDVTKKAVELGASYFILKPFDMENLVHHIRQIYG"
exp13 = "ELVTLLDEYISNQSDMEVIGTAYNGQDCLHMLEEKQPDILILDIIMPHLDGLAVLEKVRT-SFEHQPNVIMLTAFGQEDVTKKAVELGASYFILKPFDMENLVHHIRQIYG-"

test13 = TestCase (assertEqual "small real 7" exp13 (msalign scoring mod4 input13))

-- for this one the function yields a different alignment, but with an equal
-- score...
input14 = "EFAKLLKEYMNQYEDIEVLELAKDGLQAIEMIISKKPDVVVLDIIMPNLDGLGVLERLSTMQLEYRPIFIMLSAIGQDVFVQRAVNLGAEYYIIKPFDVEVLVTRIRQLY"
exp14 = "EFAKLLKEYMNQYEDIEVLELAKDGLQAIEMIISKKPDVVVLDIIMPNLDGLGVLERLSTMQLEYRPIFIMLSAIGQDVFVQRAVNLGAEYYIIKPFDVEVLVTRIRQLY--"

test14 = TestCase (assertEqual "small real 7" exp14 (msalign scoring mod4 input14))

tests = TestList [
        TestLabel "msalign" test1
        , TestLabel "msalign" test2
        , TestLabel "msalign" test3
        , TestLabel "msalign" test4
        , TestLabel "msalign" test5
        , TestLabel "msalign" test6
        , TestLabel "msalign" test7
        , TestLabel "msalign" test8
        , TestLabel "msalign" test9
        , TestLabel "msalign" test10
        , TestLabel "msalign" test11
        , TestLabel "msalign" test12
        , TestLabel "msalign" test13
        , TestLabel "msalign" test14
        ]

main = do
    runTestTT tests

