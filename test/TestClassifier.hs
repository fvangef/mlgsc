{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT
import Data.Binary
import Data.Tree    -- TODO: rm when debugged
import TestFileUtils
import NewickParser
import FastA
import Alignment
import Classifier
import MlgscTypes
import Output

fastaInput1 = unlines [
    ">ID0 Aeromonas", 
    "ACGTACGT",
    ">ID1 Aeromonas", 
    "ACGTACGT",
    ">ID3 Aeromonas", 
    "ACGTACGT",
    ">ID4 Aeromonas", 
    "ACGTACGT",
    ">ID5 Aeromonas", 
    "ACGTACGT",
    ">ID6 Bacillus", 
    "BCGTACGT",
    ">ID7 Bacillus", 
    "BCGTACGT",
    ">ID8 Bacillus", 
    "BCGTACGT",
    ">ID9 Clostridium", 
    "CCGTACGT",
    ">ID10 Clostridium", 
    "CCGTACGT",
    ">ID11 Clostridium", 
    "CCGTACGT",
    ">ID12 Clostridium", 
    "CCGTACGT",
    ">ID13 Geobacillus", 
    "GCGTACGT",
    ">ID14 Geobacillus", 
    "GCGTACGT",
    ">ID15 Geobacillus", 
    "GCGTACGT"
    ]

smallprob = 0.0001
scale = 1000

newick1 = "(Aeromonas,((Geobacillus,Bacillus)Bacillaceae,Clostridium)Firmicutes);"
(Right tree1) = parseNewickTree newick1

fastaRecs1 = fastATextToRecords $ LT.pack fastaInput1
aln1 = fastARecordsToAln fastaRecs1
map1 = alnToAlnMap aln1

clfr1@(PWMClassifier modtree1 _) = buildClassifier Prot smallprob scale map1 tree1 

q1 = ST.pack "ACGTACGT"


clssfr1 = buildClassifier DNA smallprob scale map1 tree1

-- TODO: should use extended crumbs, and not call these functions directly.
-- Now score sequences according to the classifier, e.g.

trail1 = classifySequence clssfr1 1 "ACGTACGT"
trail2 = classifySequence clssfr1 1 "CCGTACGT"
trail3 = classifySequence clssfr1 1 "CCGTACGG"

score1 = bestScore $ last trail1
score2 = bestScore $ last trail2
score3 = bestScore $ last trail3

taxon1 = otuName $ last trail1
taxon2 = otuName $ last trail2
taxon3 = otuName $ last trail3

test01 = "clssfr1 ACGTACGT score" ~: score1 ~?= 0
test02 = "clssfr1 CCGTACGT score" ~: score2 ~?= 0
test03 = "clssfr1 CCGTACGG score" ~: score3 ~?= (-4000)

test11 = "clssfr1 ACGTACGT OTU" ~: taxon1 ~?= "Aeromonas"
test12 = "clssfr1 CCGTACGT OTU" ~: taxon2 ~?= "Clostridium"
test13 = "clssfr1 CCGTACGG OTU" ~: taxon3 ~?= "Clostridium"

-- Check that we can save and load a classifier in binary form

test21 = TestCase (do
    removeIfExists "clssfr1.bcls"
    encodeFile "clssfr1.bcls" clssfr1
    clssfr2 <- decodeFile "clssfr1.bcls"
    assertEqual "store-read" clssfr1 clssfr2
    let trail1 = classifySequence clssfr2 1 "ACGTACGT"
    let trail2 = classifySequence clssfr2 1 "CCGTACGT"
    let trail3 = classifySequence clssfr2 1 "CCGTACGG"
    (bestScore $ last trail1) @?= 0
    (bestScore $ last trail2) @?= 0
    (bestScore $ last trail3) @?= (-4000)
    (otuName $ last trail1) @?= "Aeromonas"
    (otuName $ last trail2) @?= "Clostridium"
    (otuName $ last trail3) @?= "Clostridium"
    ) 

tests = TestList [
		TestLabel "nuc score" test01
		, TestLabel "nuc score" test02
		, TestLabel "nuc score" test03
		, TestLabel "nuc score" test11
		, TestLabel "nuc score" test12
		, TestLabel "nuc score" test13
		, TestLabel "nuc score" test21
		]

main = do
	runTestTT tests
