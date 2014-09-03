import Test.HUnit
import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT
import Data.Binary

import TestFileUtils
import NewickParser
import FastA
import Classifier
import Crumbs

fastaInput1 = unlines [
    ">Aeromonas", 
    "ACGTACGT",
    ">Aeromonas", 
    "ACGTACGT",
    ">Aeromonas", 
    "ACGTACGT",
    ">Aeromonas", 
    "ACGTACGT",
    ">Aeromonas", 
    "ACGTACGT",
    ">Bacillus", 
    "BCGTACGT",
    ">Bacillus", 
    "BCGTACGT",
    ">Bacillus", 
    "BCGTACGT",
    ">Clostridium", 
    "CCGTACGT",
    ">Clostridium", 
    "CCGTACGT",
    ">Clostridium", 
    "CCGTACGT",
    ">Clostridium", 
    "CCGTACGT"
    ]

smallprob = 0.0001
scale = 1000

newick1 = "(Aeromonas,(Bacillus,Clostridium));"
(Right tree1) = parseNewickTree newick1

fastaRecs1 = fastATextToRecords $ LT.pack fastaInput1
fastAMap1 = fastARecordsToAlnMap fastaRecs1

clssfr1 = buildNucClassifier smallprob scale fastAMap1 tree1


-- Now score sequences according to the classifier, e.g.

(s1, c1) = dropCrumbs (scoreCrumbs $ ST.pack "ACGTACGT") clssfr1 
(s2, c2) = dropCrumbs (scoreCrumbs $ ST.pack "CCGTACGT") clssfr1
(s3, c3) = dropCrumbs (scoreCrumbs $ ST.pack "CCGTACGG") clssfr1

test01 = "clssfr1 ACGTACGT score" ~: s1 ~?= 0
test02 = "clssfr1 CCGTACGT score" ~: s2 ~?= 0
test03 = "clssfr1 CCGTACGG score" ~: s3 ~?= (-4000)

-- And recover the OTU name from the original tree:

otu1 = followCrumbs c1 tree1
otu2 = followCrumbs c2 tree1
otu3 = followCrumbs c3 tree1

test11 = "clssfr1 ACGTACGT OTU" ~: otu1 ~?= ST.pack "Aeromonas"
test12 = "clssfr1 CCGTACGT OTU" ~: otu2 ~?= ST.pack "Clostridium"
test13 = "clssfr1 CCGTACGG OTU" ~: otu3 ~?= ST.pack "Clostridium"

-- Check that we can save and load a classifier in binary form

test21 = TestCase (do
    removeIfExists "clssfr1.bcls"
    encodeFile "clssfr1.bcls" clssfr1
    clssfr2 <- decodeFile "clssfr1.bcls"
    assertEqual "store-read" clssfr1 clssfr2
    let (sc21, cr21) = dropCrumbs (scoreCrumbs $ ST.pack "ACGTACGT") clssfr2 
    let (sc22, cr22) = dropCrumbs (scoreCrumbs $ ST.pack "CCGTACGT") clssfr2
    let (sc23, cr23) = dropCrumbs (scoreCrumbs $ ST.pack "CCGTACGG") clssfr2
    sc21 @?= 0
    sc22 @?= 0
    sc23 @?= (-4000)
    followCrumbs cr21 tree1 @?= ST.pack "Aeromonas"
    followCrumbs cr22 tree1 @?= ST.pack "Clostridium"
    followCrumbs cr23 tree1 @?= ST.pack "Clostridium"
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

