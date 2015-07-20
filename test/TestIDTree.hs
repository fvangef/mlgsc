import Test.HUnit

import qualified Data.Text.Lazy as LT
import qualified Data.Text as ST
import Data.Tree (flatten)
import Data.Tuple (swap)

import IDTree
import NewickParser
import FastA
import Alignment
import qualified Data.Map as M
import Data.Map (findWithDefault)

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
    "GCGTACGT",
    ">ID16 Moorea", 
    "GCGTACGT",
    ">ID17 Moorea", 
    "GCGTACGT"
    ]


-- A paraphyletic tree, by IDs - not by taxa. This is what one would get from a
-- tree-building program like RAxML, etc.

newick = "(((ID0,ID9,ID10),(ID1,(ID3,ID4),ID5),(ID11,ID12),(ID16,ID17)),((ID6,ID13),((ID7,ID13),(ID8,ID13))));"

-- The genera are as follows - again, note that the tree is (intentionally)
-- paraphyletic, with most taxa mixed up in several branches:

{-
              /------------+ Aeromonas  
              |                         
        /-----+------------+ Clostridium
        |     |                         
        |     \------------+ Clostridium
        |                               
        |     /------------+ Aeromonas  
        |     |                         
        |     |     /------+ Aeromonas  
        +-----+-----+                   
 /------+     |     \------+ Aeromonas  
 |      |     |                         
 |      |     \------------+ Aeromonas  
 |      |                               
 |      |     /------------+ Clostridium
 |      +-----+                         
 |      |     \------------+ Clostridium
 |      |                               
=+      |     /------------+ Moorea     
 |      \-----+                         
 |            \------------+ Moorea     
 |                                      
 |            /------------+ Bacillus   
 |      /-----+                         
 |      |     \------------+ Geobacillus
 |      |                               
 \------+           /------+ Bacillus   
        |     /-----+                   
        |     |     \------+ Geobacillus
        \-----+                         
              |     /------+ Bacillus   
              \-----+                   
                    \------+ Geobacillus
-}

(Right tree1) = parseNewickTree newick

fastaRecs1 = fastATextToRecords $ LT.pack fastaInput1
id2tax = idToTaxonMap fastaRecs1
-- The map's keys and values are lazy text; we want both strict:
rnMap = renumberedTaxonMap tree1 fastaRecs1


aln1 = fastARecordsToAln fastaRecs1
map1 = alnToAlnMap aln1


test01 = "ID tree" ~: 0 ~?= 0
test02 = "ID tree" ~: 0 ~?= 0

tests = TestList [
      TestLabel "nuc score" test01
      , TestLabel "nuc score" test02
      ]

main = do
   runTestTT tests
