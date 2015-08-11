import Test.HUnit

import qualified Data.Text.Lazy as LT
import qualified Data.Text as ST
import Data.Tree (flatten)
import Data.Tuple (swap)

import IDTree
import NewickParser
import NewickDumper
import FastA
import Alignment
import qualified Data.Map as M
import Data.Map (findWithDefault)

fastaInput1 = unlines [
    ">ID0 Aeromonas", 
    "AB-CGTACGT",
    ">ID1 Aeromonas", 
    "AA-CGTACGT",
    ">ID3 Aeromonas", 
    "AA-CGTACGT",
    ">ID4 Aeromonas", 
    "AA-CGTACGT",
    ">ID5 Aeromonas", 
    "AA-CGTACGT",
    ">ID6 Bacillus", 
    "BC-CGTACGT",
    ">ID7 Bacillus", 
    "BB-CGTACGT",
    ">ID8 Bacillus", 
    "BA-CGTACGT",
    ">ID9 Clostridium", 
    "CC-CGTACGT",
    ">ID10 Clostridium", 
    "CB-CGTACGT",
    ">ID11 Clostridium", 
    "CA-CGTACGT",
    ">ID12 Clostridium", 
    "CA-CGTACGT",
    ">ID13 Geobacillus", 
    "GC-CGTACGT",
    ">ID14 Geobacillus", 
    "GB-CGTACGT",
    ">ID15 Geobacillus", 
    "GA-CGTACGT",
    ">ID16 Moorea", 
    "M--CGTACGT",
    ">ID17 Moorea", 
    "M--CGTACGT"
    ]


-- A paraphyletic tree, by IDs - not by taxa. This is what one would get from a
-- tree-building program like RAxML, etc.

newick = "(((ID0,ID9,ID10),(ID1,(ID3,ID4),ID5),(ID11,ID12),(ID16,ID17)),((ID6,ID13),((ID7,ID14),(ID8,ID15))));"

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
-- I "know" that this works, in this example, so I assume I can use the Just
-- c'tor :-)
(Just rnMap) = renumberedTaxonMap tree1 fastaRecs1

rnTree = renumTaxonTree rnMap tree1
rnFastaRecs = renumFastaRecs rnMap fastaRecs1

{- The resulting renamed tree is as follows: note that it is no longer
paraphyletic, if we consider that some_genus.n != some_genus.m iff m != n. Note
also that Moorea did not need any renaming as it did not exhibit any paraphyly.

             /-----------+ Aeromonas.2  
             |                          
       /-----+-----------+ Clostridium.3
       |     |                          
       |     \-----------+ Clostridium.2
       |                                
       |     /-----------+ Aeromonas.1  
       |     |                          
       |     |     /-----+ Aeromonas.1  
       +-----+-----+                    
 /-----+     |     \-----+ Aeromonas.1  
 |     |     |                          
 |     |     \-----------+ Aeromonas.1  
 |     |                                
 |     |     /-----------+ Clostridium.1
 |     +-----+                          
 |     |     \-----------+ Clostridium.1
 |     |                                
=+     |     /-----------+ Moorea       
 |     \-----+                          
 |           \-----------+ Moorea       
 |                                      
 |           /-----------+ Bacillus.3   
 |     /-----+                          
 |     |     \-----------+ Geobacillus.3
 |     |                                
 \-----+           /-----+ Bacillus.2   
       |     /-----+                    
       |     |     \-----+ Geobacillus.2
       \-----+                          
             |     /-----+ Bacillus.1   
             \-----+                    
                   \-----+ Geobacillus.1

The condensed tree is now:

             /-----------+ Aeromonas.2      ID0
             |                          
       /-----+-----------+ Clostridium.3    ID9
       |     |                          
       |     \-----------+ Clostridium.2    ID10
       |                                
 /-----+-----------------+ Aeromonas.1      ID1,3-5
 |     |                                
 |     +-----------------+ Clostridium.1    ID11,12
 |     |                                
 |     \-----------------+ Moorea           ID16,17 
=+                                      
 |           /-----------+ Bacillus.3       ID6 
 |     /-----+                          
 |     |     \-----------+ Geobacillus.3    ID13
 |     |                                
 \-----+           /-----+ Bacillus.2       ID7
       |     /-----+                    
       |     |     \-----+ Geobacillus.2    ID14
       \-----+                          
             |     /-----+ Bacillus.1       ID8 
             \-----+                    
                   \-----+ Geobacillus.1    ID15

-}


test01 = "ID tree" ~: 0 ~?= 0
test02 = "ID tree" ~: 0 ~?= 0

tests = TestList [
      TestLabel "nuc score" test01
      , TestLabel "nuc score" test02
      ]

main = do
   runTestTT tests
