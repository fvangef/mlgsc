import qualified Data.Text.Lazy as LT

import NewickParser
import Classifier
import FastA
import Crumbs

-- Toy data for ghci

fastaInput = unlines [
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
    "ACGTACGT",
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

newick = "(Aeromonas,(Bacillus,Clostridium));"
(Right tree) = parseNewickTree newick

fastaRecs2 = fastATextToRecords $ LT.pack fastaInput
fastAMap = fastARecordsToAlnMap fastaRecs2
