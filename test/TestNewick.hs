 
import Test.HUnit

import Data.Text 

import NewickParser
import NewickDumper

-- If I parse a Newick string into a tree, then dump that tree to Newick
-- again, I should obtain the same string, i.e dump(parse(nw)) == nw, or IOW
-- (dump . parse) == id. I test this on a few Newick strings below (of course
-- you have to take my word that a tree is actually parsed from the string, and
-- that said tree is dumped back to Newick).

makeNewickTestCase :: String -> Test
makeNewickTestCase newick = TestCase
    (assertEqual ("RW " ++ newick) (pack newick) nwout)
    where   nwout           = treeToNewick nwin
            (Right nwin)    = parseNewickTree newick

-- To add a test case, just add a Newick string to this list.
newicks = [
    "(A,B);"
    , "(Aeromonas,(Bacillus,Clostridium));"
    , "(A,(B,C));"
    ,"(((Deep)));"
    , "A;"
    ]

testCases = Prelude.map makeNewickTestCase newicks
tests = TestList $ Prelude.map (TestLabel "Newick R/W" ) testCases

main = do
	runTestTT tests

