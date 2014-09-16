
import Test.HUnit
import qualified Data.Text.Lazy as T
import qualified Data.Text as S
import qualified Data.Map.Strict as M

import MlgscTypes
import Weights
import Alignment

-- This example is taken from
-- http://www.cs.umd.edu/class/fall2011/cmsc858s/Weights.pdf.
-- The code below was checked against the weights given in that document.
-- Everything is ok.

aln = [
    AlnRow (S.pack "myOTU") (S.pack "GCGTTAGC") 1,
    AlnRow (S.pack "myOTU") (S.pack "GAGTTGGA") 1,
    AlnRow (S.pack "myOTU") (S.pack "CGGACTAA") 1
    ]

[AlnRow _ _ wt1, AlnRow _ _ wt2, AlnRow _ _ wt3] = henikoffWeightAln aln

-- The alignment weights are integral, so I normalize the ones from the website:
test01 = "H wght seq1" ~: wt1 ~?= round (0.31250 / 0.28125)
test02 = "H wght seq2" ~: wt2 ~?= round (0.28125 / 0.28125)
test03 = "H wght seq3" ~: wt3 ~?= round (0.40625 / 0.28125)

tests = TestList [
            TestLabel "Henikoff weights" test01
            , TestLabel "Henikoff weights" test02
            , TestLabel "Henikoff weights" test03
		]

main = do
	runTestTT tests

