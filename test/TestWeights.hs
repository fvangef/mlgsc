
import Test.HUnit
import qualified Data.Text.Lazy as T
import qualified Data.Text as S
import qualified Data.Map.Strict as M

import MlgscTypes
import Weights

aln =   map T.pack [
            "GCGTTAGC",
            "GAGTTGGA",
            "CGGACTAA"
            ]

[
    (seq1, wt1)
    (seq2, wt2)
    (seq3, wt3)
] = henikoffWeightAln aln

test01 = "H wght seq1" ~: wt1 ~?= 0.31250
test02 = "H wght seq2" ~: wt2 ~?= 0.28125
test03 = "H wght seq3" ~: wt3 ~?= 0.40625

tests = TestList [
            TestLabel "Henikoff weights" test01
            , TestLabel "Henikoff weights" test02
            , TestLabel "Henikoff weights" test03
		]

main = do
	runTestTT tests

