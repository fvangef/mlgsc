import Test.HUnit
import qualified Data.Text.Lazy as T

import FastA

fastaInput = ">hdr1\naa\ntt\n>hdr2 len > 5\ngg\ncc\n>hdr3\ngaattc"

fastARecs = fastATextToRecords $ T.pack fastaInput

test1 = TestCase (assertEqual "FastA rec 1 hdr" (T.pack "hdr1") (FastA.header $ head fastARecs))
test2 = TestCase (assertEqual "FastA rec 1 seq" (T.pack "aatt") (FastA.sequence $ head fastARecs))

test3 = TestCase (assertEqual "FastA rec 2 hdr" (T.pack "hdr2 len > 5") (FastA.header $ fastARecs !! 1))
test4 = TestCase (assertEqual "FastA rec 2 seq" (T.pack "ggcc") (FastA.sequence $ fastARecs !! 1))

test5 = TestCase (assertEqual "FastA rec 3 hdr" (T.pack "hdr3") (FastA.header $ fastARecs !! 2))
test6 = TestCase (assertEqual "FastA rec 3 seq" (T.pack "gaattc") (FastA.sequence $ fastARecs !! 2))


tests = TestList [
            TestLabel "FastA parser" test1
            , TestLabel "FastA parser" test2
            , TestLabel "FastA parser" test3
            , TestLabel "FastA parser" test4
            , TestLabel "FastA parser" test5
            , TestLabel "FastA parser" test6
		]

main = do
	runTestTT tests

