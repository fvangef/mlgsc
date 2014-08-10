{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit

import NucModel

-- Numeric testing of NucModels:

small_prob = 0.0001 :: Double -- need to be specific, else logBase complains
scale_factor = 1000 :: Int

aln1 = [
	"ATGC-",
	"AACG-",
	"AACGG",
	"ATG--",
	"ATAAT"
	]

aln1Mod = alnToNucModel small_prob scale_factor aln1 

test_1 = TestCase (assertEqual "A@1(aln1)" 0 (probOf aln1Mod 'A' 1))

tests = TestList [
		    TestLabel "probOf" test_1
		]

main = do
	runTestTT tests
