import Test.HUnit

import MlgscTypes

test_seq2mol1 = TestCase (assertEqual "nuc 1" Nucleotide
	(seqToMolecule "GAATTC"))
test_seq2mol2 = TestCase (assertEqual "nuc 2" Nucleotide
	(seqToMolecule "AAAGTYGA"))
test_seq2mol3 = TestCase (assertEqual "pep 3" AminoAcid
	(seqToMolecule "RTAAGYCVINKLGGTA"))

tests = TestList [
		TestLabel "GSC types" test_seq2mol1
		, TestLabel "GSC types" test_seq2mol2
		, TestLabel "GSC types" test_seq2mol3
		]

main = do
	runTestTT tests
