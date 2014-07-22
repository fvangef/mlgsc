{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import Data.Map as Map
import Data.Tree

import MlgscTypes
import Model
import NewickParser

-- TODO: redo all tests with tight model versions, should not be too hard


-- Numeric testing of Models:

small_prob = 0.0001 :: Float -- need to be specific, else logBase complains
scale_factor = 1000 :: Int

seq_list_1 = [
	"ATGC-",
	"AACG-",
	"AACGG",
	"ATG--",
	"ATAAT"
	]

-- model_1 = pair2model strict_nt ("SeqList1", seq_list_1)
-- mat_1 = matrix model_1
min_score = round $ (fromIntegral scale_factor) * (logBase 10 small_prob)

{-
mat_1_dist_0 = mat_1 !! 0
mat_1_dist_1 = mat_1 !! 1
mat_1_dist_2 = mat_1 !! 2
mat_1_dist_3 = mat_1 !! 3
mat_1_dist_4 = mat_1 !! 4
-}

{-
test_m1d0A = TestCase (assertEqual "mat[1][A]" 0         (mat_1_dist_0 ! 'A'))
test_m1d0C = TestCase (assertEqual "mat[1][C]" min_score (mat_1_dist_0 ! 'C'))
test_m1d0d = TestCase (assertEqual "mat[1][-]" min_score (mat_1_dist_0 ! '-'))
test_m1d0G = TestCase (assertEqual "mat[1][G]" min_score (mat_1_dist_0 ! 'G'))
test_m1d0T = TestCase (assertEqual "mat[1][T]" min_score (mat_1_dist_0 ! 'T'))

test_m1d1A = TestCase (assertEqual "mat[2][A]" (-398)    (mat_1_dist_1 ! 'A'))
test_m1d1C = TestCase (assertEqual "mat[2][C]" min_score (mat_1_dist_1 ! 'C'))
test_m1d1d = TestCase (assertEqual "mat[2][-]" min_score (mat_1_dist_1 ! '-'))
test_m1d1G = TestCase (assertEqual "mat[2][G]" min_score (mat_1_dist_1 ! 'G'))
test_m1d1T = TestCase (assertEqual "mat[2][T]" (-222)    (mat_1_dist_1 ! 'T'))

test_m1d2A = TestCase (assertEqual "mat[3][A]" (-699)    (mat_1_dist_2 ! 'A'))
test_m1d2C = TestCase (assertEqual "mat[3][C]" (-398)    (mat_1_dist_2 ! 'C'))
test_m1d2d = TestCase (assertEqual "mat[3][-]" min_score (mat_1_dist_2 ! '-'))
test_m1d2G = TestCase (assertEqual "mat[3][G]" (-398)    (mat_1_dist_2 ! 'G'))
test_m1d2T = TestCase (assertEqual "mat[3][T]" min_score (mat_1_dist_2 ! 'T'))

test_m1d3A = TestCase (assertEqual "mat[4][A]" (-699)    (mat_1_dist_3 ! 'A'))
test_m1d3C = TestCase (assertEqual "mat[4][C]" (-699)    (mat_1_dist_3 ! 'C'))
test_m1d3d = TestCase (assertEqual "mat[4][-]" (-699)    (mat_1_dist_3 ! '-'))
test_m1d3G = TestCase (assertEqual "mat[4][G]" (-398)    (mat_1_dist_3 ! 'G'))
test_m1d3T = TestCase (assertEqual "mat[4][T]" min_score (mat_1_dist_3 ! 'T'))

test_m1d4A = TestCase (assertEqual "mat[5][A]" min_score (mat_1_dist_4 ! 'A'))
test_m1d4C = TestCase (assertEqual "mat[5][C]" min_score (mat_1_dist_4 ! 'C'))
test_m1d4d = TestCase (assertEqual "mat[5][-]" (-222)    (mat_1_dist_4 ! '-'))
test_m1d4G = TestCase (assertEqual "mat[5][G]" (-699)    (mat_1_dist_4 ! 'G'))
test_m1d4T = TestCase (assertEqual "mat[5][T]" (-699)    (mat_1_dist_4 ! 'T'))
-}

-- Masking a model
-- NOTE: the best way to do masking is probably at the _scoring_ level, rather
-- than at the model level. See below.
{--
 - In certain cases I want to ignore some positions in an alignment. One (easy
 - but inefficient) way of doing this is to "mask" a model by setting the score
 - of all residues to 0 at the ignored positions: the final score of a sequence
 - will not depend on those positions at all. An example would be ignoring aln
 - columns that are too poorly (or too well) conserved; I'm currently trying to
 - do this by computing a parsimony score of a phylogeny at each position (see
 - TreeScore.hs): one would then mask those positions with a too-high or too-low
 - score.
-}

{-
pos_scores :: [Int]
pos_scores = [3,0,4,5,12]

mat_2 = maskMatrix pos_scores (\n -> (n < 3) || (n > 8)) mat_1

mat_2_dist_0 = mat_2 !! 0
mat_2_dist_1 = mat_2 !! 1
mat_2_dist_2 = mat_2 !! 2
mat_2_dist_3 = mat_2 !! 3
mat_2_dist_4 = mat_2 !! 4

test_m2d0A = TestCase (assertEqual "mat[1][A]" 0         (mat_2_dist_0 ! 'A'))
test_m2d0C = TestCase (assertEqual "mat[1][C]" min_score (mat_2_dist_0 ! 'C'))
test_m2d0d = TestCase (assertEqual "mat[1][-]" min_score (mat_2_dist_0 ! '-'))
test_m2d0G = TestCase (assertEqual "mat[1][G]" min_score (mat_2_dist_0 ! 'G'))
test_m2d0T = TestCase (assertEqual "mat[1][T]" min_score (mat_2_dist_0 ! 'T'))

test_m2d1A = TestCase (assertEqual "mat[2][A]" 0         (mat_2_dist_1 ! 'A'))
test_m2d1C = TestCase (assertEqual "mat[2][C]" 0         (mat_2_dist_1 ! 'C'))
test_m2d1d = TestCase (assertEqual "mat[2][-]" 0         (mat_2_dist_1 ! '-'))
test_m2d1G = TestCase (assertEqual "mat[2][G]" 0         (mat_2_dist_1 ! 'G'))
test_m2d1T = TestCase (assertEqual "mat[2][T]" 0         (mat_2_dist_1 ! 'T'))

test_m2d2A = TestCase (assertEqual "mat[3][A]" (-699)    (mat_2_dist_2 ! 'A'))
test_m2d2C = TestCase (assertEqual "mat[3][C]" (-398)    (mat_2_dist_2 ! 'C'))
test_m2d2d = TestCase (assertEqual "mat[3][-]" min_score (mat_2_dist_2 ! '-'))
test_m2d2G = TestCase (assertEqual "mat[3][G]" (-398)    (mat_2_dist_2 ! 'G'))
test_m2d2T = TestCase (assertEqual "mat[3][T]" min_score (mat_2_dist_2 ! 'T'))

test_m2d3A = TestCase (assertEqual "mat[4][A]" (-699)    (mat_2_dist_3 ! 'A'))
test_m2d3C = TestCase (assertEqual "mat[4][C]" (-699)    (mat_2_dist_3 ! 'C'))
test_m2d3d = TestCase (assertEqual "mat[4][-]" (-699)    (mat_2_dist_3 ! '-'))
test_m2d3G = TestCase (assertEqual "mat[4][G]" (-398)    (mat_2_dist_3 ! 'G'))
test_m2d3T = TestCase (assertEqual "mat[4][T]" min_score (mat_2_dist_3 ! 'T'))

test_m2d4A = TestCase (assertEqual "mat[5][A]" 0         (mat_2_dist_4 ! 'A'))
test_m2d4C = TestCase (assertEqual "mat[5][C]" 0         (mat_2_dist_4 ! 'C'))
test_m2d4d = TestCase (assertEqual "mat[5][-]" 0         (mat_2_dist_4 ! '-'))
test_m2d4G = TestCase (assertEqual "mat[5][G]" 0         (mat_2_dist_4 ! 'G'))
test_m2d4T = TestCase (assertEqual "mat[5][T]" 0         (mat_2_dist_4 ! 'T'))
-}
-- Tests of scoring a sequence against a matrix (mat_1, in this case)

{-
score_AAAAA_exp = 0 + (-398) + (-699) + (-699) + min_score
test_sc_AAAAA = TestCase (assertEqual "score AAAAA" score_AAAAA_exp
				(matrixScoreLog mat_1 "AAAAA"))

score_ddddd_exp = min_score + min_score + min_score + (-699) + (-222)
test_sc_ddddd = TestCase (assertEqual "score -----" score_ddddd_exp
				(matrixScoreLog mat_1 "-----"))

score_ATGdC_exp = 0 + (-222) + (-398) + (-699) + min_score
test_sc_ATGdC = TestCase (assertEqual "score ATG-C" score_ATGdC_exp
				(matrixScoreLog mat_1 "ATG-C"))
-}

-- Same idea, but with a mask, which is a list of Booleans the same size as the
-- query.

seq_mask = [True, False, True, True, False]
masked = (0 :: Int)

{-
score_AAAAA_msk_exp = 0 + masked + (-699) + (-699) + masked
test_sc_AAAAA_msk = TestCase (assertEqual "score AAAAA" score_AAAAA_msk_exp
				(matrixScoreLogMask mat_1 seq_mask "AAAAA"))

score_ddddd_msk_exp = min_score + masked + min_score + (-699) + masked
test_sc_ddddd_msk = TestCase (assertEqual "score -----" score_ddddd_msk_exp
				(matrixScoreLogMask mat_1 seq_mask "-----"))

score_ATGdC_msk_exp = 0 + masked + (-398) + (-699) + masked
test_sc_ATGdC_msk = TestCase (assertEqual "score ATG-C" score_ATGdC_msk_exp
				(matrixScoreLogMask mat_1 seq_mask "ATG-C"))
-}

-- Some tests of the handling of IUPAC ambiguity codes

in1 = [('Y', 1.0)]
out1 = [('C', 0.5), ('T', 0.5)]
testIUPAC1 = TestCase (assertEqual "for Y" out1  (handleIUPAC in1))


in2 = [('M', 1.0), ('R', 0.5), ('W', 0.3)]
out2 = [('A', 0.5), ('C', 0.5), ('A', 0.25), ('G', 0.25), ('A', 0.15), ('T', 0.15)]
testIUPAC2 = TestCase (assertEqual "for MRW" out2  (handleIUPAC in2))

-- Test of the column -> probability distribution function

pair_set_1 = [
	("Ant", "AGTC"),
	("Ant", "AGTC"),
	("Ant", "AGTC"),
	("Bee", "AGTT"),
	("Bee", "AATT"),
	("Bee", "AATT"),
	("Cat", "CGGA"),
	("Cat", "CAGA"),
	("Cat", "CCGA"),
	("Cat", "CTGA"),
	("Dog", "CGGA"),
	("Dog", "CGGA"),
	("Dog", "TGGT"),
	("Dog", "TGGT"),
	("Eel", "CGGG"),
	("Eel", "C-GG"),
	("Eel", "C-GG"),
	("Eel", "C-GG")
	]

newick_1 = "((Ant,Bee),((Cat,Dog),Eel));"
(Right tree_1) = parseTree newick_1

{-
clssfr_1 = pairs2modelRTree pair_set_1 strict_nt tree_1
-- The three internal matrices of the classifier (the one at the root is not
-- used)
clssfr_1_sub_h = (matrix . rTreeData . head . kids) clssfr_1 -- (Ant,Bee)
clssfr_1_sub_l = (matrix . rTreeData . last . kids) clssfr_1 -- (Cat,Dog,Eel)
clssfr_1_sub_lh = (matrix . rTreeData . head . kids . last . kids) clssfr_1 -- (Cat,Dog)
clssfr_1_sub_ll = (matrix . rTreeData . last . kids . last . kids) clssfr_1 -- (Eel)
clssfr_1_sub_lhh = (matrix . rTreeData . head . kids . head . kids . last . kids) clssfr_1 -- (Cat)
clssfr_1_sub_lhl = (matrix . rTreeData . last . kids . head . kids . last . kids) clssfr_1 -- (Cat)

-- Check internal matrices. We check column-by-column: easier to spot problems
-- (if any ;-)

-- some common frequencies and their scores:
-- 0    -> min_score
-- 1/12 -> -1079
-- 1/8  ->  -903
-- 1/6  ->  -778
-- 1/4  ->  -602
-- 1/3  ->  -477
-- 1/2  ->  -301
-- 5/8  ->  -204
-- 3/4  ->  -125
-- 2/3  ->  -176 
-- 5/6  ->   -79
-- 1    ->     0

-- Internal matrix for the (Ant,Bee) subclade
c1shcol1_exp = fromList [('A', 0), ('C', min_score), ('-', min_score), ('G', min_score), ('T', min_score)]
c1shcol2_exp = fromList [('A', -477), ('C', min_score), ('-', min_score), ('G', -176), ('T', min_score)]
c1shcol3_exp = fromList [('A', min_score), ('C', min_score), ('-', min_score), ('G', min_score), ('T', 0)]
c1shcol4_exp = fromList [('A', min_score), ('C', -301), ('-', min_score), ('G', min_score), ('T', -301)]

test_c1shcol1 = TestCase (assertEqual "clsfr 1 intl h col 1"
			c1shcol1_exp (clssfr_1_sub_h !! 0))
test_c1shcol2 = TestCase (assertEqual "clsfr 1 intl h col 2"
			c1shcol2_exp (clssfr_1_sub_h !! 1))
test_c1shcol3 = TestCase (assertEqual "clsfr 1 intl h col 3"
			c1shcol3_exp (clssfr_1_sub_h !! 2))
test_c1shcol4 = TestCase (assertEqual "clsfr 1 intl h col 4"
			c1shcol4_exp (clssfr_1_sub_h !! 3))

-- Internal matrix for the (Cat,Dog,Eel)  subclade
c1slcol1_exp = fromList [('A', min_score), ('C', -79), ('-', min_score), ('G', min_score), ('T', -778)]
c1slcol2_exp = fromList [('A', -1079), ('C', -1079), ('-', -602), ('G', -301), ('T', -1079)]
c1slcol3_exp = fromList [('A', min_score), ('C', min_score), ('-', min_score), ('G', 0), ('T', min_score)]
c1slcol4_exp = fromList [('A', -301), ('C', min_score), ('-', min_score), ('G', -477), ('T', -778)]

test_c1slcol1 = TestCase (assertEqual "clsfr 1 intl l col 1"
			c1slcol1_exp (clssfr_1_sub_l !! 0))
test_c1slcol2 = TestCase (assertEqual "clsfr 1 intl l col 2"
			c1slcol2_exp (clssfr_1_sub_l !! 1))
test_c1slcol3 = TestCase (assertEqual "clsfr 1 intl l col 3"
			c1slcol3_exp (clssfr_1_sub_l !! 2))
test_c1slcol4 = TestCase (assertEqual "clsfr 1 intl l col 4"
			c1slcol4_exp (clssfr_1_sub_l !! 3))

-- Internal matrix for the (Cat,Dog)  subclade
c1slhcol1_exp = fromList [('A', min_score), ('C', -125), ('-', min_score), ('G', min_score), ('T', -602)]
c1slhcol2_exp = fromList [('A', -903), ('C', -903), ('-', min_score), ('G', -204), ('T', -903)]
c1slhcol3_exp = fromList [('A', min_score), ('C', min_score), ('-', min_score), ('G', 0), ('T', min_score)]
c1slhcol4_exp = fromList [('A', -125), ('C', min_score), ('-', min_score), ('G', min_score), ('T', -602)]

test_c1slhcol1 = TestCase (assertEqual "clsfr 1 intl lh col 1"
			c1slhcol1_exp (clssfr_1_sub_lh !! 0))
test_c1slhcol2 = TestCase (assertEqual "clsfr 1 intl lh col 2"
			c1slhcol2_exp (clssfr_1_sub_lh !! 1))
test_c1slhcol3 = TestCase (assertEqual "clsfr 1 intl lh col 3"
			c1slhcol3_exp (clssfr_1_sub_lh !! 2))
test_c1slhcol4 = TestCase (assertEqual "clsfr 1 intl lh col 4"
			c1slhcol4_exp (clssfr_1_sub_lh !! 3))

-- Score CGGA according to those matrices

-- CGGA vs. the (Ant,Bee) matrix

c1sh_CGGA_exp = min_score + min_score + (-176) + min_score  -- -12176
test_CGGA_by_c1sh = TestCase (assertEqual "cl1 sh CGGA" c1sh_CGGA_exp 
				(matrixScoreLog clssfr_1_sub_h "CGGA"))

-- CGGA vs. the (Cat,Dog,Eel) matrix

c1sl_CGGA_exp = (-79) + (-301) + 0 + (-301) -- -681
test_CGGA_by_c1sl = TestCase (assertEqual "cl1 sl CGGA" c1sl_CGGA_exp 
				(matrixScoreLog clssfr_1_sub_l "CGGA"))

-- CGGA vs. the (Cat,Dog) matrix

c1slh_CGGA_exp = (-125) + (-204) + 0 + (-125)  -- -454
test_CGGA_by_c1slh = TestCase (assertEqual "cl1 slh CGGA" c1slh_CGGA_exp 
				(matrixScoreLog clssfr_1_sub_lh "CGGA"))

-- CGGA vs. the (Eel) matrix

c1sll_CGGA_exp = 0 + (-602) + 0 + min_score  -- -4602
test_CGGA_by_c1sll = TestCase (assertEqual "cl1 sll CGGA" c1sll_CGGA_exp 
				(matrixScoreLog clssfr_1_sub_ll "CGGA"))

-- CGGA vs. the (Cat) matrix

c1slhh_CGGA_exp = 0 + (-602) + 0 + 0  -- -602
test_CGGA_by_c1slhh = TestCase (assertEqual "cl1 slhh CGGA"
			c1slhh_CGGA_exp 
			(matrixScoreLog clssfr_1_sub_lhh "CGGA"))

-- CGGA vs. the (Dog) matrix

c1slhl_CGGA_exp = (-301) + 0 + (-301) + 0  -- -602
test_CGGA_by_c1slhl = TestCase (assertEqual "cl1 slhl CGGA"
			c1slhl_CGGA_exp 
			(matrixScoreLog clssfr_1_sub_lhl "CGGA"))

-- Follow the decision tree for CGGA

-- 1st step: between the first two internal matrices 
score_CGGA_sh = min_score + (-176) + min_score + min_score  
score_CGGA_sl = (-79) + (-301) + 0 + (-301)     

test_clsfi_seq_3 = TestCase (assertEqual "intl mod l CGGA"
			score_CGGA_sl
			(matrixScoreLog clssfr_1_sub_l "CGGA"))

-- Now try sequences against the whole classifier

test_clsf_seq_1 = TestCase (assertEqual "clsfr 1 AGTC" ("Ant", 0)
			(modelRTreeScore clssfr_1 "AGTC"))

test_clsf_seq_2 = TestCase (assertEqual "clsfr 1 ----"
			("Eel", (3 * min_score - 125))
			(modelRTreeScore clssfr_1 "----"))

-- Here I test only the score, because Dog and Cat have the same score, and it
-- would be dangerous (and in fact wrong) to expect one over the other.

test_clsf_seq_3 = TestCase (assertEqual "clsfr 1 CGGA"
			(-602)
			(snd $ modelRTreeScore clssfr_1 "CGGA"))

-- On the other hand, masking column 2 should favour the Cat

seq_mask_2 = [True, False, True, True]

-- CGGA, masked at position 2, vs. the (Cat) matrix

c1slhh_CGGAm_exp = 0 + 0 + 0 + 0  
test_CGGAm_by_c1slhh = TestCase (assertEqual "cl1 slhh CGGAm"
		c1slhh_CGGAm_exp 
		(matrixScoreLogMask clssfr_1_sub_lhh seq_mask_2 "CGGA"))

-- CGGA, masked at position 2, vs. the (Dog) matrix

c1slhl_CGGAm_exp = (-301) + 0 + (-301) + 0  -- -602
test_CGGAm_by_c1slhl = TestCase (assertEqual "cl1 slhl CGGAm"
		c1slhl_CGGAm_exp 
		(matrixScoreLogMask clssfr_1_sub_lhl seq_mask_2 "CGGA"))

test_clsf_seq_msk_1 = TestCase (assertEqual "clsfr 1 msk CGGA"
		("Cat", 0)
		(modelRTreeScoreMask clssfr_1 seq_mask_2 "CGGA"))

cat_dog_subtree = head $ kids $ last $ kids clssfr_1

test_c1sCD_seq_msk_1 = TestCase (assertEqual "c1 sCD msk CGGA"
		("Cat", 0)
		(modelRTreeScoreMask cat_dog_subtree seq_mask_2 "CGGA"))


test_aln_1 = ["AAAAAAAAAA", "CCCCCCCCCC", "GGGGGGGGGG", "TTTTTTTTTT"]
test_mat_1 = rawProbMatrix nucleotides 0 test_aln_1 
-
-}
-- a set with some singleton sequences, for leave-one-out.

pair_set_2 = [
	("Ant", "AGTC"),
	("Bee", "AGTT"),
	("Bee", "AATT"),
	("Bee", "AATT"),
	("Cat", "CGGA"),
	("Cat", "CAGA"),
	("Cat", "CCGA"),
	("Cat", "CTGA"),
	("Dog", "CGGA"),
	("Dog", "CGGA"),
	("Dog", "TGGT"),
	("Dog", "TGGT"),
	("Eel", "CGGG"),
	("Eel", "C-GG"),
	("Eel", "C-GG"),
	("Eel", "C-GG"),
    ("Fly", "CCGG")
	]

alnMap = pairs2AlnMap pair_set_1
leaf_aln_tree_1 = fmap (seqLookup alnMap) tree_1
leaf_mod_tree_1 = fmap (pair2RawModel small_prob) leaf_aln_tree_1
merged_mod_tree_1 = mergeBalancedR leaf_mod_tree_1 
isl_mod_tree_1 = fmap (rawModel2Model small_prob scale_factor) merged_mod_tree_1 

pair_set_3 = [
    ("Connochaetes", "ATT"),
    ("Connochaetes", "ATT"),
    ("Connochaetes", "ATC"),
    ("Connochaetes", "ACC"),
    ("Gallus", "ACT"),
    ("Gallus", "GCT"),
    ("Salmo",  "GCC")
    ]

newick_2 = "((Connochaetes,Gallus),Salmo);"
(Right tree_2) = parseTree newick_2

balmodtree = balancedModelRTree small_prob scale_factor pair_set_3 tree_2

-- verified numerically that 'balmodtree' is "RoseTree {rTreeData = Model {name = "", matrix = [fromList [('A',-426),('G',-204)],fromList [('C',-90),('T',-727)],fromList [('C',-204),('T',-426)]]}, kids = [RoseTree {rTreeData = Model {name = "", matrix = [fromList [('A',-125),('G',-602)],fromList [('C',-204),('T',-426)],fromList [('C',-602),('T',-125)]]}, kids = [RoseTree {rTreeData = Model {name = "Connochaetes", matrix = [fromList [('A',0)],fromList [('C',-602),('T',-125)],fromList [('C',-301),('T',-301)]]}, kids = []},RoseTree {rTreeData = Model {name = "Gallus", matrix = [fromList [('A',-301),('G',-301)],fromList [('C',0)],fromList [('T',0)]]}, kids = []}]},RoseTree {rTreeData = Model {name = "Salmo", matrix = [fromList [('G',0)],fromList [('C',0)],fromList [('C',0)]]}, kids = []}]}"

tests = TestList [
        {-
		TestLabel "Model numerics" test_m1d0A
		, TestLabel "Model numerics" test_m1d0C
		, TestLabel "Model numerics" test_m1d0d
		, TestLabel "Model numerics" test_m1d0G
		, TestLabel "Model numerics" test_m1d0T
		, TestLabel "Model numerics" test_m1d1A
		, TestLabel "Model numerics" test_m1d1C
		, TestLabel "Model numerics" test_m1d1d
		, TestLabel "Model numerics" test_m1d1G
		, TestLabel "Model numerics" test_m1d1T
		, TestLabel "Model numerics" test_m1d2A
		, TestLabel "Model numerics" test_m1d2C
		, TestLabel "Model numerics" test_m1d2d
		, TestLabel "Model numerics" test_m1d2G
		, TestLabel "Model numerics" test_m1d2T
		, TestLabel "Model numerics" test_m1d3A
		, TestLabel "Model numerics" test_m1d3C
		, TestLabel "Model numerics" test_m1d3d
		, TestLabel "Model numerics" test_m1d3G
		, TestLabel "Model numerics" test_m1d3T
		, TestLabel "Model numerics" test_m1d4A
		, TestLabel "Model numerics" test_m1d4C
		, TestLabel "Model numerics" test_m1d4d
		, TestLabel "Model numerics" test_m1d4G
		, TestLabel "Model numerics" test_m1d4T
		, TestLabel "Matrix masking" test_m2d0A
		, TestLabel "Matrix masking" test_m2d0C
		, TestLabel "Matrix masking" test_m2d0d
		, TestLabel "Matrix masking" test_m2d0G
		, TestLabel "Matrix masking" test_m2d0T
		, TestLabel "Matrix masking" test_m2d1A
		, TestLabel "Matrix masking" test_m2d1C
		, TestLabel "Matrix masking" test_m2d1d
		, TestLabel "Matrix masking" test_m2d1G
		, TestLabel "Matrix masking" test_m2d1T
		, TestLabel "Matrix masking" test_m2d2A
		, TestLabel "Matrix masking" test_m2d2C
		, TestLabel "Matrix masking" test_m2d2d
		, TestLabel "Matrix masking" test_m2d2G
		, TestLabel "Matrix masking" test_m2d2T
		, TestLabel "Matrix masking" test_m2d3A
		, TestLabel "Matrix masking" test_m2d3C
		, TestLabel "Matrix masking" test_m2d3d
		, TestLabel "Matrix masking" test_m2d3G
		, TestLabel "Matrix masking" test_m2d3T
		, TestLabel "Matrix masking" test_m2d4A
		, TestLabel "Matrix masking" test_m2d4C
		, TestLabel "Matrix masking" test_m2d4d
		, TestLabel "Matrix masking" test_m2d4G
		, TestLabel "Matrix masking" test_m2d4T
		, TestLabel "IUPAC 1" testIUPAC1
		, TestLabel "IUPAC 2" testIUPAC2
		 TestLabel "Internal matrix" test_c1shcol1
		, TestLabel "Internal matrix" test_c1shcol2
		, TestLabel "Internal matrix" test_c1shcol3
		, TestLabel "Internal matrix" test_c1shcol4
		, TestLabel "Internal matrix" test_c1slcol1
		, TestLabel "Internal matrix" test_c1slcol2
		, TestLabel "Internal matrix" test_c1slcol3
		, TestLabel "Internal matrix" test_c1slcol4
		, TestLabel "Internal matrix" test_c1slhcol1
		, TestLabel "Internal matrix" test_c1slhcol2
		, TestLabel "Internal matrix" test_c1slhcol3
		, TestLabel "Internal matrix" test_c1slhcol4
		, TestLabel "Seq score acc. to matrix" test_CGGA_by_c1sh
		, TestLabel "Seq score acc. to matrix" test_CGGA_by_c1sl
		, TestLabel "Seq score acc. to matrix" test_CGGA_by_c1slh
		, TestLabel "Seq score acc. to matrix" test_sc_AAAAA
		, TestLabel "Seq score acc. to matrix" test_sc_ddddd
		, TestLabel "Seq score acc. to matrix" test_sc_ATGdC
		, TestLabel "Seq score acc. to matrix" test_sc_AAAAA_msk
		, TestLabel "Seq score acc. to matrix" test_sc_ddddd_msk
		, TestLabel "Seq score acc. to matrix" test_sc_ATGdC_msk
		, TestLabel "Seq score acc. to matrix" test_CGGA_by_c1sh 
		, TestLabel "Seq score acc. to matrix" test_CGGA_by_c1sl 
		, TestLabel "Seq score acc. to matrix" test_CGGA_by_c1slh 
		, TestLabel "Seq score acc. to matrix" test_CGGA_by_c1sll 
		, TestLabel "Seq score acc. to matrix" test_CGGA_by_c1slhh 
		, TestLabel "Seq score acc. to matrix" test_CGGA_by_c1slhl 
		, TestLabel "Seq score acc. to matrix" test_clsfi_seq_3
		, TestLabel "Classify sequence" test_clsf_seq_1
		, TestLabel "Classify sequence" test_clsf_seq_2
		, TestLabel "Classify sequence" test_clsf_seq_3
		, TestLabel "Classify sequence" test_CGGAm_by_c1slhh
		, TestLabel "Classify sequence" test_CGGAm_by_c1slhl
		, TestLabel "Classify sequence" test_c1sCD_seq_msk_1
		, TestLabel "Classify sequence" test_clsf_seq_msk_1
        -}
		-- , TestLabel "col->dist 1" testCol2Distr1
		-- , TestLabel "col->dist 2" testCol2Distr2
		-- , TestLabel "col->dist 3" testCol2Distr3
		]

main = do
	runTestTT tests
