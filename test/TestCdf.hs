import Test.HUnit

import Cdf

ecdf_1 = empiricalCdf [1..5]

test1 = TestCase (assertEqual "ecdf [1..5] -1" 0.0 (ecdf ecdf_1 (-1)))
test2 = TestCase (assertEqual "ecdf [1..5] 0" 0.0 (ecdf ecdf_1 0))
test3 = TestCase (assertEqual "ecdf [1..5] 1" 0.2 (ecdf ecdf_1 1))
test4 = TestCase (assertEqual "ecdf [1..5] 2" 0.4 (ecdf ecdf_1 2))
test5 = TestCase (assertEqual "ecdf [1..5] 3" 0.6 (ecdf ecdf_1 3))
test6 = TestCase (assertEqual "ecdf [1..5] 4" 0.8 (ecdf ecdf_1 4))
test7 = TestCase (assertEqual "ecdf [1..5] 5" 1.0 (ecdf ecdf_1 5))
test8 = TestCase (assertEqual "ecdf [1..5] 6" 1.0 (ecdf ecdf_1 6))

ecdf_2 = empiricalCdf [2, 4, 6, 8, 10]

test9 = TestCase (assertEqual "ecdf [1..10] -1" 0.0 (ecdf ecdf_2 (-1)))
test10 = TestCase (assertEqual "ecdf [1..10] 0" 0.0 (ecdf ecdf_2 0))
test11 = TestCase (assertEqual "ecdf [1..10] 1" 0.0 (ecdf ecdf_2 1))
test12 = TestCase (assertEqual "ecdf [1..10] 2" 0.2 (ecdf ecdf_2 2))
test13 = TestCase (assertEqual "ecdf [1..10] 3" 0.2 (ecdf ecdf_2 3))
test14 = TestCase (assertEqual "ecdf [1..10] 4" 0.4 (ecdf ecdf_2 4))
test15 = TestCase (assertEqual "ecdf [1..10] 5" 0.4 (ecdf ecdf_2 5))
test16 = TestCase (assertEqual "ecdf [1..10] 6" 0.6 (ecdf ecdf_2 6))
test17 = TestCase (assertEqual "ecdf [1..10] 7" 0.6 (ecdf ecdf_2 7))
test18 = TestCase (assertEqual "ecdf [1..10] 8" 0.8 (ecdf ecdf_2 8))
test19 = TestCase (assertEqual "ecdf [1..10] 9" 0.8 (ecdf ecdf_2 9))
test20 = TestCase (assertEqual "ecdf [1..10] 10" 1.0 (ecdf ecdf_2 10))
test21 = TestCase (assertEqual "ecdf [1..10] 11" 1.0 (ecdf ecdf_2 11))

ecdf_3 = empiricalCdf [1, 2, 2, 3, 3]

test22 = TestCase (assertEqual "ecdf multi -1" 0.0 (ecdf ecdf_3 (-1)))
test23 = TestCase (assertEqual "ecdf multi 0" 0.0 (ecdf ecdf_3 0))
test24 = TestCase (assertEqual "ecdf multi 1" 0.2 (ecdf ecdf_3 1))
test25 = TestCase (assertEqual "ecdf multi 2" 0.6 (ecdf ecdf_3 2))
test26 = TestCase (assertEqual "ecdf multi 3" 1.0 (ecdf ecdf_3 3))
test27 = TestCase (assertEqual "ecdf multi 4" 1.0 (ecdf ecdf_3 4))
test28 = TestCase (assertEqual "ecdf multi 5" 1.0 (ecdf ecdf_3 5))
test29 = TestCase (assertEqual "ecdf multi 6" 1.0 (ecdf ecdf_3 6))

tests = TestList [
		TestLabel "ecdf 1" test1
		, TestLabel "ecdf 1" test2
		, TestLabel "ecdf 1" test3
		, TestLabel "ecdf 1" test4
		, TestLabel "ecdf 1" test5
		, TestLabel "ecdf 1" test6
		, TestLabel "ecdf 1" test7
		, TestLabel "ecdf 1" test8
		, TestLabel "ecdf 1" test9
		, TestLabel "ecdf 1" test10
		, TestLabel "ecdf 1" test11
		, TestLabel "ecdf 1" test12
		, TestLabel "ecdf 1" test13
		, TestLabel "ecdf 1" test14
		, TestLabel "ecdf 1" test15
		, TestLabel "ecdf 1" test16
		, TestLabel "ecdf 1" test17
		, TestLabel "ecdf 1" test18
		, TestLabel "ecdf 1" test19
		, TestLabel "ecdf 1" test20
		, TestLabel "ecdf 1" test21
		, TestLabel "ecdf 1" test22
		, TestLabel "ecdf 1" test23
		, TestLabel "ecdf 1" test24
		, TestLabel "ecdf 1" test25
		, TestLabel "ecdf 1" test26
		, TestLabel "ecdf 1" test27
		, TestLabel "ecdf 1" test28
		, TestLabel "ecdf 1" test29
		]

main = do
	runTestTT tests

