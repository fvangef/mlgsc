{-# LANGUAGE OverloadedStrings #-}
 
import Data.Tree
import Test.HUnit

import NewickParser


newick1 = "(A,(B,C));"
Right tree1 = parseTree newick1
child1 = head $ subForest tree1
child2 = last $ subForest tree1
child3 = head $ subForest child2
child4 = last $ subForest child2

test1 = TestCase (assertEqual "leaf A" "A" (rootLabel child1))
test2 = TestCase (assertEqual "innode" "" (rootLabel child2))
test3 = TestCase (assertEqual "leaf B" "B" (rootLabel child3))
test4 = TestCase (assertEqual "leaf C" "C" (rootLabel child4))

tests = TestList [
		TestLabel "nw parser" test1
		, TestLabel "nw parser" test2
		, TestLabel "nw parser" test3
		, TestLabel "nw parser" test4
		]

main = do
	runTestTT tests

