
import Test.HUnit
--import qualified Data.Text.Lazy as T
--import qualified Data.Text as S
--import qualified Data.Map.Strict as M

import Data.Tree
import Control.Monad.Writer

import Crumbs

-- An arbitrary function of Strings to Int
arbf :: String -> Int
arbf "A" = -1
arbf "a" = 1
arbf "r" = 6
arbf "c" = 2
arbf "Z" = 5

-- Some objects to test bestByWithIndex on
objs = ["Z", "r", "a", "r", "c"]


test01 = "bestByWithIndex" ~: (bestByWithIndex objs arbf) ~?= ("r", 1)
test02 = "bestByWithIndex []" ~: (bestByWithIndex [] arbf) ~?= Crumbs.empty

-- Test dropCrumbs
-- Here is a tree of integers. We'll use the values themselves to drop the
-- crumbs, i.e. we pass the identity function as metric.

intTree =   Node 0 [
                Node 1 [
                    Node 3 [
                        Node 9 [],
                        Node 10 []
                    ],
                    Node 4 []
                ],
                Node 2 [
                    Node 5 [],
                    Node 6 [],
                    Node 7 [
                        Node 12 [],
                        Node 11 []
                    ]
                ]
            ]

test10 = "drop, int tree" ~: (dropCrumbs id intTree) ~?= (12,[1,2,0])

-- Same idea, but with a tree of strings, and using the string's length as
-- metric

stringTree =   Node "zero" [
                Node "one" [
                     Node "three" [
                         Node "nine" [],
                         Node "ten" []
                     ],
                     Node "four" []
                ],
                Node "two" [
                    Node "five" [],
                    Node "six" [],
                    Node "seven" [
                        Node "twelve" [],
                        Node "eleven" []
                    ]
                ]
            ]

test11 = "drop, str tree, length" ~: (dropCrumbs length stringTree)
                                  ~?= (4,[0,0,0])

-- debug
two = (subForest stringTree) !! 1
objs2 = subForest two

-- this time, use the second character of the string, using the ordering of Char

test12 = "drop, str tree, (!! 1)" ~: (dropCrumbs (!! 1) stringTree)
                                  ~?= ('i',[1,0])

-- followCrumbs is the "opposite" of dropCrumbs

test20 = "follow, int tree" ~: (followCrumbs [1,2,0] intTree) ~?= 12
test21 = "follow, str tree, length" ~: (followCrumbs [0,0,0] stringTree)
                                    ~?= "nine"
test22 = "follow, str tree, (!! 1)" ~: (followCrumbs [1,0] stringTree)
                                    ~?= "five"

-- followCrumbsWithTrail returns all "labels" along the path

test30 = "follow w/ trail, int tree" ~:
    (followCrumbsWithTrail [1,2,0] intTree) ~?= [0, 2, 7, 12]
test31 = "follow w/ trail, str tree,length" ~:
    (followCrumbsWithTrail [0,0,0] stringTree) ~?= ["zero","one","three","nine"]
test32 = "follow w/ trail, str tree, (!! 1)" ~:
    (followCrumbsWithTrail [1,0] stringTree) ~?= ["zero", "two", "five"]

tests = TestList [
            TestLabel "bestByWithIndex" test01
            -- , TestLabel "bestByWithIndex" test02
            , TestLabel "dropCrumbs" test10
            , TestLabel "dropCrumbs" test11
            , TestLabel "dropCrumbs" test12
            , TestLabel "followCrumbs" test20
            , TestLabel "followCrumbs" test21
            , TestLabel "followCrumbs" test22
            , TestLabel "followCrumbsWithTrail" test30
            , TestLabel "followCrumbsWithTrail" test31
            , TestLabel "followCrumbsWithTrail" test32
		]

-- Test followCrumbs, using the above cases "the other way around"

main = do
	runTestTT tests

