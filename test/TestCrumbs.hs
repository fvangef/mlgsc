
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

res1 = dropCrumbsM id intTree 
test10 = "drop, int tree" ~: (runWriter res1) ~?= (12,[1,2,0])

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

res2 = dropCrumbsM length stringTree 
test11 = "drop, str tree, length" ~: (runWriter res2) ~?= (4,[0,0,0])

-- this time, use the second character of the string, using the ordering of Char

res3 = dropCrumbsM (!! 1) stringTree
test12 = "drop, str tree, (!! 1)" ~: (runWriter res3) ~?= ('i',[1,0])

tests = TestList [
            TestLabel "bestByWithIndex" test01
            -- , TestLabel "bestByWithIndex" test02
            , TestLabel "dropCrumbs" test10
            , TestLabel "dropCrumbs" test11
            , TestLabel "dropCrumbs" test12
		]

main = do
	runTestTT tests

