
import Test.HUnit
--import qualified Data.Text.Lazy as T
--import qualified Data.Text as S
--import qualified Data.Map.Strict as M

import Data.Tree
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


test1 = "bestByWithIndex" ~: (bestByWithIndex objs arbf) ~?= ("r", 1)

tests = TestList [
            TestLabel "bestByWithIndex" test1
		]

main = do
	runTestTT tests

