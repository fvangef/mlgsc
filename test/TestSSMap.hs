{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit

-- Testing of subsequence maps (SSMaps)

-- Test a map of a single sequence, indexing dimers.

patterns = ["RR"]   -- index dimers

seq_01 = "GAATATC"

ssmap_01 = toSSMap patterns seq_01

test_01 = "get num" ~: 1 ~?= numSeq ssmap_01
test_02 = "get f(GA)" ~: 1 ~?= ssfreq ssmap_01 "GA"

-- Test getters with wobble patterns, e.g. "RXR"

-- Test scoring

-- Test merging of two maps (pattern lists must be identical)

tests = TestList [
            TestLabel "getters" test_01
        ]

main = do
    runTestTT tests
