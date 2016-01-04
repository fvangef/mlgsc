{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import qualified Data.Text as T
import Data.Binary
  
import TestFileUtils
import PWMModel
import PepModel
import NucModel
import PWMModel
import Alignment

-- Numeric testing of PepModels:

small_prob = 0.0001 :: Double -- need to be specific, else logBase complains
small_score = round (scale_factor * (logBase 10 small_prob))
scale_factor = 1000 :: Double

test_aln1 = [
    AlnRow "my-OTU" "VIKD-" 1,
    AlnRow "my-OTU" "VARE-" 1,
    AlnRow "my-OTU" "VARNX" 1,
    AlnRow "my-OTU" "VIK--" 1,
    AlnRow "my-OTU" "VIHQY" 1
    ]

aln1Mod = alnToPepModel small_prob scale_factor "my-OTU" test_aln1 
aln1PWMMod = PepPWMModel aln1Mod


test_01 = "VVVVV" ~: (colEntropy aln1PWMMod 1) ~?= 0.0
                             
-- Test model length

tests = TestList [
            TestLabel "pepScoreOf" test_01
        ]

main = do
    runTestTT tests
