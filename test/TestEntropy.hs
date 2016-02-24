{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import qualified Data.Text as T
import Data.Binary
  
import Alignment
import PWMModel
import PepModel
import PWMModel

-- Numeric testing of PepModels:

small_prob = 0.0001 :: Double -- need to be specific, else logBase complains
small_score = round (scale_factor * (logBase 10 small_prob))
scale_factor = 1000 :: Double

test_tolerance = 0.001

test_aln1 = [
    AlnRow "my-OTU" "VIKD" 1,
    AlnRow "my-OTU" "VARE" 1,
    AlnRow "my-OTU" "VARN" 1,
    AlnRow "my-OTU" "VIRA" 1
    ]

aln1Mod = alnToPepModel small_prob scale_factor "my-OTU" test_aln1 
aln1PWMMod = PepPWMModel aln1Mod


-- p(V) = 1, log2(P(V)) = 0 -> H(col 1) = 0
exp_01 = 0.0
obt_01 = colEntropy aln1PWMMod 1
test_01 = "VVVV" ~: (abs(exp_01 - obt_01) <= test_tolerance) ~?= True

-- p(I) = p(A) = 1/2, log2(1/2) = -1 -> H(col 2) = -(2*(1/2 * -1)) = 1
exp_02 = 1.0
obt_02 = colEntropy aln1PWMMod 2
test_02 = "IAAI" ~: (abs(exp_02 - obt_02) <= test_tolerance) ~?= True

-- p(K) = 1/4, p(R) = 3/4, log2(1/4) = -2, log2(3/4) =~ -0.41,
-- -> H(col 3) =~ 1/4 * (-2) + 3/4 * (-0.41) =~ (-0.81)
exp_03 = 0.811
obt_03 = colEntropy aln1PWMMod 3
test_03 = "KRRR" ~: (abs(exp_03 - obt_03) <= test_tolerance) ~?= True

-- p(.) = 1/4 -> H(col 4) = -(4 * 1/4 * (-2)) = 2
exp_04 = 2.0
obt_04 = colEntropy aln1PWMMod 4
test_04 = "DENA" ~: (abs(exp_04 - obt_04) <= test_tolerance) ~?= True
                             
-- Test model length

tests = TestList [
            test_01, test_02, test_03, test_04
        ]

main = do
    runTestTT tests
