module Output where

-- TODO: restrict exports to only those functions that are needed by callers.

import Control.Applicative

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.Text as ST
import qualified Data.Text.IO as STIO
import Text.Printf

import Data.Binary (decodeFile)
import Data.Tree
--import System.Directory
--
import MlgscTypes
import FastA
import Crumbs (followExtendedCrumbsWithTrail)
import NucModel
import OutputFormatStringParser


-- formats an output line according to a format string (à la printf). ARguments
-- are: format string, original query as a FastA record, query sequence after
-- alignment, and path through the tree (as returned by trailToExtendedTaxo).

formatResult :: FmtString -> FastA -> Sequence -> ST.Text -> ST.Text
formatResult fmtString query alnQry path = 
    ST.concat $ map (evalFmtComponent query alnQry path) format
        where (Right format) = parseOutputFormatString fmtString

evalFmtComponent :: FastA -> Sequence -> ST.Text -> FmtComponent -> ST.Text
evalFmtComponent query _ _ Header = LT.toStrict $ FastA.header query
evalFmtComponent _ alnQry _ AlignedQuery = alnQry
evalFmtComponent query _ _ QueryLength = ST.pack $ show $
    LT.length $ FastA.sequence query
evalFmtComponent _ _ path Path = path
evalFmtComponent _ _ _ (Literal c) = ST.pack [c]

-- formats a header and a classification, with an arrow ('->') in between 

stdOutputLine :: LT.Text -> ST.Text -> ST.Text
stdOutputLine header pred =
    ST.concat [LT.toStrict header, ST.pack "\t->\t", pred]

-- as above, but prints the query as well.

extOutputLine :: LT.Text -> ST.Text -> Sequence -> ST.Text
extOutputLine header pred query =
    ST.concat [LT.toStrict header, ST.pack "\t->\t", pred,
        ST.pack " [", query, ST.pack "]"]

-- Takes an extended trail (i.e., a list of (OTU name, best score, secod-best
-- score) tuples) and formats it as a taxonomy line, with empty labels remplaced
-- by 'unnamed' and labels followed by the log10 of the evidence ratio between
-- the best and second-best likelihoods.

trailToExtendedTaxo :: Trail -> ST.Text
trailToExtendedTaxo trail = ST.intercalate (ST.pack "; ") $ getZipList erLbls
    where   labels = ZipList $ tail $ map (\(lbl,_,_) -> lbl) trail
            bests = ZipList $ init $ map (\(_,best,_) -> best) trail
            seconds = ZipList $ init $ map (\(_,_,second) -> second) trail
            ers = evidenceRatio' <$> (ZipList $ repeat 1000) <*> seconds <*> bests
            erLbls = toERlbl <$> labels <*> ers
            toERlbl lbl er = ST.concat [lblOrUndef,
                                 ST.pack " (", 
                                 ST.pack erStr,
                                 ST.pack ")"]
                where erStr = case printf "%.0g" (logBase 10 er) :: String of
                            "Infinity" -> "*"
                            otherwise -> printf "%.0g" (logBase 10 er) :: String
                      lblOrUndef = if ST.empty == lbl
                                        then ST.pack "unnamed"
                                        else lbl

-- Computes the evidence ratio, i.e. exp(delta-AIC / 2), except that I use
-- delta-AIC' (in which the factor 2 is dropped, so I avoid having to multiply
-- by 2 only to divide by 2 again just after).

evidenceRatio' :: Int -> Int -> Int -> Double
evidenceRatio' scaleFactor bestScore secondBestScore = 
        exp(deltaAIC' l_min l_sec)
    where   l_min = scoreTologLikelihood scaleFactor bestScore
            l_sec = scoreTologLikelihood scaleFactor secondBestScore

-- Converts a model score (which is a scaled, rounded log-likelihood (log base
-- 10)) to a log-likelihood (log base e, i.e. ln). To do this, we _divide_ by
-- the scale factor to get an unscaled log10-likelihood, and then divide by
-- log10(e) to get a ln-based likelihood.

scoreTologLikelihood :: Int -> Int -> Double
scoreTologLikelihood scaleFactor score = log10Likelihood / logBase 10 e
    where   log10Likelihood = fromIntegral score / fromIntegral scaleFactor
            e = exp(1.0)
            
-- Computes the difference in AIC of two log-likelihoods, taking into account
-- that the number of parameters k is in our case the same in any two models,
-- and this cancels out, i.e. delta AIC = AIC1 - AIC2 = 2k -2 ln (L_1) - (2k -
-- 2 ln(L_2)) = -2 (ln (L_1) - ln (L_2)). Since the arguments are already _log_
-- likelihoods, the expression simplifies to -2 (l_1 - l_2), where l_1 =
-- ln(L_1), etc. I also drop the constant 2, since we'd be dividing by 2 right
-- away in evidenceRatio anyway.

deltaAIC' :: Double -> Double -> Double
deltaAIC' l1 l2 = - (l1 - l2)
