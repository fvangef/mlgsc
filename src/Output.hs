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
import NucModel
import OutputFormatStringParser

-- formats an output line according to a format string (à la printf). ARguments
-- are: format string, original query as a FastA record, query sequence after
-- alignment, and path through the tree (as returned by trailToExtendedTaxo).
-- TODO: shouldn't the parsing of the fmt string be done just once? The format
-- itself is constant, even though the resulting string is different for every
-- query.

-- TODO: pass only the argments that vary from query to query, and the rest
-- using a Reader monad.
-- TODO: could add a high-level way of passing an ER threshold

formatResult :: FmtString -> FastA -> Sequence -> Trail -> ST.Text
formatResult fmtString query alnQry trail = 
    ST.concat $ map (evalFmtComponent 0 query alnQry trail) format
        where (Right format) = parseOutputFormatString fmtString

{- This function should NOT take parameters as a record, or as a data type, or
 - from a Reader, because it is used from different programs (at least mlgsc and
 - mlgsc_xval) which have their own types for params, etc. Accordingly, all
 - parameters to this function are explicit. It is possible, however, to call it
 - from Reader functions (e.g. in mlgsc). -}

evalFmtComponent :: Int 
                    -> FastA
                    -> Sequence
                    -> Trail
                    -> FmtComponent
                    -> ST.Text
evalFmtComponent hlMinER query alnQry trail component = case component of
    Header          -> LT.toStrict $ FastA.header query
    QueryLength     -> ST.pack $ show $ LT.length $ FastA.sequence query
    ID              -> LT.toStrict $ fastAId query
    AlignedQuery    -> alnQry
    (Path min_er)   -> if min_er > 0 -- precedence to low-level
                        then trailToPath min_er trail 
                        else trailToPath hlMinER trail
    Score           -> ST.pack $ show leafScore
                        where (_,leafScore,_) = last trail
    (Literal c)     -> ST.pack [c]

-- Takes an extended trail (i.e., a list of (OTU name, bes
-- score) tuples) and formats it as a taxonomy line, with empty labels remplaced
-- by 'unnamed' and labels followed by the log10 of the evidence ratio between
-- the best and second-best likelihoods.

trailToExtendedTaxo :: Int -> Trail -> ST.Text
trailToExtendedTaxo min_er trail = ST.intercalate (ST.pack "; ") $ getZipList erLbls
    where   labels = ZipList $ tail $ map (\(lbl,_,_) -> lbl) trail
            bests = ZipList $ init $ map (\(_,best,_) -> best) trail
            seconds = ZipList $ init $ map (\(_,_,second) -> second) trail
            log10ers = log10evidenceRatio <$> (ZipList $ repeat 1000) <*> seconds <*> bests
            good_log10ers = cutAtFirstPoorER min_er log10ers
            erLbls = toERlbl <$> labels <*> good_log10ers
            toERlbl lbl er = ST.concat [lblOrUndef,
                                 ST.pack " (", 
                                 ST.pack erStr,
                                 ST.pack ")"]
                where erStr = case printf "%.0g" er :: String of
                            "Infinity" -> "*"
                            otherwise -> printf "%.0g" er :: String
                      lblOrUndef = if ST.empty == lbl
                                        then ST.pack "unnamed"
                                        else lbl

trailToPath :: Int -> Trail -> ST.Text
trailToPath min_er trail = ST.intercalate (ST.pack "; ") $ getZipList erLbls
    where   labels  = ZipList $ map (\(lbl,_,_)   -> lbl) trail
            bests   = ZipList $ map (\(_,best,_) -> best) trail
            seconds = ZipList $ map (\(_,_,snd)   -> snd) trail
            log10ers = log10evidenceRatio <$>
                       (ZipList $ repeat 1000) <*> seconds <*> bests
            good_log10ers = cutAtFirstPoorER min_er log10ers
            erLbls = toERlbl <$> labels <*> good_log10ers
            toERlbl lbl er = ST.concat [lblOrUndef,
                                 ST.pack " (", 
                                 ST.pack erStr,
                                 ST.pack ")"]
                where erStr = case printf "%.0g" er :: String of
                            "Infinity" -> "*"
                            otherwise -> printf "%.0g" er :: String
                      lblOrUndef = if ST.empty == lbl
                                        then ST.pack "unnamed"
                                        else lbl
-- Computes the base-10 log of the evidence ratio, i.e. exp(delta-AIC / 2),
-- except that I use delta-AIC' (in which the factor 2 is dropped, so I avoid
-- having to multiply by 2 only to divide by 2 again just after).

log10evidenceRatio :: Int -> Int -> Int -> Double
log10evidenceRatio scaleFactor bestScore secondBestScore = logBase 10 er
    where   l_min = scoreTologLikelihood scaleFactor bestScore
            l_sec = scoreTologLikelihood scaleFactor secondBestScore
            er = exp(deltaAIC' l_min l_sec) 

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

-- TODO: deltaAIC' -> deltaAIC
deltaAIC' :: Double -> Double -> Double
deltaAIC' l1 l2 = - (l1 - l2)

-- Takes a threshold and a ZipList of evidence ratios and drops any and all
-- after the first below the threshold. The idea is to keep those nodes in the
-- path that are well enough supprted, but to drop anything beyond (and
-- including) the first poorly-supported node.

cutAtFirstPoorER :: Int -> ZipList Double -> ZipList Double
cutAtFirstPoorER min_er ers = ZipList $ takeWhile (>= fromIntegral min_er) $ getZipList ers 
