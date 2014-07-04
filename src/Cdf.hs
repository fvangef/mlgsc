module Cdf (EmpiricalCDF, empiricalCdf, ecdf) where

import Data.Map (Map, (!), fromList, size, lookupLE)
import Data.List (sort)

data EmpiricalCDF a = EmpiricalCDF {
        cdfMap            :: (Map a Int)
        , cdfLength        :: Int
        } deriving (Show, Read)

-- we have to store the length of the original list, because if there are
-- redundancies (several identical scores), Data.Map.fromList below will only
-- retain the last one.

empiricalCdf :: (Ord a) => [a] -> EmpiricalCDF a
empiricalCdf l = EmpiricalCDF   (Data.Map.fromList $ zip (sort l) [1..])
                                (Prelude.length l)

ecdf :: (Ord a) => EmpiricalCDF a -> a -> Float
ecdf ecdf val = case lookupLE val (cdfMap ecdf) of
        Nothing -> 0.0
        Just (value, rank) ->   (fromIntegral rank) /
                                (fromIntegral $ cdfLength ecdf)

