module Shuffle where

import System.Random
import qualified Data.List as L

randomGens :: StdGen -> Int -> [StdGen]
randomGens gen n = take n $ iterate (\gen -> fst $ split gen) gen

shuffleList :: StdGen -> [a] -> [a]
shuffleList gen list        = map fst orderedPairs
    where   orderedPairs    = L.sortBy num pairs
            pairs           = zip list rands
            rands           = take (length list) $ randoms gen :: [Int]
            num (ax,anum) (bx,bnum)
                | anum > bnum   = GT
                | anum < bnum   = LT
                | anum == bnum  = EQ

-- See
-- http://stackoverflow.com/questions/14692059/how-to-shuffle-a-list-in-haskell
-- except that it doesn't work :-)

shuffle :: StdGen -> [a] -> [a]
shuffle g xs = shuffle' (randoms g) xs

shuffle' :: [Int] -> [a] -> [a]
shuffle' (i:is) xs = let (firsts, rest) = splitAt (i `mod` length xs) xs
                     in (last firsts) : shuffle' is (init firsts ++ rest)


l1 = [1..6]
l2 = [100..110]
l3 = [50..60]

l = [l1, l2, l3]

{- now do something like:
gen <- getStdGen
gens = randomGens $ length l
zipWith shuffleList gens l
-}
