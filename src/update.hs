-- exploring ways of updating an array or map-like structure.

import System.IO

import Data.Array.IO

import qualified Data.Map.Strict as SM
import qualified Data.Map as LM
import qualified Data.List as L

-- 1D problem: take a string, and count every character

-- Using a lazy map, memory usage grows linearly until the very end. With the
-- following command, memory exceeds 300Gb!
-- ./update +RTS -hy -p -K100M < <(head -100000 ../data/ms-rev-16S-train.fasta)
-- This reads 10,000 lines, and build the Char -> Int map, but due to lazy
-- evaluation the sums (+) are not evaluated until we ask for map LM.! 'a' at
-- the end. The effect is to accumulate terms of a huge summation expression.
-- The profile graph shows this: all the memory is used up by (+).

lazyMap :: IO ()
lazyMap = do
    string <- hGetContents stdin    
    let map = LM.fromListWith (+) $ L.zip string $ repeat 1 
    putStrLn $ show $ map LM.! 'a'

-- Using a _strict_ map, with the same command, memory never exceeds 100 kb:
-- that's because the values are evaluated as soon as possible

strictMap :: IO ()
strictMap = do
    string <- hGetContents stdin    
    let map = SM.fromListWith (+) $ L.zip string $ repeat 1 
    putStrLn $ show $ map SM.! 'a'

-- Using a map as a right fold accumulator. Needs +RTS -K1000M, else causes a
-- Stack overflow. Cost center heap use rises sharply (linearly) for 1/4 th of
-- the ttime, then falls (also linearly) back to zero for the remaining 3/4.

foldrAccumA :: IO ()
foldrAccumA = do
    string <- hGetContents stdin
    let map = L.foldr (SM.alter increment) (SM.empty) string
    putStrLn $ show $ map SM.! 'a'

-- Same, but with a strict fold (has to be a left fold, as I understand). Left
-- and right folds have different signatures, hence the flip. In this case,
-- memory consuption never exceeds 100 kb, and speed is not perceptibly
-- different from the previous approach.

foldlAccumA :: IO ()
foldlAccumA = do
    string <- hGetContents stdin
    let map = L.foldl' (flip $ SM.alter increment) (SM.empty) string
    putStrLn $ show $ map SM.! 'a'

increment :: Maybe Int -> Maybe Int
increment Nothing = Just 1
increment (Just a) = Just (a + 1)

-- Mutable array in the IO monad. Again, memory usage is imperceptible by top,
-- and this is no slower than the above.

ioArray :: IO ()
ioArray = do
    string <- hGetContents stdin
    array <- newArray ('A', 'z') 0 :: IO (IOArray Char Int)
    mapM_ (increment' array) string
    count_a <- readArray array 'a'
    putStrLn $ show $ count_a

increment' :: (IOArray Char Int) -> Char -> IO ()
increment' array c = do
    count <- readArray array c
    writeArray array c (count + 1)

main = ioArray
