import Data.List (tails)
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M

substringsOfLen :: Int -> String -> [String]
substringsOfLen n = filter (\s -> length s == n) . map (take n) . tails  

kmerMap :: [String] -> Map String Int
kmerMap kmers = M.fromListWith (+) $ zip kmers (repeat 1)
 
-- TODO: make a k-mer map that remembers position (or just use a suffix array?)

ss = substringsOfLen 2 "cgacgatcgatgc"
