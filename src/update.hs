-- exploring ways of updating an array or map-like structure.

import System.IO

import Data.Char
import Data.Array.IO
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Control.Monad

import qualified Data.Map.Strict as SM
import qualified Data.Map as LM
import qualified Data.List as L
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LTIO

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
    putStrLn $ show $ map SM.! 'A'

increment :: Maybe Int -> Maybe Int
increment Nothing = Just 1
increment (Just a) = Just (a + 1)

-- Mutable array in the IO monad. Interestingly, this is NOT better than the
-- strict left fold of foldlAccumA: memory increases linearly, and the run is
-- longer. Profiling shows that most of the memory is taken up by Int, which
-- makes me suppose that the array is accumulating Ints. I tried to make it
-- strict by using seq, but to no avail. NOTE: should try with an unboxed array,
-- as these cannot hold thunks and are therefore, if I understood correctly,
-- strict.

ioArray :: IO ()
ioArray = do
    string <- hGetContents stdin
    array <- newArray ('A', 'z') 0 :: IO (IOArray Char Int)
    mapM_ (increment' array) string
    count_a <- readArray array 'A'
    putStrLn $ show $ count_a

increment' :: (IOArray Char Int) -> Char -> IO ()
increment' array c
    | (c >= 'A' && c <= 'z') = do
        count <- readArray array c
        writeArray array c (count `seq` count + 1)
    | otherwise = return ()


-- 2D problem: several strings of the same length (say L), count frequencies at
-- each position

-- use lines, transpose, and build a list of char -> count maps. Warning: this
-- is straightforward, but does not scale well! I suppose transposition is the
-- problem.

listOfMapsTranspose :: IO ()
listOfMapsTranspose = do
    columns <- liftM (L.transpose . L.lines) $ hGetContents stdin
    let mapList = L.map buildCounts columns
    putStrLn $ show $ (last mapList) SM.! 'C'

buildCounts :: String -> SM.Map Char Int
buildCounts column = SM.fromListWith (+) $ zip column $ repeat 1

-- use a single map, indexed by character and position. Fold over chars in a
-- a line, and over the lines in the file (no transposition). Memory consumption
-- is kept at a few percent, but takes more than a minute to process 12,000
-- lines of 8 kb each (which in fact is acceptable).

singleMap :: IO ()
singleMap = do
    lines <- liftM L.lines $ hGetContents stdin
    let map = L.foldl' addLine SM.empty lines
    putStrLn $ show $ map SM.! ('-', 5000)

addLine :: SM.Map (Char, Int) Int -> [Char] -> SM.Map (Char, Int) Int 
addLine map line = L.foldl' addChar map $ zip line [0..]

addChar :: SM.Map (Char, Int) Int -> (Char, Int) -> SM.Map (Char, Int) Int
addChar map (c, i) = SM.insertWith (+) (c, i) 1 map    

-- use a mutable, unboxed array in the IO monad. This is the best strategy so
-- far: almost no memory consumption (according to top), does the 12,000, 8kb
-- sequences in about 15" (with -O2).

iouArray :: IO ()
iouArray = do
    lines <- liftM L.lines $ hGetContents stdin
    let lineLength = length $ head lines
    array <- newArray ((0,0), (4, lineLength)) 0 :: IO (IOUArray (Int, Int) Int)
    forM_ lines $ \line -> do
        let lwi = zip line [0..]
        forM_ lwi $ \(c,i) -> do
            let ci = charIndex c
            count <- readArray array (ci, i)
            writeArray array (ci, i) (count + 1)
            return ()
        return()
    a <- readArray array (charIndex 'a', 5000)
    c <- readArray array (charIndex 'c', 5000)
    g <- readArray array (charIndex 'g', 5000)
    t <- readArray array (charIndex 't', 5000)
    d <- readArray array (charIndex '-', 5000)
    putStrLn ("A: " ++ (show a) ++ ", C: " ++ (show c) ++ ", G: " ++ (show g) ++ ", T: " ++ (show t) ++ ", -:" ++ (show d))

charIndex :: Char -> Int
charIndex c = case toUpper c of
    'A' -> 0
    'C' -> 1
    'G' -> 2
    'T' -> 3
    otherwise -> 4

-- Same idea, but using the ST monad. The advantage is that we can "escape" from
-- the ST monad (via runSTUArray, in this case) while we can't escape from IO.
-- The following function is therefore pure, even though some heavy updating is
-- going on in the ST monad.

freqArray :: [String] -> UArray (Int, Int) Int
freqArray lines = runSTUArray $ do
    let lineLen = length $ head lines
    array <- newArray ((0,0), (4, lineLen)) 0 :: ST s (STUArray s (Int, Int) Int)
    forM_ lines $ \line -> do
        let lwi = zip line [0..]
        forM_ lwi $ \(c,i) -> do
            let ci = charIndex c
            count <- readArray array (ci, i)
            writeArray array (ci, i) (count + 1)
            return ()
        return()
    return array

stuArray :: IO ()
stuArray = do
    lines <- liftM L.lines $ hGetContents stdin
    let array = freqArray lines
    let a = array ! (charIndex 'a', 5000)
    let c = array ! (charIndex 'c', 5000)
    let g = array ! (charIndex 'g', 5000)
    let t = array ! (charIndex 't', 5000)
    let d = array ! (charIndex '-', 5000)
    putStrLn ("A: " ++ (show a) ++ ", C: " ++ (show c) ++ ", G: " ++ (show g) ++ ", T: " ++ (show t) ++ ", -:" ++ (show d))

-- As above, but with lazy Data.Text instead of Strings

lazyTextFreqArray :: [LT.Text] -> UArray (Int, Int) Int
lazyTextFreqArray lines = runSTUArray $ do
    let lineLen = fromIntegral $ LT.length $ head lines
    array <- newArray ((0,0), (4, lineLen)) 0 :: ST s (STUArray s (Int, Int) Int)
    forM_ lines $ \line -> do
        countChar array line 0 
    return array

countChar _ [] _ = return ()
countChar array line index = do
    let c = LT.head line
    let ci = charIndex c
    count <- readArray array (ci, index)
    writeArray array (ci, index) (count + 1)
    return $ countChar array (LT.tail line) (index + 1)

lazyTextSTUArray :: IO ()
lazyTextSTUArray = do
    lines <- liftM LT.lines $ LTIO.hGetContents stdin
    let array = lazyTextSTUArray lines
    let a = array ! (charIndex 'a', 5000)
    let c = array ! (charIndex 'c', 5000)
    let g = array ! (charIndex 'g', 5000)
    let t = array ! (charIndex 't', 5000)
    let d = array ! (charIndex '-', 5000)
    putStrLn ("A: " ++ (show a) ++ ", C: " ++ (show c) ++ ", G: " ++ (show g) ++ ", T: " ++ (show t) ++ ", -:" ++ (show d))

main :: IO ()
main = stuArray
