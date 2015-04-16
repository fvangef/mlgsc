import Data.HMM
import Control.Monad
import Data.Array
import System.IO

hmm1 = HMM  { states = [1,2]
            , events = ['A', 'G', 'C', 'T']
            , initProbs = ip
            , transMatrix = tm
            , outMatrix = om
            }

ip s
   | s == 1 = 0.1
   | s == 2 = 0.9

tm s1 s2
   | s1 == 1 && s2 == 1 = 0.9
   | s1 == 1 && s2 == 2 = 0.1
   | s1 == 2 && s2 == 1 = 0.5
   | s1 == 2 && s2 == 2 = 0.5

om s e
   | s == 1 && e == 'A' = 0.4
   | s == 1 && e == 'C' = 0.1
   | s == 1 && e == 'G' = 0.1
   | s == 1 && e == 'T' = 0.4
   | s == 2 && e == 'A' = 0.1
   | s == 2 && e == 'C' = 0.4
   | s == 2 && e == 'G' = 0.4
   | s == 2 && e == 'T' = 0.1
