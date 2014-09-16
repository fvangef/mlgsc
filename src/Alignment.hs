module Alignment (Alignment, AlnRow, fastARecordsToAln) where

import qualified Data.Text as T

data AlnRow = AlnRow {
                rowLabel    :: T.Text,
                rowChain    :: T.Text,
                rowWeight   :: Int
                }

type Alignment = [AlnRow]


