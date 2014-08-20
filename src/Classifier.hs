module Classifier (fastARecordsToAlnMap) where

import CladeModel

data Classifier mod = (CladeModel mod) => Tree mod
