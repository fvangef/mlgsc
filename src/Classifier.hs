module Classifier (fastARecordsToAlnMap) where

import Data.Tree

import MlgscTypes
import CladeModel

buildClassifier :: OTUToAlnMap -> OTUTree -> Tree mod
