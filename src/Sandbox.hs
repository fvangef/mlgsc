import qualified Data.Text.Lazy as LT
import qualified Data.Text as ST
import qualified Data.Map.Strict as M
import Data.Binary (decodeFile)
import Data.Tree
import Control.Applicative

import MlgscTypes
import NewickParser
import PWMModel
import NucModel
import Classifier
import FastA
import Crumbs
import Weights
import Alignment

aln = [
    AlnRow (ST.pack "myOTU") (ST.pack "GCGTTAGC") 1,
    AlnRow (ST.pack "myOTU") (ST.pack "GAGTTGGA") 1,
    AlnRow (ST.pack "myOTU") (ST.pack "CGGACTAA") 1
    ]

nw = normalize $ alnRawWeights aln

updateWeight row weight = row {rowWeight = weight}

