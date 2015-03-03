module MlgscTypes where

import Data.Text
import Data.Tree
import qualified Data.Map.Strict as M

data Molecule = DNA | Prot
    deriving (Show, Eq, Read)

-- shouldn't this go into Classifier.hs?
data OutputData = OutputData {
                    trail   :: Trail,
                    score   :: Int
                    } deriving (Show)

type Sequence       = Text
type Column         = Text
type Residue        = Char
type Position       = Int
type OTUName        = Text
type CladeName      = Text
type OTUTree        = Tree OTUName
type ScaleFactor    = Double
type SmallProb      = Double
type Trail          = [(OTUName, Int, Int)]
