module MlgscTypes where

import Data.Text
import Data.Tree
import qualified Data.Map.Strict as M

data Molecule = DNA | Prot
    deriving (Show, Eq, Read)

type Sequence       = Text
type Column         = Text
type Residue        = Char
type Position       = Int
type OTUName        = Text
type OTUTree        = Tree OTUName
type ScaleFactor    = Double
type SmallProb      = Double
