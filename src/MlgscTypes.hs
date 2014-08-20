module MlgscTypes where

import Data.Text
import Data.Tree
import qualified Data.Map.Strict as M

type Sequence       = Text
type Alignment      = [Sequence]
type Column         = Text
type Residue        = Char
type Position       = Int
type OTUName        = Text
type OTUToAlnMap    = M.Map OTUName Alignment
type OTUTree        = Tree OTUName
