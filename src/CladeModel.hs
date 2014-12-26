-- A type class for clade models, i.e., a conserved sequence regions that should
-- be able to recognize a clade (be it a OTU, or a species, or whatever "rank").

module CladeModel (CladeModel, scoreOf, modLength) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import MlgscTypes
import NucModel (NucModel, nucScoreOf, nucScoreSeq, nucModLength)
import PepModel

data CladeModel = NucCladeModel NucModel
                | PepCladeModel PepModel

scoreOf :: CladeModel -> Residue -> Position -> Int
scoreOf (NucCladeModel nm) res pos = nucScoreOf nm res pos

modLength :: CladeModel -> Int
modLength (NucCladeModel nm) = nucModLength nm
