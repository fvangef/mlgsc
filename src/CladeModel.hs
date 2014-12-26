-- A type class for clade models, i.e., a conserved sequence regions that should
-- be able to recognize a clade (be it a OTU, or a species, or whatever "rank").

module CladeModel (CladeModel(..), scoreOf, scoreSeq, modLength) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Binary (Binary, put, get, Get, Word8)

import MlgscTypes
import NucModel (NucModel, nucScoreOf, nucScoreSeq, nucModLength)
import PepModel

data CladeModel = NucCladeModel NucModel
                | PepCladeModel PepModel
                deriving (Show, Eq)

scoreOf :: CladeModel -> Residue -> Position -> Int
scoreOf (NucCladeModel nm) res pos = nucScoreOf nm res pos

scoreSeq :: CladeModel -> Sequence -> Int
scoreSeq (NucCladeModel nm) seq = nucScoreSeq nm seq

modLength :: CladeModel -> Int
modLength (NucCladeModel nm) = nucModLength nm

instance Binary CladeModel where
    put (NucCladeModel nm) = do
        put (0 :: Word8) >> put nm
    put (PepCladeModel pm) = do
        put (1 :: Word8) >> put pm
    
    get = do
        mol <- get :: Get Word8
        case mol of
            0 -> do
                nm <- get :: Get NucModel
                return $ NucCladeModel nm
            1 -> do
                pm <- get :: Get PepModel
                return $ PepCladeModel pm

-- TODO: factor out these two
