-- A type class for clade models, i.e., a conserved sequence regions that should
-- be able to recognize a clade (be it a OTU, or a species, or whatever "rank").

module PWMModel (PWMModel(..), scoreOf, scoreSeq, modLength, cladeName,
    absentResScore) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Binary (Binary, put, get, Get, Word8)

-- TODO: streamline imports
import MlgscTypes
import NucModel 
import PepModel

data PWMModel = NucPWMModel NucModel
                | PepPWMModel PepModel
                deriving (Show, Eq)

scoreOf :: PWMModel -> Residue -> Position -> Int
scoreOf (NucPWMModel nm) res pos = simpleNucScoreOf nm res pos
scoreOf (PepPWMModel spm) res pos = simplePepScoreOf spm res pos

scoreSeq :: PWMModel -> Sequence -> Int
scoreSeq (NucPWMModel nm) seq = simpleNucScoreSeq nm seq
scoreSeq (PepPWMModel spm) seq = simplePepScoreSeq spm seq

modLength :: PWMModel -> Int
modLength (NucPWMModel nm) = simpleNucModLength nm
modLength (PepPWMModel spm) = simplePepModLength spm

absentResScore :: PWMModel -> Int
absentResScore (NucPWMModel nm) = simpleNucAbsentResScore nm
absentResScore (PepPWMModel spm) = simplePepAbsentResScore spm

cladeName :: PWMModel -> CladeName
cladeName (PepPWMModel spm) = simplePepCladeName spm
cladeName (NucPWMModel nm) = simpleNucCladeName nm

instance Binary PWMModel where
    put (NucPWMModel nm) = do
        put (0 :: Word8) >> put nm
    put (PepPWMModel pm) = do
        put (1 :: Word8) >> put pm
    
    get = do
        mol <- get :: Get Word8
        case mol of
            0 -> do
                nm <- get :: Get NucModel
                return $ NucPWMModel nm
            1 -> do
                pm <- get :: Get PepModel
                return $ PepPWMModel pm

-- TODO: factor out these two
