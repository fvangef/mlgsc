-- a program for testing the alignment algorithm
{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import Data.Text as ST
import Data.Text.IO as STIO
import Data.Text.Lazy as LT

import FastA
import Alignment
import NucModel
import PWMModel
import Align

smallprob = 0.0001
scale = 1000

main = do
   [mod, seq] <- getArgs 
   let modf = FastA "mod" $ LT.pack mod
   let moda = fastARecordsToAln [modf]
   let modn = alnToNucModel smallprob scale "model" moda
   let modc = NucPWMModel modn 
   let alnres = msalign defScScheme modc $ ST.pack seq
   STIO.putStrLn alnres
