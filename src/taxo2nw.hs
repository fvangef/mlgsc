module Main where

import Data.Text

import MlgscTypes
import NewickDumper (treeToNewick)
import API (rawTree)

main :: IO ()
main = interact $ unpack . treeToNewick . rawTree Taxonomy
