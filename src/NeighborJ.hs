module NeighborJ where

import Data.Tree
import Data.List

-- Join two neighbors, identified by their label. It is assumed that exactly one
-- node of each label is found among the tree root's _children_.

joinNeighbors :: (Eq a, Show a) => Tree a -> a -> a -> a -> Tree a
joinNeighbors (Node origLbl origKids) lbl1 lbl2 newLbl = Node origLbl newKids
    where   newKids = joinedPair : rest
            joinedPair  = Node newLbl [kid1, kid2]
            rest    = origKids \\ [kid1, kid2]
            kid1 = case find (\ n -> rootLabel n == lbl1) origKids of
                    Just k  -> k
                    Nothing -> error ("label " ++ (show kid1) ++ " not found")
            kid2 = case find (\ n -> rootLabel n == lbl2) origKids of
                    Just k  -> k
                    Nothing -> error ("label " ++ (show kid2) ++ " not found")
