-- H. Bayesian Classifier
module Trees  where

import Data.Tree

{-
data BinaryTree a       = Node {
                                treeData        :: a,
                                left            :: (BinaryTree a),
                                right           ::(BinaryTree a)
                        }
                        | Leaf {
                                treeData :: a
                        }
                        deriving (Show, Read)

instance Functor BinaryTree where
        fmap f (Leaf a)                 = Leaf (f a)
        fmap f (Node a t1 t2)           = Node (f a) (fmap f t1) (fmap f t2)

data RoseTree a	= RoseTree {
			rTreeData	:: a,
			kids		:: [RoseTree a]
		}
		deriving (Show, Read)

instance Functor RoseTree where
	fmap f (RoseTree a kids) = RoseTree (f a) (map (fmap f) kids)

-}


fringe :: Tree a -> [a]
fringe (Node tdata []) = [tdata]
fringe (Node tdata kids) = concatMap fringe kids

-- there is probably a more elegant way, but since I have written the 'fringe'
-- function anyway, let's use it...

toStarTree :: Tree a -> Tree a
toStarTree tree@(Node tdata kids) = Node tdata kidTrees
        where kidTrees = map (\dat -> Node dat []) (fringe tree)
