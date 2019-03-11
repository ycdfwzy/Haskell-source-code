module BinaryBalance where

import BinTree
import BinaryHeight

isBalance :: BinTree.BinTree a -> Bool
isBalance Nil = True
isBalance (Node leftNode rightNode a) = (isBalance leftNode) && (isBalance rightNode) && (abs ((height leftNode)-(height rightNode)) < 2)
