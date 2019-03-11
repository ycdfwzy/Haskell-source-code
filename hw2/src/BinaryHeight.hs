module BinaryHeight where

import BinTree

height :: BinTree.BinTree a -> Integer
height Nil = 0
height (Node leftNode rightNode a) = max (height leftNode) (height rightNode) + 1
